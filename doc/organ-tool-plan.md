# Organ-Bank → Intelli-Monad tools: review of the concept, and how to pursue it

*Written 2026-09-05, after the conversation log in `full_conversation_log.md` was checked
against the actual repos (`~/src/organ-bank` @ `8b7b5ab` clean, `~/src/frankenstein` @
`82ac893` with local MlirEmit work in flight). Every claim below was verified in-source
unless marked otherwise.*

---

## 1. The big finding: the proposed pipeline is already ~80% built

The log's central proposal — a four-stage analysis pipeline
(RCT → GS-IR → PA-IR → OrganIR) feeding LLM tools — reads like a design for new
work. Ground truth: **organ-bank already IS that pipeline**, just not yet pointed at
an LLM consumer:

| Log's stage | What organ-bank already has |
|---|---|
| RCT (resilient concrete tree) | The 29 shims/frontends: each invokes the real compiler (`System.Process`), harvests its IR (CoreFn, SIL, HLDS dumps, Core Erlang, …), parses it |
| GS-IR (skeleton, decls only) | `OrganIR.Definition` with `Sort = SFun \| SVal \| SExternal \| SCon`, `Visibility`, types without bodies |
| PA-IR (paradigm-abstract) | `EffectRow` (`erEffects :: [QName]`, `erTail`) + `EffectDecl`/`Operation` — algebraic effects are **already first-class in the IR**, exactly as the Koka-influenced design intends |
| OrganIR (unified graph) | `organ-ir` itself: typed, schema'd (`spec/organ-ir.schema.json`), JSON + binary, round-trip/property/fuzz-tested |

So the correct framing is not "build a pipeline" but **"expose an existing pipeline
to a new consumer"**. That is a much smaller, much safer project — and it is
*orthogonal to compiler stability*, which was the user's own hard constraint in the
log ("side scripts on the side of a largely monolingual project").

## 2. Corrections to the transcript

Things the log's model got wrong or oversold, worth recording so the plan doesn't
inherit them:

1. **The Haskell code sketches are not intelli-monad's API.** The `:>`/`:<|>`
   type-DSL with `runIntelliMonadWithTools` does not exist in the repo and does not
   match the real extension point. The actual pattern is: a record deriving
   `JSONSchema` + `A.FromJSON`, an `instance HasFunctionObject` (name, description,
   per-field descriptions), an `instance Tool` with `data Output` and `toolExec`,
   then one `ToolProxy` line in `IntelliMonad/Tools.hs::defaultTools`. The MCP
   surfaces (`mcp-serve`, Bridge) pick the tool up automatically. The log's sketches
   are fine as *intent*, wrong as *code*.
2. **organ-bank is not a Haskell library you link into tools.** It is `organ-ir`
   (a library) + 29 separate executable shims that emit JSON on stdout. The tool
   consumes the JSON artifacts (or runs `organ-extract` as a subprocess) — that's
   the integration seam, and it's a good one (the log's "native Haskell pipeline"
   framing oversimplifies; and direct in-process linking would re-introduce the
   text-version conflicts frankenstein already hit, see §5).
3. **Cross-language checking doesn't exist yet — the effects story included.**
   `frankenstein/docs/ffi-cross-language.md` states plainly: uniform `i64` ABI,
   name-based linking, no cross-language type discipline. The *representation* for
   one (`EffectRow`, `Ty` with multiplicity) exists in OrganIR; the *checker* does
   not. So "the tool narrates effect mismatches" is a real, cheap deliverable —
   but it must be described as surfacing the rows and flagging set differences,
   not as sound checking. That soundness gap is precisely where the
   Patterson–Mushtak–Wagner–Ahmed work would land (§4).
4. **Tree-sitter's role should be demoted.** The log wavers between "tree-sitter
   skeleton indexer" and "reuse the compiler front ends". For 26 languages with
   real compilers already wired as shims, tree-sitter adds a second, weaker parser
   for languages that already have first-class extraction. Its one legitimate
   niche: languages whose shim is missing or heavy, and instant re-indexing of
   half-edited buffers. Treat it as a fallback RCT, not the architecture.
5. **The "nothing like this exists" framing.** Poplog, CLR, Graal/Truffle, and
   MLIR itself are the honest company; Cāngjié as described in the log is roughly
   right (interop + coroutines, no unified effect-typed IR). The novel combination
   here is: effect-typed unified IR + 26 front ends + *link-time* (not runtime)
   separate compilation + now an LLM observability consumer. The write-up should
   claim that combination, not uniqueness of the parts.

## 3. The sharp version of the concept

Build a **read-only observability sidecar**: an `organ-index` executable (in
organ-bank, next to `organ-extract`/`organ-diff`) that ingests OrganIR JSON and
maintains an SQLite index, plus `IntelliMonad.Tools.OrganBank` tools that query it.
Nothing in this path needs frankenstein's compiler, linker, or MLIR emitter to work
at all. A project can be half-written, a shim can crash mid-extraction, and the
tool still serves everything that survived.

Tool set (each is small; schemas generated by the existing `JSONSchema` class):

| Tool | Serves | Source of truth |
|---|---|---|
| `organ_repo_map` | token-budgeted skeleton map: module → defs (sort, visibility, type headline, effect row), no bodies | `symbols` table |
| `organ_find_symbol` | defs + refs for a name, **across languages** (name-based matching today, lattice later) | `symbols`, `refs` |
| `organ_check_boundary` | two QNames → compare `Ty` + `EffectRow`, report mismatch in prose ("caller expects pure; callee propagates `ND`") | `boundary` queries |
| `organ_diagnostics` | last extraction's per-file errors/warnings, per shim | `diagnostics` |

Schema (SQLite, WAL — same store pattern intelli-monad already uses for sessions):

```sql
CREATE TABLE symbols(   -- one row per Definition
  qname TEXT PRIMARY KEY,           -- "module/name"
  module TEXT NOT NULL,
  name TEXT NOT NULL,
  sort TEXT NOT NULL,               -- fun|val|external|con
  visibility TEXT NOT NULL,
  lang TEXT NOT NULL,               -- SourceLang tag
  file TEXT, start_line INTEGER,
  ty_json TEXT,                     -- Ty as OrganIR JSON (lossless, not a re-parse)
  effects_json TEXT                 -- EffectRow as OrganIR JSON
);
CREATE TABLE refs(name TEXT, module TEXT, file TEXT, line INTEGER, lang TEXT);
CREATE TABLE diagnostics(shim TEXT, file TEXT, severity TEXT, message TEXT, raw TEXT);
CREATE INDEX idx_symbols_name ON symbols(name);
```

Types are stored as **OrganIR JSON verbatim** — never flattened into a lossy
relational model — so the boundary checker (and later the PMWA checker) reads the
same bytes the compiler consumes. The index is a view, not a re-modeling.

## 4. The PMWA connection (rescuing the lost attempt, incrementally)

The user noted an earlier Patterson–Mushtak–Wagner–Ahmed foreign-call typechecking
attempt whose code is now lost (probably uncommitted). Rather than re-attempting
full unification, grow it inside organ-ir as `OrganIR.Boundary`, driven by the open
questions frankenstein's own doc already lists (FFI type lattice granularity,
`anyType` fallbacks, effect-row reconciliation, refinement vs unification,
ascription visibility, boundary multiplicity):

1. **Stage 0 (exists):** name-based matching — the linker already does this.
2. **Stage 1 (tool-only):** descriptive reconciliation — compare two boundary
   signatures, classify the mismatch (arity, base-type conflicts, effect-row
   differences, multiplicity), emit prose. No soundness claim; pure observability.
   This is `organ_check_boundary` and needs no compiler buy-in.
3. **Stage 2:** a *decision procedure* over a small lattice (`i64`-ish base types +
   `anyType` + effect rows as sets) that returns compatible / incompatible /
   needs-ascription. Compiler can adopt it in the linker; tool serves it as a
   strict mode.
4. **Stage 3:** refinement-style cross-language ascriptions per the paper, where
   the tool's mismatch reports become the test corpus (every mismatch the LLM
   surfaces is a test case the checker must eventually classify).

The LLM tool is then not just a consumer of the compiler — it is the *fuzzing
harness* for the typechecker.

## 5. Phased plan

**Phase A — tool-side only (intelli-monad + a new organ-index script).**
No organ-bank changes; works from whatever OrganIR JSON artifacts exist
(`spec/organ-ir-example.json`, shim outputs, `organ-playground` saves).
Deliverables: SQLite index, the four tools, wired into `defaultTools` (hence
REPL + `mcp-serve` automatically), unit tests in the existing suite style,
token-compression measured honestly (repo-map bytes vs raw source bytes).

**Phase B — organ-bank, compiler-orthogonal.**
(a) Diagnostic ingestion sweep: give shims the
`Success | PartialFailure a [Diagnostic] | FatalFailure [Diagnostic]` contract —
capture stdout + stderr even on nonzero exit, emit partial OrganIR plus a
diagnostics JSON. This is the user's own stated priority ("make sure it's capturing
and passing along what error messages are now happening") and helps humans and the
tool identically. 29 shims, mechanical. (b) `organ-index` promoted to an
organ-bank executable once it has proven itself in Phase A.

**Phase C — research (optional, the fun one).**
`OrganIR.Boundary` stages 2–3 above; the tool's mismatch log becomes the corpus.
Parallel: frankenstein's own `frankenstein.directions` Phase 6e — the
text-2.1.3/2.1.4 conflict blocking `OrganIR.Consumer` — is worth clearing since
the *compiler's* consumer of organ-bank is exactly the seam this tool family
mirrors.

**Not on the critical path:** contacting Leijen / Patterson / Ahmed is reasonable
after there's an artifact — a working effect-typed IR consumer plus a mismatch
corpus is a far better email than a description. Leijen's likely interest
(Perceus threading across 26 runtimes, FIP at scale) and Patterson/Ahmed's
(link-time interop typing actually implemented) are both genuinely served by
having Phase A/C artifacts first.

## 6. What this deliberately does not do

- No tree-sitter dependency (fallback only, if ever).
- No soundness claims for the tool's boundary reports until Stage 2 exists.
- No new IR stages: the log's RCT/GS-IR/PA-IR re-derivations are already organ-bank
  modules; introducing parallel stages would fork the one source of truth.
- No coupling to frankenstein's emitter/linker health — the tool path consumes
  shim JSON only.

---

## Phase A — COMPLETE (2026-09-05, commit c9ebdbc on feat/mcp-client)

**Shipped:** `IntelliMonad.Tools.OrganBank` — five tools, all in `defaultTools`,
so both the REPL (`:mcp add`) and `mcp-serve` expose them:

| Tool | Args | Purpose |
|---|---|---|
| `organ_ingest` | `oiPath` | Ingest OrganIR JSON file/dir into the SQLite index |
| `organ_repo_map` | `ormLang?`, `ormModule?` | Compact per-(lang, module) skeleton map |
| `organ_find_symbol` | `ofsName` | Cross-language defs/refs lookup |
| `organ_check_boundary` | `ocbModuleA/NameA/LangA?`, `ocbModuleB/NameB/LangB?` | Descriptive type/effect/arity mismatch report |
| `organ_diagnostics` | `odPath?` | Per-file ingestion diagnostics |

**Design decisions locked in by evidence:**
- Index key is `(lang, module, name)` — 25 languages each define `Factorial/factorial`;
  a bare-name key collapses them (found by e2e, fixed).
- Repo-map grouping is per `(lang, module)`, not bare module name; empty modules
  emit no header. Extracted as pure `renderRepoMap` with regression tests.
- Types stored verbatim as JSON; headlines rendered at query time.
  Effect rows are bare qnames in real organ-ir (unlike con/var-tagged types) —
  renderer matches reality.
- Filters use the `(? IS NULL OR col = ?)` pattern via `sqlMaybeText`.

**Validation:** 25/25 real example files ingest with zero diagnostics; live
`mcp-serve` stdio session serves 7 tools with schemas; filtered and unfiltered
queries return correct per-language blocks. Suite: 109 hspec + 70 doctests green
on 9.8.4 (LoopbackSpec T6 updated to the 7-tool surface).

**Next (Phase B, organ-bank side):** diagnostics-ingestion sweep across the 29
shims — capture stderr/diagnostics each shim already produces into the OrganDoc
`diagnostics` field, so `organ_diagnostics` reports compiler-level failures, not
just JSON parse errors.

## Phase B — diagnostics sidecar (2026-09-05, COMPLETE)

Both sides landed and verified end-to-end:

- **organ-bank** `6d47f07`: lua/forth/erlang frontends moved to the
  stderr+exitFailure shim convention; `organ-extract --diag FILE|DIR|-`
  emits a self-describing envelope (`organ_diag_version: "1"`) with one
  row per file: path/language/shim/status/exit_code/stderr/warnings —
  including *warnings on success*, which were previously dropped
  entirely. Missing shims are diagnostic rows, not crashes. Manifest
  mode also had a latent quote-parsing bug (never worked); fixed.
- **intelli-monad** `c60dba0`: `organ_ingest` sniffs the envelope key
  and routes to the diagnostics ingester (severity mapped
  ok→ok/warning, error→error; stderr kept verbatim). Four OrganSpec
  tests; 113 examples, 0 failures on 9.8.4.
- **Gold E2E over the real MCP wire**: fresh index, real organ-extract
  envelope (7 rows: ok ×2, parse error, missing-shim ×2, file-not-found
  ×2) → `organ_ingest` (1/0) → `organ_diagnostics` returns verbatim
  stderr for every row.

Design decisions locked by evidence:
1. Envelope is self-describing (`organ_diag_version`) — the ingest
   sniff needs no flags and old envelopes ingest as tolerable-extra.
2. Diagnostics never block module ingestion — one tool surface
   (`organ_ingest`), two artifact kinds.
3. Severity mapping is lossless only for stderr; structured warnings
   survive because the envelope carries them even on success.
