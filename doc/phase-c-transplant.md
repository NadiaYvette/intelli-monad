# Phase C — Transplant: from licensed boundaries to generated calls

Status: planning. Phases A (index + query tools) and B (diagnostics
sidecar) are complete; the representation dictionary
(`IntelliMonad.Tools.OrganBank.Dictionary`) landed 2026-09-06 and
`organ_check_boundary` now licenses or refuses crossings against an
auditable axiom table.

## Where the licensing layer stands

`organ_check_boundary` verdicts, in priority order:

1. `mismatch` — arity or effect-row differences make the call
   structurally impossible; outranks everything (and guards the
   zip-truncation hazard in argument comparison).
2. `unlicensed-boundary` — the dictionary layer ran and refused:
   narrowing, overflow-domain, family mismatch, implementation-defined
   precision, or an unproven qname (fail closed).
3. `identical` — structurally identical JSON in a descriptive
   (language-less) comparison.
4. `licensed-lossless` / `licensed-widening` /
   `licensed-with-runtime-checks` — the crossing is admitted with the
   weakest-link aggregate over all argument positions plus the
   direction-aware result position (returns flow callee→caller).
5. `differs-in-shape` — descriptive fallback when the dictionary was
   not consulted (unknown languages, non-function shapes).

The axiom table covers C, Rust, Zig, Haskell, OCaml, F#, Julia, Swift,
PureScript, Koka, Fortran (GNU defaults), SML/Mercury (honestly
`Nothing`-width), Lean/Agda (`FBigInt`), plus the `.../any` dynamic
marker for every language. Live-verified against the 25-language
spec-example corpus: Haskell→C `Int#→int32` refused (arg narrowing),
Haskell→Rust `Int#→i64` licensed-lossless, Haskell→Lean refused
(family).

## What transplant adds

The index tells an LLM *what exists*; licensing tells it *what may
cross*. Transplant is the code that makes the crossing happen:

1. **Stub generation** — given a licensed boundary, emit the pair of
   glue functions (caller-side wrapper, callee-side wrapper) plus the
   marshaling between the dictionary-licensed representations. Only
   `licensed-*` verdicts may generate; `unlicensed-*` must refuse with
   the cited axioms (the refusal is the feature).
2. **Island runtime** — one OS thread per language island, each
   runtime (GHC RTS, Koka/Perceus RC state, BEAM, …) initialized once
   per process; messages cross only through the stub layer. No shared
   heaps, no cross-runtime GC pointers.
3. **Effect reconciliation** — OrganIR effect rows decide the calling
   convention across the boundary: pure callees may be called
   synchronously; effectful ones (Koka `{div,exn}`, Mercury `exception`)
   force the stub to declare how the effects surface (exceptions,
   error codes, or explicit capability passing).

## Milestones

C1 — Stub generator skeleton (pure, testable): `Licensed a b → [Text]`
emitting a C-ABI pair for the two fixtures the dictionary already
licenses (haskell `Int#` ↔ rust `i64`; C `int32` widened into `i64`
with same-width return). Diff-able output, no compiler invocation.

C2 — Real object code for one pair: compile the C-ABI stubs, load both
islands in one process, call factorial across the boundary, check the
result. This is the first point where GHC RTS and a Rust binary
coexist in one process — scope it to that pair and nothing else.

C3 — Perceus/Koka island: the effect-row test case. Pure Koka calls
cross synchronously; a `{div,exn}` call must force the stub to map the
exception to the caller's convention. This validates that the
dictionary's effect metadata is load-bearing, not decoration.

C4 — PMWA-shaped verification, per-boundary: with C2/C3 working, write
down the interop criterion for *that* pair (representation dictionary
+ calling convention + effect mapping) as a checkable claim. Full
multi-language soundness proofs stay out of scope; each new pair pays
its own verification cost.

## Non-goals

- No N²-pair verification: pairs are licensed individually, and the
  dictionary's axioms are the shared vocabulary, not a proof.
- No shared-heap or cross-runtime pointer passing, ever.
- No dynamic-language transplants without runtime checks; `licensed-
  with-runtime-checks` means the stub generates checkers, not trust.

## C1 — DONE (2026-09-06)

`IntelliMonad.Tools.OrganBank.Stubs` is the stub generator skeleton,
pure and compiler-free:

- `planBoundary` re-runs the dictionary licensing (same axioms, same
  direction rules as organ_check_boundary, so the two cannot
  disagree) and either refuses with cited axioms or yields a
  `StubPlan`. Aggregate refusal is the weakest link; an empty
  position list refuses as `unlicensed-empty`.
- `renderCStubs` emits deterministic C-ABI text: a caller-side island
  wrapper (caller-typed params/return, `extern` declaring the callee's
  ABI, explicit conversion casts), marshal notes per position, and a
  callee-side wrapper whose trampoline is C2's to fill. Refusals
  render as comment-only debris — no compilable artifact survives an
  unlicensed crossing.
- The two licensed fixture pairs are pinned by exact-render golden
  tests: haskell `Int#` ↔ rust `i64` (lossless), C `int32` widened
  into `i64` (widening; the sign-extension note is part of the gold).
- Direction convention documented in the module header: positions
  list members in value-flow order, so arguments are caller→callee
  and the result is callee→caller — the same rule the boundary tool
  licenses with.

Suite: 148 hspec examples + 88 doctests, 0 failures on 9.8.4.

Next: C2 compiles the C fixture for real (one process, two islands),
C3 adds the Koka effect-row case, and the dictionary's canonical-core
entries (`core/bool`, `core/unit`, `core/text`) await a first shim
adoption.

## C2 spike — DONE (2026-09-06)

The full loop ran live, end to end, in one process:

1. **Wire**: two OrganIR docs with genuine dictionary qnames
   (haskell `ghc-prim/Int#`, rust `std/i64`) ingested through the real
   `mcp-serve` binary; `organ_plan_stub` returned `licensed-lossless`
   and the rendered caller-side C in both role orders (haskell→rust
   and rust→haskell).
2. **Compile**: the generated `caller.c` compiled by gcc unchanged;
   the Rust island built with `rustc --crate-type staticlib`; the
   Haskell island with `ghc -c`; the whole linked with
   `ghc -no-hs-main` so base/ghc-prim/RTS resolve.
3. **Run**: the host called all three paths and every value was
   correct:

   | path | result |
   |---|---|
   | C host → generated glue → rust island | 10! = 3628800 |
   | GHC island → wire-generated glue → rust island | 12! = 479001600 |
   | GHC island export (direct) | 7! = 5040 |

### Design finding: bridge names are a namespace of their own

The spike's first link failed with a multiple-definition error: the
Haskell island's `foreign export` used the glue symbol's name
(`omni_haskell_Factorial_factorial`), colliding with the generated
caller wrapper. Rule for C2's generator: **bridge symbols are named
after the crossing (caller + callee + `omni_` prefix), while island
exports keep their own names** — the callee trampoline must call the
island's *actual* export, never assume it shares the bridge name. The
callee-side skeleton stays `extern`-light for exactly this reason: C2
must learn the callee island's real entry point, not invent one.

### What C2 proper still owes

The spike hand-filled the callee trampoline
(`/tmp/c2spike/callee_trampoline.c`); the generator's callee side
still emits the `/* C2: trampoline */` placeholder. C2's remaining
work is to make `renderCStubs` emit the filled form given the callee
island's real export name, plus an RTS-init contract (which island
calls `hs_init`, and when) for GHC-hosted processes.

## C3 gold: the Koka effect-row crossing, compiled and run (2026-09-06)

`examples/c2-spike/run_koka.sh` closes the C3 milestone with a live
loop in the direction the subset rule *licenses*: a pure rust island
(`std/i64`, caller row ∅) calls a real koka island (`std/core/int`,
effect row `<div,exn>`), both through the wire:

```
INGEST: 2 ingested, 0 failed
VERDICT: licensed-lossless
C host -> wire glue -> koka island:      5! = 120 ok
rust island -> wire glue -> koka island: 6! = 720 ok
adapter direct:                          7! = 5040 ok
```

The reverse direction (effectful caller → pure callee) stays refused by
`planBoundary`'s `unlicensed-effect-row` verdict — pinned in
`test/StubSpec.hs`.

### What the gold taught (recipes now in run_koka.sh)

- **Koka's C ABI is `kk_integer_t` + `kk_context_t*`**, so the island
  needs an *adapter* (like the GHC island needs its Haskell-side
  projection): `factorial_kk_adapter.c` owns `int64 → kk_integer_t`
  (`kk_integer_from_int64` / `kk_smallint_from_integer`), includes the
  compiler-generated `factorial.h` instead of re-declaring symbols, and
  exposes `kk_island_factorial(int64_t)` — the name passed as
  `opsCalleeExport`, which the filled trampoline then calls.
- **Koka's init contract**: `kk_main_start` → module init chain
  (`kk_factorial__init`, statically guarded + idempotent) → calls →
  `kk_factorial__done`. Unlike GHC, koka does **not** need to be the
  process main: the gold links with plain `gcc` while GHC's loop needs
  `ghc -no-hs-main`.
- **Compile flags are part of the ABI.** kklib must be compiled
  exactly as koka compiles it or the ABI tears: `kklib.h` leaks
  `mi_heap_t` into `kk_context_t` under `KK_MIMALLOC`, so adapter/host
  TUs need `-DKK_MIMALLOC=8` *and* the mimalloc include dir;
  `KK_COMP_VERSION`/`KK_CC_NAME` are quoted C string literals
  (`'-DKK_COMP_VERSION="3.2.3"'`). The runtime itself is kklib's
  `src/all.c` compiled to one `kklib.o` (library mode `-l` does *not*
  emit it). The recipe was captured byte-faithfully with a `--cc`
  wrapper shim rather than reconstructed.
- **`koka -c -l` (library mode)** compiles the module + std_core*.o
  without a `main` trampoline, but still *typechecks* a main; the
  island carries a never-called `dummy-main`. Koka derives the module
  name from the source *path* — compile from a relative filename or
  the symbols become `examples_c2_spike_factorial_*`.
- **Koka's reader is UTF-8-strict** and its comments are `//` (not
  `--`): em-dashes in comments fail the build.

### Dictionary accuracy: koka std/core/int

The dictionary entry previously claimed "int is 64-bit two's
complement" — wrong in mechanism, right in range. kklib's actual
encoding: unboxed smallints carry a 63-bit payload (KK_TAG_BITS = 1
over a 64-bit `kk_intf_t`), and `int` is *arbitrary precision* via
heap bigints beyond that. Every int64 value is representable, so the
member width stays 64 and the citation now states the real encoding;
the FBig (arbitrary-precision) member family has since landed — see
the FBig section below.

## Generated ABI adapters, the wire refusal demo, and the FBig families (2026-09-06)

Three hardening milestones landed after the C3 gold (distinct from the
C4 PMWA milestone defined above — this is spike-era hardening, not the
per-pair verification criterion):

### 1. Generator-emitted ABI adapters

`renderCStubs` can now emit the island-side ABI projection itself —
the file the C3 gold still hand-wrote. `emitAdapter` produces, per
request (`srCalleeAdapter = Just <entry>`):

- **koka islands**: a C adapter that includes the compiler-generated
  `factorial.h` (never re-declares the ABI), owns the koka runtime
  contract (`kk_main_start` + the statically-guarded module init/done
  pair), and projects `int64_t ↔ kk_integer_t` via
  `kk_integer_from_int64` / `kk_smallint_from_integer`. Lifecycle
  symbols are namespaced `omni_kk_<module>_island_init/done` —
  deterministic from the module name and collision-safe when several
  generated adapters link into one host.
- **GHC islands**: includes the compiler's capi header
  (`<Module>_api.h`) so `StgInt`'s target spelling comes from GHC
  itself, and casts through `HsInt64`. The RTS contract (`hs_init` /
  `hs_exit`) deliberately stays with the host, which links via
  `ghc -no-hs-main`.
- **Anything else**: `Nothing` — fail-closed, keeping the one-line FFI
  export convention from C2/C3.

The adapter is gated on an explicit request (`srCalleeAdapter`); the
default plan stays byte-identical to C2/C3 output.

**The C4-era gold loop** (`examples/c2-spike/run_koka_generated.sh`)
links a rust island → wire glue → filled trampoline → *generated*
koka adapter → real koka island with zero hand-written glue; the C3
hand-written adapter becomes a reviewed diff against the generated
one (`host_koka_gen.c` is the only new hand file, and it only
declares the namespaced lifecycle symbols).

### 2. Reverse-direction refusal, over the real wire

`examples/c2-spike/run_refused.sh` drives the refusal direction
through the real `mcp-serve` binary (previously only pinned in unit
tests): the effectful koka island asks to call the pure rust island
and `organ_plan_stub` returns `unlicensed-effect-row` with comment-only
debris explaining the subset rule — no callable code is emitted.

### 3. FBig member families (arbitrary-precision domains)

The dictionary gains `FBigSigned` / `FBigUnsigned` member families for
genuinely arbitrary-precision types (koka `int`'s bigint spill beyond
the 63-bit smallint payload, GHC `Integer`, ...):

- same-family crossings license as `licensed-lossless` (a box swap,
  with the unsigned→signed zero-extend note);
- bigint ↔ fixed-width is a `unlicensed-representation` refusal —
  a *representation* mismatch, never a width guess;
- the koka `int` ABI entry stays `FSigned(64)` (every int64 is
  representable), which the C3 gold depends on;
- Lean4/Agda `Nat` are `FBigUnsigned`, so bigint-signed ↔ `Nat`
  refuses as `overflow-domain` (negatives do not transfer).

This retires the "FBig TODO" note at the end of the C3 section.

## The C3 vision closed, the multi-island gold, and the C4 PMWA criterion (2026-09-06)

All three remaining Phase C threads landed together, because the third
depends on the first two:

### 1. C3 closed: the generator-emitted effect map

The C3 milestone's open note — "a `{div,exn}` call must force the stub
to declare how the effects surface" — is now *generated*, not declared.
`organ_plan_stub` takes `opsEffectMap: true`; for a koka island the
plan carries an island-side shim (`factorial_emap.kk`) whose
`handle/try` maps any island exception to the wire's status sentinel
(`min-int64`), plus an adapter that forwards to the mapped entry.

Two ABI facts (proven live, then pinned by tests):
- koka unboxes `int64`, so the mapped entry's ABI is literally the
  wire's: `int64_t kk_factorial__emap_mapped_island_factorial(int64_t,
  kk_context_t*)` — the adapter forward needs no `kk_integer` boxing;
- koka doubles a module name's underscore in C symbols
  (`factorial_emap` → `kk_factorial__emap_*`, header
  `factorial__emap.h`); the generator mangles accordingly.

`run_koka_mapped.sh` runs the whole chain through the real wire —
value paths (`5! = 120`, `6! = 720`, `7! = 5040`) and the error path:
the demo island now genuinely *throws* on a negative argument
(`throw("island factorial: negative argument")`), the generated shim
catches it, and the host observes the sentinel `-9223372036854775808`
over plain int64. No setjmp/longjmp, no error thread, no hand-written
glue. The island's `<div,exn>` row is now fully honest: the `exn` is
really used.

### 2. The multi-island gold: one process, three runtimes

`run_multi.sh` links **C host + GHC RTS + kklib + rust** into one
process where every crossing rides `organ_plan_stub` output:

```
C host -> plan A glue -> koka island        5! = 120      ok
rust island -> plan A glue -> koka island   6! = 720      ok
rust island -> plan B glue -> GHC island    7! = 5040     ok
GHC island direct (hs_factorial)            8! = 40320    ok
koka island direct (adapter entry)          9! = 362880   ok
```

Two wire calls plan the two crossings (`rust→koka` with the generated
adapter, `rust→haskell` with the trampoline filled by `hs_factorial`);
the two rust caller qnames differ only in the dictionary so the glue
symbols don't collide, and the same island file plays both roles.
Naming rule learned: glue symbols derive from the *full* caller qname
including the language prefix (`rust:factorial_hs_call/to_haskell` →
`omni_rust_factorial_hs_call_to_haskell`).

Backward compatibility made explicit: `OrganPlanStub`'s JSON decoder
is hand-rolled — fields added after the first release
(`opsCalleeExport`, `opsCalleeAdapter`, `opsEffectMap`) default on
absence, so the original four-field wire clients (every pre-existing
driver) keep working. A derived instance would have demanded every
key and rejected exactly those callers.

### 3. C4: the PMWA-shaped interop criterion

`IntelliMonad.Tools.OrganBank.Interop` writes the per-pair criterion
down as a *checkable claim* with three legs, per the milestone text
("representation dictionary + calling convention + effect mapping"):

1. **Representation** — every crossing position must stay in the
   scalar wire domain (fixed-width ints, IEEE floats, bool, text,
   unit). Off-domain positions (FBig/FDynamic) refuse the claim as
   stated rather than over-claim: the witness does not reach them.
2. **Calling convention** — checked against what the plan actually
   emitted (`renderCStubs` is a pure function of the plan, so the
   glue is deterministic and diff-reviewable), never against intent.
3. **Effects** — `planBoundary`'s subset rule is delegated to (a
   dictionary refusal can never verify); a requested-but-ungeneratable
   effect map is `pmwa-unproven-effects`, never silently dropped.

Verdicts: `pmwa-verified`, `pmwa-refused-{representation,licensed,
effects}`, `pmwa-unproven-{effects,convention}` — with per-leg
evidence and a declared witness domain (`pmwaWitnessDomain = 2^40`):
fixed-width scalars are exhausted exactly (the wire ABI is total over
them); values beyond the bound are outside the claim, which is what
keeps the claim true. `InteropSpec.hs` pins the verdict matrix.

### 4. C5: the bounded-value marshaling contract (2026-09-07)

The last refusal that cost real crossings was unbounded-big vs
fixed-width: `FBigSigned` into `FSigned 64` died as
`unlicensed-missing-contract` even when both sides could have *agreed*
to a smaller domain. C5 makes that agreement first-class:

- **Dictionary**: `Member` gains `mBound :: Maybe Int` (the declared
  contract |v| < 2^bound). `license` routes through a bound gate that
  participates *only when a bound is explicitly declared* — no-bound
  crossings behave byte-identically to C4. Verdicts:
  `licensed-bounded` (checked crossing; sentinel headroom must exist),
  narrowing rescue by declared source domain (by construction, no
  runtime check), `unlicensed-bound-headroom` (declared bound leaves
  no sentinel headroom), `unlicensed-range` (bound but no width).
- **Generated glue**: the koka effect-map shim gains exact arg/result
  guards when the callee members declare bounds — the checks run in
  koka's arbitrary-precision `int` (no C-side overflow risk) and
  violations throw, riding the existing handle/try → sentinel path.
  The OCaml adapter checks *before* `Val_long` boxing (boxing would
  silently drop the top bits) and returns the wire sentinel directly.
  Guard semantics: |v| < 2^b exactly — ±2^b violates, ±(2^b − 1)
  passes (the `<` vs `<=` off-by-one was caught live by the OCaml
  demo's boundary checks).
- **Wire**: `organ_plan_stub` surfaces the verdict unchanged — a
  bounded crossing plans as `licensed-bounded` with the guards in the
  emitted shim; nothing else about the contract changed.

Live proofs (`examples/c2-spike/run_bounded.sh`,
`run_ocaml.sh`): 10!/19! return real values through the checked
chain; 20! (result ≥ 2^60) and ±2^b arguments surface
`-9223372036854775808`; −1 passes the guard and computes. The OCaml
gold adds a fourth live runtime (C host + OCaml + rust in one binary,
ocamlopt driving the link per organ-bank `doc/abi-notes/ocaml.md` §4).

**Honest constraint (4-runtime host)**: OCaml's startup object
(`caml_globals`/`caml_frametable`/`caml_program`) only exists inside
ocamlopt's own link step and is deleted afterward — no flag keeps it.
A single binary with GHC + OCaml + kklib + rust would need that object
captured by poisoning the link or replicating the startup codegen, so
the OCaml gold runs in a C-host + OCaml + rust process (ocamlopt as
link driver) instead of joining `run_multi.sh`'s three-runtime host.

## C5 completion — FBig crossings, guard pins, OCaml effect map (2026-09-08)

Three follow-ups to the bounded-contract milestone:

1. **FBig member-family crossings.** `boundGate` previously exited for
   non-int pairs, so an unbounded bigint caller into a bounded bigint
   callee was silently `licensed-lossless`. Big-big pairs now
   participate: the wire's int64 ABI is the carrier (bound 60 fits with
   sentinel headroom), verdict `licensed-bounded`, and the generated
   koka shim checks the bound in the island's exact big-int arithmetic.
   Bounded-big → unbounded-big is honored by construction. Dictionary
   entry `koka std/core/integer/bounded60`; wire test
   `licenses the FBig big-big bounded crossing over the wire`.

2. **Guard-boundary pins.** The declared contract is strict inequality
   (`|v| < 2^b`): corners `2^b`/`-2^b` violate, `2^b-1`/`-(2^b-1)`
   pass. StubSpec now pins the exact emitted comparisons for both
   emitters (koka bcheck shim, OCaml pre-boxing adapter) — a regression
   to the old `x < -2^b` off-by-one fails the suite.

3. **OCaml effect map.** The generated OCaml adapter calls
   `caml_callback_exn` and maps `Is_exception_result(r)` to the wire's
   status sentinel — OCaml islands join the same sentinel contract as
   koka's `handle/try` shim, and the RTS stays usable after a raised
   exception (proven live: the exception path fires, then `5!` still
   computes). The spike island declares its own domain cap (input > 25
   raises `Invalid_argument`) so the path is exercised end-to-end.

Verification: 209 hspec examples + 102 doctests green; all seven spike
scripts pass (`run.sh`, `run_koka.sh`, `run_koka_generated.sh`,
`run_refused.sh`, `run_koka_mapped.sh`, `run_bounded.sh`,
`run_ocaml.sh` — the last now with 8 contract paths including the two
effect-map checks).

## Positioning — islands are interim; the dictionary is the permanent artifact (2026-09-08)

Q: does the island/runtime-memory work (C2–C5 gold loops) undermine
organ-bank's intended use by frankenstein, which routes every language
through a Koka-shaped Core IR running on Perceus reference counting?

A: no — the islands are bridge-tier scaffolding for the world *before*
frankenstein's unification exists, and the knowledge they produce is
exactly what the unification needs.

### Why the endgame dissolves, not solves, the island problem

Frankenstein's pipeline (per its README): GHC / Mercury / rustc
frontends → shared Koka-like Core IR → Perceus refcounting → MLIR →
LLVM. In that endgame a Mercury or OCaml function lowered through the
pipeline *is* Perceus-runtime code, so calling it is an ordinary
function call — no ABI projection, no RTS init contract, no status
sentinel. The runtime-island problem does not get "solved"; it stops
arising. (Framing note: what is adopted from Koka is the IR shape and
the Perceus resource model — the Koka *frontend* is not special in the
pipeline; it is a donor like the rest.)

### What the islands are actually for

Today every language still runs under its own RTS (GHC's, kklib's,
OCaml's), and the dictionary's axioms are claims about those real
runtimes. The gold loops exist to make those claims *verified*, not to
become the product:

- **The dictionary is the Perceus-relevant knowledge.** The axiom table
  — OCaml's 63-bit tagged `int`, Koka's `KK_TAG_BITS=1` smallint
  payload, GHC's `StgInt`, the sentinel-headroom rule — is precisely
  the representation knowledge frankenstein's Core-IR lowering must
  respect to keep Perceus refcounting correct across donor languages.
  Every gold loop is a live falsification test of one or more of those
  axioms ("demonstrated by compiling and linking, not read from docs").
- **The boundary machinery survives unification, redirected.** After
  unification, `organ_check_boundary` stops being a *permission* layer
  for FFI crossings and becomes an *audit* layer: it verifies the
  lowering did not silently change a representation, and it stays
  genuinely necessary for code that never goes through the pipeline —
  C libraries, prebuilt native artifacts, OS interfaces, and the
  interim period where islands still exist.
- **The MCP surface is the usability contract.** Frankenstein's agent
  side is intelli-monad; `organ_ingest`, `organ_plan_stub`, and the
  diagnostics sidecar are how an agent consumes organ-bank. That read
  side is what this Phase C hardens.

### What is untouched for the intended use

- OrganIR's data model and the 29-shim producer contract have not
  changed. Everything added is read-side (Dictionary, Stubs, Interop in
  intelli-monad) plus demo islands that *consume* OrganIR; a
  frankenstein-side producer is unaffected.
- Nothing emitted assumes islands are the destination. The adapters are
  keyed on "the island's runtime" — i.e., the interim reality.

### The honest caveat

If the endgame is the priority, further island demos have diminishing
returns. The higher-leverage organ-bank work is deepening the
dictionary/axiom layer — that is the knowledge the lowering needs — and
the FBig / boxed-value / threading probes should be read as inputs to
the dictionary, not as a permanent FFI framework. This section records
that priority so the tooling effort does not drift.

## Quad gold — four runtimes, one process (2026-09-08)

`examples/c2-spike/run_quad.sh` closes the C4 milestone at full scale:
C host + GHC RTS + kklib + OCaml RTS (+ runtime-free rust islands) in
ONE process, all island-facing artifacts wire-generated through
`organ_plan_stub` over the real `mcp-serve` binary. The link finding
generalizes the OCaml ABI note: `ghc -v` shows ghc's own link line
resolves the four-runtime object set except the `caml_*` runtime
symbols; the missing `caml_program`/frametable come from ocamlopt's
per-link startup object, which `ocamlopt -dstartup` can be made to
emit as assembly for externally-driven links. Ten contract paths
pass, including the OCaml exception sentinel firing mid-process with
all three RTSes alive afterwards.

With this, C2–C5 all have live, wire-driven proofs; the remaining
open stub note from C3 is documented in its section above. Per the
positioning section: further demos now have diminishing returns — the
next leverage is dictionary/axiom depth for frankenstein's lowering.
