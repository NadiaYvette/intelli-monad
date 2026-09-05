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
