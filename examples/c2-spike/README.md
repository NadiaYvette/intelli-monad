# The C2 gold loop

One script, three stages, zero hand-written glue:

```sh
cabal v2-build exe:mcp-serve        # once
examples/c2-spike/run.sh            # wire → compile → run
```

Stage 1 (`drive_wire.py`) ingests two OrganIR docs — a haskell island
(`ghc-prim/Int#`) and a rust island (`std/i64`) — through the **real
`mcp-serve` binary** over stdio MCP, then asks `organ_plan_stub` for
the glue, passing `opsCalleeExport: "rs_island_factorial"` so the
**callee trampoline comes filled** (the C2 milestone; the spike era
left it as a placeholder).

Stage 2 compiles what the wire produced:

| artifact | source | compiled by |
|---|---|---|
| `build/caller.c` | generated on the wire | gcc |
| `build/callee.c` | generated on the wire, trampoline filled | gcc |
| `build/Factorial.o` | `Factorial.hs` (the GHC island) | `ghc -c` |
| `build/libfactorial_rs.a` | `factorial.rs` (the rustc island) | `rustc --crate-type staticlib` |

Everything links with `ghc -no-hs-main` (so base/ghc-prim/RTS resolve).

Stage 3 runs the host, which verifies three call paths with exact
values:

```
C host -> wire glue -> rust island        10! = 3628800   ok
GHC island -> wire glue -> rust island    12! = 479001600 ok
GHC island direct                          7! = 5040      ok
```

Any mismatch is a nonzero exit — the script is CI-able.

## The rules this loop demonstrates

- **Bridge symbols and island exports are separate namespaces.** The
  generated glue owns `omni_*` names; the islands' real entries
  (`hs_factorial`, `rs_island_factorial`) are passed to the planner via
  `opsCalleeExport` and addressed by their own names inside the
  trampoline. Colliding the namespaces was the spike's link error.
- **GHC islands need an RTS contract.** The generated haskell-callee
  trampoline carries an `hs_init`/`hs_exit` comment; the host honors it.
- **A crossing is licensed by representation *and* effect row.** Both
  islands' rows here normalize to ∅ (pure), so the C3 subset rule
  licenses the crossing as `licensed-lossless`. An effectful caller
  into a pure callee is refused regardless of type compatibility —
  see the `C3: effect-row subset rule` tests in `test/StubSpec.hs`.

## Koka (C3): the effect-row crossing, live

`run_koka.sh` runs the same loop in the direction the subset rule
licenses: a pure rust island (`std/i64`, row ∅) calling a real
koka-compiled island (`std/core/int`, row `<div,exn>`):

```
VERDICT: licensed-lossless
C host -> wire glue -> koka island:      5! = 120 ok
rust island -> wire glue -> koka island: 6! = 720 ok
adapter direct:                          7! = 5040 ok
```

The koka island's ABI is `kk_integer_t` + `kk_context_t*`, so
`factorial_kk_adapter.c` owns the int64 projection (the koka analog of
the GHC island's Haskell-side FFI) and `kk_island_init`/`kk_island_done`
wrap koka's `kk_main_start` + module init contract. Koka does not need
to be the process main, so this loop links with plain gcc. The full
recipe (kklib.o from `src/all.c`, `-DKK_MIMALLOC=8`, mimalloc includes,
quoted `-DKK_COMP_VERSION`/`-DKK_CC_NAME`) is in `run_koka.sh`,
captured byte-faithfully from koka's own compiler invocations, and the
design findings are recorded in `doc/phase-c-transplant.md`.

## Koka corpus note

The Koka corpus example (`organ-bank/spec/examples/koka.json`) declares
`std/core/int` with effect row `{std/core/div, std/core/exn}`. The
subset rule licenses haskell-pure → koka-`div/exn` and refuses the
reverse; the dictionary carries the koka int/float64 axioms, and the
crossing plans losslessly the moment its island has a real entry to
name (the adapter provides it here).

## C4: the adapter becomes generated (run_koka_generated.sh)

`run_koka_generated.sh` runs the C3 loop with **zero hand-written
glue of any kind** — the ABI adapter included. The driver passes
`opsCalleeAdapter: "kk_island_factorial"` and the wire emits
`build-koka-gen/kk_adapter.c`: the int64→`kk_integer_t` projection,
`kk_main_start` + module init/done contract, and the call to koka's
real export (`kk_factorial_island_factorial`) all generated. The
hand-written `factorial_kk_adapter.c` stays in the repo as the
reviewed reference the generated text must match.

The generated GHC adapter works the same way against the compiler's
capi header (`<Module>_api.h`), so a wire-planned GHC island can be
called from the C host too — the C2 loop's `hsCall` path no longer
needs a hand-written `Factorial.hs` export just to serve the host.

## The refusal, live (run_refused.sh)

`run_refused.sh` drives the same two koka/rust islands in the
direction the effect-row subset rule *refuses*: the effectful koka
island (`<div,exn>`) wants to call the pure rust island. Over the
real wire, `organ_plan_stub` returns `unlicensed-effect-row` with
comment-only debris — no compilable code exists for a crossing the
axioms cannot prove. Exit 0 iff the refusal is exactly as predicted.

## FBig: arbitrary-precision domains are first-class

The dictionary splits the old `FBigInt` into `FBigSigned` and
`FBigUnsigned`: the signedness of an unbounded domain still crosses
the boundary. Koka's *full* `int` domain is modeled as
`std/core/integer` (FBigSigned) beside the ABI-range entry
(`std/core/int`, FSigned 64) the C3 gold crosses; Lean4/Agda `Nat`
are FBigUnsigned. A koka-bigint → rust-i64 crossing now refuses as
`unlicensed-representation` (different domains, not a width guess),
and a signed-bigint → Nat crossing refuses as `unlicensed-overflow-domain`
(negatives do not transfer) — see the `C4: FBig member families`
tests in `test/StubSpec.hs`.

## C3 closed: the generated effect map (run_koka_mapped.sh)

The milestone's last open note — an effectful call must *declare how
the effects surface* — is now generator output. With
`opsEffectMap: true`, `organ_plan_stub` emits:

- an island-side shim (`<module>_emap.kk`): a koka `handle/try` that
  runs the island's real logic and maps any exception to the wire's
  status sentinel (`min-int64`);
- an adapter that forwards to the **mapped entry** — which, because
  koka unboxes `int64`, already has exactly the wire's plain
  `int64_t` ABI (no `kk_integer` boxing in the forward).

`run_koka_mapped.sh` drives it through the real `mcp-serve` binary:

```
value: host -> wire -> mapped koka, 5!               got 120, want 120 ok
value: rust island -> wire -> koka, 6!               got 720, want 720 ok
value: adapter direct, 7!                            got 5040, want 5040 ok
error: mapped entry surfaces the sentinel            got -9223372036854775808 ok
```

The demo island now genuinely throws (`throw("island factorial:
negative argument")` for `n < 0`) — its `<div,exn>` row is honest, and
the host sees the exception as a plain int64 return. One mangled-name
fact to know: koka doubles a module name's underscore in C symbols
(`factorial_emap` → `kk_factorial__emap_*`, header
`factorial__emap.h`); the generator handles it.

## The multi-island gold: one process, three runtimes (run_multi.sh)

`run_multi.sh` links C host + GHC RTS (Factorial.hs) + kklib
(factorial.kk) + rust (factorial.rs) into ONE process. Two
`organ_plan_stub` calls over the real wire plan both crossings
(`rust→koka` with the generated adapter, `rust→haskell` filled with
`hs_factorial`), then five call paths run:

```
C host -> plan A glue -> koka island        5! = 120      ok
rust island -> plan A glue -> koka island   6! = 720      ok
rust island -> plan B glue -> GHC island    7! = 5040     ok
GHC island direct (hs_factorial)            8! = 40320    ok
koka island direct (adapter entry)          9! = 362880   ok
```

Naming facts: glue symbols derive from the full caller qname
*including* the language prefix
(`rust:factorial_hs_call/to_haskell` →
`omni_rust_factorial_hs_call_to_haskell`); the two rust caller qnames
differ only in the dictionary so the two glue entries don't collide,
while one island file plays both caller roles. And `OrganPlanStub`'s
decoder tolerates old clients: fields added after the first release
default on absence, which is why every earlier driver still runs
unchanged against the new binary.

## C4: the PMWA interop criterion (IntelliMonad.Tools.OrganBank.Interop)

The per-pair criterion is a checkable claim with three legs —
representation (positions stay in the scalar wire domain; off-domain
positions refuse the claim rather than over-claim), calling
convention (checked against what the plan actually emitted), effects
(the subset rule is delegated to; a requested-but-ungeneratable map is
`pmwa-unproven`, never dropped). Verdicts `pmwa-verified` /
`pmwa-refused-*` / `pmwa-unproven-*` come with per-leg evidence and a
declared witness domain (`2^40`: fixed-width scalars are exhausted
exactly; anything beyond is outside the claim). `test/InteropSpec.hs`
pins the matrix.
