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
