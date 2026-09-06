// The koka island for the C3 gold. Compiled by the real koka compiler
// (library mode: `koka -c -l`), its `island-factorial` carries the
// corpus's effect row <div,exn> on std/core/int.
//
// The negative-argument guard is the island's own precondition: a
// genuinely thrown koka exception, exactly what the C3 effect map
// exists to reconcile (mapped path: run_koka_mapped.sh).
//
// `dummy-main` exists only because `-c` (even with `-l`) demands a
// main to typecheck; it is never called -- the island is linked as a
// library and driven through its C ABI (see factorial_kk_adapter.c).
module factorial
import std/core/exn

pub fun island-factorial(n : int) : <div,exn> int
  if n < 0 then throw("island factorial: negative argument")
  else if n < 2 then 1
  else n * island-factorial(n - 1)

pub fun dummy-main()
  ()
