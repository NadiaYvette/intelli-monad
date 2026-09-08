/* The C5 OCaml host: the declared-bound crossing, end to end.
 *
 *   value path: rust island -> wire caller glue -> filled trampoline
 *               -> generated adapter -> caml_callback -> OCaml island
 *   arg guard:  the adapter checks |v| < 2^61 BEFORE Val_long boxing
 *               (boxing would silently drop the top bits of a wider
 *               int64); a violation returns the wire's status sentinel.
 *
 * The result direction needs no check: OCaml's 63-bit int into the
 * wire's int64 is a widening, safe by construction.
 *
 * Link contract (ABI-proven, organ-bank doc/abi-notes/ocaml.md §4):
 * ocamlopt drives the link and the C main wins over the runtime's
 * archive member — the OCaml-side counterpart of ghc -no-hs-main.
 */
#include <stdio.h>
#include <stdint.h>

#include <limits.h>
#if !defined(LLONG_MIN)
#define SENTINEL (-9223372036854775807LL - 1)
#else
#define SENTINEL LLONG_MIN
#endif

extern void omni_oc_factorial_oc_island_init(void);
extern void omni_oc_factorial_oc_island_done(void);
extern int64_t omni_rust_factorial_rs_factorial(int64_t); /* wire glue   */
extern int64_t rs_island_factorial(int64_t);              /* rust island */
extern int64_t ocaml_island_factorial(int64_t);           /* adapter     */

static int fails = 0;

static void check(const char *what, long long got, long long want)
{
  int ok = got == want;
  if (!ok) fails++;
  printf("%-52s got %20lld, want %20lld %s\n", what, got, want, ok ? "ok" : "FAIL");
}

int main(void)
{
  omni_oc_factorial_oc_island_init(); /* caml_main: RTS up, Callback.register ran */

  check("value: adapter direct -> ocaml island, 10!", (long long)ocaml_island_factorial(10), 3628800LL);
  check("value: rust island -> wire -> ocaml, 19!", (long long)omni_rust_factorial_rs_factorial(19), 121645100408832000LL);
  /* 20! = 2432902008176640000 fits OCaml's int63 (< 2^62-1) and the
   * result direction is a widening — a real value, no check needed. */
  check("value: result widening needs no check, 20!", (long long)omni_rust_factorial_rs_factorial(20), 2432902008176640000LL);
  check("arg guard: +2^61 violates |v| < 2^61 -> sentinel", (long long)ocaml_island_factorial(2305843009213693952LL), (long long)SENTINEL);
  check("arg guard: -2^61 violates |v| < 2^61 -> sentinel", (long long)ocaml_island_factorial(-2305843009213693952LL), (long long)SENTINEL);
  check("in-range: -1 passes the guard (|v| < 2^61)", (long long)ocaml_island_factorial(-1), 1LL);
  /* Effect-map contract: the island's OWN exception (30 passes the
   * arg guard but is outside the island's domain) arrives as an
   * exception-result from caml_callback_exn and surfaces as the wire
   * sentinel — the process survives, unlike a raw caml_callback. */
  check("effect map: island exception (30) -> sentinel", (long long)ocaml_island_factorial(30), (long long)SENTINEL);
  check("rts alive after exception: 5! still real", (long long)ocaml_island_factorial(5), 120LL);

  omni_oc_factorial_oc_island_done();
  return fails ? 1 : 0;
}
