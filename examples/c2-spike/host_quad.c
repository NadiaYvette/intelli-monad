/* The quad-island host: ONE process, FOUR runtimes (C, GHC RTS, kklib,
 * OCaml RTS), all crossings through wire-generated glue.
 *
 *   path 1: C host -> plan A caller glue -> koka adapter
 *           -> koka island                                  5! = 120
 *   path 2: rust island -> plan A caller glue -> (same chain)  6! = 720
 *   path 3: rust island -> plan B caller glue -> trampoline
 *           -> GHC island (hs_factorial)                    7! = 5040
 *   path 4: C host -> plan C caller glue -> OCaml adapter
 *           -> ocaml island                                 8! = 40320
 *   path 5: C host -> hs_factorial (GHC island direct)      9! = 362880
 *   path 6: C host -> kk_island_factorial (koka direct)    10! = 3628800
 *   path 7: C host -> ocaml_island_factorial (ocaml direct) 4! = 24
 *
 * Effect-map contract, live in the shared process: the OCaml island's
 * OWN exception (30 passes the bound guard but is outside the island's
 * 25! domain) arrives as an exception-result from caml_callback_exn and
 * surfaces as the wire's status sentinel -- while the GHC and kklib
 * runtimes stay alive and keep computing afterwards.
 *
 * Init contract: hs_init before any GHC crossing (host-owned, the C2
 * contract); the koka adapter's namespaced lifecycle entries bring the
 * kklib runtime and the island module up; the OCaml adapter's
 * namespaced init brings the OCaml RTS up (caml_main) -- its done is a
 * no-op, the RTS lives until process exit (the adapter's documented
 * contract). No RTS main: the host is plain C.
 */
#include <stdio.h>
#include <stdint.h>
#include <HsFFI.h>

#include <limits.h>
#if !defined(LLONG_MIN)
#define SENTINEL (-9223372036854775807LL - 1)
#else
#define SENTINEL LLONG_MIN
#endif

/* plan A glue: rust caller -> koka callee (generated koka adapter) */
extern int64_t omni_rust_factorial_rs_factorial(int64_t);
/* plan B glue: rust caller -> haskell callee (symbol derives from the
 * FULL caller qname including the lang prefix: rust:factorial_hs_call/
 * to_haskell -> omni_rust_factorial_hs_call_to_haskell) */
extern int64_t omni_rust_factorial_hs_call_to_haskell(int64_t);
/* plan C glue: rust caller -> ocaml callee (generated OCaml adapter) */
extern int64_t omni_rust_factorial_oc_call_to_ocaml(int64_t);
/* the islands' own entries */
extern int64_t rs_island_factorial(int64_t);
extern int64_t hs_factorial(int64_t);
extern int64_t kk_island_factorial(int64_t);
extern int64_t ocaml_island_factorial(int64_t);
/* generated koka adapter lifecycle (namespaced, collision-safe) */
extern void omni_kk_factorial_island_init(void);
extern void omni_kk_factorial_island_done(void);
/* generated OCaml adapter lifecycle (namespaced, collision-safe) */
extern void omni_oc_factorial_oc_island_init(void);
extern void omni_oc_factorial_oc_island_done(void);

static int fails = 0;

static void check(const char *what, long long n, long long want,
                  int64_t (*f)(int64_t))
{
  long long got = (long long)f(n);
  int ok = got == want;
  if (!ok) fails++;
  printf("%-46s %2lld! = %-10lld %s\n", what, n, got, ok ? "ok" : "FAIL");
}

int main(void)
{
  static char *argv[] = {"quad", 0};
  char **pargv = argv;
  int argc = 1;
  hs_init(&argc, &pargv);
  omni_kk_factorial_island_init();
  omni_oc_factorial_oc_island_init();

  struct { const char *name; long long n; long long want; int64_t (*f)(int64_t); } cases[] = {
      {"C host -> plan A glue -> koka island", 5, 120, omni_rust_factorial_rs_factorial},
      {"rust island -> plan A glue -> koka island", 6, 720, rs_island_factorial},
      {"rust island -> plan B glue -> GHC island", 7, 5040, omni_rust_factorial_hs_call_to_haskell},
      {"C host -> plan C glue -> OCaml island", 8, 40320, omni_rust_factorial_oc_call_to_ocaml},
      {"GHC island direct (hs_factorial)", 9, 362880, hs_factorial},
      {"koka island direct (adapter entry)", 10, 3628800, kk_island_factorial},
      {"OCaml island direct (adapter entry)", 4, 24, ocaml_island_factorial},
  };
  int ncases = (int)(sizeof(cases) / sizeof(cases[0]));
  for (int i = 0; i < ncases; i++) {
    check(cases[i].name, cases[i].n, cases[i].want, cases[i].f);
  }

  /* Effect map, live in the shared process: the OCaml island's own
   * exception surfaces as the wire sentinel and the other two RTS
   * keep serving calls afterwards. */
  check("effect map: ocaml exception (30) -> sentinel", 30, (long long)SENTINEL,
        ocaml_island_factorial);
  check("rts alive after exception: GHC 3! still real", 3, 6, hs_factorial);
  check("rts alive after exception: koka 6! still real", 6, 720, kk_island_factorial);

  omni_kk_factorial_island_done();
  hs_exit();
  /* omni_oc_factorial_oc_island_done(): OCaml RTS lives until process
   * exit -- calling caml shutdown while the other RTSes unwind is not
   * a contract we own; process exit reclaims it. */
  return fails ? 1 : 0;
}
