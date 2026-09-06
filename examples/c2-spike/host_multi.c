/* The multi-island host: ONE process, THREE runtimes (C, GHC RTS,
 * kklib), all crossings through wire-generated glue.
 *
 *   path 1: C host -> plan A caller glue -> trampoline -> koka adapter
 *           -> koka island                                  5! = 120
 *   path 2: rust island -> plan A caller glue -> (same chain)  6! = 720
 *   path 3: rust island -> plan B caller glue -> trampoline
 *           -> GHC island (hs_factorial)                    7! = 5040
 *   path 4: C host -> hs_factorial (GHC island direct)      8! = 40320
 *   path 5: C host -> kk_island_factorial (koka direct)     9! = 362880
 *
 * Init contract: hs_init before any GHC crossing (host-owned, the C2
 * contract); the koka adapter's namespaced lifecycle entries bring the
 * kklib runtime and the island module up. No RTS main: the host is
 * plain C linked with ghc -no-hs-main.
 */
#include <stdio.h>
#include <stdint.h>
#include <HsFFI.h>

/* plan A glue: rust caller -> koka callee (generated koka adapter) */
extern int64_t omni_rust_factorial_rs_factorial(int64_t);
/* plan B glue: rust caller -> haskell callee (symbol derives from the
 * FULL caller qname including the lang prefix: rust:factorial_hs_call/
 * to_haskell -> omni_rust_factorial_hs_call_to_haskell) */
extern int64_t omni_rust_factorial_hs_call_to_haskell(int64_t);
/* the islands' own entries */
extern int64_t rs_island_factorial(int64_t);
extern int64_t hs_factorial(int64_t);
extern int64_t kk_island_factorial(int64_t);
/* generated koka adapter lifecycle (namespaced, collision-safe) */
extern void omni_kk_factorial_island_init(void);
extern void omni_kk_factorial_island_done(void);

static int fails = 0;

int main(void)
{
  static char *argv[] = {"multi", 0};
  char **pargv = argv;
  int argc = 1;
  hs_init(&argc, &pargv);
  omni_kk_factorial_island_init();

  struct { const char *name; long long n; long long want; int64_t (*f)(int64_t); } cases[] = {
      {"C host -> plan A glue -> koka island", 5, 120, omni_rust_factorial_rs_factorial},
      {"rust island -> plan A glue -> koka island", 6, 720, rs_island_factorial},
      {"rust island -> plan B glue -> GHC island", 7, 5040, omni_rust_factorial_hs_call_to_haskell},
      {"GHC island direct (hs_factorial)", 8, 40320, hs_factorial},
      {"koka island direct (adapter entry)", 9, 362880, kk_island_factorial},
  };

  int printed = 0;
  for (int i = 0; i < 5; i++) {
    long long got = (long long)cases[i].f(cases[i].n);
    int ok = got == cases[i].want;
    if (!ok) fails++;
    printf("%-42s %2lld! = %-10lld %s\n", cases[i].name, cases[i].n, got,
           ok ? "ok" : "FAIL");
    printed++;
  }
  omni_kk_factorial_island_done();
  hs_exit();
  return fails ? 1 : 0;
}
