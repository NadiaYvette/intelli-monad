/* The C3-vision host: the mapped effect path, end to end.
 *
 *   value path: rust island -> wire caller glue -> filled trampoline
 *               -> adapter -> MAPPED entry (handle/try inside koka)
 *               -> koka island                          5! = 120
 *   error path: same chain, but the island RAISES; the generated
 *               effect map surfaces the exception as the wire's
 *               status sentinel (min-int64) -- no setjmp, no error
 *               thread, a plain int64 return.
 *
 * Init contract: the generated adapter's namespaced lifecycle entries
 * (omni_kk_factorial_island_init/done) bring the koka RTS up and run
 * BOTH module init chains (island + generated _emap shim).
 */
#include <stdio.h>
#include <stdint.h>

#include <limits.h>
#if !defined(LLONG_MIN)
#define SENTINEL (-9223372036854775807LL - 1)
#else
#define SENTINEL LLONG_MIN
#endif

extern int64_t omni_rust_factorial_rs_factorial(int64_t); /* wire glue entry      */
extern int64_t kk_island_factorial(int64_t);              /* adapter (mapped)     */
extern int64_t rs_island_factorial(int64_t);              /* rust island          */
extern void omni_kk_factorial_island_init(void);
extern void omni_kk_factorial_island_done(void);

static int fails = 0;

static void check(const char *what, long long got, long long want)
{
  int ok = got == want;
  if (!ok) fails++;
  printf("%-52s got %lld, want %lld %s\n", what, got, want, ok ? "ok" : "FAIL");
}

int main(void)
{
  omni_kk_factorial_island_init();

  int64_t r1 = omni_rust_factorial_rs_factorial(5); /* value path */
  int64_t r2 = rs_island_factorial(6);              /* rust -> wire -> koka */
  int64_t r3 = kk_island_factorial(7);              /* adapter direct */

  /* The error path: koka's own semantics -- 0! = 1, so there is no
   * factorial argument whose result is min-int64; the sentinel can
   * only mean "the island raised". (The mapped demo island below is
   * the factorial island itself; the throw case is exercised by
   * run_koka_mapped.sh's divmod checks in the README walkthrough --
   * here the sentinel contract is pinned by construction: the
   * adapter's mapped call can never legitimately return LLONG_MIN
   * for n in [0, 20], the i64-factorial domain.) */
  int64_t err = kk_island_factorial(-1);

  check("value: host -> wire -> mapped koka, 5!", (long long)r1, 120);
  check("value: rust island -> wire -> koka, 6!", (long long)r2, 720);
  check("value: adapter direct, 7!", (long long)r3, 5040);
  check("error: mapped entry surfaces the sentinel", (long long)err, (long long)SENTINEL);

  omni_kk_factorial_island_done();
  return fails ? 1 : 0;
}
