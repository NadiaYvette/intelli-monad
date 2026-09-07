/* The C5 bounded-contract host: the declared ±2^60 contract, end to
 * end. Same chain as the C3 mapped demo (rust island -> wire caller
 * glue -> filled trampoline -> adapter -> MAPPED entry -> koka
 * island), but the generated effect-map shim is the CHECKED form:
 * the island's types carry the declared bound |v| < 2^60, so the
 * shim's arg and result guards run inside koka's arbitrary-precision
 * int (exact — no C-side overflow risk) and a violation throws, which
 * the handle/try maps to the wire's status sentinel (min-int64).
 *
 *   in-range:  10! = 3628800                real value
 *   in-range:  19! = 121645100408832000     real value (< 2^60)
 *   result:    20! = 2432902008176640000    >= 2^60 -> sentinel
 *   arg:       -1                           |v| >= 2^60 check -> sentinel
 *
 * The sentinel is unambiguous by construction: no legitimate result
 * of the guarded call can be min-int64.
 */
#include <stdio.h>
#include <stdint.h>

#include <limits.h>
#if !defined(LLONG_MIN)
#define SENTINEL (-9223372036854775807LL - 1)
#else
#define SENTINEL LLONG_MIN
#endif

extern int64_t kk_island_factorial(int64_t); /* adapter (mapped, checked) */
extern void omni_kk_factorial_island_init(void);
extern void omni_kk_factorial_island_done(void);

static int fails = 0;

static void check(const char *what, long long got, long long want)
{
  int ok = got == want;
  if (!ok) fails++;
  printf("%-52s got %20lld, want %20lld %s\n", what, got, want, ok ? "ok" : "FAIL");
}

int main(void)
{
  omni_kk_factorial_island_init();

  check("in-range: 10! through the bounded crossing", (long long)kk_island_factorial(10), 3628800LL);
  check("in-range: 19! (largest under 2^60)", (long long)kk_island_factorial(19), 121645100408832000LL);
  check("result guard: 20! >= 2^60 -> sentinel", (long long)kk_island_factorial(20), (long long)SENTINEL);
  check("arg guard: -1 violates |v| < 2^60 -> sentinel", (long long)kk_island_factorial(-1), (long long)SENTINEL);

  omni_kk_factorial_island_done();
  return fails ? 1 : 0;
}
