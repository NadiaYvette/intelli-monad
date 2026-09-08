/* The C5 FBig gold host: the arbitrary-precision bounded contract,
 * end to end. Same chain as run_bounded.sh (rust island -> wire caller
 * glue -> filled trampoline -> adapter -> MAPPED entry -> koka
 * island), but the island's OrganIR types carry the FBig-family
 * member std/core/integer/bounded60 — the LICENSE is the big-big
 * bounded gate (widthless big side, wire int64 carrier), not the
 * fixed-width one. The binaries are identical to the bounded demo;
 * what is under test is the license path and the exact-arithmetic
 * guards it requires.
 *
 *   in-range:   10! = 3628800               real value
 *   in-range:   19! = 121645100408832000    real value (< 2^60; the
 *               largest factorial inside the declared range)
 *   result:     20! = 2432902008176640000   >= 2^60 -> the shim's
 *               result check throws in koka's EXACT big-int
 *               arithmetic (this value fits no int64 — the check
 *               runs before any truncation could happen) -> sentinel
 *   arg upper:  n = 2^60                    |v| >= 2^60 -> sentinel
 *   arg lower:  n = -2^60                   |v| >= 2^60 -> sentinel
 *   island pre: n = -1                      passes the bound guard,
 *               the island's own negative-argument precondition
 *               throws -> sentinel (the effect-map path, distinct
 *               mechanism from the bound contract)
 *   rts alive:  5! = 120 after every violation
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

  check("in-range: 10! through the FBig crossing", (long long)kk_island_factorial(10), 3628800LL);
  check("in-range: 19! (largest factorial < 2^60)", (long long)kk_island_factorial(19), 121645100408832000LL);
  check("result guard: 20! >= 2^60 -> sentinel", (long long)kk_island_factorial(20), (long long)SENTINEL);
  check("arg guard upper corner: +2^60 -> sentinel", (long long)kk_island_factorial(1152921504606846976LL), (long long)SENTINEL);
  check("arg guard lower corner: -2^60 -> sentinel", (long long)kk_island_factorial(-1152921504606846976LL), (long long)SENTINEL);
  check("island precondition: -1 (passes bound) -> sentinel", (long long)kk_island_factorial(-1), (long long)SENTINEL);
  check("rts alive after violations: 5! still real", (long long)kk_island_factorial(5), 120LL);

  omni_kk_factorial_island_done();
  return fails ? 1 : 0;
}
