/* C3 gold host: the rust island calls the koka island through the
 * wire-generated glue, plus both islands' direct entries as controls.
 *
 *   path 1: C host -> wire caller glue -> filled trampoline
 *           -> kk_island_factorial (adapter) -> koka island   5! = 120
 *   path 2: rust island -> wire caller glue -> (same chain)   6! = 720
 *   path 3: adapter direct                                    7! = 5040
 *
 * Koka init contract: kk_island_init() before the first crossing,
 * kk_island_done() before exit (see factorial_kk_adapter.c).
 */
#include <stdio.h>
#include <stdint.h>

extern int64_t omni_rust_factorial_rs_factorial(int64_t); /* wire glue entry */
extern int64_t kk_island_factorial(int64_t);              /* adapter entry   */
extern int64_t rs_island_factorial(int64_t);              /* rust island     */
extern void kk_island_init(void);
extern void kk_island_done(void);

int main(void)
{
  kk_island_init();

  int64_t r1 = omni_rust_factorial_rs_factorial(5);  /* host -> wire glue -> koka */
  int64_t r2 = rs_island_factorial(6);               /* rust island -> wire glue -> koka */
  int64_t r3 = kk_island_factorial(7);               /* adapter direct control */

  printf("C host -> wire glue -> koka island:      5! = %lld %s\n",
         (long long)r1, r1 == 120 ? "ok" : "FAIL");
  printf("rust island -> wire glue -> koka island: 6! = %lld %s\n",
         (long long)r2, r2 == 720 ? "ok" : "FAIL");
  printf("adapter direct:                          7! = %lld %s\n",
         (long long)r3, r3 == 5040 ? "ok" : "FAIL");

  kk_island_done();
  return (r1 == 120 && r2 == 720 && r3 == 5040) ? 0 : 1;
}
