/* The gold loop's host process. Everything it links against was
 * generated on the wire except the islands' own logic:
 *
 *   caller.c  — organ_plan_stub caller side (haskell island projection)
 *   callee.c  — organ_plan_stub callee side, trampoline FILLED with the
 *               Rust island's real entry (rs_island_factorial)
 *   Factorial.o — the GHC island (hs_factorial entry, rsCall export)
 *   libfactorial_rs.a — the rustc island
 *
 * Three call paths, one process:
 *   1. C host → caller glue → filled callee trampoline → rust island
 *   2. GHC island (rsCall) → caller glue → filled callee trampoline → rust island
 *   3. C host → hs_factorial (haskell island direct)
 */
#include <stdio.h>
#include <stdint.h>
#include <HsFFI.h>

extern int64_t omni_haskell_Factorial_factorial(int64_t); /* caller glue */
extern int64_t rsCall(int64_t);                           /* GHC island export */
extern int64_t hs_factorial(int64_t);                     /* GHC island entry */

int main(void) {
  static char *argv[] = {"c2spike", 0};
  char **pargv = argv;
  int argc = 1;
  hs_init(&argc, &pargv);

  int failed = 0;
  struct { long long n; long long want; int64_t (*f)(int64_t); const char *name; } cases[] = {
      {10, 3628800, omni_haskell_Factorial_factorial, "C host -> wire glue -> rust island"},
      {12, 479001600, rsCall, "GHC island -> wire glue -> rust island"},
      {7, 5040, hs_factorial, "GHC island direct"},
  };
  for (int i = 0; i < 3; i++) {
    long long got = (long long)cases[i].f(cases[i].n);
    int ok = got == cases[i].want;
    printf("%-40s %lld! = %lld %s\n", cases[i].name, cases[i].n, got,
           ok ? "ok" : "MISMATCH");
    failed |= !ok;
  }

  hs_exit();
  return failed;
}
