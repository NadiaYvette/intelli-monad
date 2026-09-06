#!/usr/bin/env bash
# The C3 gold loop: wire -> compile (koka + rust + gcc) -> run.
#
#   1. drive_koka_wire.py ingests a pure rust island (std/i64) and the
#      effectful koka island (std/core/int, <div,exn>) through the real
#      mcp-serve binary. The C3 effect-row subset rule licenses the
#      crossing (caller row ∅ ⊆ callee row) and organ_plan_stub saves
#      BOTH glue sides, trampoline filled via opsCalleeExport.
#   2. The koka island compiles with the real koka compiler (library
#      mode). The adapter, wire glue, and host compile with gcc using
#      koka's own captured recipe (gnuWarn subset, -I kklib/include,
#      -DKK_MIMALLOC=8, mimalloc include for adapter/host TUs).
#   3. Everything links plain gcc -- no RTS main needed: kk_main_start
#      runs inside kk_island_init (koka does not require being the
#      process main, unlike GHC's hs_init in the C2 loop).
#   4. The host verifies three call paths with exact values.
#
# Usage: examples/c2-spike/run_koka.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-koka"
KKLIB="$HOME/.local/share/koka/v3.2.3/kklib"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire =="
python3 "$HERE/drive_koka_wire.py" "$SRV" --build-dir "$BUILD"

echo "== stage 2: koka island =="
# Real koka compiler, library mode. Emits factorial.o + the std_core*.o
# set + declares kk_factorial_island_factorial / kk_factorial__init /
# kk_factorial__done in factorial.h. Run from the build dir with a
# relative filename: koka derives the module/package name from the
# source path, and an absolute path leaks directory components into
# the symbol names (examples/c2-spike/factorial instead of factorial).
mkdir -p "$BUILD/koka-out"
cp "$HERE/factorial.kk" "$BUILD/koka-out/"
(cd "$BUILD/koka-out" && koka -c -l factorial.kk --outputdir=.)

echo "== stage 3: compile glue + rust + adapter + host =="
# Koka's own recipe for TUs that include kklib headers (captured via a
# --cc shim; see doc/phase-c-transplant.md):
#   gnuWarn subset, -I kklib/include, -DKK_MIMALLOC=8, and (because
#   kklib.h leaks mi_heap_t into kk_context_t) the mimalloc include dir
#   for any TU that includes kklib.h.
KKWARN="-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing -Wno-unknown-pragmas -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value -Wno-unused-but-set-variable"
KKCFLAGS="$KKWARN -O2 -I $KKLIB/include -I $KKLIB/mimalloc/include -DKK_MIMALLOC=8"

# The kklib runtime itself: koka's linker step always includes a
# kklib.o compiled from kklib/src/all.c (mimalloc statically included --
# that is where mi_free and the exported kk_* runtime symbols live).
# Library mode (-l) emits only the module objects, so compile it here
# with koka's own captured flags for all.c:
#   -DKK_MIMALLOC=8 -DKK_COMP_VERSION=... -DKK_CC_NAME=...
# koka passes the version/cc name as quoted C string literals:
#   -DKK_COMP_VERSION="3.2.3" -DKK_CC_NAME="gcc"
# (escaping survives gcc because the shell strips one level; the
# compiler sees the literal quotes, exactly as koka's captured command
# showed).
gcc $KKWARN -O2 -c -I $KKLIB/include -I $KKLIB/mimalloc/include \
    -DKK_MIMALLOC=8 '-DKK_COMP_VERSION="3.2.3"' '-DKK_CC_NAME="gcc"' \
    -o "$BUILD/kklib.o" "$KKLIB/src/all.c"

rustc --edition 2021 -O --crate-type staticlib "$HERE/factorial.rs" -o "$BUILD/libfactorial_rs.a"
gcc $KKCFLAGS -c "$BUILD/caller.c"  -o "$BUILD/caller.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/callee.c"  -o "$BUILD/callee.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$HERE/factorial_kk_adapter.c" -o "$BUILD/adapter.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$HERE/host_koka.c" -o "$BUILD/host_koka.o"

echo "== stage 4: link =="
# Plain gcc: koka's runtime initializes lazily via kk_island_init ->
# kk_main_start; no RTS-provided main is required (contrast the GHC
# link in run.sh, which needs ghc -no-hs-main).
gcc -o "$BUILD/c3spike" \
    "$BUILD/host_koka.o" "$BUILD/caller.o" "$BUILD/callee.o" \
    "$BUILD/adapter.o" \
    "$BUILD/libfactorial_rs.a" \
    "$BUILD/kklib.o" \
    "$BUILD"/koka-out/*.o \
    -lm -lpthread

echo "== stage 5: run =="
"$BUILD/c3spike"
