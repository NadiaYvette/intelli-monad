#!/usr/bin/env bash
# The multi-island gold: ONE process, THREE runtimes, ALL crossings on
# wire-generated glue. This is the C2 + C4 machinery at full scale:
#
#   C host  (this program, plain gcc)
#   GHC RTS (Factorial.hs, linked ghc -no-hs-main)
#   kklib   (factorial.kk, kk_main_start inside the generated adapter)
#   rust    (factorial.rs, runtime-free)
#
#   1. drive_multi_wire.py plans TWO crossings through the real
#      mcp-serve binary (rust->koka with the generated adapter,
#      rust->haskell with the trampoline filled by hs_factorial).
#   2. Everything compiles; nothing island-facing is hand-written.
#   3. The host drives five call paths and checks exact values.
#
# Usage: examples/c2-spike/run_multi.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-multi"
KKLIB="$HOME/.local/share/koka/v3.2.3/kklib"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire (two crossings) =="
python3 "$HERE/drive_multi_wire.py" "$SRV" --build-dir "$BUILD"

echo "== stage 2: koka island =="
mkdir -p "$BUILD/koka-out"
cp "$HERE/factorial.kk" "$BUILD/koka-out/"
(cd "$BUILD/koka-out" && koka -c -l factorial.kk --outputdir=.)

echo "== stage 3: compile =="
KKWARN="-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing -Wno-unknown-pragmas -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value -Wno-unused-but-set-variable"
KKCFLAGS="$KKWARN -O2 -I $KKLIB/include -I $KKLIB/mimalloc/include -DKK_MIMALLOC=8"

gcc $KKWARN -O2 -c -I $KKLIB/include -I $KKLIB/mimalloc/include \
    -DKK_MIMALLOC=8 '-DKK_COMP_VERSION="3.2.3"' '-DKK_CC_NAME="gcc"' \
    -o "$BUILD/kklib.o" "$KKLIB/src/all.c"

rustc --edition 2021 -O --crate-type staticlib "$HERE/factorial.rs" -o "$BUILD/libfactorial_rs.a"

gcc $KKCFLAGS -c "$BUILD/caller_a.c" -o "$BUILD/caller_a.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/callee_a.c" -o "$BUILD/callee_a.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/kk_adapter_multi.c" -o "$BUILD/adapter_a.o" -I"$BUILD/koka-out"
gcc $KKWARN -O2 -c "$BUILD/caller_b.c" -o "$BUILD/caller_b.o"
gcc $KKWARN -O2 -c "$BUILD/callee_b.c" -o "$BUILD/callee_b.o"

export PATH="$HOME/.ghcup/bin:$PATH"
GHCLIB="$(ghc --print-libdir)"
GHCINC="$GHCLIB/x86_64-linux-ghc-9.8.4/rts-1.0.2/include"
gcc -O2 -c "$HERE/host_multi.c" -o "$BUILD/host_multi.o" -I"$GHCINC"
ghc -O2 -c "$HERE/Factorial.hs" -outputdir "$BUILD/hsout" -o "$BUILD/Factorial.o"

echo "== stage 4: link (ghc -no-hs-main, one process, three runtimes) =="
ghc -no-hs-main "$BUILD/host_multi.o" \
    "$BUILD/caller_a.o" "$BUILD/callee_a.o" "$BUILD/adapter_a.o" \
    "$BUILD/caller_b.o" "$BUILD/callee_b.o" \
    "$BUILD/Factorial.o" "$BUILD/libfactorial_rs.a" \
    "$BUILD/kklib.o" \
    "$BUILD"/koka-out/*.o \
    -o "$BUILD/multispike" -lm -lpthread

echo "== stage 5: run =="
"$BUILD/multispike"
