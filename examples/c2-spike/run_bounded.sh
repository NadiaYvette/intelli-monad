#!/usr/bin/env bash
# The C5 bounded-value marshaling contract, live:
#
#   "a declared bound |v| < 2^60 licenses the crossing that the
#    unbounded member would refuse; the generated glue checks it at
#    the crossing and violations surface as the wire's status
#    sentinel, never silent truncation"
#
#   1. drive_koka_wire.py --bounded --adapter ingests the islands
#      through the real mcp-serve binary; the island's types carry the
#      dictionary's declared-bound member std/core/int/bounded60, the
#      verdict is licensed-bounded, and the generated factorial_emap.kk
#      is the CHECKED shim (arg + result guards, handle/try sentinel).
#   2. Koka compiles island + generated shim: the guards run in koka's
#      arbitrary-precision int (exact), throwing on |v| >= 2^60; the
#      handle/try maps every throw to min-int64.
#   3. The host exercises the four paths of the contract: in-range
#      values return real results; the result violation (20! >= 2^60)
#      and the arg violation (-1) surface the sentinel.
#
# Usage: examples/c2-spike/run_bounded.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-bounded"
KKLIB="$HOME/.local/share/koka/v3.2.3/kklib"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire (bounded types + effect map + adapter requested) =="
python3 "$HERE/drive_koka_wire.py" "$SRV" --build-dir "$BUILD" --bounded --adapter

echo "== stage 2: koka islands (island + generated CHECKED shim) =="
# The shim source comes straight from the wire (factorial_emap.kk):
# nothing hand-written. Compiling island + shim together lets the
# shim's `import factorial` resolve; both modules' __init/__done are
# statically guarded and idempotent.
mkdir -p "$BUILD/koka-out"
cp "$HERE/factorial.kk" "$BUILD/koka-out/"
cp "$BUILD/factorial_emap.kk" "$BUILD/koka-out/"
(cd "$BUILD/koka-out" && koka -c -l factorial.kk factorial_emap.kk --outputdir=.)

echo "== stage 3: compile glue + generated adapter + host =="
# Koka's own captured recipe (see run_koka.sh for the full derivation):
KKWARN="-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing -Wno-unknown-pragmas -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value -Wno-unused-but-set-variable"
KKCFLAGS="$KKWARN -O2 -I $KKLIB/include -I $KKLIB/mimalloc/include -DKK_MIMALLOC=8"

gcc $KKWARN -O2 -c -I $KKLIB/include -I $KKLIB/mimalloc/include \
    -DKK_MIMALLOC=8 '-DKK_COMP_VERSION="3.2.3"' '-DKK_CC_NAME="gcc"' \
    -o "$BUILD/kklib.o" "$KKLIB/src/all.c"

gcc $KKCFLAGS -c "$BUILD/caller.c"  -o "$BUILD/caller.o"  -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/callee.c"  -o "$BUILD/callee.o"  -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/kk_adapter.c" -o "$BUILD/adapter.o" -I"$BUILD/koka-out"
gcc -O2 -c "$HERE/host_bounded.c" -o "$BUILD/host_bounded.o"

echo "== stage 4: link =="
gcc -o "$BUILD/c5bounded" \
    "$BUILD/host_bounded.o" "$BUILD/caller.o" "$BUILD/callee.o" \
    "$BUILD/adapter.o" \
    "$BUILD/kklib.o" \
    "$BUILD"/koka-out/*.o \
    -lm -lpthread

echo "== stage 5: run =="
"$BUILD/c5bounded"
