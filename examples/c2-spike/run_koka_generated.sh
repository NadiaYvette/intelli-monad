#!/usr/bin/env bash
# The C4 gold loop: wire -> compile -> run, with EVERY artifact
# wire-generated -- including the koka ABI adapter.
#
# Difference from run_koka.sh: no factorial_kk_adapter.c exists here.
# organ_plan_stub emits the adapter (opsCalleeAdapter), projecting the
# wire's plain int64_t ABI onto koka's real kk_integer_t +
# kk_context_t* ABI and owning the kk_main_start/module-init contract.
# The hand-written adapter the C3 gold used becomes a reviewed diff
# against the generated one, not a build input.
#
# Usage: examples/c2-spike/run_koka_generated.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-koka-gen"
KKLIB="$HOME/.local/share/koka/v3.2.3/kklib"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire (adapter generated) =="
python3 "$HERE/drive_koka_wire.py" "$SRV" --build-dir "$BUILD" --adapter

echo "== stage 2: koka island =="
mkdir -p "$BUILD/koka-out"
cp "$HERE/factorial.kk" "$BUILD/koka-out/"
(cd "$BUILD/koka-out" && koka -c -l factorial.kk --outputdir=.)

echo "== stage 3: compile glue + rust + GENERATED adapter + host =="
KKWARN="-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing -Wno-unknown-pragmas -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value -Wno-unused-but-set-variable"
KKCFLAGS="$KKWARN -O2 -I $KKLIB/include -I $KKLIB/mimalloc/include -DKK_MIMALLOC=8"

gcc $KKWARN -O2 -c -I $KKLIB/include -I $KKLIB/mimalloc/include \
    -DKK_MIMALLOC=8 '-DKK_COMP_VERSION="3.2.3"' '-DKK_CC_NAME="gcc"' \
    -o "$BUILD/kklib.o" "$KKLIB/src/all.c"

rustc --edition 2021 -O --crate-type staticlib "$HERE/factorial.rs" -o "$BUILD/libfactorial_rs.a"
gcc $KKCFLAGS -c "$BUILD/caller.c"      -o "$BUILD/caller.o"  -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/callee.c"      -o "$BUILD/callee.o"  -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/kk_adapter.c"  -o "$BUILD/adapter.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$HERE/host_koka_gen.c" -o "$BUILD/host_koka_gen.o"

echo "== stage 4: link =="
gcc -o "$BUILD/c3spike-gen" \
    "$BUILD/host_koka_gen.o" "$BUILD/caller.o" "$BUILD/callee.o" \
    "$BUILD/adapter.o" \
    "$BUILD/libfactorial_rs.a" \
    "$BUILD/kklib.o" \
    "$BUILD"/koka-out/*.o \
    -lm -lpthread

echo "== stage 5: run =="
"$BUILD/c3spike-gen"
