#!/usr/bin/env bash
# The C3 vision, complete: the effectful koka island called through the
# wire's plain int64_t ABI, with the island's exceptions mapped to the
# caller's convention -- the doc's C3 milestone text, now running:
#
#   "a {div,exn} call must force the stub to declare how the effects
#    surface (exceptions, error codes, or explicit capability passing)"
#
#   1. drive_koka_wire.py --effect-map --adapter ingests the islands
#      through the real mcp-serve binary and saves ALL artifacts: the
#      wire glue, the C4 adapter (forwarding to the MAPPED entry), and
#      the island-side factorial_emap.kk shim.
#   2. Koka compiles the island + the generated shim; the shim's
#      handle/try maps any island exception to min-int64, so the
#      crossing's error channel is a plain int64 return value.
#   3. The host exercises the mapped path end to end (rust island ->
#      wire glue -> trampoline -> adapter -> mapped entry -> koka
#      island) and checks exact values.
#
# Usage: examples/c2-spike/run_koka_mapped.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-koka-mapped"
KKLIB="$HOME/.local/share/koka/v3.2.3/kklib"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire (effect map + adapter requested) =="
python3 "$HERE/drive_koka_wire.py" "$SRV" --build-dir "$BUILD" --effect-map --adapter

echo "== stage 2: koka islands (island + generated effect-map shim) =="
# The shim source comes straight from the wire (factorial_emap.kk):
# nothing hand-written. Compiling island + shim together lets the
# shim's `import factorial` resolve; both modules' __init/__done are
# statically guarded and idempotent.
mkdir -p "$BUILD/koka-out"
cp "$HERE/factorial.kk" "$BUILD/koka-out/"
cp "$BUILD/factorial_emap.kk" "$BUILD/koka-out/"
(cd "$BUILD/koka-out" && koka -c -l factorial.kk factorial_emap.kk --outputdir=.)

echo "== stage 3: compile glue + rust + generated adapter + host =="
# Koka's own captured recipe (see run_koka.sh for the full derivation):
KKWARN="-Wall -Wextra -Wpointer-arith -Wshadow -Wstrict-aliasing -Wno-unknown-pragmas -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value -Wno-unused-but-set-variable"
KKCFLAGS="$KKWARN -O2 -I $KKLIB/include -I $KKLIB/mimalloc/include -DKK_MIMALLOC=8"

gcc $KKWARN -O2 -c -I $KKLIB/include -I $KKLIB/mimalloc/include \
    -DKK_MIMALLOC=8 '-DKK_COMP_VERSION="3.2.3"' '-DKK_CC_NAME="gcc"' \
    -o "$BUILD/kklib.o" "$KKLIB/src/all.c"

rustc --edition 2021 -O --crate-type staticlib "$HERE/factorial.rs" -o "$BUILD/libfactorial_rs.a"
gcc $KKCFLAGS -c "$BUILD/caller.c"  -o "$BUILD/caller.o"  -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/callee.c"  -o "$BUILD/callee.o"  -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/kk_adapter.c" -o "$BUILD/adapter.o" -I"$BUILD/koka-out"
gcc -O2 -c "$HERE/host_koka_mapped.c" -o "$BUILD/host_koka_mapped.o"

echo "== stage 4: link =="
gcc -o "$BUILD/c3mapped" \
    "$BUILD/host_koka_mapped.o" "$BUILD/caller.o" "$BUILD/callee.o" \
    "$BUILD/adapter.o" \
    "$BUILD/libfactorial_rs.a" \
    "$BUILD/kklib.o" \
    "$BUILD"/koka-out/*.o \
    -lm -lpthread

echo "== stage 5: run =="
"$BUILD/c3mapped"
