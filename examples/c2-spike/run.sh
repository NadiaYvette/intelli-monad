#!/usr/bin/env bash
# The gold loop, end to end: wire -> compile -> run.
#
#   1. drive_wire.py ingests two OrganIR docs through the real
#      mcp-serve binary and saves BOTH organ_plan_stub glue sides.
#      Nothing is hand-written afterward: the callee trampoline comes
#      filled (opsCalleeExport).
#   2. The Rust island compiles as a staticlib; the Haskell island with
#      ghc -c; the wire glue with gcc; everything links with
#      ghc -no-hs-main (so base/ghc-prim/RTS resolve).
#   3. The host drives three call paths and checks exact values; any
#      mismatch is a nonzero exit.
#
# Usage: examples/c2-spike/run.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire =="
python3 "$HERE/drive_wire.py" "$SRV" --build-dir "$BUILD"

echo "== stage 2: compile =="
export PATH="$HOME/.ghcup/bin:$PATH"
GHCLIB="$(ghc --print-libdir)"
GHCINC="$GHCLIB/x86_64-linux-ghc-9.8.4/rts-1.0.2/include"

rustc --edition 2021 -O --crate-type staticlib "$HERE/factorial.rs" -o "$BUILD/libfactorial_rs.a"
gcc -O2 -c "$BUILD/caller.c" -o "$BUILD/caller.o"
gcc -O2 -c "$BUILD/callee.c" -o "$BUILD/callee.o"
gcc -O2 -c "$HERE/host.c" -o "$BUILD/host.o" -I"$GHCINC"
ghc -O2 -c "$HERE/Factorial.hs" -outputdir "$BUILD/hsout" -o "$BUILD/Factorial.o"
ghc -no-hs-main "$BUILD/host.o" "$BUILD/caller.o" "$BUILD/callee.o" \
    "$BUILD/Factorial.o" "$BUILD/libfactorial_rs.a" -o "$BUILD/c2spike"

echo "== stage 3: run =="
"$BUILD/c2spike"
