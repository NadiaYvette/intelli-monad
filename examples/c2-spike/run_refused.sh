#!/usr/bin/env bash
# The C3 rule, proven live from the refused side.
#
# The C3 gold loop (run_koka.sh) crosses the direction the effect-row
# subset rule licenses: a pure rust caller into the koka island's
# <div,exn> callee. This script drives the SAME two islands the
# OTHER way -- the effectful koka island calling the pure rust one --
# and expects organ_plan_stub to refuse over the wire:
#
#   verdict: unlicensed-effect-row
#   reason:  caller requires effects the callee does not provide
#
# The refusal renders as comment-only debris: a stub generator must
# refuse to emit code for unlicensed crossings. Exit is 0 iff the
# refusal came back exactly as predicted.
#
# Usage: examples/c2-spike/run_refused.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-refused"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== wire: koka caller (effectful) -> rust callee (pure) =="
python3 "$HERE/drive_refused_wire.py" "$SRV" --build-dir "$BUILD"

echo
echo "refusal artifacts (comment-only debris, by design):"
sed -n '1,8p' "$BUILD/refused.c"
