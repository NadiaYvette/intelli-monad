#!/usr/bin/env bash
# The C5 OCaml gold: the declared-bound OCaml island called through
# the wire's plain int64_t ABI, with the adapter checking the bound
# BEFORE Val_long boxing -- the doc's C5 milestone text, now running:
#
#   "a declared bound licenses the crossing that the unbounded member
#    would refuse; the generated glue checks it at the crossing and
#    violations surface as the wire's status sentinel, never silent
#    truncation"
#
#   1. drive_ocaml_wire.py ingests the islands through the real
#      mcp-serve binary; the island's types carry the dictionary's
#      declared-bound member Stdlib/int/bounded61, the verdict is
#      licensed-bounded, and the generated adapter is the CHECKED
#      form (pre-boxing range guard, sentinel on violation).
#   2. ocamlopt compiles the island; the wire glue and adapter compile
#      with gcc against the OCaml stdlib includes.
#   3. ocamlopt DRIVES THE LINK (ABI note 4): its C main wins over the
#      runtime's archive member -- the OCaml-side counterpart of
#      ghc -no-hs-main.
#   4. The host exercises the contract paths: in-range values through
#      the wire glue and adapter-direct, and the arg-guard violations
#      surfacing the sentinel.
#
# Usage: examples/c2-spike/run_ocaml.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-ocaml"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire (bounded OCaml callee + adapter requested) =="
python3 "$HERE/drive_ocaml_wire.py" "$SRV" --build-dir "$BUILD"

echo "== stage 2: compile (ocaml island + glue + adapter + host) =="
OCINC="$(ocamlfind ocamlc -where)"

rustc --edition 2021 -O --crate-type staticlib "$HERE/factorial.rs" -o "$BUILD/libfactorial_rs.a"
gcc -O2 -c "$BUILD/caller.c" -o "$BUILD/caller.o"
gcc -O2 -c "$BUILD/callee.c" -o "$BUILD/callee.o"
gcc -O2 -c "$BUILD/oc_adapter.c" -o "$BUILD/oc_adapter.o" -I"$OCINC"
gcc -O2 -c "$HERE/host_ocaml.c" -o "$BUILD/host_ocaml.o"
ocamlopt -c "$HERE/factorial_oc.ml" -I "$HERE" -o "$BUILD/factorial_oc.cmx"

echo "== stage 3: link (ocamlopt drives) =="
ocamlopt -I "$HERE" -o "$BUILD/c5ocaml" \
    "$BUILD/host_ocaml.o" "$BUILD/caller.o" "$BUILD/callee.o" \
    "$BUILD/oc_adapter.o" "$BUILD/factorial_oc.cmx" \
    "$BUILD/libfactorial_rs.a"

echo "== stage 4: run =="
"$BUILD/c5ocaml"
