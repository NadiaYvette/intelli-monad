#!/usr/bin/env bash
# The quad-island gold: ONE process, FOUR runtimes, ALL crossings on
# wire-generated glue. This is the C2 + C4 + C5 machinery at full scale:
#
#   C host    (this program, plain gcc)
#   GHC RTS   (Factorial.hs)
#   kklib     (factorial.kk, kk_main_start inside the generated adapter)
#   OCaml RTS (factorial_oc.ml, caml_main inside the generated adapter)
#   rust      (factorial.rs, runtime-free)
#
#   1. drive_quad_wire.py plans THREE crossings through the real
#      mcp-serve binary (rust->koka and rust->ocaml with the generated
#      adapters, rust->haskell with the trampoline filled by hs_factorial).
#   2. Everything compiles; nothing island-facing is hand-written.
#   3. The host drives nine call paths and checks exact values --
#      including the OCaml effect-map sentinel firing in the shared
#      process while the GHC and kklib runtimes keep serving calls.
#
# Link contract (ABI note 4 in organ-bank doc/abi-notes/ocaml.md,
# extended to the four-runtime case): ghc drives the link exactly as
# in the three-runtime gold (run_multi.sh) — -no-hs-main semantics,
# the host's C main is the only main. The OCaml runtime joins the
# SAME link line as an extra archive + island object: caml_main is
# called from the generated adapter, so the runtime's own main member
# is never pulled in and never competes. (Probed with ghc -v: the
# four-runtime object set links clean under ghc's own command; the
# only unresolved symbols were caml_*, fixed by libasmrun.)
#
# Usage: examples/c2-spike/run_quad.sh [path-to-mcp-serve]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRV="${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.8.4/intelli-monad-0.1.3.0/x/mcp-serve/build/mcp-serve/mcp-serve}"
BUILD="$HERE/build-quad"
KKLIB="$HOME/.local/share/koka/v3.2.3/kklib"

if [ ! -x "$SRV" ]; then
  echo "mcp-serve not found at $SRV" >&2
  echo "build it first: cabal v2-build exe:mcp-serve  (or pass the path as \$1)" >&2
  exit 2
fi

echo "== stage 1: wire (three crossings) =="
python3 "$HERE/drive_quad_wire.py" "$SRV" --build-dir "$BUILD"

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

OCINC="$(ocamlfind ocamlc -where)"

gcc $KKCFLAGS -c "$BUILD/caller_a.c" -o "$BUILD/caller_a.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/callee_a.c" -o "$BUILD/callee_a.o" -I"$BUILD/koka-out"
gcc $KKCFLAGS -c "$BUILD/kk_adapter_quad.c" -o "$BUILD/adapter_a.o" -I"$BUILD/koka-out"
gcc $KKWARN -O2 -c "$BUILD/caller_b.c" -o "$BUILD/caller_b.o"
gcc $KKWARN -O2 -c "$BUILD/callee_b.c" -o "$BUILD/callee_b.o"
gcc $KKWARN -O2 -c "$BUILD/caller_c.c" -o "$BUILD/caller_c.o"
gcc $KKWARN -O2 -c "$BUILD/callee_c.c" -o "$BUILD/callee_c.o"
gcc $KKWARN -O2 -c "$BUILD/oc_adapter_quad.c" -o "$BUILD/adapter_c.o" -I"$OCINC"

export PATH="$HOME/.ghcup/bin:$PATH"
GHCLIB="$(ghc --print-libdir)"
GHCINC="$GHCLIB/x86_64-linux-ghc-9.8.4/rts-1.0.2/include"
gcc -O2 -c "$HERE/host_quad.c" -o "$BUILD/host_quad.o" -I"$GHCINC"
ghc -O2 -c "$HERE/Factorial.hs" -outputdir "$BUILD/hsout" -o "$BUILD/Factorial.o"
ocamlopt -c "$HERE/factorial_oc.ml" -I "$HERE" -o "$BUILD/factorial_oc.cmx"

# The startup object ocamlopt links into every native executable defines
# caml_program (the module-initializer chain, ending in the island's
# entry) plus the frametable the RTS unwinder needs. ghc drives our
# link, so we emit it ourselves (-dstartup) and assemble it — exactly
# the pieces ocamlopt would have added.
ocamlopt -dstartup -I "$BUILD" -o "$BUILD/quadstartup" "$HERE/factorial_oc.ml"
gcc -c "$BUILD/quadstartup.startup.s" -o "$BUILD/quadstartup.o"

echo "== stage 4: link (ghc drives, one process, four runtimes) =="
# ghc drives exactly as in run_multi.sh; the OCaml island object and
# the native runtime archive join the same command line.
OCINC="$(ocamlfind ocamlc -where)"
ghc -no-hs-main "$BUILD/host_quad.o" \
    "$BUILD/caller_a.o" "$BUILD/callee_a.o" "$BUILD/adapter_a.o" \
    "$BUILD/caller_b.o" "$BUILD/callee_b.o" \
    "$BUILD/caller_c.o" "$BUILD/callee_c.o" "$BUILD/adapter_c.o" \
    "$BUILD/Factorial.o" "$BUILD/libfactorial_rs.a" \
    "$BUILD/factorial_oc.o" \
    "$BUILD/quadstartup.o" \
    "$BUILD/kklib.o" \
    "$BUILD"/koka-out/*.o \
    "$OCINC/stdlib.a" "$OCINC/std_exit.o" "$OCINC/libasmrun.a" \
    -o "$BUILD/quadspike" -lm -lpthread -ldl

echo "== stage 5: run =="
"$BUILD/quadspike"
