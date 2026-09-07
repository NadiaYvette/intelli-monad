#!/usr/bin/env python3
"""C5 gold, stage 1: drive the wire with an OCaml callee.

Rust island (std/i64, pure) calls into an OCaml island whose types
carry the dictionary's declared-bound member Stdlib/int/bounded61
(|v| < 2^61 over the 63-bit tagged int) — the crossing the unbounded
Stdlib/int would refuse as a narrowing. The verdict is
licensed-bounded and the generated adapter checks the bound BEFORE
Val_long boxing (which would silently drop the top bits).

Artifacts land in build-ocaml/ and nothing is hand-written afterward:
caller.c/callee.c (wire glue, callee trampoline filled via
opsCalleeExport) plus the generated OCaml ABI adapter (opsCalleeAdapter).
"""
import argparse
import json
import os
import pathlib
import subprocess
import sys

ap = argparse.ArgumentParser()
ap.add_argument("mcp_serve", help="path to the mcp-serve binary")
ap.add_argument("--build-dir", default="build-ocaml")
args = ap.parse_args()

out = pathlib.Path(args.build_dir)
if out.exists():
    import shutil
    shutil.rmtree(out)
out.mkdir(parents=True)


def q(m, n):
    return {"con": {"qname": {"module": m, "name": {"text": n}}}}

def fn(effects, args_, res):
    return {"fn": {"args": [{"multiplicity": "many", "type": q(*a)} for a in args_],
                   "effect": {"effects": [{"module": m, "name": {"text": e}} for (m, e) in effects]},
                   "result": q(*res)}}

def doc(lang, mod, defs):
    return {"schema_version": "1.0.0",
            "metadata": {"source_language": lang, "shim_version": "0.1.0"},
            "module": {"name": mod, "definitions": defs, "data_types": [], "effect_decls": []}}

def defn(mod, name, ty):
    return {"name": {"module": mod, "name": {"text": name, "unique": 1}},
            "type": ty, "expr": {}, "sort": "fun", "visibility": "public"}

# Caller: the rust island, pure over std/i64 (dictionary-known).
rust = doc("rust", "factorial_rs", [
    defn("factorial_rs", "factorial", fn([], [("std", "i64")], ("std", "i64")))])

# Callee: the OCaml island with the declared-bound int — the C5
# contract. Plain Stdlib/int would refuse an i64 caller (narrowing).
ocaml = doc("ocaml", "factorial_oc", [
    defn("factorial_oc", "island-factorial",
         fn([], [("Stdlib/int", "bounded61")], ("Stdlib/int", "bounded61")))])

(out / "factorial_rs.json").write_text(json.dumps(rust))
(out / "factorial_oc.json").write_text(json.dumps(ocaml))

env = dict(os.environ, ORGAN_INDEX=str(out / "index.db"))
p = subprocess.Popen([args.mcp_serve], stdin=subprocess.PIPE,
                     stdout=subprocess.PIPE, env=env, text=True)

def send(o):
    p.stdin.write(json.dumps(o) + "\n"); p.stdin.flush()

def recv():
    return json.loads(p.stdout.readline())

send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
      "params": {"protocolVersion": "2025-06-18", "capabilities": {},
                 "clientInfo": {"name": "c5ocaml", "version": "0"}}})
recv()
send({"jsonrpc": "2.0", "method": "notifications/initialized"})

send({"jsonrpc": "2.0", "id": 2, "method": "tools/call",
      "params": {"name": "organ_ingest", "arguments": {"oiPath": str(out)}}})
ing = json.loads(recv()["result"]["content"][0]["text"])
print(f"INGEST: {ing['oiIngested']} ingested, {ing['oiFailed']} failed")

send({"jsonrpc": "2.0", "id": 3, "method": "tools/call",
      "params": {"name": "organ_plan_stub", "arguments": {
          "opsModuleA": "factorial_rs", "opsNameA": "factorial", "opsLangA": "rust",
          "opsModuleB": "factorial_oc", "opsNameB": "island-factorial", "opsLangB": "ocaml",
          "opsCalleeExport": "ocaml_island_factorial",
          "opsCalleeAdapter": "ocaml_island_factorial"}}})
plan = json.loads(recv()["result"]["content"][0]["text"])
p.terminate()

print("VERDICT:", plan["opsoVerdict"])
(out / "caller.c").write_text(plan["opsoCaller"] + "\n")
(out / "callee.c").write_text(plan["opsoCallee"] + "\n")
(out / "plan.json").write_text(json.dumps(plan, indent=2))
print("wrote", out / "caller.c")
print("wrote", out / "callee.c")
adapter_lines = plan.get("opsoAdapter") or []
if adapter_lines:
    adapter_text = adapter_lines if isinstance(adapter_lines, str) else "\n".join(adapter_lines)
    (out / "oc_adapter.c").write_text(adapter_text + "\n")
    print("wrote", out / "oc_adapter.c", "(C5 generated OCaml ABI adapter)")
sys.exit(0 if plan["opsoVerdict"] == "licensed-bounded" else 1)
