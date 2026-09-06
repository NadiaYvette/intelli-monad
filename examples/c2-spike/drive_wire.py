#!/usr/bin/env python3
"""Gold loop, stage 1: drive the wire.

Two OrganIR docs with genuine dictionary qnames (haskell
ghc-prim/Int# and rust std/i64) are ingested through the real
mcp-serve binary. organ_plan_stub renders BOTH glue sides with the
callee trampoline filled (via opsCalleeExport) — the artifacts land in
build/ and nothing is hand-written afterward.
"""
import argparse
import json
import os
import pathlib
import shutil
import subprocess
import sys

ap = argparse.ArgumentParser()
ap.add_argument("mcp_serve", help="path to the mcp-serve binary")
ap.add_argument("--build-dir", default="build")
args = ap.parse_args()

out = pathlib.Path(args.build_dir)
if out.exists():
    shutil.rmtree(out)
out.mkdir(parents=True)


def q(m, n):
    return {"con": {"qname": {"module": m, "name": {"text": n}}}}


def fn(effects, args_, res):
    return {
        "fn": {
            "args": [{"multiplicity": "many", "type": q(*a)} for a in args_],
            "effect": {"effects": [{"module": m, "name": {"text": e}} for (m, e) in effects]},
            "result": q(*res),
        }
    }


def doc(lang, mod, defs):
    return {
        "schema_version": "1.0.0",
        "metadata": {"source_language": lang, "shim_version": "0.1.0"},
        "module": {"name": mod, "definitions": defs, "data_types": [], "effect_decls": []},
    }


def defn(mod, name, ty):
    return {
        "name": {"module": mod, "name": {"text": name, "unique": 1}},
        "type": ty,
        "expr": {},
        "sort": "fun",
        "visibility": "public",
    }


# Real dictionary qnames; pure rows on both sides normalize to ∅, so
# the crossing licenses as lossless under the C3 subset rule.
haskell = doc("haskell", "Factorial", [
    defn("Factorial", "factorial", fn([], [("ghc-prim", "Int#")], ("ghc-prim", "Int#")))])
rust = doc("rust", "factorial_rs", [
    defn("factorial_rs", "factorial", fn([], [("std", "i64")], ("std", "i64")))])

(pathlib.Path(args.build_dir)).mkdir(exist_ok=True, parents=True)
(out / "factorial.json").write_text(json.dumps(haskell))
(out / "factorial_rs.json").write_text(json.dumps(rust))

env = dict(os.environ, ORGAN_INDEX=str(out / "index.db"))
p = subprocess.Popen([args.mcp_serve], stdin=subprocess.PIPE, stdout=subprocess.PIPE, env=env, text=True)


def send(o):
    p.stdin.write(json.dumps(o) + "\n")
    p.stdin.flush()


def recv():
    return json.loads(p.stdout.readline())


send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
      "params": {"protocolVersion": "2025-06-18", "capabilities": {},
                 "clientInfo": {"name": "c2spike", "version": "0"}}})
recv()
send({"jsonrpc": "2.0", "method": "notifications/initialized"})

send({"jsonrpc": "2.0", "id": 2, "method": "tools/call",
      "params": {"name": "organ_ingest", "arguments": {"oiPath": str(out)}}})
ing = json.loads(recv()["result"]["content"][0]["text"])
print(f"INGEST: {ing['oiIngested']} ingested, {ing['oiFailed']} failed")

# caller = haskell island, callee = rust island. The callee island's
# real entry is passed explicitly (opsCalleeExport) — the same name the
# Rust island actually exports (see factorial.rs).
send({"jsonrpc": "2.0", "id": 3, "method": "tools/call",
      "params": {"name": "organ_plan_stub", "arguments": {
          "opsModuleA": "Factorial", "opsNameA": "factorial", "opsLangA": "haskell",
          "opsModuleB": "factorial_rs", "opsNameB": "factorial", "opsLangB": "rust",
          "opsCalleeExport": "rs_island_factorial"}}})
plan = json.loads(recv()["result"]["content"][0]["text"])
p.terminate()

print("VERDICT:", plan["opsoVerdict"])
(out / "caller.c").write_text(plan["opsoCaller"] + "\n")
(out / "callee.c").write_text(plan["opsoCallee"] + "\n")
(out / "plan.json").write_text(json.dumps(plan, indent=2))
print("wrote", out / "caller.c")
print("wrote", out / "callee.c")
sys.exit(0 if plan["opsoVerdict"] == "licensed-lossless" else 1)
