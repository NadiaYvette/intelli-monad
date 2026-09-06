#!/usr/bin/env python3
"""C3 gold, stage 1: drive the wire with a Koka callee.

Rust island (std/i64, pure) calls into a Koka island (std/core/int,
effect row {std/core/div, std/core/exn}) — the direction the C3
subset rule licenses (caller's row ∅ ⊆ callee's row). The reverse is
refused by the same rule (exercised in test/StubSpec.hs).

Artifacts land in build-koka/ and nothing is hand-written afterward:
the callee trampoline comes filled via opsCalleeExport with the
adapter's entry (kk_island_factorial, see factorial_kk_adapter.c).
"""
import argparse
import json
import os
import pathlib
import subprocess
import sys

ap = argparse.ArgumentParser()
ap.add_argument("mcp_serve", help="path to the mcp-serve binary")
ap.add_argument("--build-dir", default="build-koka")
args = ap.parse_args()

out = pathlib.Path(args.build_dir)
if out.exists():
    import shutil
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


# Caller: the rust island, pure over std/i64 (dictionary-known).
rust = doc("rust", "factorial_rs", [
    defn("factorial_rs", "factorial", fn([], [("std", "i64")], ("std", "i64")))])

# Callee: the koka island, the corpus's real shape — module `factorial`,
# std/core/int (arbitrary precision, ABI i64-representable) with effect
# row <div,exn>. Caller row ∅ ⊆ callee row, so the subset rule licenses.
koka = doc("koka", "factorial", [
    defn("factorial", "island-factorial",
         fn([("std/core", "div"), ("std/core", "exn")],
            [("std/core", "int")],
            ("std/core", "int")))])

(out / "factorial_rs.json").write_text(json.dumps(rust))
(out / "factorial_kk.json").write_text(json.dumps(koka))

env = dict(os.environ, ORGAN_INDEX=str(out / "index.db"))
p = subprocess.Popen([args.mcp_serve], stdin=subprocess.PIPE,
                     stdout=subprocess.PIPE, env=env, text=True)


def send(o):
    p.stdin.write(json.dumps(o) + "\n")
    p.stdin.flush()


def recv():
    return json.loads(p.stdout.readline())


send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
      "params": {"protocolVersion": "2025-06-18", "capabilities": {},
                 "clientInfo": {"name": "c3spike", "version": "0"}}})
recv()
send({"jsonrpc": "2.0", "method": "notifications/initialized"})

send({"jsonrpc": "2.0", "id": 2, "method": "tools/call",
      "params": {"name": "organ_ingest", "arguments": {"oiPath": str(out)}}})
ing = json.loads(recv()["result"]["content"][0]["text"])
print(f"INGEST: {ing['oiIngested']} ingested, {ing['oiFailed']} failed")

# caller = rust island (pure), callee = koka island (<div,exn>).
# opsCalleeExport is the ADAPTER's entry — the int64 ABI the filled
# trampoline calls; the adapter in turn reaches koka's real export.
send({"jsonrpc": "2.0", "id": 3, "method": "tools/call",
      "params": {"name": "organ_plan_stub", "arguments": {
          "opsModuleA": "factorial_rs", "opsNameA": "factorial", "opsLangA": "rust",
          "opsModuleB": "factorial", "opsNameB": "island-factorial", "opsLangB": "koka",
          "opsCalleeExport": "kk_island_factorial"}}})
plan = json.loads(recv()["result"]["content"][0]["text"])
p.terminate()

print("VERDICT:", plan["opsoVerdict"])
(out / "caller.c").write_text(plan["opsoCaller"] + "\n")
(out / "callee.c").write_text(plan["opsoCallee"] + "\n")
(out / "plan.json").write_text(json.dumps(plan, indent=2))
print("wrote", out / "caller.c")
print("wrote", out / "callee.c")
if plan["opsoVerdict"] != "licensed-lossless":
    print("UNEXPECTED VERDICT — full plan:", json.dumps(plan, indent=2))
sys.exit(0 if plan["opsoVerdict"] == "licensed-lossless" else 1)
