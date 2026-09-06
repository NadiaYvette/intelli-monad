#!/usr/bin/env python3
"""Reverse-direction gold: the refusal, driven over the real wire.

The C3 gold (drive_koka_wire.py) crosses pure rust -> effectful koka,
the direction the subset rule licenses. This driver swaps the roles:
the EFFECTFUL koka island wants to call the PURE rust island. The
effect-row subset rule must refuse it -- caller <div,exn> is not a
subset of callee's empty row -- and organ_plan_stub must return

    unlicensed-effect-row

with comment-only debris in the stubs. A refusal here is the feature:
the same wire, the same islands, the same tool -- the other direction
cannot be transplanted, and the output says exactly why.
"""
import argparse
import json
import os
import pathlib
import subprocess
import sys

ap = argparse.ArgumentParser()
ap.add_argument("mcp_serve", help="path to the mcp-serve binary")
ap.add_argument("--build-dir", default="build-refused")
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


# Same two islands as the C3 gold -- identical qnames, identical
# effect rows. Only the request direction differs.
rust = doc("rust", "factorial_rs", [
    defn("factorial_rs", "factorial", fn([], [("std", "i64")], ("std", "i64")))])

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
                 "clientInfo": {"name": "c3-refused", "version": "0"}}})
recv()
send({"jsonrpc": "2.0", "method": "notifications/initialized"})

send({"jsonrpc": "2.0", "id": 2, "method": "tools/call",
      "params": {"name": "organ_ingest", "arguments": {"oiPath": str(out)}}})
ing = json.loads(recv()["result"]["content"][0]["text"])
print(f"INGEST: {ing['oiIngested']} ingested, {ing['oiFailed']} failed")

# THE REVERSED REQUEST: caller = the koka island (effectful <div,exn>),
# callee = the rust island (pure). No adapter, no export override: a
# refused crossing must refuse before any of that matters.
send({"jsonrpc": "2.0", "id": 3, "method": "tools/call",
      "params": {"name": "organ_plan_stub", "arguments": {
          "opsModuleA": "factorial", "opsNameA": "island-factorial", "opsLangA": "koka",
          "opsModuleB": "factorial_rs", "opsNameB": "factorial", "opsLangB": "rust"}}})
plan = json.loads(recv()["result"]["content"][0]["text"])
p.terminate()

print("VERDICT:", plan["opsoVerdict"])
(out / "refused.c").write_text("\n".join(plan["opsoStubs"]) + "\n")
(out / "plan.json").write_text(json.dumps(plan, indent=2))

stubs = plan["opsoStubs"]
comment_only = all(s.startswith("//") for s in stubs)
print("comment-only debris:", comment_only)
for line in stubs:
    if "caller requires effects" in line or "caller row:" in line:
        print("  ", line)

ok = (plan["opsoVerdict"] == "unlicensed-effect-row"
      and plan["opsoCaller"] == "" and plan["opsoCallee"] == ""
      and comment_only
      and any("std/core/exn" in s for s in stubs)
      and any("subset" in s for s in stubs))
print("REFUSAL AS PREDICTED" if ok else "UNEXPECTED PLAN — see plan.json")
sys.exit(0 if ok else 1)
