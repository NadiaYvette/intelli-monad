#!/usr/bin/env python3
"""Multi-island gold, stage 1: plan TWO crossings over one wire.

Three islands, one process (run_multi.sh links them together):

  haskell (Factorial.hs, GHC RTS)   rust (factorial.rs, no runtime)
  koka (factorial.kk, kklib)        C host driving everything

Two organ_plan_stub calls through the real mcp-serve binary:

  plan A: rust caller  -> koka callee   (glue omni_rust_factorial_rs_factorial,
                                         generated koka ABI adapter)
  plan B: rust caller  -> haskell callee (glue omni_factorial_hs_call_to_haskell,
                                          trampoline filled with hs_factorial)

The two rust caller qnames differ ONLY in the dictionary (they name the
glue symbols); both role-play the same island. Artifacts land in
build-multi/ and nothing is hand-written afterward.
"""
import argparse
import json
import os
import pathlib
import subprocess
import sys

ap = argparse.ArgumentParser()
ap.add_argument("mcp_serve", help="path to the mcp-serve binary")
ap.add_argument("--build-dir", default="build-multi")
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


# The three islands' dictionary shapes (real qnames, dictionary-known).
haskell = doc("haskell", "Factorial", [
    defn("Factorial", "factorial", fn([], [("ghc-prim", "Int#")], ("ghc-prim", "Int#")))])
# Plan A's rust caller (glue symbol derives from this qname).
rust_a = doc("rust", "factorial_rs", [
    defn("factorial_rs", "factorial", fn([], [("std", "i64")], ("std", "i64")))])
# Plan B's rust caller: a DIFFERENT qname so the two caller glue entries
# do not collide; same island file plays both roles.
rust_b = doc("rust", "factorial_hs_call", [
    defn("factorial_hs_call", "to_haskell", fn([], [("std", "i64")], ("std", "i64")))])
koka = doc("koka", "factorial", [
    defn("factorial", "island-factorial",
         fn([("std/core", "div"), ("std/core", "exn")],
            [("std/core", "int")],
            ("std/core", "int")))])

for name, d in [("factorial.json", haskell), ("factorial_rs.json", rust_a),
                ("factorial_hs_call.json", rust_b), ("factorial_kk.json", koka)]:
    (out / name).write_text(json.dumps(d))

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
                 "clientInfo": {"name": "multispike", "version": "0"}}})
recv()
send({"jsonrpc": "2.0", "method": "notifications/initialized"})

send({"jsonrpc": "2.0", "id": 2, "method": "tools/call",
      "params": {"name": "organ_ingest", "arguments": {"oiPath": str(out)}}})
ing = json.loads(recv()["result"]["content"][0]["text"])
print(f"INGEST: {ing['oiIngested']} ingested, {ing['oiFailed']} failed")


def plan(rpc_id, caller, callee, callee_export, adapter=None):
    (cm, cn, cl), (km, kn, kl) = caller, callee
    send({"jsonrpc": "2.0", "id": rpc_id, "method": "tools/call",
          "params": {"name": "organ_plan_stub", "arguments": {
              "opsModuleA": cm, "opsNameA": cn, "opsLangA": cl,
              "opsModuleB": km, "opsNameB": kn, "opsLangB": kl,
              "opsCalleeExport": callee_export,
              **({"opsCalleeAdapter": adapter} if adapter else {})}}})
    resp = recv()
    if "result" not in resp:
        print("RPC ERROR:", json.dumps(resp))
        raise SystemExit(1)
    return json.loads(resp["result"]["content"][0]["text"])


# plan A: rust -> koka, WITH the generated koka ABI adapter.
pa = plan(3, ("factorial_rs", "factorial", "rust"),
          ("factorial", "island-factorial", "koka"),
          "kk_island_factorial", adapter="kk_island_factorial")
# plan B: rust -> haskell, trampoline filled with the GHC island's entry.
pb = plan(4, ("factorial_hs_call", "to_haskell", "rust"),
          ("Factorial", "factorial", "haskell"),
          "hs_factorial")
p.terminate()

for tag, plan_ in [("A rust->koka", pa), ("B rust->haskell", pb)]:
    print(f"PLAN {tag}:", plan_["opsoVerdict"])

(out / "caller_a.c").write_text(pa["opsoCaller"] + "\n")
(out / "callee_a.c").write_text(pa["opsoCallee"] + "\n")
(out / "plan_a.json").write_text(json.dumps(pa, indent=2))
(out / "caller_b.c").write_text(pb["opsoCaller"] + "\n")
(out / "callee_b.c").write_text(pb["opsoCallee"] + "\n")
(out / "plan_b.json").write_text(json.dumps(pb, indent=2))
if pa.get("opsoAdapter"):
    adapter_lines = pa["opsoAdapter"]
    adapter_text = adapter_lines if isinstance(adapter_lines, str) else "\n".join(adapter_lines)
    (out / "kk_adapter_multi.c").write_text(adapter_text + "\n")
    print("wrote", out / "kk_adapter_multi.c", "(generated koka adapter)")
print("wrote", out / "caller_a.c", "/", out / "callee_a.c")
print("wrote", out / "caller_b.c", "/", out / "callee_b.c")
ok = pa["opsoVerdict"] == "licensed-lossless" and pb["opsoVerdict"] == "licensed-lossless"
sys.exit(0 if ok else 1)
