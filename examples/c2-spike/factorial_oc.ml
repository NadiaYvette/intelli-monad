(* The OCaml island for the C5 bounded-contract gold.
 *
 * Compiled by the real ocamlopt; its `island-factorial` carries the
 * dictionary's declared-bound member Stdlib/int/bounded61 (|v| < 2^61
 * over the 63-bit tagged int). The entry is registered under the same
 * name the generated adapter fetches with caml_named_value — the
 * island's own convention (organ-bank doc/abi-notes/ocaml.md §2).
 *)
let rec island_factorial n =
  if n < 2 then 1 else n * island_factorial (n - 1)

let () = Callback.register "island-factorial" island_factorial
