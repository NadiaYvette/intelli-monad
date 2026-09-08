(* The OCaml island for the C5 bounded-contract gold.
 *
 * Compiled by the real ocamlopt; its `island-factorial` carries the
 * dictionary's declared-bound member Stdlib/int/bounded61 (|v| < 2^61
 * over the 63-bit tagged int). The entry is registered under the same
 * name the generated adapter fetches with caml_named_value — the
 * island's own convention (organ-bank doc/abi-notes/ocaml.md §2).
 *)
let rec island_factorial n =
  (* Effect-map contract: the island's own exception (distinct from the
     wire-side bound guard) must surface as the wire's status sentinel
     via caml_callback_exn in the generated adapter — and the RTS must
     stay usable afterwards. The island domain stops at 25! so out-of-
     domain inputs raise instead of silently wrapping the int63. *)
  if n > 25 then raise (Invalid_argument "island domain: input > 25")
  else if n < 2 then 1 else n * island_factorial (n - 1)

let () = Callback.register "island-factorial" island_factorial
