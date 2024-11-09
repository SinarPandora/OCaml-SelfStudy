(** BNF:
  bop ::= + | *
*)
type bop =
  | Add
  | Mult

(** BNF:
  e ::= i
    | e1 bop e2
    | (e)
*)
type expr =
  | Int of int
  | Binop of bop * expr * expr (* 二元操作 *)
