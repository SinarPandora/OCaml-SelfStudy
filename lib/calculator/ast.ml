type bop =
  | Add
  | Mult

type expr =
  | Int of int
  | Binop of bop * expr * expr (* 二元操作 *)
