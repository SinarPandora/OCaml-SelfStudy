(** BNF:
  bop ::= + | *
*)
type bop =
  | Add
  | Mult

(** BNF:
  e ::= i
    | e1 bop e2 
    | ( e )
    | x
    | let x = e1 in e2
  
  i ::= integers
  x ::= identifiers
  v ::= i
*)
type expr =
  | Var of string
  | Int of int
  | Binop of bop * expr * expr (* 二元操作 *)
  | Let of string * expr * expr
