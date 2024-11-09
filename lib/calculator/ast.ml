(** BNF:
  bop ::= + | * | <=
*)
type bop =
  | Add
  | Mult
  | Leq

(** BNF:
  e ::= x | i | b
    | e1 bop e2 
    | ( e )
    | x
    | let x = e1 in e2
  
  i ::= integers
  b ::= booleans
  x ::= identifiers
  v ::= i | b
*)
type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr (* 二元操作 *)
  | Let of string * expr * expr
  | If of expr * expr * expr
