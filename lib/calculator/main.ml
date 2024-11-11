open Ast

let unbound_var_err = "Unbound variable"

let syntax_err = "Syntax error"

let precondition_violated_err = "Precondition violated"

let bop_err = "Operator and operand type mismatch"

let if_guard_err = "Guard of if must have type bool"

(** [Env] 是语句执行过程中的上下文环境 *)
module Env = Map.Make (String)

(** [empty_env] 将创建一个空的环境 *)
let empty_env = Env.empty

(** [env] 表示环境的类型，其中 [value] 指的是我们现在支持的值，
    有整数和布尔两种 *)
type env = value Env.t

and value =
  | VInt of int
  | VBool of bool

(** [parse s] 解析 [s] 到 AST *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [eval e] 大步执行表达式，直接从 [e] 执行到 [v] *)
let rec eval (env : env) (e : expr) : value =
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Var x -> eval_var env x
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3

and eval_var env x =
  try Env.find x env with
  | Not_found -> failwith unbound_var_err

and eval_let env x e1 e2 =
  let v1 = eval env e1 in
  let env' = Env.add x v1 env in
  eval env' e2

and eval_bop env bop e1 e2 =
  match (bop, eval env e1, eval env e2) with
  | (Add, VInt a, VInt b) -> VInt (a + b)
  | (Mult, VInt a, VInt b) -> VInt (a * b)
  | (Leq, VInt a, VInt b) -> VBool (a <= b)
  | _ -> failwith bop_err

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool true -> eval env e2
  | VBool false -> eval env e3
  | _ -> failwith if_guard_err

(** [string_of_val e] 将表达式 [e] 转换为字符串 *)
let string_of_val = function
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i

(** [interp s] 解析源代码 [s] 通过词法和语法分析器，大步执行它，
    并将结果作为 [string] 返回 *)
let interp (s : string) : string = s |> parse |> eval empty_env |> string_of_val
