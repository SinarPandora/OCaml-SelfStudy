open Ast

(** [parse s] 解析 [s] 到 AST *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] 将表达式 [e] 转换为字符串 *)
let string_of_val = function
  | Int i -> string_of_int i
  | Binop _ -> failwith "precondition violated"

(** [is_value e] 判断表达式 [e] 是否只是一个值 *)
let is_value = function
  | Int _ -> true
  | Binop _ -> false

(** [step e] 对表达式 [e] 进行一次求值 *)
let rec step : expr -> expr = function
  | Int _ -> failwith "precondition violated"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)

and step_bop bop e1 e2 =
  match (bop, e1, e2) with
  | (Add, Int a, Int b) -> Int (a + b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | _ -> failwith "precondition violated"

(** [eval e] 执行表达式 [e]，直至获得最终的值表达式，并返回它 *)
let rec eval (e : expr) : expr =
  if is_value e then
    e
  else
    e |> step |> eval

(** [interp s] 解析源代码 [s] 通过词法和语法分析器，执行它，
    并将结果作为 [string] 返回 *)
let interp (s : string) : string = s |> parse |> eval |> string_of_val
