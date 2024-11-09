open Ast

(** [parse s] 解析 [s] 到 AST *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] 将表达式 [e] 转换为字符串 *)
let string_of_val = function
  | Int i -> string_of_int i
  | Var _
  | Let _
  | Binop _ ->
    failwith "precondition violated"

(** [is_value e] 判断表达式 [e] 是否只是一个值 *)
let is_value = function
  | Int _ -> true
  | Var _
  | Let _
  | Binop _ ->
    false

(** [subst e v x] 执行替换操作 [e{v/x}] *)
let rec subst e v x =
  match e with
  | Var new_name ->
    (* 如果是同一个变量，直接为值 *)
    if x = new_name then
      v
    else
      e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    (* 先计算右值 *)
    let e1' = subst e1 v x in
    if x = y then
      (* 如果是覆盖赋值（变量遮蔽，let x = x ... in x ...），
         可以在下一个 step 再计算 e2，因为变量还存在 *)
      Let (y, e1', e2)
    else
      (* 如果不是覆盖值（let y = x + 1 in x + y），
         先把 e2 中的该变量替换完，再进行下一个 step，
         这样就没这个变量什么事了 *)
      Let (y, e1', subst e2 v x)

let unbound_var_err = "Unbound variable"

(** [step e] 对表达式 [e] 进行一次求值 *)
let rec step : expr -> expr = function
  (* 由于测试语言中不存在单独一行变量名的语法，因此直接报错（不支持顶层变量） *)
  | Var _ -> failwith unbound_var_err
  | Int _ -> failwith "precondition violated"
  (* 优先执行左侧表达式 *)
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x (* e2{e1/x} *)
  | Let (x, e1, e2) -> Let (x, step e1, e2)

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
