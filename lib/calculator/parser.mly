(* 携带 int 数据的 token *)
%token <int> INT
%token <string> ID
(* 本身表示特定含义的 token *)
%token TRUE
%token FALSE
%token PLUS
%token TIMES
%token LEQ
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

(* 声明结合性，并且越往下，操作符优先级越高 *)
(* nonassoc：不能结合，表示 x + y + z 不能同时出现，
   必须拆分成子句（比如使用括号）*)
%nonassoc ELSE
%nonassoc IN
(* 左结合： x + y + z => (x + y) + z，优先计算左侧值 *)
%left LEQ
%left PLUS
%left TIMES

(* 指名解析出的内容为我们定义的 Ast 模块下的 expr 类型 *)
%start <Ast.expr> prog

%%

prog:
  (* 返回 e *)
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  (** 左边一个表达式，中间一个加号，右边一个表达式。
      构造结果：二元操作（加法，左表达式，右表达式）
      分开构建（而不是创建一个统一的二元操作模式）有助于提升性能
  *)
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e = expr; RPAREN { e }
  ;
