%token <int> INT
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

(* 声明结合性，并且越往下，操作符优先级越高 *)
(* 左结合说明 AST 尽量往左侧搭建 *)
%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
  (* 返回 e *)
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  (* 左边一个表达式，中间一个加号，右边一个表达式。
     构造结果：二元操作（加法，左表达式，右表达式） *)
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;
