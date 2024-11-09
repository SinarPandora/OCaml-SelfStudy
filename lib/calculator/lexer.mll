{
  (* 此处的代码将复制到生成的 lexer.ml 文件中 *)
  open Parser
}

(* 此处为类似正则的语法，OCamllex 不支持缩写转义字符比如 \s
   参考：https://ocaml.org/manual/5.2/lexyacc.html *)
let white = [' ' '\t']+
(* 数字，包含 0-9 *)
let digit = ['0'-'9']
(* 整数，包含一个可选的 -，和任意个数字 *)
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* rule 和 parse 是关键字 *)
rule read =
  parse
  (* 遇到空白字符时直接递归调用自己（跳过了空白字符）*)
  (* 大括号内的是返回值 *)
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  (* Lexing.lexeme lexbuf 的含义是当前捕获到的全部内容，
     此处应用 int_of_string 将其转换为整数 *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  (* 文件结束符 eof 是预定义的 *)
  | eof { EOF }
