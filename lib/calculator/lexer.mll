{
    open Parser
}

let white = [' ' '\t']+
(* 数字，包含 0-9 *)
let digit = ['0'-'9']
(* 整数，包含一个可选的 -，和任意个数字 *)
let int = '-'? digit+

rule read =
  parse
  (* 遇到空白字符时直接递归调用自己（跳过了空白字符）*)
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) } (* 调用 Lexing.lexeme 方法，解析任意数量的整数，并返回那个字符串 *)
  | eof { EOF }
