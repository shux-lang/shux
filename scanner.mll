{
  open Parser
  let lineno = ref 1
  let depth = ref 0
  let filename = ref "" (* what do with this *)
}

let whitespace = [' ' '\t' '\r']
let newline = '\n'

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])


rule token = parse
  whitespace  { token lexbuf }
| newline     { incr lineno; token lexbuf }
| "/*"    { incr depth; comment lexbuf }

(* parens *)
| '('     { LPAREN } 
| ')'     { RPAREN }
| '{'     { LBRACE }
| '}'     { RBRACE }
| '['     { LBRACK }
| ']'     { RBRACK }

(* strings *)
| '"'     { DBL_QUOTE }
| '\''    { SNG_QUOTE }

(* separators *)
| ';'     { SEMI }
| ','     { COMMA }
| ".."    { DOTDOT }
| '.'     { DOT }

(* arithmetic operators *)
| '+'     { PLUS }
| '-'     { MINUS }
| '*'     { TIMES }
| '/'     { DIVIDE }
| '%'     { MOD }

(* assignment operators *)
| '='     { ASSIGN }
| "+="    { ADD_ASN }
| "-="    { SUB_ASN }
| "*="    { MUL_ASN }
| "/="    { DIV_ASN }
| "%="    { MOD_ASN }

(* logical operators *)
| "&&"    { LOG_AND }
| "||"    { LOG_OR }
| "!"     { LOG_NOT }

(* do we want to do this? 
(* bitwise operators *)
| '&'     { BIT_AND }
| '|'     { BIT_OR }
| '^'     { BIT_XOR }
| '~'     { BIT_NOT }
| "<<"    { LSHIFT }
| ">>"    { RSHIFT }
*)

(* comparison operators *)
| '<'     { LT }
| '>'     { GT }
| "=="    { EQ }
| "!="    { NEQ }
| "<="    { LEQ }
| ">="    { GEQ }

(* shuxxx *)
| '?'     { QUES }
| ':'     { COLON }
| "::"    { FILTER }
| '@'     { MAP }
| "->"    { FUNC }

(* control keywords *)
| "if"      { IF }
| "then"    { THEN }
| "elif"    { ELIF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "do"      { DO }

(* declarations *)
| "ns"      { NS }
| "gn"      { GN }
| "kn"      { KN }
| "struct"  { STRUCT }
| "let"     { LET }
| "var"     { VAR }

(* types *)
| "int"     { INT }
| "scalar"  { SCALAR }
| "string"  { STRING }
| "bool"    { BOOL }
| "vector"  { VECTOR }


(* ye good olde *)
| eof { EOF }
| _ as e    { raise (Exceptions.IllegalCharacter(!filename, illegal, !lineno)) }

(* comments *)
(* does not support nested comments yet *)
and comment = parse
  "/*"      { incr depth; comment lexbuf }
| "*/"      { decr depth; if depth > 0 then token lexbuf else token lexbuf }
| _         { comment lexbuf }
