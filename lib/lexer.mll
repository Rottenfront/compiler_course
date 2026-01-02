{
open Parser
}

rule read = parse
| [' ' '\t' '\n' '\r'] { read lexbuf }

| "("  { LPAREN }
| ")"  { RPAREN }
| "["  { LBRACK }
| "]"  { RBRACK }
| ":"  { COLON }

| "type"   { TYPE }
| "define" { DEFINE }
| "if"     { IF }
| "let"    { LET }

| "#t"  { TRUE }
| "#f" { FALSE }

| ['0'-'9']+ as i { INT (int_of_string i) }

| [^ ' ' '\t' '\n' '\r'
     '(' ')' '[' ']' '{' '}'
     '"' ';' ':' ]+
  as id
  { IDENT id }

| eof { EOF }
