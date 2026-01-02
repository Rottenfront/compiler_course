%{
open Ast
%}

%token <int> INT
%token <string> IDENT

%token DEFINE IF LET TYPE
%token TRUE FALSE

%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COLON

%token EOF

%start <Ast.program> program

%%

parenthesized(p):
  | LPAREN; x = p; RPAREN { x }
  | LBRACE; x = p; RBRACE { x }
  | LBRACK; x = p; RBRACK { x }

program:
  | defs = list(parenthesized(definition)); EOF { defs }

function_signature:
  | name = IDENT; params = list(parenthesized(param)); COLON; ret = ty { (name, params, ret) }

definition:
  | DEFINE;
      signature = parenthesized(function_signature);
      body = expr
    { Define (signature, body) }
  | TYPE;
      name = IDENT;
      value = ty;
    { TypeDef (name, value) }

param:
  | name = IDENT; COLON; t = ty
    { (name, t) }

ty:
  | name = IDENT { TyNamed name }
  | LPAREN; RPAREN { TyUnit }
  | LPAREN; ts = list(ty); RPAREN { TyTuple ts }

binding:
  | name = IDENT; e = expr { (name, e) }

paren_expr:
  | IF; c = expr; t = expr; e = expr
    { If (c, t, e) }

  | LET;
      binds = parenthesized(list(parenthesized(binding)));
      body = expr
    { Let (binds, body) }

  | f = IDENT; args = list(expr)
    { Call (f, args) }

expr:
  | INT          { Int $1 }
  | TRUE         { Bool true }
  | FALSE        { Bool false }
  | IDENT        { Var $1 }

  | paren = parenthesized(paren_expr) { paren }
