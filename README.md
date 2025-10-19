# Compiler course language

It has no name, as it should not have any

## Supported syntax
Any functions should have declaration and implementation, in any order or position of file.

### Declaration

Type is type.
```
int
bool
()
(<type>)
<type> -> <type>
```

Declarations consist of function name and type signature.

Template:
```
decl <name> :: <type>
```

Sample:
```
decl square :: int -> int
```

Right now high-order functions are supported by parsers and checkers, but not assembly generator. I'll fix it later.

### Implementation

Expression can be either:
- literal (integer or Boolean constant (true/false))
- variable/parameter
- application (as in Metalang languages)
- operator application (only binary operators are supported now):
  * OpAdd -> "+"
  * OpSub -> "-"
  * OpMul -> "*"
  * OpDiv -> "/"
  * OpEq -> "=="
  * OpNe -> "!="
  * OpLess -> "<"
  * OpGreater -> ">"
  * OpLessEq -> "<="
  * OpGreaterEq -> ">="
  * OpAnd -> "&&"
  * OpOr -> "||"
  * OpXor -> "^"
  * OpSemicolon -> ";"
- variable creation:
  ```
  let <name> = <expr> in <expr>
  ```
- parentheses with expression inside. I hope example is not required

Implementation has function name, parameter names (divided by space) and body:
```
impl <name> <parames> = <expr>
```
Sample:
```
impl fib2 n a b =
  if n <= 1 then
    b
  else
    fib2 (n - 1) b (a + b)
```
