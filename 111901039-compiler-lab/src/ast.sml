structure Ast = struct

datatype BinOp = Plus
               | Minus
               | Mul
               | Div

datatype Expr  = Const of int
               | Var of string
               | Op  of Expr * BinOp * Expr
               
datatype Stmt  = Print of Expr
               | Assign of string * Expr
               | For of string * int * int * Stmt list


(* Functions to support the binary operations and assignment *)
fun plus    a b = Op (a, Plus, b)
fun minus   a b = Op (a, Minus, b)
fun mul     a b = Op (a, Mul, b)
fun div     a b = Op (a, Div, b)
fun assign  a b = Assign (a, b)
fun for (Var(v)) (Const(c1)) (Const(c2)) e = For(v,c1,c2,e)

end
