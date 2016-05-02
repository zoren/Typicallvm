// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "ast.fs"
#load "Evaluator.fs"
open Typicallvm
open AST
// Define your library scripting code here
let c5 = Int 5

open Evaluator
let lid = RuntimeValue.FuncValue id
let v = 3
let getInt =
  function
  | (LiteralValue(Int x)) -> x
  | _ -> failwith "not a int"
let addFunc = FuncValue(fun x -> FuncValue(fun y -> LiteralValue(Int(getInt x + getInt y))))
let subFunc = FuncValue(fun x -> FuncValue(fun y -> LiteralValue(Int(getInt x - getInt y))))
let eq x y  = if x = y then 1 else 0
let eqFunc = FuncValue(fun x -> FuncValue(fun y -> LiteralValue(Int(eq (getInt x) (getInt y)))))
let initEnv = Map.ofList ["id", lid; "add", addFunc; "sub", subFunc; "eq", eqFunc]
let eval = Evaluator.eval initEnv
let v1 = eval <| Apply(Deref "id", ExpLit c5)
let mkApp e e1 = Apply(e, e1)
let mkApp2 e e1 e2 = mkApp(mkApp e e1) e2
let v2 = eval <| Apply(Apply(Deref "add", ExpLit c5), ExpLit c5)

let mkIf e te ee =
  Apply(
    Lambda(Cons((Pattern.Const <| Int 1, te),
         Element(Pattern.Const <| Int 0, ee))),
    e)
let mkEq e1 e2 = mkApp2 (Deref "eq") e1 e2
let mkAdd e1 e2 = mkApp2 (Deref "add") e1 e2
let mkSub e1 e2 = mkApp2 (Deref "sub") e1 e2
let e0 = (ExpLit(Int 0))
let e1 = (ExpLit(Int 1))
let v3 = eval <| Let(((Var "f"), Lambda(Element(Var "x", mkIf (mkEq (Deref "x") e0)
                                                  e0
                                                  (mkAdd (Deref "x")
                                                    (mkApp (Deref "f") (mkSub (Deref "x") e1)))))),
            Apply(Deref "f", (ExpLit(Int 10))))
