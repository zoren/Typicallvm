#load "ast.fs"
#load "Evaluator.fs"
open Typicallvm
open AST
open Evaluator

let getInt =
  function
  | Int x -> x
let mkFunc f = ref << Some <| function [|Int x; Int y|] -> Int <| f x y | _ -> failwith "arity mismatch"
let addFunc = mkFunc (+)
let subFunc = mkFunc (-)
let eqFunc = mkFunc <| fun x y -> if x = y then 1 else 0

let initFEnv : FunctionEnv = Map.ofList ["add", addFunc; "sub", subFunc; "eq", eqFunc]
let eval e = Evaluator.evalExp initFEnv e
let mkApp fname e1 = Call(fname, [|e1|])
let mkApp2 fname e1 e2 = Call(fname, [|e1; e2|])
let mkIf e te ee =
  Match(e,
    Cons((Pattern.Const <| Int 1, te),
         Element(Pattern.Const <| Int 0, ee)))
let mkEq e1 e2 = mkApp2 "eq" e1 e2
let mkAdd e1 e2 = mkApp2 "add" e1 e2
let mkSub e1 e2 = mkApp2 "sub" e1 e2
let e0 = ExpLit(Int 0)
let e1 = ExpLit(Int 1)
let ex = Deref "x"
let ebody =
  (mkIf
    (mkEq ex e0)
    e0
    (mkAdd ex (Call("f", [|mkSub ex e1|]))))
let d = "f", FunctionDecl([|"x"|], ebody)
let runFunc i =
  let fenv' = evalDecl initFEnv d
  evalExp fenv' (Call("f", [|ExpLit <| Int i|])) Map.empty
let v = List.map runFunc [10;100;1000]
