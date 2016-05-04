#load "ast.fs"
open Typicallvm
open AST

let getInt =
  function
  | Int x -> x
let mkFunc f = ref << Some <| function [|Int x; Int y|] -> Int <| f x y | _ -> failwith "arity mismatch"
let addFunc = mkFunc (+)
let subFunc = mkFunc (-)

let mkApp fname e1 = Call(fname, [|e1|])
let mkApp2 fname e1 e2 = Call(fname, [|e1; e2|])
let mkIf e te ee = If(e, te, ee)
let mkAdd e1 e2 = mkApp2 "add" e1 e2
let mkSub e1 e2 = mkApp2 "sub" e1 e2
let e0 = ExpLit(Int 0)
let e1 = ExpLit(Int 1)
let ex = Deref "x"
let ebody =
  mkIf
    ex
    (mkAdd ex <| Call("gauss", [|mkSub ex e1|]))
    e0
let d = "gauss", FunctionDecl([|"x"|], ebody)

//#load "Evaluator.fs"
//open Evaluator
//let initFEnv : FunctionEnv = Map.ofList ["+", addFunc; "-", subFunc]
//let runFunc i =
//  let fenv' = evalDecl initFEnv d
//  evalExp fenv' (Call("f", [|ExpLit <| Int i|])) Map.empty
//let v = List.map runFunc [10;100;1000]
#load "LLLang.fs"
#load "Compiler.fs"
open Typicallvm.Compiler
let cfunc = compileDecl d
#load "LLangPrettyPrint.fs"
open Typicallvm.LLangPrettyPrint
let s = ppFunc cfunc
System.IO.File.WriteAllText("test.ll", s)
