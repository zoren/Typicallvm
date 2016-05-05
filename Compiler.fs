namespace Typicallvm

open AST
open LLLang

module Compiler =
  let counter = ref 0
  let fresh() = incr counter; string !counter
  let newLabel s : Label = Local <| s + fresh()
  let mkTmpVar s : Variable = Local <| s + fresh()

  let compileExp currentbb e =
    let closedRef = ref []
    let addClosed (closedBB:ClosedBB) = closedRef := closedBB :: !closedRef
    let rec loop currentbb =
      function
      | ExpLit lit ->
        match lit with
        | Literal.Int i -> Const i, currentbb
      | Deref var -> Variable <| Local var, currentbb
      | If(econd, et, ef) ->
        let cecond, c = loop currentbb econd
        let ltrue, lfalse = newLabel "true", newLabel "false"
        let b = mkTmpVar "b"
        addClosed <| c ++ Icmp(b, NE, I 32, cecond, Const 0)
                       ++/ BrCond(I 1, Variable b, ltrue, lfalse)
        let lendif = newLabel "endif"
        let tval, tcode = loop (Entry ltrue) et
        addClosed <| tcode ++/ Br lendif
        let fval, fcode = loop (Entry lfalse) ef
        addClosed <| fcode ++/ Br lendif
        let tmpVar = mkTmpVar "ifResult"
        let prolog = Phi(lendif, tmpVar, I 32, [(tval, ltrue); (fval, lfalse)])
        Variable tmpVar, prolog
      | Exp.Call(fname, args) ->
        let l = System.Collections.Generic.List<_>()
        let rec compArg cbb (arg:Exp) =
          let v, c = loop cbb arg
          l.Add v
          c
        let c' = Seq.fold compArg currentbb args
        let tmpVar = mkTmpVar <| fname + "Result"
        let inst =
          match fname with
          | "add" -> Binop(tmpVar, Add [NSW], I 32, l.[0], l.[1])
          | "sub" -> Binop(tmpVar, Sub [NSW], I 32, l.[0], l.[1])
          | _ -> Inst.Call(tmpVar, I 32, Global fname, l |> Seq.map (fun v -> I 32, v) |> Seq.toArray)
        Variable tmpVar, c' ++ inst
    loop currentbb e, !closedRef

  let compileDecl (id:Identifier, FunctionDecl(parameters, ebody)) : Func =
    let llparams = parameters |> Seq.map (fun p -> I 32, Local p) |> Seq.toList
    let (tmpVar, bb), bbs = compileExp (Entry <| Local "entry") ebody
    let bbs' = List.rev <| (bb ++/ Ret(I 32, tmpVar)) :: bbs
    I 32, Global id, llparams, bbs'
