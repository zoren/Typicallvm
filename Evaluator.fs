namespace Typicallvm

open AST

module Evaluator =
  type RuntimeValue = Literal

  type Env = Map<Variable, RuntimeValue>
  type CompiledFunc = RuntimeValue list -> RuntimeValue
  type FunctionEnv = Map<FunctionName, CompiledFunc option ref>

  exception RuntimeErrorException of string

  let evalExp (fenv:FunctionEnv) =
    let rec loop (e:Exp) : Env -> RuntimeValue =
      match e with
      | ExpLit l -> fun _ -> l
      | Deref v -> Map.find v
      | Call(fname, args) ->
        let cargs = List.map loop args
        fun env ->
          let f = Option.get <| !(Map.find fname fenv)
          let vargs = List.map (fun f -> f env) cargs
          f vargs
      | If(econd, et, ef) ->
        let cecond = loop econd
        let cet = loop et
        let cef = loop ef
        fun env ->
          if cecond env <> Int 0
          then cet env
          else cef env
    loop

  let evalDecl fenv (fname, FunctionDecl(parameters, e)) : FunctionEnv =
    let fRef = ref None
    let fenv' = Map.add fname fRef fenv
    let ce = evalExp fenv' e
    let f =
      fun args ->
        let env = Seq.zip parameters args |> Map.ofSeq
        ce env
    fRef := Some f
    Map.add fname fRef fenv
