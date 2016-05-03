namespace Typicallvm

open AST

module Evaluator =
  type RuntimeValue = Literal

  type Env = Map<Variable, RuntimeValue>
  type CompiledFunc = RuntimeValue array -> RuntimeValue
  type FunctionEnv = Map<FunctionName, CompiledFunc option ref>

  exception RuntimeErrorException of string

  let evalExp (fenv:FunctionEnv) e =
    let evalPat =
      function
      | Pattern.Const l ->
        fun l' env -> if l = l' then Some env else None
      | Pattern.Var var ->
        fun value env -> Some <| Map.add var value env

    let rec evalPatterns =
      function
      | Element (pat, e) ->
        let ep = evalPat pat
        let ce = eval e
        fun value env ->
          match ep value env with
          | None -> raise <| RuntimeErrorException "pattern match not exhaustive"
          | Some env' -> ce env'
      | Cons((pat, e), l) ->
        let ep = evalPat pat
        let ce = eval e
        let el = evalPatterns l
        fun value env ->
          match ep value env with
          | None -> el value env
          | Some env' -> ce env'

    and eval (e:Exp) : Env -> RuntimeValue =
      match e with
      | ExpLit l -> fun _ -> l
      | Deref v -> Map.find v
      | Call(fname, args) ->
        let cargs = Array.map eval args
        fun env ->
          let f = Option.get <| !(Map.find fname fenv)
          let vargs = Array.map (fun f -> f env) cargs
          f vargs
      | Match(e1, mrules) ->
        let ce1 = eval e1
        let eb = evalPatterns mrules
        fun env -> eb (ce1 env) env
    eval e

  let evalDecl (fenv:FunctionEnv) (fname:FunctionName, FunctionDecl(parameters:ParameterName array, e: Exp)) : FunctionEnv =
    let fRef = ref None
    let fenv' = Map.add fname fRef fenv
    let ce = evalExp fenv' e
    let f =
      fun args ->
        let env = Array.zip parameters args |> Map.ofSeq
        ce env
    fRef := Some f
    Map.add fname fRef fenv
