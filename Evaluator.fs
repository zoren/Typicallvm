namespace Typicallvm

open AST

module Evaluator =
  type RuntimeValue =
    | LiteralValue of Literal
    | FuncValue of (RuntimeValue -> RuntimeValue)
    | RefValue of RuntimeValue option ref

  type Env = Map<Variable, RuntimeValue>

  let normalizeValue =
    function
    | RefValue refVal ->
      match !refVal with
      | None -> failwith "reference not set"
      | Some v -> v
    | v -> v

  let getFunc v =
    match normalizeValue v with
    | FuncValue f -> f
    | _ -> failwith "not a function"

  let evalPat =
    function
    | Pattern.Const l ->
      fun value env ->
        match normalizeValue value with
        | LiteralValue l' -> if l = l' then Some env else None
        | _ -> failwith "not a const"
    | Pattern.Var var ->
      fun value env -> Some <| Map.add var value env

  let rec evalPatterns =
    function
    | Element (pat, e) ->
      let ep = evalPat pat
      let ce = eval e
      fun value env ->
        match ep value env with
        | None -> failwith "pattern match not exhaustive"
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
    | ExpLit l -> fun _ -> LiteralValue l
    | Deref v -> normalizeValue << Map.find v
    | Lambda bindings ->
      let eb = evalPatterns bindings
      fun env -> FuncValue <| fun v -> eb v env
    | Apply(f, a) ->
      let fv = eval f
      let av = eval a
      fun env -> getFunc (fv env) <| av env
    | Let((pat, e1), e2) ->
      let vRef = ref None
      let ep = evalPat pat
      let ce1 = eval e1
      let ce2 = eval e2
      fun env ->
        match ep (RefValue vRef) env with
        | None -> failwith "let pattern did not match"
        | Some env' ->
          vRef := Some <| ce1 env'
          ce2 env'
