namespace Typicallvm

open AST

module Evaluator =
  type RuntimeValue =
    | LiteralValue of Literal
    | FuncValue of (RuntimeValue -> RuntimeValue)
    | RefValue of RuntimeValue option ref

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
      fun value env ->
        match ep value env with
        | None -> failwith "pattern match not exhaustive"
        | Some env' -> eval env' e
    | Cons((pat, e), l) ->
      let ep = evalPat pat
      let el = evalPatterns l
      fun value env ->
        match ep value env with
        | None -> el value env
        | Some env' -> eval env' e

  and eval env e =
    match e with
    | ExpLit l -> LiteralValue l
    | Deref v -> normalizeValue <| Map.find v env
    | Lambda bindings ->
      let eb = evalPatterns bindings
      FuncValue <| fun v -> eb v env
    | Apply(f, a) ->
      let fv = eval env f
      let av = eval env a
      getFunc fv <| av
    | Let((pat, e1), e2) ->
      let vRef = ref None
      match evalPat pat (RefValue vRef) env with
      | None -> failwith "let pattern did not match"
      | Some env' ->
        vRef := Some <| eval env' e1
        eval env' e2
