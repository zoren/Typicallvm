namespace Typicallvm

open LLLang

module LLangPrettyPrint =
  let rec ppType : BasicType -> string =
    function
    | I i -> sprintf "i%i" i
    | Pointer t -> ppType t + "*"

  let ppLocal (Local id) = "%" + id
  let ppVar = ppLocal
  let ppLabel = ppLocal
  let ppGlobal (Global id) = "@" + id

  let ppValue =
    function
    | Const i -> i.ToString()
    | Variable l -> ppLocal l

  let ppWrap = function | NUW -> "nuw" | NSW -> "nsw"

  let ppCmpFlag = function | EQ -> "eq" | NE -> "ne" | _ -> failwith "not implemented"

  let ppBinop =
    function
    | Add wflags -> sprintf "add %s" << String.concat " " <| Seq.map ppWrap wflags
    | Sub wflags -> sprintf "sub %s" << String.concat " " <| Seq.map ppWrap wflags
    | Mul wflags -> sprintf "mul %s" << String.concat " " <| Seq.map ppWrap wflags
  let parens strings = sprintf "(%s)" <| String.concat ", " strings
  let sepSp = sprintf "%s %s"
  let ppInst =
    function
    | Alloca _ -> failwith ""
    | Store _ -> failwith ""
    | Load _ -> failwith ""
    | Icmp (var, flag, ctype, v0, v1) ->
      sprintf "%s = icmp %s %s %s, %s" (ppVar var) (ppCmpFlag flag) (ppType ctype) (ppValue v0) (ppValue v1)
    | Call (var, retType, fname, args) ->
      sprintf "%s = call %s %s (%s)" (ppVar var) (ppType retType) (ppGlobal fname)
        << String.concat ", " <| Seq.map (fun (argt, v) -> sepSp (ppType argt) (ppValue v)) args
    | Binop(var, op, vtype, v0, v1) ->
      sprintf "%s = %s %s %s, %s" (ppVar var) (ppBinop op) (ppType vtype) (ppValue v0) (ppValue v1)

  let ppBranchingInst =
    function
    | Br label ->
      sprintf "br label %s" <| ppLabel label
    | BrCond(vtype, value, tlab, flab) ->
      sprintf "br %s %s, label %s, label %s" (ppType vtype) (ppValue value) (ppLabel tlab) (ppLabel flab)
    | Ret(vtype, value) ->
      sprintf "ret %s %s" (ppType vtype) (ppValue value)

  let indent = "  "
  let ppIndentLine s = sprintf "%s%s\n" indent s
  let rec ppBB : BB -> string =
    function
    | Entry (Local label) -> sprintf "%s:\n" label
    | Phi(Local label, var, vtype, vals) ->
      sprintf "%s:\n%s%s = phi %s %s\n" label indent (ppVar var) (ppType vtype)
        << String.concat ", " <|
        Seq.map (fun (value, label) -> sprintf "[%s, %s]" (ppValue value) (ppLabel label)) vals
    | Cons(bb, inst) ->
      ppBB bb + ppIndentLine(ppInst inst)

  let ppClosedBB ((bb, brInst):ClosedBB) =
    ppBB bb + ppIndentLine (ppBranchingInst brInst)

  let ppFunc ((retType, fname, parameters, bbs):Func) =
    sprintf "define %s %s(%s) #0 {\n%s}\n"
      (ppType retType)
      (ppGlobal fname)
      (String.concat ", " <|
        Seq.map (fun(ptype, pname) -> sepSp (ppType ptype) (ppVar pname)) parameters)
      (String.concat "\n" <| Seq.map ppClosedBB bbs)
