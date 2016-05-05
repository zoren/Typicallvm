namespace Typicallvm

module Parser =
  open ParserCombinators
  open DerivedParsers
  open System

  let spaces = many (psat1(Char.IsWhiteSpace)) |>> ignore
  let addWS p = p .>> spaces

  let pint =
    many1 (psat1 Char.IsDigit)
      |>> (Array.ofList >> String)
      |>> Int32.Parse
      |> addWS

  let keywords = set ["if"; "then"; "else"]
  let pidentifier =
    let pstart = psat1(Char.IsLetter)
    let pfollow = psat1(fun c -> Char.IsLetter c || Char.IsDigit c)
    pstart >>= (fun c -> many pfollow |>> (fun cs -> c :: cs))
      |>> (Array.ofList >> String)
      |> pfilter (fun s -> not <| Set.contains s keywords)
      |> addWS

  let pchws c = pchar c .>> spaces
  let pstrws s = pstring s .>> spaces
  let parens p = pchws '(' >>. p .>> pchws ')'
  let sepByComma p = sepBy p <| pchws ','
  open AST

  let plit = pint |>> Int
  let pexpForward, pexpSetter = createForwardedParser()
  let rec pexp =
    choice [
      plit |>> ExpLit
      pidentifier .>>. parens (sepByComma pexpForward) |>> Call
      pidentifier |>> Deref
      tuple3
        (pstrws "if" >>. pexpForward)
        (pstrws "then" >>. pexpForward)
        (pstrws "else" >>. pexpForward) |>> If
    ]
  do pexpSetter pexp
  let pdecl =
    choice [
      parens(sepByComma pidentifier) .>>. pexp |>> FunctionDecl
    ]
  let pprogram : Parser<Program> =
    spaces >>. many (pidentifier .>> pchws '=' .>>. pdecl) .>> eof
