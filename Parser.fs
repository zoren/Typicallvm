namespace Typicallvm

module Parser =
  open ParserCombinators
  open DerivedParsers
  open System

  let spaces = many (satisfy(Char.IsWhiteSpace)) |>> ignore
  let addWS p = p .>> spaces

  let pint =
    many1 (satisfy Char.IsDigit)
      |>> (Array.ofList >> String)
      |>> Int32.Parse
      |> addWS

  let pidentifier =
    let pstart = satisfy(Char.IsLetter)
    let pfollow = satisfy(fun c -> Char.IsLetter c || Char.IsDigit c)
    pstart >>= (fun c -> many pfollow |>> (fun cs -> c :: cs))
      |>> (Array.ofList >> String)
      |> addWS

  let pchws c = pchar c .>> spaces
  let pstrws s = pstring s .>> spaces
  let parens p = pchws '(' >>. p .>> pchws ')'
  let sepByComma p = sepBy p <| pchws ','
  open AST

  let plit = pint |>> Int
  let pexpForward, pexpSetter = createForwardedParser()

  let keywordStartingExp =
    Map.ofList
      ["if",
        tuple3
          pexpForward
          (pstrws "then" >>. pexpForward)
          (pstrws "else" >>. pexpForward) |>> If]
  let rec pexp =
    choice [
      plit |>> ExpLit
      pidentifier >>=
        fun id ->
          defaultArg
            (Map.tryFind id keywordStartingExp)
            (parens (sepByComma pexpForward) |>> fun args -> Call(id, args)
              <|> (preturn <| Deref id))
              ]
  do pexpSetter pexp
  let pdecl =
    choice [
      parens(sepByComma pidentifier) .>>. pexp |>> FunctionDecl
    ]
  let pprogram : Parser<Program> =
    spaces >>. many (pidentifier .>> pchws '=' .>>. pdecl) .>> eof
