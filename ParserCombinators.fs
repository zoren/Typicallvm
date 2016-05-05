namespace Typicallvm

module ParserCombinators =
  type Stream =
    { String : string
      mutable Index : int }
  let mkStream s = {String = s; Index = 0}
  type Reply<'a> = Success of 'a | Failure
  type Parser<'a> = Stream -> Reply<'a>

  let getSuccess =
    function
    | Success v -> v
    | Failure -> failwith ""
  let eof : Parser<unit> =
    fun stream ->
      if stream.Index = stream.String.Length
      then Success()
      else Failure

  let anyString n : Parser<string> =
    fun stream ->
      if stream.Index + n <= stream.String.Length
      then
        let s = stream.String.Substring(stream.Index, n)
        stream.Index <- stream.Index + n
        Success s
      else Failure

  let (>>=) (p1: Parser<'a>) (p2ctor: 'a -> Parser<'b>) : Parser<'b> =
    fun stream ->
      let before1 = stream.Index
      let reply1 = p1 stream
      match reply1 with
      | Success v1 -> p2ctor v1 stream
      | Failure ->
        stream.Index <- before1
        Failure

  let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    fun stream ->
      let before1 = stream.Index
      let reply1 = p1 stream
      match reply1 with
      | Failure when stream.Index = before1 -> p2 stream
      | _ -> reply1

  let preturn (v:'a) : Parser<'a> = fun _ -> Success v
  let pzero : Parser<'a> = fun _ -> Failure
  let createForwardedParser() : Parser<'a> * (Parser<'a> -> unit) =
    let dummy _ = failwith "forwarded reference not set"
    let pref = ref dummy
    let wasSet = ref false
    let setter setParser =
      if !wasSet
      then failwith "a forwarded parser was already set"
      pref := setParser
      wasSet := true
    (fun stream -> !pref stream), setter

module DerivedParsers =
  open ParserCombinators
  let (|>>) p f = p >>= (preturn << f)
  let pseq f p1 p2 = p1 >>= (fun v1 -> p2 |>> (fun v2 -> f v1 v2))
  let (.>>.) p1 p2 = pseq (fun v1 v2 -> (v1, v2)) p1 p2
  let (>>.) p1 p2 = pseq (fun _ v2 -> v2) p1 p2
  let (.>>) p1 p2 = pseq (fun v1 _ -> v1) p1 p2
  let pfilter pred (p:Parser<_>) =
    p >>= (fun v -> if pred v then preturn v else pzero)
  let satisfy pred = anyString 1 |>> (fun s -> s.Chars 0) |> pfilter pred
  let pchar c = satisfy ((=)c)
  let pstring (s:string) = anyString s.Length |> pfilter ((=)s)
  let rec many p =
    (p >>= (fun x -> many p |>> (fun xs -> x :: xs)))
    <|>
    (preturn [])
  let many1 p =
    p >>= (fun x -> many p |>> (fun xs -> x :: xs))
  let choice ps = Seq.reduce (<|>) ps
  let sepBy1 p psep =
    p >>= fun v -> many (psep >>. p) |>> (fun vs -> v::vs)
  let sepBy p psep = sepBy1 p psep <|> preturn []
  let tuple3 p1 p2 p3 =
    p1 >>= (fun v1 -> p2 >>= (fun v2 -> p3 |>> (fun v3 -> (v1, v2, v3))))
