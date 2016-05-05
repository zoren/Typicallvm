#load "ast.fs"
#load "ParserCombinators.fs"
open Typicallvm.ParserCombinators
let text = System.IO.File.ReadAllText("gauss.typ")
let stream = mkStream text
#load "Parser.fs"
open Typicallvm.Parser
let p = getSuccess <| pprogram stream

#load "LLLang.fs"
#load "Compiler.fs"
open Typicallvm.Compiler
let cfunc = List.map compileDecl p
#load "LLangPrettyPrint.fs"
open Typicallvm.LLangPrettyPrint
let s = String.concat "\n" <| Seq.map ppFunc cfunc
System.IO.File.WriteAllText("llvm/test.ll", s)
