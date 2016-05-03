namespace Typicallvm

module AST =
  type Literal =
    | Int of int

  type Identifier = string
  type Variable = Identifier
  type FunctionName = Identifier

  type Pattern =
    | Const of Literal
    | Var of Variable

  type TypeFunctionName = string

  type Type =
    | TypeApply of TypeFunctionName * Type array

  type Exp =
    | ExpLit of Literal
    | Deref of Variable
    | Call of FunctionName * Exp array
    | If of Exp * Exp * Exp

  type ParameterName = Identifier

  type Decl =
    | FunctionDecl of ParameterName array * Exp

  type Program =
    (Identifier * Decl) list
