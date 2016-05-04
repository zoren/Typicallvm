namespace Typicallvm

module AST =
  type Literal =
    | Int of int

  type Identifier = string

  type Variable = Identifier
  type FunctionName = Identifier
  type ParameterName = Identifier

  type Exp =
    | ExpLit of Literal
    | Deref of Variable
    | Call of FunctionName * Exp array
    | If of Exp * Exp * Exp

  type Decl =
    | FunctionDecl of ParameterName array * Exp

  type Program =
    (Identifier * Decl) list
