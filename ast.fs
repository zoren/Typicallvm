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
    | Call of FunctionName * Exp list
    | If of Exp * Exp * Exp

  type Decl =
    | FunctionDecl of ParameterName list * Exp

  type Program =
    (Identifier * Decl) list
