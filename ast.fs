namespace Typicallvm

module AST =
  type Literal =
    | Int of int

  type Variable = string

  type Pattern =
    | Const of Literal
    | Var of Variable

  type TypeFunctionName = string

  type Type =
    | TypeApply of TypeFunctionName * Type array

  type NonEmptyList<'a> =
    | Element of 'a
    | Cons of 'a * NonEmptyList<'a>

  type Exp =
    | ExpLit of Literal
    | Deref of Variable
    | Lambda of NonEmptyList<Pattern * Exp>
    | Apply of Exp * Exp
    | Let of Decl * Exp

  and Decl = Pattern * Exp
