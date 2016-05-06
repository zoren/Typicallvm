namespace Typicallvm

module LLLang =
  type BasicType =
    | I of int
    | Pointer of BasicType

  type Alignment = Int
  type GlobalId = Global of string
  type LocalId = Local of string
  type Variable = LocalId
  type Value =
    | Const of int
    | Variable of Variable
  type WrapFlag = NUW | NSW
  type CmpFlag = | EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE
  type FunctionName = GlobalId
  type BinOp =
    | Add of WrapFlag list
    | Sub of WrapFlag list
    | Mul of WrapFlag list
  type Label = LocalId
  type Inst =
    | Alloca of Variable * BasicType * Alignment option
    | Store of BasicType * Value * BasicType * Value * Alignment option
    | Load of Variable * BasicType * BasicType * Value * Alignment option
    | Icmp of Variable * CmpFlag * BasicType * Value * Value
    | Call of Variable * BasicType * FunctionName * (BasicType * Value) array // or value instead of function name
    | Binop of Variable * BinOp * BasicType * Value * Value

  type BrancingInst =
    | Br of Label
    | BrCond of BasicType * Value * Label * Label
    | Ret of BasicType * Value

  type BB =
    | Entry of Label
    | Phi of Label * Variable * BasicType * (Value * Label) list
    | Cons of BB * Inst

  type ClosedBB = BB * BrancingInst

  type Func = BasicType * FunctionName * (BasicType * Variable) list * ClosedBB list
  type Prog = Func list

  let (++) (bb:BB) (i:Inst) : BB = Cons(bb, i)
  let (++/) (bb:BB) (br:BrancingInst) : ClosedBB = bb, br
  let rec getLabel =
    function
    | Entry label
    | Phi(label, _, _, _) -> label
    | Cons(bb, _) -> getLabel bb
