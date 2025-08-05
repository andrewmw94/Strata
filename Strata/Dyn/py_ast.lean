import Lean.Data.Json
import Lean.Data.Json.Parser
import Lean.Data.RBTree

-- Base structure for all Python AST nodes
structure BaseNode where
  _type : String
deriving Repr, Lean.FromJson, Lean.ToJson

-- Custom type for constant values
inductive ConstantValue where
  | num : Lean.JsonNumber → ConstantValue
  | str : String → ConstantValue
  | bool : Bool → ConstantValue
  | null : ConstantValue
deriving Repr

-- Custom JSON parsing for ConstantValue
instance : Lean.FromJson ConstantValue where
  fromJson? json := 
    match json with
    | Lean.Json.num n => Except.ok (ConstantValue.num n)
    | Lean.Json.str s => Except.ok (ConstantValue.str s)
    | Lean.Json.bool b => Except.ok (ConstantValue.bool b)
    | Lean.Json.null => Except.ok ConstantValue.null
    | _ => Except.error "Invalid constant value"

instance : Lean.ToJson ConstantValue where
  toJson cv := 
    match cv with
    | ConstantValue.num n => Lean.Json.num n
    | ConstantValue.str s => Lean.Json.str s
    | ConstantValue.bool b => Lean.Json.bool b
    | ConstantValue.null => Lean.Json.null

-- Context types for variable access
structure LoadContext extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

structure StoreContext extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

-- Operators
structure AddOp extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

structure MultOp extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

structure LtOp extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

-- Individual function argument
structure Arg extends BaseNode where
  arg : String
  annotation : Option String := none
  type_comment : Option String := none
deriving Repr, Lean.FromJson, Lean.ToJson

-- Function arguments container
structure Arguments extends BaseNode where
  posonlyargs : Array Arg := #[]
  args : Array Arg := #[]
  vararg : Option Arg := none
  kwonlyargs : Array Arg := #[]
  kw_defaults : Array String := #[]
  kwarg : Option Arg := none
  defaults : Array String := #[]
deriving Repr, Lean.FromJson, Lean.ToJson

-- Forward declarations for mutual recursion
mutual
  -- Context union type
  inductive ContextNode where
    | Load : LoadContext → ContextNode
    | Store : StoreContext → ContextNode
  deriving Repr, Lean.FromJson, Lean.ToJson

  -- Operator union type
  inductive Operator where
    | Add : AddOp → Operator
    | Mult : MultOp → Operator
    | Lt : LtOp → Operator
  deriving Repr, Lean.FromJson, Lean.ToJson

  -- Expressions
  structure Name extends BaseNode where
    id : String
    ctx : ContextNode
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure Constant extends BaseNode where
    value : ConstantValue
    kind : Option String := none
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure BinOp extends BaseNode where
    left : Expression
    op : Operator
    right : Expression
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure Compare extends BaseNode where
    left : Expression
    ops : Array Operator
    comparators : Array Expression
  deriving Repr, Lean.FromJson, Lean.ToJson

  -- Expression union type
  inductive Expression where
    | Name : Name → Expression
    | Constant : Constant → Expression
    | BinOp : BinOp → Expression
    | Compare : Compare → Expression
  deriving Repr, Lean.FromJson, Lean.ToJson

  -- Statements
  structure Assign extends BaseNode where
    targets : Array Expression
    value : Expression
    type_comment : Option String := none
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure AugAssign extends BaseNode where
    target : Expression
    op : Operator
    value : Expression
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure FunctionDef extends BaseNode where
    name : String
    args : Arguments
    body : Array Statement
    decorator_list : Array Expression := #[]
    returns : Option Expression := none
    type_comment : Option String := none
    type_params : Array String := #[]
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure Global extends BaseNode where
    names : Array String
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure Return extends BaseNode where
    value : Option Expression := none
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure While extends BaseNode where
    test : Expression
    body : Array Statement
    orelse : Array Statement := #[]
  deriving Repr, Lean.FromJson, Lean.ToJson

  -- Statement union type
  inductive Statement where
    | Assign : Assign → Statement
    | AugAssign : AugAssign → Statement
    | FunctionDef : FunctionDef → Statement
    | Global : Global → Statement
    | Return : Return → Statement
    | While : While → Statement
  deriving Repr, Lean.FromJson, Lean.ToJson
end

-- Top-level module structure
structure Module extends BaseNode where
  body : Array Statement
  type_ignores : Array String := #[]
deriving Repr, Lean.FromJson, Lean.ToJson

-- Utility functions for creating common nodes
def mkLoadContext : ContextNode := 
  ContextNode.Load { _type := "Load" }

def mkStoreContext : ContextNode := 
  ContextNode.Store { _type := "Store" }

def mkAddOp : Operator := 
  Operator.Add { _type := "Add" }

def mkMultOp : Operator := 
  Operator.Mult { _type := "Mult" }

def mkLtOp : Operator := 
  Operator.Lt { _type := "Lt" }

def mkName (id : String) (ctx : ContextNode) : Expression :=
  Expression.Name { _type := "Name", id := id, ctx := ctx }

def mkConstant (value : ConstantValue) : Expression :=
  Expression.Constant { _type := "Constant", value := value, kind := none }

def mkBinOp (left : Expression) (op : Operator) (right : Expression) : Expression :=
  Expression.BinOp { _type := "BinOp", left := left, op := op, right := right }

def mkCompare (left : Expression) (ops : Array Operator) (comparators : Array Expression) : Expression :=
  Expression.Compare { _type := "Compare", left := left, ops := ops, comparators := comparators }

def mkAssign (targets : Array Expression) (value : Expression) : Statement :=
  Statement.Assign { _type := "Assign", targets := targets, value := value, type_comment := none }

def mkAugAssign (target : Expression) (op : Operator) (value : Expression) : Statement :=
  Statement.AugAssign { _type := "AugAssign", target := target, op := op, value := value }

def mkReturn (value : Option Expression := none) : Statement :=
  Statement.Return { _type := "Return", value := value }

def mkGlobal (names : Array String) : Statement :=
  Statement.Global { _type := "Global", names := names }

def mkWhile (test : Expression) (body : Array Statement) (orelse : Array Statement := #[]) : Statement :=
  Statement.While { _type := "While", test := test, body := body, orelse := orelse }

def mkFunctionDef (name : String) (args : Arguments) (body : Array Statement) : Statement :=
  Statement.FunctionDef { 
    _type := "FunctionDef", 
    name := name, 
    args := args, 
    body := body, 
    decorator_list := #[], 
    returns := none, 
    type_comment := none, 
    type_params := #[] 
  }

def mkArg (arg : String) : Arg :=
  { _type := "arg", arg := arg, annotation := none, type_comment := none }

def mkArguments (args : Array Arg) : Arguments :=
  { 
    _type := "arguments", 
    posonlyargs := #[], 
    args := args, 
    vararg := none, 
    kwonlyargs := #[], 
    kw_defaults := #[], 
    kwarg := none, 
    defaults := #[] 
  }

-- JSON loading utility
def loadJsonFile (path : System.FilePath) : IO Module := do
  let contents ← IO.FS.readFile path
  let json ← match Lean.Json.parse contents with
    | Except.ok json => pure json
    | Except.error err => throw (IO.userError s!"Failed to parse JSON: {err}")
  match Lean.FromJson.fromJson? json with
    | Except.ok module => pure module
    | Except.error err => throw (IO.userError s!"Failed to convert JSON to Module: {err}")

-- Pretty printing utility
def printToJson (module : Module) : IO Unit :=
  let s := (Lean.ToJson.toJson module).pretty
  IO.println s

-- Example usage demonstrating the AST construction
def exampleModule : Module := {
  _type := "Module",
  body := #[
    mkAssign 
      #[mkName "count" mkStoreContext] 
      (mkConstant (ConstantValue.num (Lean.JsonNumber.fromNat 0))),
    mkFunctionDef 
      "square" 
      (mkArguments #[mkArg "x"])
      #[
        mkGlobal #["count"],
        mkAugAssign 
          (mkName "count" mkStoreContext) 
          mkAddOp 
          (mkConstant (ConstantValue.num (Lean.JsonNumber.fromNat 1))),
        mkReturn (some (mkBinOp 
          (mkName "x" mkLoadContext) 
          mkMultOp 
          (mkName "x" mkLoadContext)))
      ]
  ],
  type_ignores := #[]
}

-- Test the example
-- #eval printToJson exampleModule
