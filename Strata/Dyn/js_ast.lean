import Lean.Data.Json
import Lean.Data.Json.Parser
import Lean.Data.RBTree

set_option trace.Elab.Deriving true
structure Position where
  line : Nat
  column : Nat
  index : Nat
deriving Repr, Lean.FromJson, Lean.ToJson

structure SourceLocation where
  start_loc : Position
  end_loc : Position
  identifierName: Option String
deriving Repr, Lean.FromJson, Lean.ToJson

structure BaseNode where
  type : String
  start_loc : Nat
  end_loc : Nat
  loc : SourceLocation
deriving Repr, Lean.FromJson, Lean.ToJson

structure Identifier extends BaseNode where
  name : String
deriving Repr, Lean.FromJson, Lean.ToJson

structure TSNumberKeyword extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

structure TSTypeAnnotation extends BaseNode where
  typeAnnotation : TSNumberKeyword
deriving Repr, Lean.FromJson, Lean.ToJson

structure Parameter extends BaseNode where
  name : String
  typeAnnotation : TSTypeAnnotation
  identifierName? : Option String := none
deriving Repr, Lean.FromJson, Lean.ToJson

structure ReturnStatement extends BaseNode where
  argument : Identifier
deriving Repr, Lean.FromJson, Lean.ToJson

structure BinaryExpression extends BaseNode where
  left : Identifier
  operator : String
  right : Identifier
deriving Repr, Lean.FromJson, Lean.ToJson

mutual
  structure ConditionalExpression extends BaseNode where
      test : Expression
      consequent : Identifier
      alternate : Identifier
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure CallExpression extends BaseNode where
    callee : Identifier
    arguments : List Expression
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure LogicalExpression extends BaseNode where
    left : Expression
    operator : String
    right : Expression
  deriving Repr, Lean.FromJson, Lean.ToJson

  inductive Expression where
    | BinaryExpression : BinaryExpression → Expression
    | ConditionalExpression : ConditionalExpression → Expression
    | CallExpression : CallExpression → Expression
    | LogicalExpression : LogicalExpression → Expression
  deriving Repr, Lean.FromJson, Lean.ToJson
end

structure VariableDeclarator extends BaseNode where
  id : Identifier
  init: Expression
deriving Repr, Lean.FromJson, Lean.ToJson

structure VariableDeclaration extends BaseNode where
  declarations : Array VariableDeclarator := #[]
  kind : Option String
deriving Repr, Lean.FromJson, Lean.ToJson

structure EmptyStatement extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

structure ExpressionStatement extends BaseNode where
    expression : Expression
deriving Repr, Lean.FromJson, Lean.ToJson

mutual
  structure BlockStatement extends BaseNode where
    body : Array Statement
    directives : Array String := #[]
  deriving Repr, Lean.FromJson, Lean.ToJson

  structure IfStatement extends BaseNode where
    test : Expression
    consequent : Statement
    alternate : Option Statement
  deriving Repr, Lean.FromJson, Lean.ToJson

  inductive Statement where
    | IfStatement : IfStatement → Statement
    | ReturnStatement : ReturnStatement → Statement
    | VariableDeclaration : VariableDeclaration → Statement
    | ExpressionStatement : ExpressionStatement → Statement
    | BlockStatement : BlockStatement → Statement
  deriving Repr, Lean.FromJson, Lean.ToJson
end

structure Comment extends BaseNode where
deriving Repr, Lean.FromJson, Lean.ToJson

structure FunctionDeclaration extends BaseNode where
  id : Identifier
  generator : Bool
  async : Bool
  params : Array Parameter
  returnType : TSTypeAnnotation
  body : Statement
  leadingComments : Option (Array Comment)
deriving Repr, Lean.FromJson, Lean.ToJson

structure Program extends BaseNode where
  sourceType : String
  interpreter : Option String := none
  body : Array FunctionDeclaration
  directives : Array String := #[]
deriving Repr, Lean.FromJson, Lean.ToJson

structure File extends BaseNode where
  errors : Array String := #[]
  program : Program
  comments : Array Comment
deriving Repr, Lean.FromJson, Lean.ToJson




def printToJson (fd: FunctionDeclaration) : IO Unit :=
  let s := (Lean.ToJson.toJson fd).pretty
  IO.println s

def loc : SourceLocation := {start_loc := {line := 2, column := 0, index := 23}, end_loc := {line := 7, column := 6, index := 123}, identifierName := .none}
def fn_identifier : Identifier := {type := "FunctionDeclaration", start_loc := 33, end_loc := 123, loc := loc, name := ""}
def ret_type : TSTypeAnnotation := {type := "TSTypeAnnotation", start_loc := 33, end_loc := 123, loc := loc, typeAnnotation := {type := "TSNumberKeyword", start_loc := 33, end_loc := 123, loc := loc}}
def ident_n : Identifier := {type := "Identifier", start_loc := 33, end_loc := 123, loc := loc, name := "n"}
def ident_m : Identifier := {type := "Identifier", start_loc := 33, end_loc := 123, loc := loc, name := "m"}
def ret_stmt : ReturnStatement := {type := "ReturnStatement", start_loc := 33, end_loc := 123, loc := loc, argument := ident_n}
def test_expr : Expression := Expression.BinaryExpression {type := "BinaryExpression", start_loc := 33, end_loc := 123, loc := loc, left:= ident_n, operator := "<", right := ident_m}

def cond_expr : ConditionalExpression := {type := "BinaryExpression", start_loc := 33, end_loc := 123, loc := loc, test:= test_expr, consequent:= ident_m, alternate:= ident_n}
def init_expr : Expression := (Expression.ConditionalExpression cond_expr)
def var_decl : VariableDeclarator := { type := "VariableDeclarator", start_loc := 33, end_loc := 123, loc := loc, id := ident_n, init := init_expr}
def cond_assign : Statement := Statement.VariableDeclaration ({type := "VariableDeclaration", start_loc := 33, end_loc := 123, loc := loc, declarations := #[var_decl], kind := "let" : VariableDeclaration})

def consequent : Statement := Statement.BlockStatement {type := "BlockStatement", start_loc := 33, end_loc := 123, loc := loc, body := #[Statement.ReturnStatement ret_stmt], directives := #[] : BlockStatement}
def if_stmt : IfStatement := {type := "IfStatement", start_loc := 33, end_loc := 123, loc := loc, test := test_expr, consequent := consequent, alternate := none}
-- def bdy : Array Statement := #[Statement.IfStatement if_stmt]
def bdy : Array Statement := #[cond_assign]
def block_stmt : Statement := Statement.BlockStatement {type := "BlockStatement", start_loc := 33, end_loc := 123, loc := loc, body := bdy, directives := #[] : BlockStatement}
def fd : FunctionDeclaration := {type := "FunctionDeclaration", start_loc := 33, end_loc := 123, loc := loc, id := fn_identifier, generator := false, async := false, params := #[], returnType := ret_type, body := block_stmt, leadingComments := .none}
-- #eval printToJson fd


def loadJsonFile (path : System.FilePath) : IO File := do
  let contents ← IO.FS.readFile path
  let json ← match Lean.Json.parse contents with
    | Except.ok json => pure json
    | Except.error err => throw (IO.userError s!"Failed to parse JSON: {err}")
  match Lean.FromJson.fromJson? json with
    | Except.ok file => pure file
    | Except.error err => throw (IO.userError s!"Failed to convert JSON to Node: {err}")
