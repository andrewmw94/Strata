import Lean.Data.Json

structure TestAssign where
  _type : String
  value : Nat
deriving Repr, Lean.FromJson, Lean.ToJson

inductive TestStatement where
  | Assign : TestAssign → TestStatement
deriving Repr, Lean.FromJson, Lean.ToJson

-- Test what format Lean expects
def testStmt : TestStatement := TestStatement.Assign { _type := "Assign", value := 42 }

#eval Lean.ToJson.toJson testStmt

-- Test parsing Python AST format
def pythonFormat : String := "{\"_type\": \"Assign\", \"value\": 42}"

#eval do
  let json ← match Lean.Json.parse pythonFormat with
    | Except.ok j => pure j
    | Except.error e => throw (IO.userError e)
  match Lean.FromJson.fromJson? json with
    | Except.ok (stmt : TestStatement) => IO.println s!"Success: {repr stmt}"
    | Except.error e => IO.println s!"Error: {e}"
