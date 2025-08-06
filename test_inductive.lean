import Lean.Data.Json

structure TestBase where
  _type : String
deriving Repr, Lean.FromJson, Lean.ToJson

structure TestAssign extends TestBase where
  value : Nat
deriving Repr, Lean.FromJson, Lean.ToJson

inductive TestStatement where
  | Assign : TestAssign → TestStatement
deriving Repr, Lean.FromJson, Lean.ToJson

def testJson : String := "{\"Assign\": {\"_type\": \"Assign\", \"value\": 42}}"

#eval do
  let json ← match Lean.Json.parse testJson with
    | Except.ok j => pure j
    | Except.error e => throw (IO.userError e)
  match Lean.FromJson.fromJson? json with
    | Except.ok (stmt : TestStatement) => IO.println s!"Success: {repr stmt}"
    | Except.error e => IO.println s!"Error: {e}"
