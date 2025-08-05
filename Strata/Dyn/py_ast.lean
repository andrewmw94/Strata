import Lean.Data.Json
import Lean.Data.Json.Parser

-- Simple wrapper around JSON that we can extend later
structure Module where
  json : Lean.Json

instance : Lean.FromJson Module where
  fromJson? json := Except.ok { json := json }

instance : Lean.ToJson Module where
  toJson module := module.json

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

-- Helper function to get a field from JSON using the getObjVal? method
def getJsonField (json : Lean.Json) (field : String) : Option Lean.Json :=
  match json.getObjVal? field with
  | Except.ok val => some val
  | Except.error _ => none

-- Helper functions to extract information from the JSON
def getModuleBody (module : Module) : Option (Array Lean.Json) :=
  match getJsonField module.json "body" with
  | some (Lean.Json.arr arr) => some arr
  | _ => none

def getNodeType (json : Lean.Json) : Option String :=
  match getJsonField json "_type" with
  | some (Lean.Json.str s) => some s
  | _ => none

