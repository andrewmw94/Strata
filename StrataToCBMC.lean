/-
  Copyright Strata Contributors

  SPDX-License-Identifier: Apache-2.0 OR MIT
-/

import Lean.Data.Json
import Strata.DL.Util.Map

open Lean

-- Simple structure to hold any JSON node
structure JsonNode where
  id : String
  namedSub : Option Json := none
  sub : Option (Array Json) := none
  deriving FromJson, ToJson

-- Location structure
structure Location where
  id : String := ""
  namedSub : Option Json := none
  deriving FromJson, ToJson

-- Main CBMC Symbol structure with defaults
structure CBMCSymbol where
  baseName : String
  isAuxiliary : Bool := false
  isExported : Bool := false
  isExtern : Bool := false
  isFileLocal : Bool := false
  isInput : Bool := false
  isLvalue : Bool := false
  isMacro : Bool := false
  isOutput : Bool := false
  isParameter : Bool := false
  isProperty : Bool := false
  isStateVar : Bool := false
  isStaticLifetime : Bool := false
  isThreadLocal : Bool := false
  isType : Bool := false
  isVolatile : Bool := false
  isWeak : Bool := false
  location : Location
  mode : String
  module : String
  name : String
  prettyName : String := ""
  prettyType : String := ""
  prettyValue : String := ""
  type : Json
  value : Json
  deriving FromJson, ToJson

def createSymbol (baseName : String) (line : String) (isParameter : Bool) (isStateVar : Bool) (namePrefix : String) (prettyName : String := "") : CBMCSymbol :=
  let locationNamedSub := Json.mkObj [
    ("file", Json.mkObj [("id", "from_andrew.c")]),
    ("function", Json.mkObj [("id", "simpleTest")]),
    ("line", Json.mkObj [("id", line)]),
    ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
  ]

  let location : Location := {
    id := "",
    namedSub := some locationNamedSub
  }

  let typeNamedSub := Json.mkObj [
    ("#c_type", Json.mkObj [("id", "signed_int")]),
    ("width", Json.mkObj [("id", "32")])
  ]

  let typeJson := Json.mkObj [
    ("id", "signedbv"),
    ("namedSub", typeNamedSub)
  ]

  let valueJson := Json.mkObj [("id", "nil")]
  let fullName := s!"{namePrefix}{baseName}"

  {
    baseName := baseName,
    isAuxiliary := false,
    isExported := false,
    isExtern := false,
    isFileLocal := true,
    isInput := false,
    isLvalue := true,
    isMacro := false,
    isOutput := false,
    isParameter := isParameter,
    isProperty := false,
    isStateVar := isStateVar,
    isStaticLifetime := false,
    isThreadLocal := true,
    isType := false,
    isVolatile := false,
    isWeak := false,
    location := location,
    mode := "C",
    module := "from_andrew",
    name := fullName,
    prettyName := if prettyName.isEmpty then "" else prettyName,
    prettyType := "signed int",
    prettyValue := "",
    type := typeJson,
    value := valueJson
  }

def createParameterSymbol (baseName : String) : CBMCSymbol :=
  createSymbol baseName "1" true true "simpleTest::"

def createLocalSymbol (baseName : String) : CBMCSymbol :=
  let fullName := s!"simpleTest::1::{baseName}"
  createSymbol baseName "5" false false "simpleTest::1::" fullName

instance : ToJson (Map String CBMCSymbol) where
  toJson m := Json.mkObj (m.map fun (k, v) => (k, toJson v))

def testSymbols : IO Unit := do
  let xSymbol := createParameterSymbol "x"
  let ySymbol := createParameterSymbol "y"
  let zSymbol := createLocalSymbol "z"

  let m : Map String CBMCSymbol := [("\"simpleTest::x\"", xSymbol),
                                    ("\"simpleTest::y\"", ySymbol),
                                    ("\"simpleTest::z\"", zSymbol)]

  IO.println (toString (toJson m))

def main (args : List String) : IO Unit := do
  match args with
  | ["test"] => testSymbols
  | [filename] =>
    let content ← IO.FS.readFile filename
    match Json.parse content with
    | .ok json =>
      match json.getArr? with
      | .ok arr =>
        let symbols := arr.filterMap fun j =>
          match fromJson? j with
          | .ok (s : CBMCSymbol) => some s
          | .error _ => none
        IO.println s!"Successfully parsed {symbols.size} symbols"
        for symbol in symbols do
          IO.println s!"Symbol: {symbol.name} (type: {symbol.prettyType})"
      | .error e => IO.println s!"Error getting array: {e}"
    | .error e => IO.println s!"Error parsing JSON: {e}"
  | _ => IO.println "Usage: StrataToCBMC filename or StrataToCBMC test"
