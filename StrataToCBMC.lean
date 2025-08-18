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

-- Parameter structure for contracts
structure Parameter where
  id : String := "parameter"
  namedSub : Json
  deriving FromJson, ToJson

-- Lambda expression structure
structure LambdaExpr where
  id : String := "lambda"
  namedSub : Json
  sub : Array Json
  deriving FromJson, ToJson

-- Contract type structure
structure ContractType where
  id : String := "code"
  namedSub : Json
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

def createContractSymbol (functionName : String) : CBMCSymbol :=
  let location : Location := {
    id := "",
    namedSub := some (Json.mkObj [
      ("file", Json.mkObj [("id", "from_andrew.c")]),
      ("function", Json.mkObj [("id", "")]),
      ("line", Json.mkObj [("id", "1")]),
      ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
    ])
  }

  let intType := Json.mkObj [
    ("id", "signedbv"),
    ("namedSub", Json.mkObj [
      ("#c_type", Json.mkObj [("id", "signed_int")]),
      ("width", Json.mkObj [("id", "32")])
    ])
  ]

  let sourceLocation := Json.mkObj [
    ("id", ""),
    ("namedSub", Json.mkObj [
      ("file", Json.mkObj [("id", "from_andrew.c")]),
      ("function", Json.mkObj [("id", functionName)]),
      ("line", Json.mkObj [("id", "2")]),
      ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
    ])
  ]

  let mathFunctionType := Json.mkObj [
    ("id", "mathematical_function"),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", ""),
        ("sub", Json.arr #[intType, intType, intType])
      ],
      Json.mkObj [("id", "bool")]
    ])
  ]

  let parameterTuple := Json.mkObj [
    ("id", "tuple"),
    ("namedSub", Json.mkObj [("type", Json.mkObj [("id", "tuple")])]),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("identifier", Json.mkObj [("id", "__CPROVER_return_value")]),
          ("type", intType)
        ])
      ],
      Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("identifier", Json.mkObj [("id", s!"{functionName}::x")]),
          ("type", intType)
        ])
      ],
      Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("identifier", Json.mkObj [("id", s!"{functionName}::y")]),
          ("type", intType)
        ])
      ]
    ])
  ]

  let requiresLambda := Json.mkObj [
    ("id", "lambda"),
    ("namedSub", Json.mkObj [
      ("#source_location", sourceLocation),
      ("type", mathFunctionType)
    ]),
    ("sub", Json.arr #[
      parameterTuple,
      Json.mkObj [
        ("id", ">"),
        ("namedSub", Json.mkObj [
          ("#source_location", sourceLocation),
          ("type", Json.mkObj [("id", "bool")])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "symbol"),
            ("namedSub", Json.mkObj [
              ("#base_name", Json.mkObj [("id", "y")]),
              ("#id_class", Json.mkObj [("id", "1")]),
              ("#lvalue", Json.mkObj [("id", "1")]),
              ("#source_location", sourceLocation),
              ("identifier", Json.mkObj [("id", s!"{functionName}::y")]),
              ("type", intType)
            ])
          ],
          Json.mkObj [
            ("id", "constant"),
            ("namedSub", Json.mkObj [
              ("#base", Json.mkObj [("id", "10")]),
              ("#source_location", sourceLocation),
              ("type", intType),
              ("value", Json.mkObj [("id", "0")])
            ])
          ]
        ])
      ]
    ])
  ]

  let ensuresLambda := Json.mkObj [
    ("id", "lambda"),
    ("namedSub", Json.mkObj [
      ("#source_location", Json.mkObj [
        ("id", ""),
        ("namedSub", Json.mkObj [
          ("file", Json.mkObj [("id", "from_andrew.c")]),
          ("function", Json.mkObj [("id", functionName)]),
          ("line", Json.mkObj [("id", "3")]),
          ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
        ])
      ]),
      ("type", mathFunctionType)
    ]),
    ("sub", Json.arr #[
      parameterTuple,
      Json.mkObj [
        ("id", "notequal"),
        ("namedSub", Json.mkObj [
          ("#source_location", Json.mkObj [
            ("id", ""),
            ("namedSub", Json.mkObj [
              ("file", Json.mkObj [("id", "from_andrew.c")]),
              ("function", Json.mkObj [("id", functionName)]),
              ("line", Json.mkObj [("id", "3")]),
              ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
            ])
          ]),
          ("type", Json.mkObj [("id", "bool")])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "constant"),
            ("namedSub", Json.mkObj [
              ("#base", Json.mkObj [("id", "10")]),
              ("#source_location", Json.mkObj [
                ("id", ""),
                ("namedSub", Json.mkObj [
                  ("file", Json.mkObj [("id", "from_andrew.c")]),
                  ("function", Json.mkObj [("id", functionName)]),
                  ("line", Json.mkObj [("id", "3")]),
                  ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
                ])
              ]),
              ("type", intType),
              ("value", Json.mkObj [("id", "1")])
            ])
          ],
          Json.mkObj [
            ("id", "constant"),
            ("namedSub", Json.mkObj [
              ("type", intType),
              ("value", Json.mkObj [("id", "0")])
            ])
          ]
        ])
      ]
    ])
  ]

  let parameters := Json.mkObj [
    ("id", ""),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "parameter"),
        ("namedSub", Json.mkObj [
          ("#base_name", Json.mkObj [("id", "x")]),
          ("#identifier", Json.mkObj [("id", s!"{functionName}::x")]),
          ("#source_location", Json.mkObj [
            ("id", ""),
            ("namedSub", Json.mkObj [
              ("file", Json.mkObj [("id", "from_andrew.c")]),
              ("function", Json.mkObj [("id", functionName)]),
              ("line", Json.mkObj [("id", "1")]),
              ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
            ])
          ]),
          ("type", intType)
        ])
      ],
      Json.mkObj [
        ("id", "parameter"),
        ("namedSub", Json.mkObj [
          ("#base_name", Json.mkObj [("id", "y")]),
          ("#identifier", Json.mkObj [("id", s!"{functionName}::y")]),
          ("#source_location", Json.mkObj [
            ("id", ""),
            ("namedSub", Json.mkObj [
              ("file", Json.mkObj [("id", "from_andrew.c")]),
              ("function", Json.mkObj [("id", functionName)]),
              ("line", Json.mkObj [("id", "1")]),
              ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
            ])
          ]),
          ("type", intType)
        ])
      ]
    ])
  ]

  let contractType := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", Json.mkObj [
        ("id", ""),
        ("namedSub", Json.mkObj [
          ("file", Json.mkObj [("id", "from_andrew.c")]),
          ("function", Json.mkObj [("id", "")]),
          ("line", Json.mkObj [("id", "1")]),
          ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
        ])
      ]),
      ("#spec_assigns", Json.mkObj [("id", "")]),
      ("#spec_ensures", Json.mkObj [
        ("id", ""),
        ("sub", Json.arr #[ensuresLambda])
      ]),
      ("#spec_frees", Json.mkObj [("id", "")]),
      ("#spec_requires", Json.mkObj [
        ("id", ""),
        ("sub", Json.arr #[requiresLambda])
      ]),
      ("parameters", parameters),
      ("return_type", intType)
    ])
  ]

  {
    baseName := functionName,
    isProperty := true,
    location := location,
    mode := "C",
    module := "from_andrew",
    name := s!"contract::{functionName}",
    prettyName := functionName,
    prettyType := "signed int (signed int x, signed int y)",
    type := contractType,
    value := Json.mkObj [("id", "nil")]
  }

def createParameterSymbol (baseName : String) : CBMCSymbol :=
  createSymbol baseName "1" true true "simpleTest::"

def createLocalSymbol (baseName : String) : CBMCSymbol :=
  let fullName := s!"simpleTest::1::{baseName}"
  createSymbol baseName "5" false false "simpleTest::1::" fullName

instance : ToJson (Map String CBMCSymbol) where
  toJson m := Json.mkObj (m.map fun (k, v) => (k, toJson v))

def testSymbols : IO Unit := do
  let contractSymbol := createContractSymbol "simpleTest"
  let xSymbol := createParameterSymbol "x"
  let ySymbol := createParameterSymbol "y"
  let zSymbol := createLocalSymbol "z"

  let m : Map String CBMCSymbol := [("contract::simpleTest", contractSymbol),
                                    ("simpleTest::x", xSymbol),
                                    ("simpleTest::y", ySymbol),
                                    ("simpleTest::1::z", zSymbol)]

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
