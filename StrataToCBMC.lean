/-
  Copyright Strata Contributors

  SPDX-License-Identifier: Apache-2.0 OR MIT
-/

import Lean.Data.Json
import Strata.DL.Util.Map
import Strata.Languages.C_Simp.C_Simp
import Strata.Languages.C_Simp.Verify


-- Our test program
def SimpleTestEnv :=
#strata
program C_Simp;

procedure simpleTest (x: int, y: int) -> int
  @pre y > #0
  @post true
{
  var z : int;
  z := x + y;
  @assert [test_assert] z > x;
  if (z > #10) then {
    z := z - #1;
  } else {
    z := z + #1;
  }
  @assume [test_assume] z > #0;
  return #0;
}

#end

open Strata.C_Simp in
def SimpleTestEnvAST := TransM.run (translateProgram (SimpleTestEnv.commands))

def myFunc : Strata.C_Simp.Function := SimpleTestEnvAST.fst.funcs.head!

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

def mkSourceLocation (file : String) (function : String) (line : String) : Json :=
  Json.mkObj [
    ("id", ""),
    ("namedSub", Json.mkObj [
      ("file", Json.mkObj [("id", file)]),
      ("function", Json.mkObj [("id", function)]),
      ("line", Json.mkObj [("id", line)]),
      ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
    ])
  ]

def mkIntType : Json :=
  Json.mkObj [
    ("id", "signedbv"),
    ("namedSub", Json.mkObj [
      ("#c_type", Json.mkObj [("id", "signed_int")]),
      ("width", Json.mkObj [("id", "32")])
    ])
  ]

def mkParameter (baseName : String) (functionName : String) (line : String) : Json :=
  Json.mkObj [
    ("id", "parameter"),
    ("namedSub", Json.mkObj [
      ("#base_name", Json.mkObj [("id", baseName)]),
      ("#identifier", Json.mkObj [("id", s!"{functionName}::{baseName}")]),
      ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
      ("type", mkIntType)
    ])
  ]

def mkSymbol (identifier : String) (symbolType : Json) : Json :=
  Json.mkObj [
    ("id", "symbol"),
    ("namedSub", Json.mkObj [
      ("identifier", Json.mkObj [("id", identifier)]),
      ("type", symbolType)
    ])
  ]

def mkConstant (value : String) (base : String) (sourceLocation : Json) : Json :=
  Json.mkObj [
    ("id", "constant"),
    ("namedSub", Json.mkObj [
      ("#base", Json.mkObj [("id", base)]),
      ("#source_location", sourceLocation),
      ("type", mkIntType),
      ("value", Json.mkObj [("id", value)])
    ])
  ]



def mkCodeBlock (statement : String) (line : String) (functionName : String) (sub : Array Json) : Json :=
  Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
      ("statement", Json.mkObj [("id", statement)]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr sub)
  ]

def mkSideEffect (statement : String) (line : String) (functionName : String) (effectType : Json) (sub : Array Json) : Json :=
  Json.mkObj [
    ("id", "side_effect"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
      ("statement", Json.mkObj [("id", statement)]),
      ("type", effectType)
    ]),
    ("sub", Json.arr sub)
  ]

def mkLvalueSymbol (identifier : String) (line : String) (functionName : String) : Json :=
  Json.mkObj [
    ("id", "symbol"),
    ("namedSub", Json.mkObj [
      ("#lvalue", Json.mkObj [("id", "1")]),
      ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
      ("identifier", Json.mkObj [("id", identifier)]),
      ("type", mkIntType)
    ])
  ]

def mkBinaryOp (op : String) (line : String) (functionName : String) (left : Json) (right : Json) : Json :=
  Json.mkObj [
    ("id", op),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
      ("type", mkIntType)
    ]),
    ("sub", Json.arr #[left, right])
  ]

def mkComparison (op : String) (line : String) (functionName : String) (left : Json) (right : Json) : Json :=
  Json.mkObj [
    ("id", op),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
      ("type", Json.mkObj [("id", "bool")])
    ]),
    ("sub", Json.arr #[left, right])
  ]

def mkBuiltinFunction (_funcName : String) (paramTypes : Array Json) : Json :=
  Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20"),
      ("parameters", Json.mkObj [
        ("id", ""),
        ("sub", Json.arr paramTypes)
      ]),
      ("return_type", Json.mkObj [
        ("id", "empty"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
        ])
      ])
    ])
  ]

def mkAssertParam : Json :=
  Json.mkObj [
    ("id", "parameter"),
    ("namedSub", Json.mkObj [
      ("#base_name", Json.mkObj [("id", "assertion")]),
      ("#identifier", Json.mkObj [("id", "")]),
      ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "__CPROVER_assert" "20"),
      ("type", Json.mkObj [("id", "bool")])
    ])
  ]

def mkStringParam : Json :=
  Json.mkObj [
    ("id", "parameter"),
    ("namedSub", Json.mkObj [
      ("#base_name", Json.mkObj [("id", "description")]),
      ("#identifier", Json.mkObj [("id", "")]),
      ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "__CPROVER_assert" "20"),
      ("type", Json.mkObj [
        ("id", "pointer"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "__CPROVER_assert" "20"),
          ("width", Json.mkObj [("id", "64")])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "signedbv"),
            ("namedSub", Json.mkObj [
              ("#c_type", Json.mkObj [("id", "char")]),
              ("#constant", Json.mkObj [("id", "1")]),
              ("width", Json.mkObj [("id", "8")])
            ])
          ]
        ])
      ])
    ])
  ]

def mkAssumeParam : Json :=
  Json.mkObj [
    ("id", "parameter"),
    ("namedSub", Json.mkObj [
      ("#base_name", Json.mkObj [("id", "assumption")]),
      ("#identifier", Json.mkObj [("id", "")]),
      ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "__CPROVER_assume" "20"),
      ("type", Json.mkObj [("id", "bool")])
    ])
  ]

def mkStringConstant (value : String) (line : String) (functionName : String) : Json :=
  Json.mkObj [
    ("id", "address_of"),
    ("namedSub", Json.mkObj [
      ("type", Json.mkObj [
        ("id", "pointer"),
        ("namedSub", Json.mkObj [("width", Json.mkObj [("id", "64")])]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "signedbv"),
            ("namedSub", Json.mkObj [
              ("#c_type", Json.mkObj [("id", "char")]),
              ("width", Json.mkObj [("id", "8")])
            ])
          ]
        ])
      ])
    ]),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "index"),
        ("namedSub", Json.mkObj [
          ("type", Json.mkObj [
            ("id", "signedbv"),
            ("namedSub", Json.mkObj [
              ("#c_type", Json.mkObj [("id", "char")]),
              ("width", Json.mkObj [("id", "8")])
            ])
          ])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "string_constant"),
            ("namedSub", Json.mkObj [
              ("#lvalue", Json.mkObj [("id", "1")]),
              ("#source_location", mkSourceLocation "from_andrew.c" functionName line),
              ("type", Json.mkObj [
                ("id", "array"),
                ("namedSub", Json.mkObj [
                  ("size", Json.mkObj [
                    ("id", "constant"),
                    ("namedSub", Json.mkObj [
                      ("type", Json.mkObj [
                        ("id", "signedbv"),
                        ("namedSub", Json.mkObj [
                          ("#c_type", Json.mkObj [("id", "signed_long_int")]),
                          ("width", Json.mkObj [("id", "64")])
                        ])
                      ]),
                      ("value", Json.mkObj [("id", "C")])
                    ])
                  ])
                ]),
                ("sub", Json.arr #[
                  Json.mkObj [
                    ("id", "signedbv"),
                    ("namedSub", Json.mkObj [
                      ("#c_type", Json.mkObj [("id", "char")]),
                      ("width", Json.mkObj [("id", "8")])
                    ])
                  ]
                ])
              ]),
              ("value", Json.mkObj [("id", value)])
            ])
          ],
          Json.mkObj [
            ("id", "constant"),
            ("namedSub", Json.mkObj [
              ("type", Json.mkObj [
                ("id", "signedbv"),
                ("namedSub", Json.mkObj [
                  ("#c_type", Json.mkObj [("id", "signed_long_int")]),
                  ("width", Json.mkObj [("id", "64")])
                ])
              ]),
              ("value", Json.mkObj [("id", "0")])
            ])
          ]
        ])
      ]
    ])
  ]

def createImplementationSymbol (functionName : String) : CBMCSymbol :=
  let location : Location := {
    id := "",
    namedSub := some (Json.mkObj [
      ("file", Json.mkObj [("id", "from_andrew.c")]),
      ("function", Json.mkObj [("id", "")]),
      ("line", Json.mkObj [("id", "1")]),
      ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
    ])
  }

  let implType := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" "" "1"),
      ("parameters", Json.mkObj [
        ("id", ""),
        ("sub", Json.arr #[
          mkParameter "x" functionName "1",
          mkParameter "y" functionName "1"
        ])
      ]),
      ("return_type", mkIntType)
    ])
  ]

  -- Variable declaration: signed int z;
  let declStmt := mkCodeBlock "decl" "5" functionName #[
    Json.mkObj [
      ("id", "symbol"),
      ("namedSub", Json.mkObj [
        ("#source_location", mkSourceLocation "from_andrew.c" functionName "5"),
        ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
        ("type", mkIntType)
      ])
    ]
  ]

  -- Assignment: z = x + y;
  let assignStmt := mkCodeBlock "expression" "6" functionName #[
    mkSideEffect "assign" "6" functionName mkIntType #[
      mkLvalueSymbol s!"{functionName}::1::z" "6" functionName,
      mkBinaryOp "+" "6" functionName
        (mkLvalueSymbol s!"{functionName}::x" "6" functionName)
        (mkLvalueSymbol s!"{functionName}::y" "6" functionName)
    ]
  ]

  -- Assert: __CPROVER_assert(z > x, "test_assert");
  let assertStmt := mkCodeBlock "expression" "7" functionName #[
    mkSideEffect "function_call" "7" functionName
      (Json.mkObj [
        ("id", "empty"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
        ])
      ]) #[
      Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("#lvalue", Json.mkObj [("id", "1")]),
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
          ("identifier", Json.mkObj [("id", "__CPROVER_assert")]),
          ("type", mkBuiltinFunction "__CPROVER_assert" #[mkAssertParam, mkStringParam])
        ])
      ],
      Json.mkObj [
        ("id", "arguments"),
        ("sub", Json.arr #[
          mkComparison ">" "7" functionName
            (mkLvalueSymbol s!"{functionName}::1::z" "7" functionName)
            (mkLvalueSymbol s!"{functionName}::x" "7" functionName),
          mkStringConstant "test_assert" "7" functionName
        ])
      ]
    ]
  ]

  -- If statement: if(z > 10) { z = z - 1; } else { z = z + 1; }
  let ifStmt := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "8"),
      ("statement", Json.mkObj [("id", "ifthenelse")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[
      mkComparison ">" "8" functionName
        (mkLvalueSymbol s!"{functionName}::1::z" "8" functionName)
        (mkConstant "A" "10" (mkSourceLocation "from_andrew.c" functionName "8")),
      Json.mkObj [
        ("id", "code"),
        ("namedSub", Json.mkObj [
          ("#end_location", mkSourceLocation "from_andrew.c" functionName "10"),
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "8"),
          ("statement", Json.mkObj [("id", "block")]),
          ("type", Json.mkObj [("id", "empty")])
        ]),
        ("sub", Json.arr #[
          mkCodeBlock "expression" "9" functionName #[
            mkSideEffect "assign" "9" functionName mkIntType #[
              mkLvalueSymbol s!"{functionName}::1::z" "9" functionName,
              mkBinaryOp "-" "9" functionName
                (mkLvalueSymbol s!"{functionName}::1::z" "9" functionName)
                (mkConstant "1" "10" (mkSourceLocation "from_andrew.c" functionName "9"))
            ]
          ]
        ])
      ],
      Json.mkObj [
        ("id", "code"),
        ("namedSub", Json.mkObj [
          ("#end_location", mkSourceLocation "from_andrew.c" functionName "12"),
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "10"),
          ("statement", Json.mkObj [("id", "block")]),
          ("type", Json.mkObj [("id", "empty")])
        ]),
        ("sub", Json.arr #[
          mkCodeBlock "expression" "11" functionName #[
            mkSideEffect "assign" "11" functionName mkIntType #[
              mkLvalueSymbol s!"{functionName}::1::z" "11" functionName,
              mkBinaryOp "+" "11" functionName
                (mkLvalueSymbol s!"{functionName}::1::z" "11" functionName)
                (mkConstant "1" "10" (mkSourceLocation "from_andrew.c" functionName "11"))
            ]
          ]
        ])
      ]
    ])
  ]

  -- Assume: __CPROVER_assume(z > 0);
  let assumeStmt := mkCodeBlock "expression" "13" functionName #[
    mkSideEffect "function_call" "13" functionName
      (Json.mkObj [
        ("id", "empty"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
        ])
      ]) #[
      Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("#lvalue", Json.mkObj [("id", "1")]),
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "13"),
          ("identifier", Json.mkObj [("id", "__CPROVER_assume")]),
          ("type", mkBuiltinFunction "__CPROVER_assume" #[mkAssumeParam])
        ])
      ],
      Json.mkObj [
        ("id", "arguments"),
        ("sub", Json.arr #[
          mkComparison ">" "13" functionName
            (mkLvalueSymbol s!"{functionName}::1::z" "13" functionName)
            (mkConstant "0" "10" (mkSourceLocation "from_andrew.c" functionName "13"))
        ])
      ]
    ]
  ]

  -- Return statement: return 0;
  let returnStmt := mkCodeBlock "return" "14" functionName #[
    mkConstant "0" "10" (mkSourceLocation "from_andrew.c" functionName "14")
  ]

  let implValue := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#end_location", mkSourceLocation "from_andrew.c" functionName "15"),
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "4"),
      ("statement", Json.mkObj [("id", "block")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[declStmt, assignStmt, assertStmt, ifStmt, assumeStmt, returnStmt])
  ]

  {
    baseName := functionName,
    isLvalue := true,
    location := location,
    mode := "C",
    module := "from_andrew",
    name := functionName,
    prettyName := functionName,
    prettyType := "signed int (signed int x, signed int y)",
    prettyValue := "{\n  signed int z;\n  z = x + y;\n  __CPROVER_assert(z > x, \"test_assert\");\n  if(z > 10)\n  {\n    z = z - 1;\n  }\n\n  else\n  {\n    z = z + 1;\n  }\n  __CPROVER_assume(z > 0);\n  return 0;\n}",
    type := implType,
    value := implValue
  }

def createParameterSymbol (baseName : String) : CBMCSymbol :=
  createSymbol baseName "1" true true "simpleTest::"

def createLocalSymbol (baseName : String) : CBMCSymbol :=
  let fullName := s!"simpleTest::1::{baseName}"
  createSymbol baseName "5" false false "simpleTest::1::" fullName

instance : ToJson (Map String CBMCSymbol) where
  toJson m := Json.mkObj (m.map fun (k, v) => (k, toJson v))

-- Convert LExpr to CBMC JSON format
def lexprToCBMC (expr : Strata.C_Simp.Expression.Expr) (functionName : String) : Json :=
  match expr with
  | .app (.app (.op "Int.Gt" _) (.fvar varName _)) (.const value _) =>
    mkComparison ">" "2" functionName
      (Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("#base_name", Json.mkObj [("id", varName)]),
          ("#id_class", Json.mkObj [("id", "1")]),
          ("#lvalue", Json.mkObj [("id", "1")]),
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "2"),
          ("identifier", Json.mkObj [("id", s!"{functionName}::{varName}")]),
          ("type", mkIntType)
        ])
      ])
      (mkConstant value "10" (mkSourceLocation "from_andrew.c" functionName "2"))
  | .const "true" _ =>
    Json.mkObj [
      ("id", "notequal"),
      ("namedSub", Json.mkObj [
        ("#source_location", mkSourceLocation "from_andrew.c" functionName "3"),
        ("type", Json.mkObj [("id", "bool")])
      ]),
      ("sub", Json.arr #[
        mkConstant "1" "10" (mkSourceLocation "from_andrew.c" functionName "3"),
        Json.mkObj [
          ("id", "constant"),
          ("namedSub", Json.mkObj [
            ("type", mkIntType),
            ("value", Json.mkObj [("id", "0")])
          ])
        ]
      ])
    ]
  | _ => Json.mkObj [("id", "true")]

def createContractSymbolFromAST (func : Strata.C_Simp.Function) : CBMCSymbol :=
  let location : Location := {
    id := "",
    namedSub := some (Json.mkObj [
      ("file", Json.mkObj [("id", "from_andrew.c")]),
      ("function", Json.mkObj [("id", "")]),
      ("line", Json.mkObj [("id", "1")]),
      ("working_directory", Json.mkObj [("id", "/home/ub-backup/tautschn/cbmc-github.git")])
    ])
  }

  let sourceLocation := mkSourceLocation "from_andrew.c" func.name "2"
  let ensuresSourceLocation := mkSourceLocation "from_andrew.c" func.name "3"

  let mathFunctionType := Json.mkObj [
    ("id", "mathematical_function"),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", ""),
        ("sub", Json.arr #[mkIntType, mkIntType, mkIntType])
      ],
      Json.mkObj [("id", "bool")]
    ])
  ]

  let parameterTuple := Json.mkObj [
    ("id", "tuple"),
    ("namedSub", Json.mkObj [("type", Json.mkObj [("id", "tuple")])]),
    ("sub", Json.arr #[
      mkSymbol "__CPROVER_return_value" mkIntType,
      mkSymbol s!"{func.name}::x" mkIntType,
      mkSymbol s!"{func.name}::y" mkIntType
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
      lexprToCBMC func.pre func.name
    ])
  ]

  let ensuresLambda := Json.mkObj [
    ("id", "lambda"),
    ("namedSub", Json.mkObj [
      ("#source_location", ensuresSourceLocation),
      ("type", mathFunctionType)
    ]),
    ("sub", Json.arr #[
      parameterTuple,
      lexprToCBMC func.post func.name
    ])
  ]

  let parameters := Json.mkObj [
    ("id", ""),
    ("sub", Json.arr #[
      mkParameter "x" func.name "1",
      mkParameter "y" func.name "1"
    ])
  ]

  let contractType := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" "" "1"),
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
      ("return_type", mkIntType)
    ])
  ]

  {
    baseName := func.name,
    isProperty := true,
    location := location,
    mode := "C",
    module := "from_andrew",
    name := s!"contract::{func.name}",
    prettyName := func.name,
    prettyType := "signed int (signed int x, signed int y)",
    type := contractType,
    value := Json.mkObj [("id", "nil")]
  }

def testSymbols : IO Unit := do
  -- Generate symbols using AST data
  let contractSymbol := createContractSymbolFromAST myFunc
  let implSymbol := createImplementationSymbol myFunc.name

  -- Get parameter names from AST
  let paramNames := myFunc.inputs.keys

  -- Hardcode local variable for now
  let zSymbol := createLocalSymbol "z"

  -- Build symbol map
  let mut m : Map String CBMCSymbol := Map.empty
  m := m.insert s!"contract::{myFunc.name}" contractSymbol
  m := m.insert myFunc.name implSymbol

  -- Add parameter symbols
  for paramName in paramNames do
    let paramSymbol := createParameterSymbol paramName
    m := m.insert s!"{myFunc.name}::{paramName}" paramSymbol

  -- Add local variable
  m := m.insert s!"{myFunc.name}::1::z" zSymbol

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
