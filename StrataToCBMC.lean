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

  let sourceLocation := mkSourceLocation "from_andrew.c" functionName "2"

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
      mkSymbol s!"{functionName}::x" mkIntType,
      mkSymbol s!"{functionName}::y" mkIntType
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
              ("type", mkIntType)
            ])
          ],
          mkConstant "0" "10" sourceLocation
        ])
      ]
    ])
  ]

  let ensuresSourceLocation := mkSourceLocation "from_andrew.c" functionName "3"
  let ensuresLambda := Json.mkObj [
    ("id", "lambda"),
    ("namedSub", Json.mkObj [
      ("#source_location", ensuresSourceLocation),
      ("type", mathFunctionType)
    ]),
    ("sub", Json.arr #[
      parameterTuple,
      Json.mkObj [
        ("id", "notequal"),
        ("namedSub", Json.mkObj [
          ("#source_location", ensuresSourceLocation),
          ("type", Json.mkObj [("id", "bool")])
        ]),
        ("sub", Json.arr #[
          mkConstant "1" "10" ensuresSourceLocation,
          Json.mkObj [
            ("id", "constant"),
            ("namedSub", Json.mkObj [
              ("type", mkIntType),
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
      mkParameter "x" functionName "1",
      mkParameter "y" functionName "1"
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
  let declStmt := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "5"),
      ("statement", Json.mkObj [("id", "decl")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "symbol"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "5"),
          ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
          ("type", mkIntType)
        ])
      ]
    ])
  ]

  -- Assignment: z = x + y;
  let assignStmt := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "6"),
      ("statement", Json.mkObj [("id", "expression")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "side_effect"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "6"),
          ("statement", Json.mkObj [("id", "assign")]),
          ("type", mkIntType)
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "symbol"),
            ("namedSub", Json.mkObj [
              ("#lvalue", Json.mkObj [("id", "1")]),
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "6"),
              ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
              ("type", mkIntType)
            ])
          ],
          Json.mkObj [
            ("id", "+"),
            ("namedSub", Json.mkObj [
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "6"),
              ("type", mkIntType)
            ]),
            ("sub", Json.arr #[
              Json.mkObj [
                ("id", "symbol"),
                ("namedSub", Json.mkObj [
                  ("#lvalue", Json.mkObj [("id", "1")]),
                  ("#source_location", mkSourceLocation "from_andrew.c" functionName "6"),
                  ("identifier", Json.mkObj [("id", s!"{functionName}::x")]),
                  ("type", mkIntType)
                ])
              ],
              Json.mkObj [
                ("id", "symbol"),
                ("namedSub", Json.mkObj [
                  ("#lvalue", Json.mkObj [("id", "1")]),
                  ("#source_location", mkSourceLocation "from_andrew.c" functionName "6"),
                  ("identifier", Json.mkObj [("id", s!"{functionName}::y")]),
                  ("type", mkIntType)
                ])
              ]
            ])
          ]
        ])
      ]
    ])
  ]

  -- Assert: __CPROVER_assert(z > x, "test_assert");
  let assertStmt := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
      ("statement", Json.mkObj [("id", "expression")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "side_effect"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
          ("statement", Json.mkObj [("id", "function_call")]),
          ("type", Json.mkObj [
            ("id", "empty"),
            ("namedSub", Json.mkObj [
              ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
            ])
          ])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "symbol"),
            ("namedSub", Json.mkObj [
              ("#lvalue", Json.mkObj [("id", "1")]),
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
              ("identifier", Json.mkObj [("id", "__CPROVER_assert")]),
              ("type", Json.mkObj [
                ("id", "code"),
                ("namedSub", Json.mkObj [
                  ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20"),
                  ("parameters", Json.mkObj [
                    ("id", ""),
                    ("sub", Json.arr #[
                      Json.mkObj [
                        ("id", "parameter"),
                        ("namedSub", Json.mkObj [
                          ("#base_name", Json.mkObj [("id", "assertion")]),
                          ("#identifier", Json.mkObj [("id", "")]),
                          ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "__CPROVER_assert" "20"),
                          ("type", Json.mkObj [("id", "bool")])
                        ])
                      ],
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
                    ])
                  ]),
                  ("return_type", Json.mkObj [
                    ("id", "empty"),
                    ("namedSub", Json.mkObj [
                      ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
                    ])
                  ])
                ])
              ])
            ])
          ],
          Json.mkObj [
            ("id", "arguments"),
            ("sub", Json.arr #[
              Json.mkObj [
                ("id", ">"),
                ("namedSub", Json.mkObj [
                  ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
                  ("type", Json.mkObj [("id", "bool")])
                ]),
                ("sub", Json.arr #[
                  Json.mkObj [
                    ("id", "symbol"),
                    ("namedSub", Json.mkObj [
                      ("#lvalue", Json.mkObj [("id", "1")]),
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
                      ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
                      ("type", mkIntType)
                    ])
                  ],
                  Json.mkObj [
                    ("id", "symbol"),
                    ("namedSub", Json.mkObj [
                      ("#lvalue", Json.mkObj [("id", "1")]),
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
                      ("identifier", Json.mkObj [("id", s!"{functionName}::x")]),
                      ("type", mkIntType)
                    ])
                  ]
                ])
              ],
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
                          ("#source_location", mkSourceLocation "from_andrew.c" functionName "7"),
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
                          ("value", Json.mkObj [("id", "test_assert")])
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
            ])
          ]
        ])
      ]
    ])
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
      Json.mkObj [
        ("id", ">"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "8"),
          ("type", Json.mkObj [("id", "bool")])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "symbol"),
            ("namedSub", Json.mkObj [
              ("#lvalue", Json.mkObj [("id", "1")]),
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "8"),
              ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
              ("type", mkIntType)
            ])
          ],
          mkConstant "A" "10" (mkSourceLocation "from_andrew.c" functionName "8")
        ])
      ],
      Json.mkObj [
        ("id", "code"),
        ("namedSub", Json.mkObj [
          ("#end_location", mkSourceLocation "from_andrew.c" functionName "10"),
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "8"),
          ("statement", Json.mkObj [("id", "block")]),
          ("type", Json.mkObj [("id", "empty")])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "code"),
            ("namedSub", Json.mkObj [
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "9"),
              ("statement", Json.mkObj [("id", "expression")]),
              ("type", Json.mkObj [("id", "empty")])
            ]),
            ("sub", Json.arr #[
              Json.mkObj [
                ("id", "side_effect"),
                ("namedSub", Json.mkObj [
                  ("#source_location", mkSourceLocation "from_andrew.c" functionName "9"),
                  ("statement", Json.mkObj [("id", "assign")]),
                  ("type", mkIntType)
                ]),
                ("sub", Json.arr #[
                  Json.mkObj [
                    ("id", "symbol"),
                    ("namedSub", Json.mkObj [
                      ("#lvalue", Json.mkObj [("id", "1")]),
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "9"),
                      ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
                      ("type", mkIntType)
                    ])
                  ],
                  Json.mkObj [
                    ("id", "-"),
                    ("namedSub", Json.mkObj [
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "9"),
                      ("type", mkIntType)
                    ]),
                    ("sub", Json.arr #[
                      Json.mkObj [
                        ("id", "symbol"),
                        ("namedSub", Json.mkObj [
                          ("#lvalue", Json.mkObj [("id", "1")]),
                          ("#source_location", mkSourceLocation "from_andrew.c" functionName "9"),
                          ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
                          ("type", mkIntType)
                        ])
                      ],
                      mkConstant "1" "10" (mkSourceLocation "from_andrew.c" functionName "9")
                    ])
                  ]
                ])
              ]
            ])
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
          Json.mkObj [
            ("id", "code"),
            ("namedSub", Json.mkObj [
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "11"),
              ("statement", Json.mkObj [("id", "expression")]),
              ("type", Json.mkObj [("id", "empty")])
            ]),
            ("sub", Json.arr #[
              Json.mkObj [
                ("id", "side_effect"),
                ("namedSub", Json.mkObj [
                  ("#source_location", mkSourceLocation "from_andrew.c" functionName "11"),
                  ("statement", Json.mkObj [("id", "assign")]),
                  ("type", mkIntType)
                ]),
                ("sub", Json.arr #[
                  Json.mkObj [
                    ("id", "symbol"),
                    ("namedSub", Json.mkObj [
                      ("#lvalue", Json.mkObj [("id", "1")]),
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "11"),
                      ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
                      ("type", mkIntType)
                    ])
                  ],
                  Json.mkObj [
                    ("id", "+"),
                    ("namedSub", Json.mkObj [
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "11"),
                      ("type", mkIntType)
                    ]),
                    ("sub", Json.arr #[
                      Json.mkObj [
                        ("id", "symbol"),
                        ("namedSub", Json.mkObj [
                          ("#lvalue", Json.mkObj [("id", "1")]),
                          ("#source_location", mkSourceLocation "from_andrew.c" functionName "11"),
                          ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
                          ("type", mkIntType)
                        ])
                      ],
                      mkConstant "1" "10" (mkSourceLocation "from_andrew.c" functionName "11")
                    ])
                  ]
                ])
              ]
            ])
          ]
        ])
      ]
    ])
  ]

  -- Assume: __CPROVER_assume(z > 0);
  let assumeStmt := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "13"),
      ("statement", Json.mkObj [("id", "expression")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[
      Json.mkObj [
        ("id", "side_effect"),
        ("namedSub", Json.mkObj [
          ("#source_location", mkSourceLocation "from_andrew.c" functionName "13"),
          ("statement", Json.mkObj [("id", "function_call")]),
          ("type", Json.mkObj [
            ("id", "empty"),
            ("namedSub", Json.mkObj [
              ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
            ])
          ])
        ]),
        ("sub", Json.arr #[
          Json.mkObj [
            ("id", "symbol"),
            ("namedSub", Json.mkObj [
              ("#lvalue", Json.mkObj [("id", "1")]),
              ("#source_location", mkSourceLocation "from_andrew.c" functionName "13"),
              ("identifier", Json.mkObj [("id", "__CPROVER_assume")]),
              ("type", Json.mkObj [
                ("id", "code"),
                ("namedSub", Json.mkObj [
                  ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20"),
                  ("parameters", Json.mkObj [
                    ("id", ""),
                    ("sub", Json.arr #[
                      Json.mkObj [
                        ("id", "parameter"),
                        ("namedSub", Json.mkObj [
                          ("#base_name", Json.mkObj [("id", "assumption")]),
                          ("#identifier", Json.mkObj [("id", "")]),
                          ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "__CPROVER_assume" "20"),
                          ("type", Json.mkObj [("id", "bool")])
                        ])
                      ]
                    ])
                  ]),
                  ("return_type", Json.mkObj [
                    ("id", "empty"),
                    ("namedSub", Json.mkObj [
                      ("#source_location", mkSourceLocation "<builtin-architecture-strings>" "" "20")
                    ])
                  ])
                ])
              ])
            ])
          ],
          Json.mkObj [
            ("id", "arguments"),
            ("sub", Json.arr #[
              Json.mkObj [
                ("id", ">"),
                ("namedSub", Json.mkObj [
                  ("#source_location", mkSourceLocation "from_andrew.c" functionName "13"),
                  ("type", Json.mkObj [("id", "bool")])
                ]),
                ("sub", Json.arr #[
                  Json.mkObj [
                    ("id", "symbol"),
                    ("namedSub", Json.mkObj [
                      ("#lvalue", Json.mkObj [("id", "1")]),
                      ("#source_location", mkSourceLocation "from_andrew.c" functionName "13"),
                      ("identifier", Json.mkObj [("id", s!"{functionName}::1::z")]),
                      ("type", mkIntType)
                    ])
                  ],
                  mkConstant "0" "10" (mkSourceLocation "from_andrew.c" functionName "13")
                ])
              ]
            ])
          ]
        ])
      ]
    ])
  ]

  -- Return statement: return 0;
  let returnStmt := Json.mkObj [
    ("id", "code"),
    ("namedSub", Json.mkObj [
      ("#source_location", mkSourceLocation "from_andrew.c" functionName "14"),
      ("statement", Json.mkObj [("id", "return")]),
      ("type", Json.mkObj [("id", "empty")])
    ]),
    ("sub", Json.arr #[
      mkConstant "0" "10" (mkSourceLocation "from_andrew.c" functionName "14")
    ])
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

def testSymbols : IO Unit := do
  let contractSymbol := createContractSymbol "simpleTest"
  let implSymbol := createImplementationSymbol "simpleTest"
  let xSymbol := createParameterSymbol "x"
  let ySymbol := createParameterSymbol "y"
  let zSymbol := createLocalSymbol "z"

  let m : Map String CBMCSymbol := [("contract::simpleTest", contractSymbol),
                                    ("simpleTest", implSymbol),
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
