/-
  Copyright Strata Contributors

  SPDX-License-Identifier: Apache-2.0 OR MIT
-/

-- Executable for verifying a Strata program from a file.
import Strata.Dyn.py_ast
import Strata.Dyn.Py_to_Boogie
import Std.Internal.Parsec


def usageMessage : String :=
  "Usage: StrataTranslate file.json"


def main (args : List String) : IO Unit := do
  -- Need to use the Lean-compatible JSON format
  match args with
  | [filename] =>
    let ast ← loadJsonFile filename
    let boogie_prog := (translate_py_to_boogie ast)
    IO.println boogie_prog
  | _ => IO.println "Usage: Executable filename"
