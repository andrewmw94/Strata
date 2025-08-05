/-
  Copyright Strata Contributors

  SPDX-License-Identifier: Apache-2.0 OR MIT
-/

-- Executable for verifying a Strata program from a file.
import Strata.Dyn.py_ast
import Std.Internal.Parsec


def usageMessage : String :=
  "Usage: StrataTranslate file.json"

def output := r"
procedure double(x : int) returns (ret : int)
spec {
  ensures (ret == 2*x);
  ensures (old(count) + 1 == count);
}
{
  ret := x + x;
};
"

def translate_py_to_boogie (m: Module) : String :=
  output

def main (args : List String) : IO Unit := do
  -- Need to use the Lean-compatible JSON format
  match args with
  | [filename] =>
    let ast ← loadJsonFile filename
    let boogie_prog := (translate_py_to_boogie ast)
    IO.println boogie_prog
  | _ => IO.println "Usage: Executable filename"
