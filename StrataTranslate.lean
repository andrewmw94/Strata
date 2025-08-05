/-
  Copyright Strata Contributors

  SPDX-License-Identifier: Apache-2.0 OR MIT
-/

-- Executable for verifying a Strata program from a file.
import Strata.Languages.Boogie.Verifier
import Strata.Languages.C_Simp.Verify
import Std.Internal.Parsec

open Strata


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

def main (args : List String) : IO UInt32 := do
  if args.length != 1 then
    IO.print usageMessage
    return 1
  else
    IO.print output
    return 0
