import Linen.Data.Json
import Linen.System.Console.Ansi

open System.Console.Ansi

def main : IO Unit :=
  IO.println $ setFg .red ++ s!"Hello, {Data.Json.Value.null.isNull}!"
