import Linen.Data.Json


def main : IO Unit :=
  IO.println s!"Hello, {Data.Json.Value.null.isNull}!"
