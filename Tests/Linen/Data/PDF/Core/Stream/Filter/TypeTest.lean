/-
  Tests for `Linen.Data.PDF.Core.Stream.Filter.Type`.

  `StreamFilter` is a plain record of a name plus an `IO`-returning decoder,
  so this is exercised with a trivial `#eval` smoke test: build one by hand
  and check that calling `filterDecode` runs and returns what it's told to.
-/
import Linen.Data.PDF.Core.Stream.Filter.Type

open Data.PDF.Core.Stream.Filter.Type

namespace Tests.Data.PDF.Core.Stream.Filter.Type

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

-- A no-op filter's `filterDecode` just returns the stream it was given.
private def identityFilter : StreamFilter :=
  { filterName := mkName "Identity"
    filterDecode := fun _ is => pure is }

#guard identityFilter.filterName == mkName "Identity"

#eval show IO Unit from do
  let is ← Data.PDF.Stream.fromByteString (String.toUTF8 "hello")
  let is' ← identityFilter.filterDecode none is
  let chunks ← Data.PDF.Stream.toList is'
  let out := chunks.foldl (· ++ ·) ByteArray.empty
  unless out == String.toUTF8 "hello" do
    throw (IO.userError s!"unexpected decoded content: {out.toList}")

end Tests.Data.PDF.Core.Stream.Filter.Type
