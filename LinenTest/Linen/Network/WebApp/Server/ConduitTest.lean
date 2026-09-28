/-
  Tests for `Linen.Network.WebApp.Server.Conduit`.
-/
import Linen.Network.WebApp.Server.Conduit

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.Conduit

/-- A source that yields "hello" once, then empty (EOF). -/
private def mkOnceSource : IO Source := do
  let done ← IO.mkRef false
  Source.mk' do
    let isDone ← done.get
    if isDone then
      return ByteArray.empty
    else
      done.set true
      return "hello".toUTF8

#eval show IO Unit from do
  -- Known-length body, limited below the source's chunk size.
  let src ← mkOnceSource
  let isrc ← ISource.mkKnown src 3
  let chunk ← isrc.read
  assert! chunk == "hel".toUTF8
  -- The remaining 2 bytes were pushed back as leftover on the source.
  let leftover ← src.readLeftover
  assert! leftover == "lo".toUTF8
  -- Further reads return empty once the known length is exhausted.
  let chunk2 ← isrc.read
  assert! chunk2 == ByteArray.empty

#eval show IO Unit from do
  -- Chunked body: reads straight from the source until EOF.
  let src ← mkOnceSource
  let isrc ← ISource.mkChunked src
  let chunk ← isrc.read
  assert! chunk == "hello".toUTF8
  let chunk2 ← isrc.read
  assert! chunk2 == ByteArray.empty

#eval show IO Unit from do
  -- Known-length body larger than what the source ever produces.
  let src ← mkOnceSource
  let isrc ← ISource.mkKnown src 100
  let chunk ← isrc.read
  assert! chunk == "hello".toUTF8
  let chunk2 ← isrc.read
  assert! chunk2 == ByteArray.empty

end Tests.Network.WebApp.Server.Conduit
