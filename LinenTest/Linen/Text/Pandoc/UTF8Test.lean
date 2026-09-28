/-
  Tests for `Linen.Text.Pandoc.UTF8`.
-/
import Linen.Text.Pandoc.UTF8

namespace Tests.Linen.Text.Pandoc.UTF8

open _root_.Linen.Text.Pandoc

-- ── Round-trip ────────────────────────────────────────────────────────

#guard UTF8.toString (UTF8.fromString "héllo, wörld") == "héllo, wörld"
#guard UTF8.fromString "abc" == "abc".toUTF8

-- ── BOM stripping ─────────────────────────────────────────────────────

private def bomHello : ByteArray :=
  (ByteArray.mk #[0xEF, 0xBB, 0xBF]) ++ "hello".toUTF8

#guard UTF8.toString bomHello == "hello"
#guard UTF8.dropBOM bomHello == "hello".toUTF8

-- ── CR stripping ──────────────────────────────────────────────────────

#guard UTF8.toString "a\r\nb\r\n".toUTF8 == "a\nb\n"
-- encoding never re-adds a BOM or CR
#guard UTF8.fromString "a\nb" == "a\nb".toUTF8

-- ── Identity helpers ──────────────────────────────────────────────────

#guard UTF8.encodePath "foo/bar" == "foo/bar"
#guard UTF8.decodeArg "x" == "x"

end Tests.Linen.Text.Pandoc.UTF8
