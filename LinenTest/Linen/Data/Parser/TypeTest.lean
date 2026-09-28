/-
  Tests for `Data.Parser.Type`.

  The pure `Step`/`Initial`/`Final` helpers are checked with `#guard`. The
  parser combinators (`splitWith`/`alt`/`splitMany`/…) are exercised through the
  `unsafe` list driver in `Tests.Linen.Data.ParserTest`, since driving a parser
  is `unsafe`.
-/
import Linen.Data.Parser.Type

open Data.Parser

namespace Tests.Data.Parser.Type

-- ── Step helpers ──────────────────────────────────────────────────────────────

#guard Step.mapCount (· + 1) (Step.SPartial (s := Nat) (b := Nat) 0 5) == Step.SPartial 1 5
#guard Step.mapCount (· + 1) (Step.SDone (s := Nat) (b := Nat) 2 7) == Step.SDone 3 7
#guard Step.mapSnd (· + 1) (Step.SDone (s := Nat) 1 7) == Step.SDone (s := Nat) 1 8
#guard Step.mapSnd (· + 1) (Step.SPartial (s := Nat) (b := Nat) 1 9) == Step.SPartial 1 9
#guard Step.mapFst (· + 1) (Step.SContinue (s := Nat) (b := Nat) 1 3) == Step.SContinue 1 4
#guard Step.bimap (· + 1) (· * 2) (Step.SDone (s := Nat) 0 5) == Step.SDone (s := Nat) 0 10
#guard Step.bimap (· + 1) (· * 2) (Step.SContinue (b := Nat) 0 5) == Step.SContinue 0 6
#guard (Step.SError "e" : Step Nat Nat) == Step.mapCount (· + 1) (Step.SError "e")
#guard bimapOverrideCount 9 (· + 1) (· * 2) (Step.SPartial (b := Nat) 0 4) == Step.SPartial 9 5

-- ── Initial helpers ───────────────────────────────────────────────────────────

#guard Initial.mapSnd (· + 1) (Initial.IDone (s := Nat) 4) == Initial.IDone (s := Nat) 5
#guard Initial.mapFst (· + 1) (Initial.IPartial (b := Nat) 4) == Initial.IPartial 5
#guard (Initial.IError "x" : Initial Nat Nat) == Initial.mapSnd (· + 1) (Initial.IError "x")

-- ── Final helpers ─────────────────────────────────────────────────────────────

#guard Final.mapSnd (· + 1) (Final.FDone (s := Nat) 0 4) == Final.FDone (s := Nat) 0 5
#guard Final.mapFst (· + 1) (Final.FContinue (b := Nat) 0 4) == Final.FContinue 0 5
#guard bimapFinalOverrideCount 3 (· + 1) (· * 2) (Final.FContinue (b := Nat) 0 4) == Final.FContinue 3 5
#guard bimapFinalOverrideCount 3 (· + 1) (· * 2) (Final.FDone (s := Nat) 0 4) == Final.FDone (s := Nat) 3 8
#guard bimapMorphOverrideCount 3 (· + 1) (· * 2) (Final.FDone (s := Nat) 0 4) == Step.SDone (s := Nat) 3 8
#guard bimapMorphOverrideCount 3 (· + 1) (· * 2) (Final.FContinue (b := Nat) 0 4) == Step.SContinue 3 5

-- ── ParseError data ───────────────────────────────────────────────────────────

#guard (ParseError.mk "boom" == ParseError.mk "boom")
#guard (ParseErrorPos.mk 3 "oops" == ParseErrorPos.mk 3 "oops")

end Tests.Data.Parser.Type
