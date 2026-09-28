/-
  Linen.Crypto.ConstantTime — comparing secrets without a timing oracle

  `==` on `ByteArray` or `String` returns at the first differing byte, so the
  time it takes tells an attacker how long a prefix of their guess was right.
  Comparing a MAC tag, an HMAC signature or a bearer token that way lets them
  recover it byte by byte. `eq` below reads every byte whatever it finds:
  it folds `x ^^^ y` into an accumulator with `|||`, which neither branches
  nor stops early, and decides once at the end.

  ## What is and is not hidden
  The **contents** are hidden; the **lengths** are not — arrays of different
  sizes compare unequal at once. That is the usual contract (OpenSSL's
  `CRYPTO_memcmp` takes one length for both): a tag's or a token's length is
  public, fixed by the algorithm or the configuration.

  ## Provenance
  Moved from the sibling services `lun` and `lode` (`Server.lean`, the
  bearer-token check), which carried identical copies, so that `liaison`'s
  warrant-tag check and `Crypto.JOSE.JWS`'s HMAC verification could use it
  too.

  ## Limits
  Written in Lean, not in C: Lean's compiler does not turn the fold into an
  early exit (there is no short-circuiting `|||`), but no compiler gives a
  proof of timing behaviour, and that is not claimed here — see
  `LinenTest/Linen/Crypto/ConstantTimeTest.lean` for what is checked
  (correctness, on equal, unequal and differently sized inputs).
-/

namespace Crypto.ConstantTime

-- ── Comparison ──

/-- Whether `a` and `b` hold the same bytes, reading every byte of both
    (when their sizes agree) whatever they contain.

    $$\mathrm{eq}(a, b) \iff |a| = |b| \land \bigvee_{i < |a|} (a_i \oplus b_i) = 0$$ -/
def eq (a b : ByteArray) : Bool :=
  a.size == b.size &&
    Nat.fold a.size (fun i _ acc => acc ||| (a[i]! ^^^ b[i]!)) (0 : UInt8) == 0

/-- `eq` on the strings' UTF-8 encodings: for comparing a presented token
    (an `Authorization` header, say) with the configured one. -/
def eqString (a b : String) : Bool :=
  eq a.toUTF8 b.toUTF8

end Crypto.ConstantTime
