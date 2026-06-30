/-
  Linen.Network.HTTP.Types.Version — HTTP version
-/

namespace Network.HTTP.Types

/-- HTTP protocol version.
    $$\text{HttpVersion} = \{ \text{major} : \mathbb{N},\; \text{minor} : \mathbb{N} \}$$ -/
structure HttpVersion where
  major : Nat
  minor : Nat
deriving BEq, Repr

instance : ToString HttpVersion where
  toString v := s!"HTTP/{v.major}.{v.minor}"

instance : Ord HttpVersion where
  compare a b :=
    match compare a.major b.major with
    | .eq => compare a.minor b.minor
    | ord => ord

/-- HTTP/0.9 -/
def http09 : HttpVersion := ⟨0, 9⟩
/-- HTTP/1.0 -/
def http10 : HttpVersion := ⟨1, 0⟩
/-- HTTP/1.1 -/
def http11 : HttpVersion := ⟨1, 1⟩
/-- HTTP/2.0 -/
def http20 : HttpVersion := ⟨2, 0⟩

-- ── Well-formedness theorems ────────────────────────────────────────────────

/-- HTTP/0.9 has major = 0 and minor = 9. -/
theorem http09_valid : http09.major = 0 ∧ http09.minor = 9 := ⟨rfl, rfl⟩

/-- HTTP/1.0 has major = 1 and minor = 0. -/
theorem http10_valid : http10.major = 1 ∧ http10.minor = 0 := ⟨rfl, rfl⟩

/-- HTTP/1.1 has major = 1 and minor = 1. -/
theorem http11_valid : http11.major = 1 ∧ http11.minor = 1 := ⟨rfl, rfl⟩

/-- HTTP/2.0 has major = 2 and minor = 0. -/
theorem http20_valid : http20.major = 2 ∧ http20.minor = 0 := ⟨rfl, rfl⟩

end Network.HTTP.Types
