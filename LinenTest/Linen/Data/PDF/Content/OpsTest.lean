import Linen.Data.PDF.Content.Ops
import Linen.Data.Text.Encoding

open Data.PDF.Content.Ops

private def bs (s : String) : Data.ByteString := Data.Text.Encoding.encodeUtf8 s

-- ── `toOp` recognizes every operator keyword ──

#guard toOp (bs "q") == Op.q
#guard toOp (bs "Q") == Op.Q
#guard toOp (bs "cm") == Op.cm
#guard toOp (bs "f*") == Op.f_star
#guard toOp (bs "B*") == Op.B_star
#guard toOp (bs "b*") == Op.b_star
#guard toOp (bs "W*") == Op.W_star
#guard toOp (bs "T*") == Op.T_star
#guard toOp (bs "'") == Op.apostrophe
#guard toOp (bs "\"") == Op.quote
#guard toOp (bs "TJ") == Op.TJ
#guard toOp (bs "BDC") == Op.BDC
#guard toOp (bs "EMC") == Op.EMC
#guard toOp (bs "Do") == Op.Do
#guard toOp (bs "BX") == Op.BX
#guard toOp (bs "EX") == Op.EX

-- ── Unknown keywords fall back to `UnknownOp` ──

#guard toOp (bs "xyz") == Op.UnknownOp (bs "xyz")
#guard toOp (bs "") == Op.UnknownOp (bs "")

-- ── `Expr` wraps either an operand or an operator ──

#guard (Expr.op Op.q) == Expr.op Op.q
#guard (Expr.op Op.q) != Expr.op Op.Q
