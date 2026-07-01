/-
  Tests for `Linen.PostgREST.ApiRequest.Types`.
-/
import Linen.PostgREST.ApiRequest.Types

open PostgREST.ApiRequest
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.ApiRequest.Types

/-! ### `Mutation` / `InvokeMethod` / `Action` -/

#guard toString Mutation.insert == "INSERT"
#guard toString Mutation.delete == "DELETE"

#guard toString InvokeMethod.invGet == "GET"
#guard toString InvokeMethod.invPost == "POST"

#guard toString (Action.actionRead false) == "GET (read)"
#guard toString (Action.actionRead true) == "HEAD (read)"
#guard toString (Action.actionMutate .insert) == "MUTATE (INSERT)"
#guard toString (Action.actionInvoke .invPost) == "INVOKE (POST)"
#guard toString Action.actionInfo == "OPTIONS (info)"
#guard toString (Action.actionInspect false) == "GET (inspect)"

/-! ### `JsonOperation` -/

#guard toString (JsonOperation.arrowRight "foo") == "->foo"
#guard toString (JsonOperation.arrowRightRight "foo") == "->>foo"

/-! ### Operators -/

#guard toString SimpleOperator.opEqual == "eq"
#guard toString SimpleOperator.opILike == "ilike"
#guard toString SimpleOperator.opIsDistinct == "isdistinct"

#guard toString (FtsOperator.fts none) == "fts"
#guard toString (FtsOperator.fts (some "english")) == "fts(english)"
#guard toString (FtsOperator.plfts (some "french")) == "plfts(french)"

#guard toString QuantOperator.any == "any"
#guard toString QuantOperator.all == "all"

#guard toString (FilterOperator.simple .opEqual) == "eq"
#guard toString (FilterOperator.fts (.fts none)) == "fts"
#guard toString (FilterOperator.quantified .any .opGreaterThan) == "any.gt"

/-! ### `Filter` -/

#guard toString ({ field := "age", operator := .simple .opGreaterThan, value := "18" } : Filter) ==
  "age.gt.18"
#guard toString ({ field := "meta", jsonPath := [.arrowRight "k"], operator := .simple .opEqual, value := "v" } : Filter) == "meta->k.eq.v"
#guard ({ field := "age", operator := .simple .opGreaterThan, value := "18" } : Filter) ==
  ({ field := "age", operator := .simple .opGreaterThan, value := "18" } : Filter)

/-! ### `LogicTree` -/

#guard toString (LogicTree.stmnt { field := "age", operator := .simple .opGreaterThan, value := "18" }) ==
  "age.gt.18"
#guard toString (LogicTree.expr false .and_
    #[ LogicTree.stmnt { field := "age", operator := .simple .opGreaterThan, value := "18" },
       LogicTree.stmnt { field := "name", operator := .simple .opEqual, value := "bob" } ]) ==
  "and(age.gt.18, name.eq.bob)"
#guard toString (LogicTree.expr true .or_ #[]) == "not.or()"

/-! ### Ordering -/

#guard toString OrderDirection.asc == "asc"
#guard toString OrderDirection.desc == "desc"
#guard toString OrderNulls.nullsFirst == "nullsfirst"
#guard toString OrderNulls.nullsLast == "nullslast"

#guard toString ({ otTerm := "age" } : OrderTerm) == "age.asc"
#guard toString ({ otTerm := "age", otDirection := .desc, otNulls := some .nullsLast } : OrderTerm) ==
  "age.desc.nullslast"

/-! ### `SelectItem` -/

#guard toString SelectItem.star == "*"
#guard toString (SelectItem.field "name" none none []) == "name"
#guard toString (SelectItem.field "name" (some "n") (some "text") []) == "n:name::text"
#guard toString (SelectItem.computed "count(*)" (some "total")) == "total:count(*)"
#guard toString (SelectItem.spread "address" #[SelectItem.field "city" none none []]) ==
  "...address(city)"
#guard toString (SelectItem.relationship "posts" none none false
    #[SelectItem.field "title" none none []]) == "posts(title)"
#guard toString (SelectItem.relationship "posts" (some "p") (some "fk") true
    #[SelectItem.field "title" none none []]) == "p:posts!fk!inner(title)"

/-! ### `Payload` -/

#guard toString (Payload.jsonPayload "{}") == "JSON(2 chars)"
#guard toString (Payload.urlEncodedPayload [("a", "1")]) == "URLEncoded(1 pairs)"
#guard toString (Payload.rawPayload (ByteArray.mk #[1, 2, 3]) "application/octet-stream") ==
  "Raw(3 bytes, application/octet-stream)"

#guard Payload.jsonPayload "{}" == Payload.jsonPayload "{}"
#guard Payload.jsonPayload "{}" != Payload.jsonPayload "[]"
#guard Payload.jsonPayload "{}" != Payload.urlEncodedPayload []

/-! ### `IsVal` -/

#guard toString IsVal.null_ == "null"
#guard toString IsVal.notNull == "not null"
#guard toString IsVal.unknown_ == "unknown"

/-! ### `Target` -/

#guard toString (Target.table { qiSchema := "public", qiName := "users" }) == "table public.users"
#guard toString (Target.routine { qiSchema := "public", qiName := "add" }) == "routine public.add"

end Tests.PostgREST.ApiRequest.Types
