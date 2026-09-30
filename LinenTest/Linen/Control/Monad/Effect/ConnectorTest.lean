import Linen.Control.Monad.Effect.Connector

open Control.Monad.Effect Control.Monad.Effect.Connector

namespace ConnectorTests

abbrev storage : Capability :=
  { provider := "s3", connection := "reports",
    scopes := [{ operation := "objects.read", root := ["bucket", "reports"] },
               { operation := "objects.write", root := ["bucket", "reports"] }] }

#guard storage.permits "objects.read" ["bucket", "reports", "2026", "invoice.json"]
#guard !storage.permits "objects.read" ["bucket", "reports-private", "invoice.json"]
#guard !storage.permits "objects.delete" ["bucket", "reports", "invoice.json"]
#guard !storage.permits "objects.read" ["another-bucket", "reports", "invoice.json"]

abbrev calendar : Capability :=
  { provider := "google-calendar", connection := "calendar",
    scopes := [{ operation := "events.read", root := ["primary"] },
               { operation := "events.create", root := ["primary"], descendants := false }] }

#guard calendar.permits "events.read" ["primary", "event-1"]
#guard calendar.permits "events.create" ["primary"]
#guard !calendar.permits "events.create" ["primary", "event-1"]
#guard !calendar.permits "events.read" ["team", "event-1"]
#guard !calendar.permits "events.delete" ["primary", "event-1"]

#guard !({ provider := "gmail", connection := "mailbox" } : Capability).permits "messages.send" []
#guard (ScopedResource.check? storage "objects.read" ["bucket", "reports", "x"]).isSome
#guard (ScopedResource.check? storage "objects.read" ["bucket", "reports-evil", "x"]).isNone

def readReport : Eff [Connector storage] Lean.Json :=
  call "objects.read" ["bucket", "reports", "invoice.json"]

example : storage.Narrows storage := Capability.Narrows.refl storage

example (scope : Scope) (h : scope.covers operation resource = true) :
    scope.root.IsPrefix resource := Scope.covers_confined h
example (scope : Scope) (exactScope : scope.descendants = false)
    (h : scope.covers operation resource = true) : scope.root = resource :=
  Scope.covers_exact exactScope h

def authority : Authority := ⟨storage, storage, storage, storage⟩
#guard authority.permits "objects.read" ["bucket", "reports", "invoice.json"]
#guard !(AuthorizedResource.check? authority "objects.delete" ["bucket", "reports", "invoice.json"]).isSome
#guard !(AuthorizedResource.check? { authority with organization := { storage with scopes := [] } }
  "objects.read" ["bucket", "reports", "invoice.json"]).isSome
#guard !(AuthorizedResource.check? { authority with warrant := { storage with connection := "other" } }
  "objects.read" ["bucket", "reports", "invoice.json"]).isSome

example (resource : AuthorizedResource authority "objects.read") :
    storage.permits "objects.read" resource.resource = true := resource.organization_permits

abbrev narrow : Capability :=
  { storage with
    scopes := [{ operation := "objects.read", root := ["bucket", "reports", "2026"], descendants := false }]
    maxRequestBytes := 128
    maxResponseBytes := 1024 }

#guard narrow.narrows storage
#guard (narrow.checkNarrows? storage).isSome
#guard !(storage.checkNarrows? narrow).isSome
#guard !({ narrow with provider := "azure" }).narrows storage
#guard !({ narrow with connection := "other" }).narrows storage
#guard !({ narrow with maxRequestBytes := storage.maxRequestBytes + 1 }).narrows storage
#guard !({ narrow with scopes := [{ operation := "objects.delete", root := [] }] }).narrows storage
#guard !({ narrow with scopes := [{ operation := "objects.read", root := ["bucket", "reports-private"] }] }).narrows storage

example : narrow.Narrows storage := Capability.narrows_sound (by decide)
example (h : narrow.permits op resource = true) : storage.permits op resource = true :=
  (Capability.narrows_sound (child := narrow) (parent := storage) (by decide)).2.2.2.2 op resource h

abbrev drive : Capability :=
  { provider := "gdrive", connection := "drive", scopes :=
      [{ operation := "files.read", root := ["file-1"], descendants := false },
       { operation := "files.create", root := ["folder-1"], descendants := false }] }
#guard drive.permits "files.read" ["file-1"]
#guard !drive.permits "files.share" ["file-1"]
#guard !drive.permits "files.read" ["folder-1", "file-1"]

abbrev email : Capability :=
  { provider := "gmail", connection := "mail", scopes :=
      [{ operation := "messages.read", root := ["me", "inbox"] },
       { operation := "drafts.create", root := ["me", "drafts"], descendants := false }] }
#guard email.permits "messages.read" ["me", "inbox", "message-1"]
#guard !email.permits "messages.send" ["me", "drafts"]
#guard !email.permits "messages.delete" ["me", "inbox", "message-1"]
#guard !email.permits "messages.read" ["other-user", "inbox", "message-1"]

#guard (AuthorizedRequest.check? authority "objects.read" ["bucket", "reports", "x"] (Lean.Json.mkObj [])).isSome
#guard (AuthorizedRequest.check? { authority with warrant := { storage with maxRequestBytes := 1 } }
  "objects.read" ["bucket", "reports", "x"] (Lean.Json.mkObj [])).isNone
#guard (AuthorizedResource.check? authority "objects.read" ["bucket", "reports", "..", "escape"]).isNone
#guard (AuthorizedResource.check? authority "objects.read" ["bucket", "reports", "a/b"]).isNone
#guard (AuthorizedResource.check? authority "objects.read" ["bucket", "reports", "a\\b"]).isNone
#guard !validOperation "objects.*"
#guard !Resource.valid ["\x00"]
#guard !Resource.valid ["a%2fb"]
#guard !Resource.valid ["\x85"]
#guard !Resource.valid [String.ofList (List.replicate 128 'é')]
#guard Resource.valid [String.ofList (List.replicate 127 'é')]
#guard (Capability.parse (Lean.toJson storage)).isOk
#guard (Capability.parse ((Lean.toJson storage).setObjVal! "maxRequestBytes" Lean.Json.null)).toOption.isNone
#guard (Capability.parse ((Lean.toJson storage).setObjVal! "unexpected" (Lean.Json.bool true))).toOption.isNone
def missingDescendants := (Lean.toJson storage).setObjVal! "scopes"
  (Lean.Json.arr #[Lean.Json.mkObj [("operation", "objects.read"), ("root", Lean.toJson (["reports"] : List String))]])
#guard (Capability.parse missingDescendants).toOption.isNone

example (r : AuthorizedRequest authority "objects.read") :
    r.payload.compress.toUTF8.size ≤ storage.maxRequestBytes := r.warrant_bounded

end ConnectorTests
