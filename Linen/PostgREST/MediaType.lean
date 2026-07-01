/-
  PostgREST.MediaType — Content type handling

  Enumerates the media types supported by PostgREST and provides
  parsing/rendering to MIME type strings.  PostgREST supports standard
  media types (JSON, CSV, XML, GeoJSON) plus a custom vendor type
  `application/vnd.pgrst.object` and plan types for `EXPLAIN`.

  ## Haskell source
  - `PostgREST.MediaType` (postgrest package)

  ## Design
  - `MediaType` is a closed inductive so the compiler can verify
    exhaustive matching for content negotiation
  - Each variant maps to exactly one MIME string (proven by `toMime`/`ofMime` roundtrip)
-/

namespace PostgREST.MediaType

-- ────────────────────────────────────────────────────────────────────
-- Plan options (for EXPLAIN output)
-- ────────────────────────────────────────────────────────────────────

/-- Options for the `application/vnd.pgrst.plan` media type. -/
inductive MTVndPlanOption where
  | analyze
  | verbose
  | settings
  | buffers
  | wal
  deriving BEq, Repr, Hashable

instance : ToString MTVndPlanOption where
  toString
    | .analyze => "analyze"
    | .verbose => "verbose"
    | .settings => "settings"
    | .buffers => "buffers"
    | .wal => "wal"

/-- Format for EXPLAIN plan output. -/
inductive MTVndPlanFormat where
  | json
  | text_
  deriving BEq, Repr, Hashable

instance : ToString MTVndPlanFormat where
  toString
    | .json => "json"
    | .text_ => "text"

-- ────────────────────────────────────────────────────────────────────
-- Media type
-- ────────────────────────────────────────────────────────────────────

/-- Media types supported by PostgREST.
    $$\text{MediaType} \in \{\text{JSON}, \text{CSV}, \text{XML}, \ldots\}$$ -/
inductive MediaType where
  /-- `application/json` -/
  | applicationJSON
  /-- `text/csv` -/
  | textCSV
  /-- `text/plain` -/
  | textPlain
  /-- `text/xml` -/
  | textXML
  /-- `application/octet-stream` -/
  | applicationOctetStream
  /-- `application/geo+json` -/
  | applicationGeoJSON
  /-- `application/openapi+json` — OpenAPI specification -/
  | applicationOpenAPI
  /-- `application/vnd.pgrst.object+json` — single JSON object (not array) -/
  | applicationVndSingularJSON
  /-- `application/vnd.pgrst.object` — singular object -/
  | applicationVndObject
  /-- `application/vnd.pgrst.plan` — EXPLAIN plan output -/
  | applicationVndPlan (format : MTVndPlanFormat) (options : List MTVndPlanOption)
  /-- A media type specified via custom media handler in PostgreSQL -/
  | other (rawMime : String)
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- MIME string conversion
-- ────────────────────────────────────────────────────────────────────

/-- Convert a media type to its MIME string representation.
    $$\text{toMime} : \text{MediaType} \to \text{String}$$ -/
def MediaType.toMime : MediaType → String
  | .applicationJSON => "application/json"
  | .textCSV => "text/csv"
  | .textPlain => "text/plain"
  | .textXML => "text/xml"
  | .applicationOctetStream => "application/octet-stream"
  | .applicationGeoJSON => "application/geo+json"
  | .applicationOpenAPI => "application/openapi+json"
  | .applicationVndSingularJSON => "application/vnd.pgrst.object+json"
  | .applicationVndObject => "application/vnd.pgrst.object"
  | .applicationVndPlan _ _ => "application/vnd.pgrst.plan"
  | .other raw => raw

instance : BEq MediaType where
  beq a b := a.toMime == b.toMime

/-- Convert a media type to the HTTP Content-Type header value
    (includes charset for text types). -/
def MediaType.toContentType : MediaType → String
  | .applicationJSON => "application/json; charset=utf-8"
  | .textCSV => "text/csv; charset=utf-8"
  | .textPlain => "text/plain; charset=utf-8"
  | .textXML => "text/xml; charset=utf-8"
  | .applicationGeoJSON => "application/geo+json; charset=utf-8"
  | .applicationOpenAPI => "application/openapi+json; charset=utf-8"
  | .applicationVndSingularJSON => "application/vnd.pgrst.object+json; charset=utf-8"
  | .applicationVndObject => "application/vnd.pgrst.object; charset=utf-8"
  | .applicationOctetStream => "application/octet-stream"
  | mt => mt.toMime

instance : ToString MediaType where
  toString := MediaType.toMime

/-- Parse a MIME string into a media type (best-effort). -/
def MediaType.ofMime (s : String) : MediaType :=
  let base := (s.splitOn ";").head!.trimAscii.toString
  match base with
  | "application/json" => .applicationJSON
  | "text/csv" => .textCSV
  | "text/plain" => .textPlain
  | "text/xml" => .textXML
  | "application/octet-stream" => .applicationOctetStream
  | "application/geo+json" => .applicationGeoJSON
  | "application/openapi+json" => .applicationOpenAPI
  | "application/vnd.pgrst.object+json" => .applicationVndSingularJSON
  | "application/vnd.pgrst.object" => .applicationVndObject
  | "application/vnd.pgrst.plan" => .applicationVndPlan .json []
  | raw => .other raw

instance : Inhabited MediaType := ⟨.applicationJSON⟩

/-- Is this a JSON-like media type? -/
def MediaType.isJSON : MediaType → Bool
  | .applicationJSON | .applicationGeoJSON | .applicationOpenAPI
  | .applicationVndSingularJSON | .applicationVndObject => true
  | .applicationVndPlan .json _ => true
  | _ => false

/-- Is this a text-based media type (that should include charset)? -/
def MediaType.isText : MediaType → Bool
  | .textCSV | .textPlain | .textXML => true
  | _ => false

-- ────────────────────────────────────────────────────────────────────
-- Roundtrip theorems
-- ────────────────────────────────────────────────────────────────────

/-- A "standard" media type is any variant that is not `.other` and not
    `.applicationVndPlan` (which loses format/options in the MIME string). -/
inductive MediaType.IsStandard : MediaType → Prop where
  | applicationJSON : IsStandard .applicationJSON
  | textCSV : IsStandard .textCSV
  | textPlain : IsStandard .textPlain
  | textXML : IsStandard .textXML
  | applicationOctetStream : IsStandard .applicationOctetStream
  | applicationGeoJSON : IsStandard .applicationGeoJSON
  | applicationOpenAPI : IsStandard .applicationOpenAPI
  | applicationVndSingularJSON : IsStandard .applicationVndSingularJSON
  | applicationVndObject : IsStandard .applicationVndObject

/-- Roundtrip verification for standard media types via `BEq`:
    `(ofMime (toMime mt)) == mt = true` for all standard types.
    Uses `BEq` (MIME-string comparison) which is the canonical equality on MediaType.

    Each theorem below witnesses that `ofMime` correctly parses back the
    MIME string produced by `toMime` for the corresponding standard variant.
    $$\forall \text{standard}\ mt,\; \text{ofMime}(\text{toMime}(mt)) == mt = \text{true}$$ -/

theorem MediaType.roundtrip_applicationJSON :
    BEq.beq (MediaType.ofMime MediaType.applicationJSON.toMime) MediaType.applicationJSON = true := by native_decide

theorem MediaType.roundtrip_textCSV :
    BEq.beq (MediaType.ofMime MediaType.textCSV.toMime) MediaType.textCSV = true := by native_decide

theorem MediaType.roundtrip_textPlain :
    BEq.beq (MediaType.ofMime MediaType.textPlain.toMime) MediaType.textPlain = true := by native_decide

theorem MediaType.roundtrip_textXML :
    BEq.beq (MediaType.ofMime MediaType.textXML.toMime) MediaType.textXML = true := by native_decide

theorem MediaType.roundtrip_applicationOctetStream :
    BEq.beq (MediaType.ofMime MediaType.applicationOctetStream.toMime) MediaType.applicationOctetStream = true := by native_decide

theorem MediaType.roundtrip_applicationGeoJSON :
    BEq.beq (MediaType.ofMime MediaType.applicationGeoJSON.toMime) MediaType.applicationGeoJSON = true := by native_decide

theorem MediaType.roundtrip_applicationOpenAPI :
    BEq.beq (MediaType.ofMime MediaType.applicationOpenAPI.toMime) MediaType.applicationOpenAPI = true := by native_decide

theorem MediaType.roundtrip_applicationVndSingularJSON :
    BEq.beq (MediaType.ofMime MediaType.applicationVndSingularJSON.toMime) MediaType.applicationVndSingularJSON = true := by native_decide

theorem MediaType.roundtrip_applicationVndObject :
    BEq.beq (MediaType.ofMime MediaType.applicationVndObject.toMime) MediaType.applicationVndObject = true := by native_decide

end PostgREST.MediaType
