#!/usr/bin/env python3
import os, re, heapq
from collections import defaultdict

ROOT = "/Users/nicolas.grislain/Typednotes/hale"
SRC = os.path.join(ROOT, "Hale")
OUT_DIR = "/Users/nicolas.grislain/Typednotes/linen/docs"

# Modules whose dependency closure should be pulled as early as possible in the
# topological order, so we can implement them with the least prerequisite work.
PRIORITIZE = [
    "Hale.Network.Network.Socket.EventDispatcher",
]

# Modules that need no further work — either ported into `linen` (Linen/), or
# covered entirely by the Lean standard library so nothing is re-declared (per
# the import rules, e.g. `Either` → `Sum`/`Except`). Emitted first (commented
# out) so the active TODO list starts right after them. Keep in sync.
DONE = {
    "Hale.Aeson.Data.Aeson.Types",
    "Hale.Aeson.Data.Aeson.Decode",
    "Hale.Aeson.Data.Aeson.Encode",
    "Hale.Aeson.Data.Aeson",
    "Hale.Aeson",
    "Hale.AnsiTerminal.System.Console.ANSI",
    "Hale.AnsiTerminal",
    "Hale.AutoUpdate.Control.AutoUpdate",
    "Hale.AutoUpdate",
    "Hale.Base.Control.Applicative",
    "Hale.Base.Control.Category",
    "Hale.Base.Control.Monad",
    "Hale.Base.Control.Arrow",
    "Hale.Base.Control.Exception",
    "Hale.Base.Data.Bifunctor",
    "Hale.Base.Data.Bits",
    "Hale.Base.Data.Bool",
    "Hale.Base.Data.Char",
    "Hale.Base.Data.Complex",
    "Hale.Base.Data.Function",
    "Hale.Base.Data.Functor.Compose",
    "Hale.Base.Data.Functor.Const",
    "Hale.Base.Data.Functor.Contravariant",
    "Hale.Base.Data.Functor.Identity",   # = Lean `Id`; no file
    "Hale.Base.Data.Functor.Product",
    "Hale.Base.Data.Functor.Sum",
    "Hale.Base.Data.IORef",   # = stdlib `IO.Ref` (mkRef/get/set/modify/modifyGet); no file
    "Hale.Base.Data.Ix",
    "Hale.Base.Data.List.NonEmpty",
    "Hale.Base.Data.Foldable",
    "Hale.Base.Data.List",
    "Hale.Base.Data.Maybe",   # = stdlib `Option`/`List` (elim/getD/get/filterMap/head?/toList); no file
    "Hale.Base.Data.Newtype",
    "Hale.Base.Data.Ord",
    "Hale.Base.Data.Proxy",
    "Hale.Base.Data.Ratio",   # = stdlib `Rat` (Field, mkRat, floor/ceil/abs); only `Rat.round` added
    "Hale.Base.Data.Either",   # covered by stdlib `Sum`/`Except` (+ `List.partitionMap`); no file
    "Hale.Base.Control.Concurrent.MVar",
    "Hale.Base.Control.Concurrent.Chan",
    "Hale.Base.Control.Concurrent.QSem",
    "Hale.Base.Control.Concurrent.QSemN",
    "Hale.Base.Control.Concurrent.Green",
    "Hale.Base.Control.Concurrent.Scheduler",
    "Hale.Base.Control.Concurrent",
    "Hale.Network.Network.Socket.Types",
    "Hale.Network.Network.Socket.FFI",
    "Hale.Network.Network.Socket",
    "Hale.Network.Network.Socket.EventDispatcher",
    "Hale.Base.Data.Fixed",
    "Hale.Base.Data.String",
    "Hale.Base.Data.Traversable",
    "Hale.Base.Data.Tuple",   # covered: core `Prod.swap`/`Function.curry`/`uncurry` + `Data.Bifunctor` (`bimap`/`mapFst`/`mapSnd`); no file
    "Hale.Base.Data.Unique",
    "Hale.Base.Data.Void",
    "Hale.Base.System.Environment",   # covered: core `IO.getEnv` (= lookupEnv) + `Option.getD`; getHome/getPath are `IO.getEnv "HOME"`/`"PATH"`; no file
    "Hale.Base.System.Exit",
    "Hale.Base.System.IO",   # covered: IOMode=`IO.FS.Mode`, Handle=`IO.FS.Handle`, h*/withFile/putStr*/getLine all core (`IO.FS.Handle.*`, `IO.FS.withFile`, `IO.print`/`println`, `IO.getStd*`); no file
    "Hale.Base",   # aggregator: re-exports all `Base.*`; linen's root `Linen.lean` already plays this role; no file
    "Hale.Base64.Data.ByteString.Base64",   # ported to Linen/Data/Base64.lean (over core `ByteArray`; no `ByteString` slice needed)
    "Hale.Base64",   # aggregator: re-exports the Base64 module; covered by linen's root; no file
    "Hale.BsbHttpChunked.Network.HTTP.Chunked",   # ported to Linen/Network/HTTP/Chunked.lean (over core `ByteArray`; hex via `Nat.toDigits`)
    "Hale.BsbHttpChunked",   # aggregator: re-exports the Chunked module; covered by linen's root; no file
    "Hale.ByteString.Data.ByteString.Internal",   # ported to Linen/Data/ByteString.lean (slice over `ByteArray`; idiomatic single module, no Internal/public split)
    "Hale.ByteString.Data.ByteString",   # public re-export of Internal; merged into Linen/Data/ByteString.lean; no file
    "Hale.ByteString.Data.ByteString.Char8",   # ported to Linen/Data/ByteString/Char8.lean (lines/words rewritten structural, no fuel)
    "Hale.ByteString.Data.ByteString.Lazy.Internal",   # ported to Linen/Data/ByteString/Lazy.lean (Thunk-chunked; structural recursion through Thunk)
    "Hale.ByteString.Data.ByteString.Lazy",   # public re-export of Lazy.Internal; merged into Linen/Data/ByteString/Lazy.lean; no file
    "Hale.ByteString.Data.ByteString.Lazy.Char8",   # ported to Linen/Data/ByteString/Lazy/Char8.lean (thin Char view over LazyByteString)
    "Hale.ByteString.Data.ByteString.Short",   # ported to Linen/Data/ByteString/Short.lean (ByteArray newtype; bogus self-ToString instance fixed)
    "Hale.ByteString.Data.ByteString.Builder",   # ported to Linen/Data/ByteString/Builder.lean (diff-list; wordHex via `Nat.toDigits`, no fuel)
    "Hale.ByteString",   # aggregator: re-exports all `ByteString.*`; covered by linen's root; no file
    "Hale.CaseInsensitive.Data.CaseInsensitive",   # ported to Linen/Data/CaseInsensitive.lean (FoldCase class + CI wrapper)
    "Hale.CaseInsensitive",   # aggregator: re-exports the CaseInsensitive module; covered by linen's root; no file
    "Hale.Conduit.Data.Conduit.Internal.Pipe",   # ported to Linen/Data/Conduit/Internal/Pipe.lean — SOUND (no unsafe): Freer pipeM (strictly positive) + strict spine; total Functor/Monad
    "Hale.Conduit.Data.Conduit.Internal.Conduit",   # ported to Linen/Data/Conduit/Internal/Conduit.lean (namespace Data.Conduit): ConduitT (CPS codensity over Pipe) + Functor/Pure/Bind/Monad/MonadLift, await/yield/leftoverC/liftConduit/awaitForever, fusePipes/`.|`, runPipe/runConduit/runConduitPure/runConduitRes, bracketP; `unsafe` (awaitForever recurses on a runtime `none` with no structural/well-founded measure, same corecursion Hale accepts via laziness) — adapted from Hale's Thunk-wrapped single-field pipeM to Linen's strict two-field pipeM Pipe
    "Hale.Conduit.Data.Conduit.Combinators",   # ported to Linen/Data/Conduit/Combinators.lean (namespace Data.Conduit.Combinators): sourceList/sourceArray/unfoldC/repeatC/replicateC/enumFromToC/repeatMC, sinkList/sinkArray/sinkNull/foldlC/foldMC/foldMapC/headC/lastC/lengthC/sumC/productC/nullC/allC/anyC/elemC/findC/maximumC/minimumC, mapC/mapMC/filterC/filterMC/takeC/dropC/takeWhileC/dropWhileC/concatMapC/mapMaybeC/intersperseC/scanlC/iterMC/mapM_C/concatC/chunksOfC; `unsafe` (built directly on the `unsafe` ConduitT primitives); body verbatim from Hale, no Thunk/pipeM adaptation needed since it never touches raw Pipe constructors
    "Hale.Conduit.Data.Conduit",   # aggregator: re-exports Pipe/Internal.Conduit/Combinators; covered by linen's root; no file
    "Hale.Conduit",   # aggregator: re-exports Data.Conduit; covered by linen's root; no file
    "Hale.ConfiguratorPg.Data.Configurator.Types",   # ported to Linen/Data/Configurator/Types.lean (Value.toString made structural, no partial)
    "Hale.ConfiguratorPg.Data.Configurator",   # ported to Linen/Data/Configurator.lean (parsers rewritten structural, no Id.run/while; dead isSpace dropped)
    "Hale.ConfiguratorPg",   # aggregator: re-exports Data.Configurator; covered by linen's root; no file
    "Hale.Containers.Data.IntMap",   # ported to Linen/Data/IntMap.lean (Data.IntMap API over Std.HashMap Nat; already stdlib-backed in Hale)
    "Hale.Containers.Data.Map",   # ported to Linen/Data/Map.lean (Data.Map API over Lean.RBMap; lookupMin/Max simplified to RBMap.min/max)
    "Hale.Containers.Data.Map.Strict",   # re-export of Data.Map (Lean is strict — no lazy/strict distinction); covered by Linen/Data/Map.lean; no file
    "Hale.Containers.Data.Set",   # ported to Linen/Data/Set.lean (Data.Set `Set'` over Lean.RBMap _ Unit; findMin/Max simplified)
    "Hale.Containers",   # aggregator: re-exports Map/Map.Strict/Set/IntMap; covered by linen's root; no file
    "Hale.Cookie.Web.Cookie",   # ported to Linen/Web/Cookie.lean (parsers/renderers rewritten pure, no Id.run/for/mut)
    "Hale.Cookie",   # aggregator: re-exports Web.Cookie; covered by linen's root; no file
    "Hale.DataDefault.Data.Default",   # ported to Linen/Data/Default.lean (Default class; kept distinct from Inhabited)
    "Hale.DataDefault",   # aggregator: re-exports Data.Default; covered by linen's root; no file
    "Hale.DataFrame.DataFrame.Internal.Types",   # ported to Linen/DataFrame/Internal/Types.lean (proof-carrying DataFrame; getRowSafe rewritten as well-founded rowAux, no fuel)
    "Hale.DataFrame.DataFrame.IO.CSV",   # ported to Linen/DataFrame/IO/CSV.lean (for-loop state machine over finite list; sound, no while/partial/fuel)
    "Hale.DataFrame.DataFrame.Internal.Column",   # ported to Linen/DataFrame/Internal/Column.lean (filterByMask/unique rewritten pure via zip/filterMap/foldl)
    "Hale.DataFrame.DataFrame.Display",   # ported to Linen/DataFrame/Display.lean (Id.run for-loops rewritten pure via map/flatMap; unused padLeft dropped)
    "Hale.DataFrame.DataFrame.Operations.Join",   # ported to Linen/DataFrame/Operations/Join.lean (nested-loop join kept imperative over finite ranges; alignment proof via map_column_aligned)
    "Hale.DataFrame.DataFrame.Operations.Sort",   # ported to Linen/DataFrame/Operations/Sort.lean (pure; List.mergeSort over row-index permutation; needs import Init.Data.List.Sort.Lemmas)
    "Hale.DataFrame.DataFrame.Operations.Statistics",   # ported to Linen/DataFrame/Operations/Statistics.lean (Id.run accumulators rewritten pure via filterMap/foldl)
    "Hale.DataFrame.DataFrame.Operations.Aggregation",   # ported to Linen/DataFrame/Operations/Aggregation.lean (groupBy rewritten pure foldl+map; aggregate append-alignment proof)
    "Hale.DataFrame.DataFrame.Operations.Subset",   # ported to Linen/DataFrame/Operations/Subset.lean (mask builders rewritten pure filter/map; filter/extract/rename alignment proofs)
    "Hale.DataFrame.DataFrame.Operations.Transform",   # ported to Linen/DataFrame/Operations/Transform.lean (info rewritten pure; push/map/filter alignment proofs)
    "Hale.DataFrame.DataFrame",   # umbrella: re-exports all DataFrame.* sub-modules; covered by linen's root; no file
    "Hale.DataFrame",   # outer aggregator: re-exports Hale.DataFrame.DataFrame; covered by linen's root; no file
    "Hale.FastLogger.System.Log.FastLogger",   # ported to Linen/System/Log/FastLogger.lean (dropped vestigial AutoUpdate import; withTimedFastLogger→withFastLogger as it does no timing)
    "Hale.FastLogger",   # aggregator: re-exports System.Log.FastLogger; covered by linen's root; no file
    "Hale.Hasql.Database.PostgreSQL.LibPQ.Types",   # ported to Linen/Database/PostgreSQL/LibPQ/Types.lean (pure: opaque handles + status enums; verbatim, FFI in next module)
    "Hale.Hasql.Database.PostgreSQL.LibPQ",   # ported to Linen/Database/PostgreSQL/LibPQ.lean + ffi/postgres.c (linen_pg_* libpq bindings; libpq via pkg-config in lakefile, CI installs libpq-dev)
    "Hale.Hasql.Hasql.Connection",   # ported to Linen/Database/SQL/Connection.lean (namespace SQL not Hasql, per request): Settings/acquire/release/withConnection
    "Hale.Hasql.Hasql.Encoders",   # ported to Linen/Database/SQL/Encoders.lean (namespace SQL not Hasql): pure Params encoders + width laws
    "Hale.Hasql.Hasql.Session",   # ported to Linen/Database/SQL/Session.lean (namespace SQL): Session = ReaderT/ExceptT IO stack (stdlib instances, dropped bespoke Monad), sql/query/transaction/run
    "Hale.Hasql.Hasql.Decoders",   # ported to Linen/Database/SQL/Decoders.lean (namespace SQL): Value/Row/Result decoders + row width laws (kept hand-rolled parseFloat?, no stdlib String.toFloat?)
    "Hale.Hasql.Hasql.Pool",   # ported to Linen/Database/SQL/Pool.lean (namespace SQL): IO.Ref-backed connection pool, PoolSettings proofs, create/use/destroy/stats
    "Hale.Hasql.Hasql.Statement",   # ported to Linen/Database/SQL/Statement.lean (namespace SQL): Statement = Encoders.Params + Decoders.Result (dropped dup Encoder/Decoder aliases), run/command/sql_/mapResult/contramapParams
    "Hale.Hasql",   # aggregator: re-exports the whole libpq + SQL tier (LibPQ.Types/LibPQ + SQL.Connection/Session/Encoders/Decoders/Pool/Statement); covered by linen's root; no file
    "Hale.Http2.Network.HTTP2.Frame.Types",   # ported to Linen/Network/HTTP2/Frame/Types.lean (namespace Network.HTTP2): RFC 9113 framing types + total conversions/proofs (fixed Nat.toDigits arg order in ToString)
    "Hale.Http2.Network.HTTP2.Frame.Decode",   # ported to Linen/Network/HTTP2/Frame/Decode.lean: wire decoders (replaced fuel-recursion in decodeSettingsPayload with List.range.mapM over Option)
    "Hale.Http2.Network.HTTP2.Frame.Encode",   # ported to Linen/Network/HTTP2/Frame/Encode.lean: wire encoders + frame builders (replaced fuel-recursion in splitHeaderBlock with ceil-division List.range.map)
    "Hale.Http2.Network.HTTP2.HPACK.Huffman",   # ported to Linen/Network/HTTP2/HPACK/Huffman.lean: UPGRADED from Hale's pass-through stub to a full RFC 7541 App. B Huffman codec (257-entry table from the RFC, trie decode + padding validation, RFC-vector tested) — generic Hackage `huffman` pkg doesn't fit (frequency-based, not the fixed HPACK code)
    "Hale.Http2.Network.HTTP2.HPACK.Table",   # ported to Linen/Network/HTTP2/HPACK/Table.lean: 61-entry static table + DynamicTable FIFO (replaced fuel-recursion in evict with a stop-flag foldl keeping the longest size-fitting prefix)
    "Hale.Http2.Network.HTTP2.HPACK.Decode",   # ported to Linen/Network/HTTP2/HPACK/Decode.lean: decodeInteger (fuel→bounded foldl over List.range 10) + decodeString + decodeHeaders (fuel→well-founded on bs.size-offset via decodeInteger_consumed lemma); RFC App. C vectors
    "Hale.Http2.Network.HTTP2.HPACK.Encode",   # ported to Linen/Network/HTTP2/HPACK/Encode.lean: encodeInteger (fuel→well-founded on v, v/128<v) + encodeString + encodeHeaderRep/encodeHeaders; encode↔decode round-trip tested (dropped unused Huffman import)
    "Hale.Http2.Network.HTTP2.Types",   # ported to Linen/Network/HTTP2/Types.lean (namespace Network.HTTP2): ConnectionError/StreamError, HeaderBlockState (CONTINUATION assembly), HTTP2Result map/bind; pure, verbatim
    "Hale.Http2.Network.HTTP2.Stream",   # ported to Linen/Network/HTTP2/Stream.lean (namespace Network.HTTP2): StreamState machine + StreamTable over Std.HashMap (openClientStream/updateState/updatePriority/activeStreamCount); pure, verbatim (Lean accepts `open` constructor)
    "Hale.Http2.Network.HTTP2.FlowControl",   # ported to Linen/Network/HTTP2/FlowControl.lean (namespace Network.HTTP2): FlowWindow/ConnectionFlowControl + stream window updates; FIXED adjust to use signed Int subtraction (Hale used Nat sub which truncated a negative settings delta to 0)
    "Hale.Http2.Network.HTTP2.Server",   # ported to Linen/Network/HTTP2/Server.lean (namespace Network.HTTP2): IO connection handler (preface/SETTINGS/PING/WINDOW_UPDATE/GOAWAY/HEADERS+CONTINUATION/HPACK); removed loopFuel & attempts fuel counters (while-loops driven by done/EOF/remaining, matching EventDispatcher/AutoUpdate idiom)
    "Hale.Http2",   # aggregator: re-exports the whole HTTP/2 tier (Frame.*, HPACK.*, Types, Stream, FlowControl, Server); covered by linen's root; no file
    "Hale.Http3.Network.HTTP3.Error",   # ported to Linen/Network/HTTP3/Error.lean (namespace Network.HTTP3): H3Error enum + toCode/fromCode + 17 roundtrip theorems; pure, verbatim
    "Hale.Http3.Network.HTTP3.Frame",   # ported to Linen/Network/HTTP3/Frame.lean (namespace Network.HTTP3): FrameType + QUIC varint codec + Frame/H3Settings; replaced fuel-recursion in decodeSettingsPairs with well-founded (decodeVarInt_consumed lemma)
    "Hale.Http3.Network.HTTP3.QPACK.Table",   # ported to Linen/Network/HTTP3/QPACK/Table.lean (namespace Network.HTTP3.QPACK): 99-entry RFC 9204 App. A static table + staticLookup/staticFind; pure, verbatim
    "Hale.Http3.Network.HTTP3.QPACK.Decode",   # ported to Linen/Network/HTTP3/QPACK/Decode.lean: decodeQInt (fuel→bounded foldl) + decodeHeaderEntries (fuel→well-founded via decodeQInt_consumed); changed fromUTF8! to fromUTF8? (none on bad UTF-8 instead of panic)
    "Hale.Http3.Network.HTTP3.QPACK.Encode",   # ported to Linen/Network/HTTP3/QPACK/Encode.lean: encodeQInt (replaced Id.run while-loop with well-founded encodeQIntCont, v/128<v) + encodeStringLiteral/encodeHeaders; encode↔decode round-trip tested
    "Hale.HttpDate.Network.HTTP.Date",   # ported to Linen/Network/HTTP/Date.lean (namespace Network.HTTP.Date): HTTPDate + parseHTTPDate (IMF-fixdate/asctime) + formatHTTPDate (Zeller dow); pure, verbatim
    "Hale.HttpDate",   # aggregator: re-exports Network.HTTP.Date; covered by linen's root; no file
    "Hale.HttpTypes.Network.HTTP.Types.Header",   # ported to Linen/Network/HTTP/Types/Header.lean (namespace Network.HTTP.Types): HeaderName=CI String + ~50 standard header constants; pure, verbatim (import Hale.CaseInsensitive→Linen.Data.CaseInsensitive)
    "Hale.HttpTypes.Network.HTTP.Types.Method",   # ported to Linen/Network/HTTP/Types/Method.lean (namespace Network.HTTP.Types): StdMethod/Method + parseMethod/renderMethod + RFC 9110 isSafe/isIdempotent + laws; pure, verbatim
    "Hale.HttpTypes.Network.HTTP.Types.Status",   # ported to Linen/Network/HTTP/Types/Status.lean (namespace Network.HTTP.Types): proof-carrying Status (100-999) + ~50 codes/aliases + class predicates + RFC 9110 mustNotHaveBody + theorems; pure, verbatim
    "Hale.HttpTypes.Network.HTTP.Types.URI",   # ported to Linen/Network/HTTP/Types/URI.lean (namespace Network.HTTP.Types): parseQuery/renderQuery + urlEncode/urlDecode (urlDecode is structural recursion over chars, no fuel); pure, verbatim
    "Hale.HttpTypes.Network.HTTP.Types.Version",   # ported to Linen/Network/HTTP/Types/Version.lean (namespace Network.HTTP.Types): HttpVersion + lex Ord + http09/10/11/20 + theorems; pure, verbatim
    "Hale.HttpTypes",   # aggregator: re-exports Network.HTTP.Types.{Header,Method,Status,URI,Version}; covered by linen's root; no file
    "Hale.HttpClient.Network.HTTP.Client.Types",   # ported to Linen/Network/HTTP/Client/Types.lean (namespace Network.HTTP.Client): Connection/Request/Response + Response accessors; import Hale.HttpTypes→specific Types.{Method,Status,Header} (avoided Types.Version to prevent HttpVersion clash); pure, verbatim
    "Hale.HttpClient.Network.HTTP.Client.Request",   # ported to Linen/Network/HTTP/Client/Request.lean (namespace Network.HTTP.Client): serializeRequest (auto Host/Content-Length/Connection, structural for-loop) + sendRequest; pure, verbatim
    "Hale.HttpClient.Network.HTTP.Client.Response",   # ported to Linen/Network/HTTP/Client/Response.lean: converted 5 partial-def network loops to while-loops; findCRLF/findCharIdx → stdlib find?/findIdx?; FIXED chunked-decode bug (readExactly truncated & dropped buffered following chunks → added non-truncating fillTo); #eval IO tests via mock Connection
    "Hale.HttpClient.Network.HTTP.Client.Connection",   # ported to Linen/Network/HTTP/Client/Connection.lean: connectPlain/connectTLS/connect over Data.Streaming.Network + Network.TLS + Network.Socket.Blocking; defaultPort; IO establishers pinned by type in tests (real network IO)
    "Hale.HttpClient.Network.HTTP.Client.Redirect",   # ported to Linen/Network/HTTP/Client/Redirect.lean: executeWithRedirects/execute (301/302/303→GET+drop body, 307/308 preserve) + parseLocation URL split; maxRedirects is a genuine caller-specified budget (not a fuel dodge), decreasing structurally n+1→n; verbatim
    "Hale.HttpClient",   # aggregator: re-exports Network.HTTP.Client.{Types,Connection,Request,Response,Redirect}; covered by linen's root; no file
    "Hale.HttpConduit.Network.HTTP.Client.Conduit",   # ported to Linen/Network/HTTP/Client/Conduit.lean (namespace Network.HTTP.Client.Conduit): httpSource/httpSink build on the already-`unsafe` ConduitT layer (no new unboundedness introduced); withResponse is plain IO; IO-performing defs pinned by type in tests (real network IO), verbatim
    "Hale.HttpConduit.Network.HTTP.Simple",   # ported to Linen/Network/HTTP/Simple.lean (namespace Network.HTTP.Simple): parseUrl/parseUrl!/simpleHttp/httpBS/httpLbs; parseUrl is pure and #guard-checked field-by-field (Request has no BEq); verbatim
    "Hale.HttpConduit",   # aggregator: re-exports Network.HTTP.Client.Conduit + Network.HTTP.Simple; covered by linen's root; no file
    "Hale.Req.Network.HTTP.Req",   # ported to Linen/Network/HTTP/Req.lean (namespace Network.HTTP.Req): type-safe req client — Scheme-phantom Url/ReqOption (HTTPS-only basicAuth/oAuth2Bearer/oAuth2Token via `ReqOption .Https`), HttpMethod/HttpBody/HttpBodyAllowed compile-time method-body constraint (no instance for NoBody/YesBody), HttpResponse (IgnoreResponse/BsResponse), Req monad (ReaderT HttpConfig over IO), req/runReq built on the already-ported Client.Connection/Redirect; the docstring's illustrative `req GET ...` example needed `GET.mk` (GET is the phantom method *type*, not a term) to match Hale's own upstream, which likewise never compiles a real req call in its test suite (network roundtrip only) — Tests mirror that: compile-time proofs + typeclass instance #guards are exercised, the req/runReq roundtrip itself is pinned by type only; `attribute [reducible] HttpMethod.allowsBody HttpBody.providesBody` added so `HttpBodyAllowed`'s instance search — keyed on the *reduced* CanHaveBody value — can see past these stuck projections (discrimination-tree indexing only unfolds reducible defs when matching keys), which is what makes `req GET.mk ...`/`req POST.mk ...` actually elaborate now (exercised directly in ReqTest); verbatim otherwise
    "Hale.Req",   # aggregator: re-exports Network.HTTP.Req; covered by linen's root; no file
    "Hale.WAI.Network.Wai.Internal",   # ported to Linen/Network/WebApp/Internal.lean (namespace Network.WebApp): renamed WAI → WebApp (idiomatic Lean naming, not a Haskell-specific acronym); Request/Response/ResponseReceived/AppM indexed-monad-over-Green exactly-once-response encoding; verbatim otherwise
    "Hale.WAI.Network.Wai",   # ported to Linen/Network/WebApp.lean (namespace Network.WebApp): responseLBS/requestHeader/mapRequestHeaders/idMiddleware/composeMiddleware/addHeader/modifyRequest/modifyResponse/ifRequest + algebraic-law theorems; Hale's `partial def strictRequestBody` rewritten as a plain `def` using a `while` loop over local mutable state (same idiom as Client.Response's body readers) instead of explicit unbounded recursion — no termination proof needed, no partial; verbatim otherwise
    "Hale.WAI",   # aggregator: re-exports Network.Wai.Internal + Network.Wai; renamed WAI → WebApp; covered by linen's root (Network.WebApp.Internal + Network.WebApp); no file
    "Hale.WaiAppStatic.WaiAppStatic.Types",   # ported to Linen/Network/WebApp/Static/Types.lean (namespace Network.WebApp.Static): renamed WaiAppStatic → WebApp.Static; Piece (proof-carrying no-dot/no-slash path segment) + toPiece/unsafeToPiece/toPieces, MaxAge, File, LookupResult, StaticSettings; verbatim otherwise
    "Hale.WaiAppStatic.WaiAppStatic.Storage.Filesystem",   # ported to Linen/Network/WebApp/Static/Storage/Filesystem.lean (namespace Network.WebApp.Static.Storage): defaultFileServerSettings over System.FilePath.metadata; verbatim
    "Hale.WaiAppStatic.Network.Wai.Application.Static",   # ported to Linen/Network/WebApp/Static/Application.lean (namespace Network.WebApp.Static): staticApp/static; Hale's `private partial def tryIndices` is already structurally recursive on the shrinking index-name list, so it ports as a plain pattern-matching `def` — no partial needed; verbatim otherwise
    "Hale.WaiAppStatic",   # aggregator: re-exports WaiAppStatic.Types + .Storage.Filesystem + Network.Wai.Application.Static; renamed WaiAppStatic → WebApp.Static; covered by linen's root; no file
    "Hale.WaiExtra.Network.Wai.Header",   # ported to Linen/Network/WebApp/Extra/Header.lean (namespace Network.WebApp.Extra); verbatim
    "Hale.WaiExtra.Network.Wai.Request",   # ported to Linen/Network/WebApp/Extra/Request.lean (namespace Network.WebApp.Extra); verbatim
    "Hale.WaiExtra.Network.Wai.UrlMap",   # ported to Linen/Network/WebApp/Extra/UrlMap.lean (namespace Network.WebApp.Extra); verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.AcceptOverride",   # ported to Linen/Network/WebApp/Extra/Middleware/AcceptOverride.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.AddHeaders",   # ported to Linen/Network/WebApp/Extra/Middleware/AddHeaders.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Autohead",   # ported to Linen/Network/WebApp/Extra/Middleware/Autohead.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.CleanPath",   # ported to Linen/Network/WebApp/Extra/Middleware/CleanPath.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.CombineHeaders",   # ported to Linen/Network/WebApp/Extra/Middleware/CombineHeaders.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.ForceDomain",   # ported to Linen/Network/WebApp/Extra/Middleware/ForceDomain.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.ForceSSL",   # ported to Linen/Network/WebApp/Extra/Middleware/ForceSSL.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.HealthCheckEndpoint",   # ported to Linen/Network/WebApp/Extra/Middleware/HealthCheckEndpoint.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Local",   # ported to Linen/Network/WebApp/Extra/Middleware/Local.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.MethodOverride",   # ported to Linen/Network/WebApp/Extra/Middleware/MethodOverride.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.MethodOverridePost",   # ported to Linen/Network/WebApp/Extra/Middleware/MethodOverridePost.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Rewrite",   # ported to Linen/Network/WebApp/Extra/Middleware/Rewrite.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Routed",   # ported to Linen/Network/WebApp/Extra/Middleware/Routed.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Select",   # ported to Linen/Network/WebApp/Extra/Middleware/Select.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.StreamFile",   # ported to Linen/Network/WebApp/Extra/Middleware/StreamFile.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.StripHeaders",   # ported to Linen/Network/WebApp/Extra/Middleware/StripHeaders.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Timeout",   # ported to Linen/Network/WebApp/Extra/Middleware/Timeout.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.ValidateHeaders",   # ported to Linen/Network/WebApp/Extra/Middleware/ValidateHeaders.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Vhost",   # ported to Linen/Network/WebApp/Extra/Middleware/Vhost.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.HttpAuth",   # ported to Linen/Network/WebApp/Extra/Middleware/HttpAuth.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.RequestSizeLimit",   # ported to Linen/Network/WebApp/Extra/Middleware/RequestSizeLimit.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.RequestSizeLimit.Internal",   # ported to Linen/Network/WebApp/Extra/Middleware/RequestSizeLimit/Internal.lean; re-exports RequestSizeLimit, no new content
    "Hale.WaiExtra.Network.Wai.Middleware.RealIp",   # ported to Linen/Network/WebApp/Extra/Middleware/RealIp.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Jsonp",   # ported to Linen/Network/WebApp/Extra/Middleware/Jsonp.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Approot",   # ported to Linen/Network/WebApp/Extra/Middleware/Approot.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.RequestLogger",   # ported to Linen/Network/WebApp/Extra/Middleware/RequestLogger.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.RequestLogger.JSON",   # ported to Linen/Network/WebApp/Extra/Middleware/RequestLogger/JSON.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Middleware.Gzip",   # ported to Linen/Network/WebApp/Extra/Middleware/Gzip.lean (actual zlib compression deferred, matching Hale's own upstream TODO — passthrough no-op documented in tests)
    "Hale.WaiExtra.Network.Wai.Parse",   # ported to Linen/Network/WebApp/Extra/Parse.lean; verbatim
    "Hale.WaiExtra.Network.Wai.Test",   # ported to Linen/Network/WebApp/Extra/Test.lean (namespace Network.WebApp.Extra.Test): SRequest/SResponse simulated testing harness; requestBody given a one-shot read-then-empty contract (Hale's Haskell version relies on lazy evaluation for the same effect)
    "Hale.WaiExtra.Network.Wai.Test.Internal",   # ported to Linen/Network/WebApp/Extra/Test/Internal.lean; re-exports Test, no new content
    "Hale.WaiExtra.Network.Wai.EventSource",   # ported to Linen/Network/WebApp/Extra/EventSource.lean (namespace Network.WebApp.Extra.EventSource): ServerEvent + render, eventSourceApp; verbatim
    "Hale.WaiExtra.Network.Wai.EventSource.EventStream",   # ported to Linen/Network/WebApp/Extra/EventSource/EventStream.lean: dataEvent/namedEvent/retryEvent/commentEvent helpers; verbatim
    "Hale.WaiExtra",   # aggregator: re-exports all Network.Wai.{Header,Request,UrlMap,Middleware.*,Parse,Test,Test.Internal,EventSource,EventSource.EventStream}; renamed WaiExtra → WebApp.Extra; covered by linen's root; no file
    "Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.LRU",   # ported to Linen/Network/WebApp/Extra/Middleware/Push/Referer/LRU.lean (namespace Network.WebApp.Extra.Middleware.Push.Referer): list-backed LRU cache (empty/lookup/insert/size); verbatim
    "Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.ParseURL",   # ported to Linen/Network/WebApp/Extra/Middleware/Push/Referer/ParseURL.lean: extractPath/isStaticResource; verbatim
    "Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.Types",   # ported to Linen/Network/WebApp/Extra/Middleware/Push/Referer/Types.lean: PushPath/PushEntry/PushSettings; verbatim
    "Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.Manager",   # ported to Linen/Network/WebApp/Extra/Middleware/Push/Referer/Manager.lean: PushManager.new/.record/.getPushes; verbatim
    "Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer",   # ported to Linen/Network/WebApp/Extra/Middleware/Push/Referer.lean (namespace Network.WebApp.Extra.Middleware.Push): pushOnReferer; namespace reused from WaiExtra's Push tree rather than a new WebApp.Http2Extra top-level, mirroring Hale's own Network.Wai.Middleware reuse across WaiExtra/WaiHttp2Extra
    "Hale.WaiHttp2Extra",   # aggregator: re-exports Network.Wai.Middleware.Push.Referer.{LRU,ParseURL,Types,Manager} + Push.Referer; renamed WaiHttp2Extra → WebApp.Extra.Middleware.Push; covered by linen's root; no file
    "Hale.WaiLogger.Network.Wai.Logger",   # ported to Linen/Network/WebApp/Logger.lean (namespace Network.WebApp.Logger): apacheFormat/apacheFormatWithDate/ApacheLogger.log; verbatim
    "Hale.WaiLogger",   # aggregator: re-exports Network.Wai.Logger; renamed WaiLogger → WebApp.Logger; covered by linen's root; no file
    "Hale.Time.Data.Time.Clock",   # ported to Linen/Data/Time/Clock.lean (namespace Data.Time): NominalDiffTime (Int nanoseconds) + fromSeconds/fromMilliseconds/fromMicroseconds/toSeconds/toMilliseconds/toMicroseconds/Add/Sub/Neg/Ord/ToString, UTCTime (Nat nanosSinceEpoch) + getCurrentTime (IO.monoNanosNow)/diffUTCTime/addUTCTime, fromSeconds_toSeconds/diffUTCTime_self theorems; verbatim
    "Hale.Time",   # aggregator: re-exports Data.Time.Clock; covered by linen's root; no file
    "Hale.TimeManager.System.TimeManager",   # ported to Linen/System/TimeManager.lean (namespace System.TimeManager): HandleState (active deadline/paused/canceled) + Handle (IO.Ref state + onTimeout) + Manager (timeoutUs, IO.Ref (Array Handle), Std.CancellationToken) with a dedicated-task cooperative-cancellation sweeper (IO.asTask (prio := .dedicated), same pattern as Control.AutoUpdate); Manager.new/register/stop, Handle.tickle/cancel/pause/resume; verbatim except IO.monoNanosNow used directly (no Data.Time.Clock dependency needed)
    "Hale.TimeManager",   # aggregator: re-exports System.TimeManager; covered by linen's root; no file
    "Hale.UnixCompat.System.Posix.Compat",   # ported to Linen/System/Posix/Compat.lean (namespace System.Posix): Fd (fd bookkeeping) + closeFd, FileStatus + getFileStatus/fileExist over System.FilePath.metadata; pure passthrough, verbatim
    "Hale.UnixCompat",   # aggregator: re-exports System.Posix.Compat; covered by linen's root; no file
    "Hale.UnliftIO.Control.Monad.IO.Unlift",   # ported to Linen/Control/Monad/IO/Unlift.lean (namespace Control.Monad.IO): MonadUnliftIO (CPS withRunInIO over MonadLiftT IO m) + toIO/liftIOOp, IO/ReaderT r IO instances; built directly on Lean's own ReaderT/read/MonadLiftT (no bespoke reader type needed); Tests mirror Hale's own Tests/UnliftIO/TestUnliftIO.lean coverage (IO instance, ReaderT env capture/preservation, toIO roundtrip); verbatim
    "Hale.UnliftIO",   # aggregator: re-exports Control.Monad.IO.Unlift; covered by linen's root; no file
    "Hale.IpRoute.Data.IP",   # ported to Linen/Data/IP.lean (namespace Data.IP): IPv4/IPv6/IP + AddrRange4/6 (bounded mask proof) + isMatchedTo + parseIPv4/parseCIDR4; pure, verbatim
    "Hale.IpRoute",   # aggregator: re-exports Data.IP; covered by linen's root; no file
    "Hale.Jose.Crypto.JOSE.FFI",   # ported to Linen/Crypto/JOSE/FFI.lean + ffi/jose.c (linen_jose_* OpenSSL bindings: HMAC/RSA-verify/EC-verify/key-build/base64url); OpenSSL via pkg-config in lakefile (nativeLinkArgs = libpq++openssl), CI installs libssl-dev
    "Hale.Jose.Crypto.JOSE.Types",   # ported to Linen/Crypto/JOSE/Types.lean (namespace Crypto.JOSE): JWSAlgorithm/ECCurve/JWKKeyType/JWKKeyMaterial/JWK (coherence proof)/ClaimsSet/JWSHeader/JWTValidationSettings/JwtError + laws; kept local Repr ByteArray (stdlib lacks it); pure, verbatim
    "Hale.Jose.Crypto.JOSE.JWK",   # ported to Linen/Crypto/JOSE/JWK.lean (namespace Crypto.JOSE.JWK): parseOctKey + toDerPublicKey over the OpenSSL FFI; #eval IO tests exercise the real base64url FFI; verbatim
    "Hale.Jose.Crypto.JOSE.JWS",   # ported to Linen/Crypto/JOSE/JWS.lean (namespace Crypto.JOSE.JWS): splitCompact + verifySignature (HMAC/RSA/EC via FFI); #eval IO tests do real HMAC verify round-trips; verbatim
    "Hale.Jose.Crypto.JOSE.JWT",   # ported to Linen/Crypto/JOSE/JWT.lean (namespace Crypto.JOSE.JWT): pure validateClaims (exp/nbf/aud/iss + bounded skew) + IO verifyJWT (compact parse -> signature over candidate JWK set -> validateClaims); #guard covers validateClaims, #eval does a full HS256 round-trip via OpenSSL; verbatim
    "Hale.Jose",   # aggregator: re-exports the 5 Crypto.JOSE.* modules; covered by linen's root; no file
    "Hale.MimeTypes.Network.Mime",   # ported to Linen/Network/Mime.lean (namespace Network.Mime): MimeType/Extension/FileName/MimeMap, defaultMimeMap, fileNameExtensions (partial -> structural recursion on splitOn "." components), mimeByExt (List.findSome? + List.lookup instead of bespoke assocLookup), defaultMimeLookup; 24 #guard tests; verbatim table
    "Hale.MimeTypes",   # aggregator: re-exports Network.Mime; covered by linen's root; no file
    "Hale.Mtl.Control.Monad.Except",   # ported to Linen/Control/Monad/Except.lean (namespace Control.Monad.Except): mtl-named throwError/catchError/liftEither/mapExceptT/withExceptT/runExceptT over Lean's own ExceptT/Except; pure, verbatim
    "Hale.Mtl.Control.Monad.Reader",   # ported to Linen/Control/Monad/Reader.lean (namespace Control.Monad.Reader): Reader alias + mtl-named ask/asks/local/runReaderT/runReader/mapReaderT over Lean's own ReaderT/read/ReaderT.adapt; pure, verbatim
    "Hale.Mtl.Control.Monad.State",   # ported to Linen/Control/Monad/State.lean (namespace Control.Monad.State): State alias + mtl-named put/gets/runStateT/evalStateT/execStateT/runState/evalState/execState over Lean's own StateT; get/set/modify NOT re-wrapped (core's root-level MonadState get/set/modify are already the exact same names/behaviour -- re-declaring them just shadows and creates ambiguous-term errors); adapted
    "Hale.Mtl.Control.Monad.Trans",   # ported to Linen/Control/Monad/Trans.lean (namespace Control.Monad.Trans): mtl-named lift over Lean's own MonadLift/monadLift; no bespoke MonadTrans class or per-transformer instances declared (core's MonadLift/LawfulMonadLift already generalize it, with existing lawful instances for ExceptT/ReaderT/StateT), just lift_pure/lift_bind laws restated generically once; adapted
    "Hale.Mtl",   # aggregator: re-exports the 4 Control.Monad.* mtl modules (Except/Reader/State/Trans); covered by linen's root; no file
    "Hale.Network.Network.Socket.Blocking",   # ported to Linen/Network/Socket/Blocking.lean (namespace Network.Socket.Blocking): blocking accept/connect/send/sendAll/recv over the non-blocking Socket API, looping on wouldBlock; partial def dropped -- each retry loop rewritten as `while true do ... ; unreachable!` (return on every terminating outcome) or a plain bounded `while`, matching the no-partial idiom already used by EventDispatcher.sendAllGreen; verbatim
    "Hale.Network.Network.Socket.ByteString",   # merged into Linen/Network/Socket.lean (namespace Network.Socket) rather than a new file -- Lean has no separate ByteString type to bridge; adds sendAll/sendTo/recvFrom over the existing Socket FFI (recv dropped as a duplicate of Network.Socket.Blocking.recv); socketSend/socketSendTo/socketRecvFrom FFI opaques switched from IO USize (or USize nested in a tuple) to IO Nat to avoid a compiled-mode ABI SIGSEGV under #eval, same fix already applied to socketGetFd; verbatim
    "Hale.Network",   # aggregator: re-exports the 6 Network.Socket.* modules (Types/FFI/Socket/ByteString/Blocking/EventDispatcher); covered by linen's root; no file
    "Hale.OptParse.Options.Applicative.Types",   # ported to Linen/Options/Applicative/Types.lean (namespace Options.Applicative): ReadM/Mod/InfoMod/OptDescr/Parser/ParserInfo; Parser kept as a functional record (List String -> Except String (a x List String)) rather than Haskell's free-applicative GADT, which Lean's positivity checker rejects; pure, verbatim
    "Hale.OptParse.Options.Applicative.Builder",   # ported to Linen/Options/Applicative/Builder.lean (namespace Options.Applicative): modifier constructors (long/short/help/metavar/hidden/showDefault), readers (str/eitherReader/auto + FromString class), option/strOption/switch/flag/flag'/argument/subparser builders, Pure/Functor/Seq/SeqLeft/SeqRight/Applicative/OrElse instances for Parser, withDefault/optionWithDefault/strOptionWithDefault/command; pure, verbatim
    "Hale.OptParse.Options.Applicative.Extra",   # ported to Linen/Options/Applicative/Extra.lean (namespace Options.Applicative): renderHelp/helper/info/hsubparser/execParser/execParserPure, help-text rendering (renderOptionName/padRight/renderOptDescr/collectCmdDescrs); verbatim
    "Hale.OptParse.Options.Applicative",   # aggregator: re-exports the 3 Options.Applicative.* modules (Types/Builder/Extra); covered by linen's root; no file
    "Hale.OptParse",   # aggregator: re-exports Options.Applicative; covered by linen's root; no file
    "Hale.PostgREST.PostgREST.ApiRequest.Preferences",   # ported to Linen/PostgREST/ApiRequest/Preferences.lean (namespace PostgREST.ApiRequest.Preferences): PreferCount/PreferReturn/PreferResolution/PreferTransaction/PreferMissing/PreferHandling enums + ToString, Preferences struct, parsePreferences/tokenize/applyToken; BEq/Inhabited derived on the enums (added Inhabited to each) instead of Hale's hand-written Preferences BEq/Inhabited instances; verbatim otherwise
    "Hale.PostgREST.PostgREST.Auth.Types",   # ported to Linen/PostgREST/Auth/Types.lean (namespace PostgREST.Auth): AuthResult (authRole + non-emptiness proof + authClaims) and AuthResult.lookupClaim; verbatim
    "Hale.PostgREST.PostgREST.Auth",   # ported to Linen/PostgREST/Auth.lean (namespace PostgREST.Auth): extractBearerToken/findAuthHeader/extractRole/authenticate (stub JWT validation, matching Hale's own not-yet-wired JOSE integration); verbatim
    "Hale.PostgREST.PostgREST.Cache.Sieve",   # ported to Linen/PostgREST/Cache/Sieve.lean (namespace PostgREST.Cache): SieveEntry/SieveCache, create/lookup/insert/remove/size, evictAndInsert's sweep is a structural `for _ in List.range entries.size do ... return ...` inside Id.run, no partial; verbatim
    "Hale.PostgREST.PostgREST.Config.JSPath",   # ported to Linen/PostgREST/Config/JSPath.lean (namespace PostgREST.Config): JSPathSegment/JSPath + ToString, isEmpty/depth/parse/follow/followNested/defaultRoleClaimPath; verbatim
    "Hale.PostgREST.PostgREST.Config.PgVersion",   # ported to Linen/PostgREST/Config/PgVersion.lean (namespace PostgREST.Config): PGVersion + ToString/Ord/Inhabited, fromVersionNum/toVersionNum/parse, pgVersionMin/isSupported/isAtLeastMajor/isAtLeast; verbatim
    "Hale.PostgREST.PostgREST.Config.Proxy",   # ported to Linen/PostgREST/Config/Proxy.lean (namespace PostgREST.Config): UriScheme + ToString/defaultPort, ProxyUri + ToString/toUri/parse, openApiServerUrl; verbatim
    "Hale.PostgREST.PostgREST.Cors",   # ported to Linen/PostgREST/Cors.lean (namespace PostgREST.Cors): defaultExposedHeaders/defaultAllowedHeaders, corsHeaders, preflightHeaders; verbatim
    "Hale.PostgREST.PostgREST.Debounce",   # ported to Linen/PostgREST/Debounce.lean (namespace PostgREST.Debounce): Debouncer (IO.Ref-backed), Debouncer.create/run; verbatim
    "Hale.PostgREST.PostgREST.Listener",   # ported to Linen/PostgREST/Listener.lean (namespace PostgREST.Listener): pgrstChannel/listenSql, NotificationAction, parseNotification; verbatim
    "Hale.PostgREST.PostgREST.Logger",   # ported to Linen/PostgREST/Logger.lean (namespace PostgREST.Logger): LogLevel + Ord/ToString, log/logCrit/logError/logWarn/logInfo/logDebug; verbatim
    "Hale.PostgREST.PostgREST.MediaType",   # ported to Linen/PostgREST/MediaType.lean (namespace PostgREST.MediaType): MTVndPlanOption/MTVndPlanFormat, MediaType + BEq/ToString/Inhabited, toMime/toContentType/ofMime/isJSON/isText, IsStandard + 9 native_decide roundtrip theorems; verbatim
    "Hale.PostgREST.PostgREST.Network",   # ported to Linen/PostgREST/Network.lean (namespace PostgREST.Network): resolveHost; verbatim
    "Hale.PostgREST.PostgREST.RangeQuery",   # ported to Linen/PostgREST/RangeQuery.lean (namespace PostgREST.RangeQuery): NonnegRange + ToString/unlimited/size/isUnlimited/restrictTo, parseRange, ContentRange + BEq/Inhabited/ToString, contentRangeHeader, ContentRange.fromNonnegRange; verbatim
    "Hale.PostgREST.PostgREST.Response",   # ported to Linen/PostgREST/Response.lean (namespace PostgREST.Response): contentRangeHeader, readHeaders, mutateHeaders, readStatus, readStatus_valid theorem; verbatim
    "Hale.PostgREST.PostgREST.Response.GucHeader",   # ported to Linen/PostgREST/Response/GucHeader.lean (namespace PostgREST.Response.GucHeader): gucHeaderPrefix/gucStatusVar, parseGucHeaders (upstream stub, JSON parsing not yet wired in), parseGucStatus; verbatim
    "Hale.PostgREST.PostgREST.Response.Performance",   # ported to Linen/PostgREST/Response/Performance.lean (namespace PostgREST.Response.Performance): serverTimingHeader, serverTimingValue, timingHeaders; verbatim
    "Hale.PostgREST.PostgREST.SchemaCache.Identifiers",   # ported to Linen/PostgREST/SchemaCache/Identifiers.lean (namespace PostgREST.SchemaCache.Identifiers): Schema/TableName/FieldName/FunctionName/ConstraintName aliases, QualifiedIdentifier + BEq/Hashable/Ord/ToString, escapeIdent/quoteIdent/quoteQi/toQi, anyElement/isAnyElement, RelIdentifier, 4 quoting correctness theorems; verbatim
    "Hale.PostgREST.PostgREST.ApiRequest.Types",   # ported to Linen/PostgREST/ApiRequest/Types.lean (namespace PostgREST.ApiRequest): Mutation/InvokeMethod/Action, JsonOperation, SimpleOperator/FtsOperator/QuantOperator/FilterOperator, Filter, LogicOperator/LogicTree, OrderDirection/OrderNulls/OrderTerm, SelectItem, Payload, IsVal, Target; LogicTree.toString/SelectItem.toString ported without `partial` (Lean's structural recursion checker handles the nested Array-of-self recursion directly); verbatim otherwise
    "Hale.PostgREST.PostgREST.Config",   # ported to Linen/PostgREST/Config.lean (namespace PostgREST.Config): LogLevel, OpenAPIMode, refined Port (0 < val <= 65535), AppConfig (flat record with configDbSchemas_nonempty/configDbPoolSize_pos proof fields), AppConfig.default, hasJwtSecret/hasAdminServer/hasRootSpec/hasPreRequest/mainSchema, LogLevel.parse_toString_roundtrip/OpenAPIMode.parse_toString_roundtrip theorems; verbatim
    "Hale.PostgREST.PostgREST.Config.Database",   # ported to Linen/PostgREST/Config/Database.lean (namespace PostgREST.Config): DbUriParts + Repr/Inhabited, DbUriParts.toUri, searchPathSql/searchPathDisplay, setRoleSql/resetRoleSql, TxMode/setTxModeSql, TxEnd; verbatim
    "Hale.PostgREST.PostgREST.Error.Types",   # ported to Linen/PostgREST/Error/Types.lean (namespace PostgREST.Error): RangeError/QPError/ApiRequestError/SchemaCacheError/JwtError/PgError/Error hierarchy with ToString/BEq/Repr, *.toHttpStatus mapping, 3 toHttpStatus_valid (100-599) theorems; PgError.pgCode_len default proof changed `by omega` -> `by decide` (omega cannot reduce String.length on a literal)
    "Hale.PostgREST.PostgREST.Error",   # ported to Linen/PostgREST/Error.lean (namespace PostgREST.Error): jsonEscape/jsonString/jsonOptionalField, errorPayload (JSON error body incl. PGRST error codes), errorHeaders (Content-Type + WWW-Authenticate for JWT/401 pg errors); fixed a double-JSON-escaping bug in the jwtError/schemaCacheError branches of errorPayload (message was run through jsonEscape manually and then through jsonString, which escapes again) by dropping the redundant manual jsonEscape call
    "Hale.PostgREST.PostgREST.MainTx",   # ported to Linen/PostgREST/MainTx.lean (namespace PostgREST.MainTx): sqlLit, setSearchPath, setRole, setRequestContext (role/JWT claims/method/path/headers SET LOCAL statements), preRequestSql; verbatim
    "Hale.PostgREST.PostgREST.Plan.Types",   # ported to Linen/PostgREST/Plan/Types.lean (namespace PostgREST.Plan): JsonOperation, FilterOperator/LogicOperator/OrderDirection/OrderNulls (local, duplicate ApiRequest.Types pending unification), CoercibleField, AggregateFunction + toSql/ToString, CoercibleSelectField, CoercibleFilter, CoercibleLogicTree (recursive, no BEq/Repr derived - matches upstream), CoercibleOrderTerm, SpreadType, RelJsonEmbedMode, ConflictAction; verbatim
    "Hale.PostgREST.PostgREST.Query.SqlFragment",   # ported to Linen/PostgREST/Query/SqlFragment.lean (namespace PostgREST.Query): pgFmtIdent/pgFmtQi/pgFmtLit, pgFmtColumn/pgFmtField, simpleOpToSql/ftsOpToSql, pgFmtFilter, pgFmtLogicTree, pgFmtOrderTerm, asJsonF/asJsonSingleF, setConfigLocal/setConfigWithConstantName, pgFmtOrderClause/pgFmtWhereClause, 2 quoting theorems; dropped `partial` from pgFmtLogicTree (structural recursion through Array.map is accepted directly, same pattern as ApiRequest.Types.LogicTree.toString) per the no-partial-def rule
    "Hale.PostgREST.PostgREST.SchemaCache.Relationship",   # ported to Linen/PostgREST/SchemaCache/Relationship.lean (namespace PostgREST.SchemaCache): Cardinality (o2m/m2o/o2o/m2m with junction table + BEq/ToString), Relationship + BEq/ToString, localColumns/foreignColumns; verbatim
    "Hale.PostgREST.PostgREST.Plan.ReadPlan",   # ported to Linen/PostgREST/Plan/ReadPlan.lean (namespace PostgREST.Plan): ReadPlan (recursive via rpRelationships : Array (Relationship x ReadPlan), Repr derives fine through the nested Array/Product recursion), hasEmbeds/embedCount/hasFilters/hasOrdering; deduplicated the upstream module's locally-redeclared NonnegRange (identical offset+limit fields) to reuse Linen.PostgREST.RangeQuery.NonnegRange instead (rpRange default changed from `allRows`/`{}` to `.unlimited`)
    "Hale.PostgREST.PostgREST.Plan.MutatePlan",   # ported to Linen/PostgREST/Plan/MutatePlan.lean (namespace PostgREST.Plan): MutatePlan (insert/update/delete, using RangeQuery.NonnegRange for update/delete's range_ per the ReadPlan dedup), targetTable/returningFields/hasReturning; verbatim
    "Hale.PostgREST.PostgREST.SchemaCache.Representations",   # ported to Linen/PostgREST/SchemaCache/Representations.lean (namespace PostgREST.SchemaCache): Representation + BEq/Repr, MediaHandler + BEq/Repr; verbatim
    "Hale.PostgREST.PostgREST.SchemaCache.Routine",   # ported to Linen/PostgREST/SchemaCache/Routine.lean (namespace PostgREST.SchemaCache): Volatility/IsolationLevel/ParamMode + ToString, RoutineParam, RoutineReturnType + isSetof, Routine + toQi/requiredParams/isSafeForGet/ToString, isSafeForGet_iff_not_volatile theorem; verbatim
    "Hale.PostgREST.PostgREST.Plan.CallPlan",   # ported to Linen/PostgREST/Plan/CallPlan.lean (namespace PostgREST.Plan): CallPlan (binds a Routine to param values + returning clause), routineQi/isSetof/isSafeForGet/paramCount; verbatim
    "Hale.PostgREST.PostgREST.SchemaCache.Table",   # ported to Linen/PostgREST/SchemaCache/Table.lean (namespace PostgREST.SchemaCache): Column + BEq/Repr/ToString, Table + Repr/ToString with pk_subset proof field (verified the default `by intro c hc; simp_all` tactic both discharges for a genuine PK subset and correctly rejects a PK column absent from tableColumns), toQi/findColumn/columnNames/pkColumnNames/hasPrimaryKey; verbatim
    "Hale.PostgREST.PostgREST.SchemaCache",   # ported to Linen/PostgREST/SchemaCache.lean (namespace PostgREST.SchemaCache, nested namespace SchemaCache): SchemaCache aggregate + Repr, empty/findTable/findRelationships/findRoutines/tablesInSchemas, tablesSql/columnsSql/relationshipsSql/routinesSql/versionSql catalog-introspection SQL literals; verbatim. Table/Routine still have no BEq (matches upstream), so tests compare projected fields or use isEmpty/isNone instead of `==`
    "Hale.PostgREST.PostgREST.AppState",   # ported to Linen/PostgREST/AppState.lean (namespace PostgREST.AppState, nested namespace AppState): Observation events, Metrics, AppState (IO.Ref-backed schema cache/metrics + observer callback), create/getSchemaCache/putSchemaCache/observe/incRequestCount/incErrorCount; verbatim. IO-effectful, so tested with `#eval show IO Unit from do ...` per the Debouncer/Listener convention rather than #guard
    "Hale.PostgREST.PostgREST.Metrics",   # ported to Linen/PostgREST/Metrics.lean (namespace PostgREST.Metrics): renderMetrics (Prometheus text exposition format from AppState.Metrics counters); verbatim, no naming clash with AppState.Metrics since it lives in its own namespace
    "Hale.PostgREST.PostgREST.Admin",   # ported to Linen/PostgREST/Admin.lean (namespace PostgREST.Admin): handleAdminRequest (/live, /ready, /metrics, 404 fallback), IO-effectful over AppState; verbatim, tested with `#eval show IO Unit from do ...` per the AppState/Debouncer convention
    "Hale.PostgREST.PostgREST.Observation",   # ported to Linen/PostgREST/Observation.lean (namespace PostgREST.Observation): defaultObserver (stderr logging for every AppState.Observation variant); verbatim, tested with `#eval show IO Unit from do ...` exercising every variant
    "Hale.PostgREST.PostgREST.TimeIt",   # ported to Linen/PostgREST/TimeIt.lean (namespace PostgREST.TimeIt): timeIt/timeIt_ (elapsed-time IO wrappers over IO.monoMsNow); verbatim, tested with `#eval show IO Unit from do ...`
    "Hale.PostgREST.PostgREST.Unix",   # ported to Linen/PostgREST/Unix.lean (namespace PostgREST.Unix): defaultSocketMode; verbatim
    "Hale.PostgREST.PostgREST.Version",   # ported to Linen/PostgREST/Version.lean (namespace PostgREST.Version): version/prettyVersion, with the branding adapted from "-hale"/"Hale/Lean 4 port" to "-linen"/"Linen/Lean 4 port" since this is the Linen port, not Hale
    "Hale.PostgREST.PostgREST.App",   # ported to Linen/PostgREST/App.lean (namespace PostgREST.App): SimpleRequest/SimpleResponse, jsonResponse/errorResponse, handleRequest (root listing, CORS preflight, RPC stub, table GET/HEAD/POST/PATCH/DELETE/OPTIONS dispatch, 404/405/501 errors, metrics + observation recording, CORS headers), printBanner; verbatim, IO-effectful so tested with `#eval show IO Unit from do ...`
    "Hale.PostgREST.PostgREST.CLI",   # ported to Linen/PostgREST/CLI.lean (namespace PostgREST.CLI): Command (no BEq, matches upstream - tests pattern-match), parseArgs, printUsage; verbatim
    "Hale.PostgREST.PostgREST.Response.OpenAPI",   # ported to Linen/PostgREST/Response/OpenAPI.lean (namespace PostgREST.Response.OpenAPI): pgTypeToOpenAPI, columnSchema, generateOpenAPISpec (openapi 3.0.0 JSON from a SchemaCache); verbatim
    "Hale.PostgREST",   # no dedicated Linen file: this is only the Hale staging area's package-root re-export (44 `import Hale.PostgREST.PostgREST.*` lines with no other code). Linen never mirrors that per-package root-file pattern (confirmed: no Hale.<Package>-shaped root file exists anywhere under Linen/) - every submodule imports directly into the single Linen.lean root, and all 44 corresponding Linen.PostgREST.* modules are already imported there
    "Hale.Recv.Network.Socket.Recv",   # no file: recv/recvString are thin wrappers around blocking-socket recv, functionally identical to the already-ported Linen/Network/Socket/Blocking.lean's `recv`; duplicate, not re-ported
    "Hale.Recv",   # aggregator: re-exports Network.Socket.Recv; covered by linen's root (and the duplicate is skipped above); no file
    "Hale.ResourceT.Control.Monad.Trans.Resource",   # ported to Linen/Control/Monad/Trans/Resource.lean (namespace Control.Monad.Trans.Resource): ReleaseKey, ResourceT = ReaderT (IO.Ref CleanupMap) over stdlib ReaderT (dropped Hale's hand-rolled Monad/MonadLift instances and its dead Std.Data.HashMap import), allocate/release/runResourceT (LIFO cleanup, exception-safe via try/finally), releaseKey_eq theorem; verbatim otherwise
    "Hale.ResourceT",   # aggregator: re-exports Control.Monad.Trans.Resource; covered by linen's root; no file
    "Hale.QUIC.Network.QUIC.Types",   # ported to Linen/Network/QUIC/Types.lean (namespace Network.QUIC): proof-carrying ConnectionId (bytes.size <= 20)/Version/TransportParams/StreamId/TransportError/TLSConfig; pure, verbatim
    "Hale.QUIC.Network.QUIC.Config",   # ported to Linen/Network/QUIC/Config.lean (namespace Network.QUIC): ServerConfig/ClientConfig bundling TLSConfig + transport-parameter/host/port defaults; pure, verbatim
    "Hale.QUIC.Network.QUIC.Connection",   # ported to Linen/Network/QUIC/Connection.lean (namespace Network.QUIC): opaque, only-internally-constructible Connection handle + ConnectionState; stream/close/state ops stubbed pending TLS 1.3 FFI, matching upstream
    "Hale.QUIC.Network.QUIC.Client",   # ported to Linen/Network/QUIC/Client.lean (namespace Network.QUIC): connect : ClientConfig -> IO Connection, stubbed pending TLS 1.3 FFI, matching upstream
    "Hale.QUIC.Network.QUIC.Server",   # ported to Linen/Network/QUIC/Server.lean (namespace Network.QUIC): run/accept : ServerConfig -> IO Connection, stubbed pending TLS 1.3 FFI, matching upstream
    "Hale.QUIC.Network.QUIC.Stream",   # ported to Linen/Network/QUIC/Stream.lean (namespace Network.QUIC): QUICStream (Connection + StreamId pair) with send/recv/close; verbatim
    "Hale.Http3.Network.HTTP3.Server",   # ported to Linen/Network/HTTP3/Server.lean (namespace Network.HTTP3): H3Request/H3Response/H3Handler, sendResponse (QPACK-encodes and frames a response over a QUICStream), handleRequestStream; handleConnection stubbed pending QUIC stream-accept support, matching upstream
    "Hale.Http3",   # aggregator: re-exports the 5 Network.HTTP3.* modules (Error/Frame/QPACK.*/Server); covered by linen's root; no file
    "Hale.QUIC",   # aggregator: re-exports the 6 Network.QUIC.* modules (Types/Config/Connection/Stream/Server/Client); covered by linen's root; no file
    "Hale.STM.Control.Monad.STM",   # ported to Linen/Control/Monad/STM.lean (namespace Control.Monad): STM = BaseIO (STMResult _), global-mutex-serialized (Std.Mutex Unit) atomically/retry/orElse/check; atomically's retry loop rewritten from `partial def` to a `while true do ... ; unreachable!` loop
    "Hale.STM.Control.Concurrent.STM.TVar",   # ported to Linen/Control/Concurrent/STM/TVar.lean (namespace Control.Concurrent.STM): TVar α := IO.Ref α, newTVarIO/newTVar/readTVar/writeTVar/modifyTVar'; verbatim
    "Hale.STM.Control.Concurrent.STM.TMVar",   # ported to Linen/Control/Concurrent/STM/TMVar.lean: TMVar α := TVar (Option α), newTMVar(IO)/newEmptyTMVar(IO)/takeTMVar/putTMVar/readTMVar/tryTakeTMVar/tryPutTMVar/isEmptyTMVar; verbatim
    "Hale.STM.Control.Concurrent.STM.TQueue",   # ported to Linen/Control/Concurrent/STM/TQueue.lean: two-list amortized FIFO TQueue, newTQueue(IO)/writeTQueue/readTQueue/tryReadTQueue/isEmptyTQueue/peekTQueue; verbatim
    "Hale.STM",   # aggregator: re-exports Control.Monad.STM + the 3 Control.Concurrent.STM.* modules; covered by linen's root; no file
    "Hale.Scientific.Data.Scientific",   # ported to Linen/Data/Scientific.lean (namespace Data): arbitrary-precision `coefficient * 10^exponent` Scientific with normalize/isZero/isInteger/toRealFloat/fromFloatDigits/toBoundedInteger/toDecimalDigits, Add/Sub/Mul/Neg/BEq/Ord/OfNat/OfScientific/ToString instances, plus proven theorems (isZero_iff, normalize_zero, neg_neg, ...); no stdlib equivalent, verbatim port
    "Hale.Scientific",   # aggregator: re-exports Data.Scientific; covered by linen's root; no file
    "Hale.SimpleSendfile.Network.Sendfile",   # ported to Linen/Network/Sendfile.lean (namespace Network.Sendfile): FilePart, sendFile/sendFileSimple (portable chunked read + Blocking.sendAll fallback, no zero-copy syscall); verbatim except the Blocking import
    "Hale.SimpleSendfile",   # aggregator: re-exports Network.Sendfile; covered by linen's root; no file
    "Hale.StreamingCommons.Data.Streaming.Network",   # ported to Linen/Data/Streaming/Network.lean (namespace Data.Streaming.Network): AppData, bindPortTCP/getSocketTCP/mkAppData/runTCPServer, acceptSafe (retry-on-accept-error loop rewritten from `partial def` to a `while true do ... ; unreachable!` loop); verbatim otherwise
    "Hale.StreamingCommons",   # aggregator: re-exports Data.Streaming.Network; covered by linen's root; no file
    "Hale.TLS.Network.TLS.Types",   # ported to Linen/Network/TLS/Types.lean (namespace Network.TLS): TLSVersion, CipherID, TLSOutcome (ok/wantRead/wantWrite/error); verbatim
    "Hale.TLS.Network.TLS.Context",   # ported to Linen/Network/TLS/Context.lean + ffi/tls.c (linen_tls_* OpenSSL FFI, renamed from hale_tls_*): opaque TLSContext/TLSSession, createContext/setAlpn/acceptSocket/read/write/close/getVersion/getAlpn, non-blocking *NB variants, client-side createClientContext/connectSocketRaw/connectSocketNB; connectSocket's WANT_READ/WANT_WRITE retry loop rewritten from `partial def` to a `while true do ... ; unreachable!` loop; plus a new createClientContextWithCA (custom CA trust, not in Hale) added to make the self-signed-cert loopback test in Tests/Linen/Network/TLS/ContextTest.lean fully offline
    "Hale.TLS",   # aggregator: re-exports Network.TLS.Types + Network.TLS.Context; covered by linen's root; no file
    "Hale.Text.Data.Text",   # ported to Linen/Data/Text.lean (namespace Data, abbrev Text := String): full Data.Text API on String/List Char; words/unlines/unwords delegate to Linen.Data.String instead of reimplementing; transpose reformulated as a fuel-free List.range map, chunksOf/isInfixOf rewritten from fuel-counter recursion to genuine structural/well-founded recursion (no partial, no fuel)
    "Hale.Text.Data.Text.Encoding",   # ported to Linen/Data/Text/Encoding.lean (namespace Data.Text.Encoding): encodeUtf8 (String.toUTF8) + decodeUtf8' delegates to stdlib String.fromUTF8? instead of hand-rolling a validator; decodeUtf8With/decodeUtf8Lenient/decodeLatin1 keep a byte-level scanner (no stdlib equivalent for lenient replace-on-error) but decodeOneUtf8 embeds a proof `1 ≤ consumed` in its return type, giving genuine well-founded recursion on `bs.len - i` with no fuel parameter; decodeLatin1 is a plain List.map
    "Hale.Text",   # aggregator: re-exports Data.Text + Data.Text.Encoding; covered by linen's root; no file
    "Hale.Vault.Data.Vault",   # ported to Linen/Data/Vault.lean (namespace Data): Key/Vault backed by Std.HashMap Nat Erased; Erased is a dedicated `private opaque Erased := Unit` (Hale's bespoke reuse of an unrelated Data.Newtype.Any boolean-monoid type as its unsafeCast erasure target was not ported — replaced with a purpose-built opaque type); insert/lookup/delete/adjust/union
    "Hale.Vault",   # aggregator: re-exports Data.Vault; covered by linen's root; no file
    "Hale.Vector.Data.Vector",   # NOT ported as a competing `Vector` type — Lean 4 core already defines `Vector α n`, and Array already provides nearly all of Haskell's Data.Vector API; ported only the genuinely-missing combinators (generate/ifilter/foldl1'/foldr1/ifoldl'/ifoldr/and/or/product/notElem/backpermute/slice) as Array extensions in Linen/Data/Vector.lean (top-level namespace Array), per the stdlib-substitution rule
    "Hale.Vector",   # aggregator: re-exports Data.Vector; covered by linen's root; no file
}


def path_to_module(path):
    rel = os.path.relpath(path, ROOT)
    return rel[:-len(".lean")].replace(os.sep, ".")


# All .lean modules under Hale/, plus the Hale.lean root.
modules = {}
for dirpath, _, files in os.walk(SRC):
    for f in files:
        if f.endswith(".lean"):
            p = os.path.join(dirpath, f)
            modules[path_to_module(p)] = p
root_file = os.path.join(ROOT, "Hale.lean")
if os.path.exists(root_file):
    modules["Hale"] = root_file

def strip_block_comments(src):
    """Remove Lean /- ... -/ block comments (which nest), leaving line structure."""
    out = []
    depth = 0
    i = 0
    n = len(src)
    while i < n:
        two = src[i:i + 2]
        if two == "/-":
            depth += 1
            i += 2
        elif two == "-/" and depth > 0:
            depth -= 1
            i += 2
        else:
            if depth == 0:
                out.append(src[i])
            elif src[i] == "\n":
                out.append("\n")  # preserve line breaks inside comments
            i += 1
    return "".join(out)


# Real Lean imports sit at column 0 and precede any declaration.
import_re = re.compile(r'^import\s+(Hale(?:\.[A-Za-z0-9_]+)*)\s*$')

edges = defaultdict(set)
nodes = set(modules.keys())
for mod, path in modules.items():
    with open(path, encoding="utf-8") as fh:
        src = strip_block_comments(fh.read())
    for line in src.splitlines():
        # drop any trailing line comment
        line = line.split("--", 1)[0].rstrip()
        m = import_re.match(line)
        if m:
            dep = m.group(1)
            edges[mod].add(dep)
            nodes.add(dep)

# Kahn topo sort: a module appears AFTER everything it imports.
deps_of = {n: set(edges.get(n, set())) for n in nodes}
rdeps = defaultdict(set)
for n, ds in deps_of.items():
    for d in ds:
        rdeps[d].add(n)

# Transitive dependency closure of the prioritized targets (targets + all the
# modules they transitively import).
closure = set()
stack = [t for t in PRIORITIZE if t in nodes]
while stack:
    n = stack.pop()
    if n in closure:
        continue
    closure.add(n)
    stack.extend(deps_of.get(n, ()))

# Sanity check: DONE should be downward-closed (every dependency of a ported
# module is also ported); otherwise the "done first" block can't stay contiguous.
done_gaps = sorted(d for n in DONE if n in nodes for d in deps_of[n] if d not in DONE)
if done_gaps:
    print("WARNING: DONE modules import non-DONE modules:", done_gaps)

# Priority tiers for the topological sort (lower = earlier):
#   0 — already ported (DONE): emitted first, commented out.
#   1 — remaining EventDispatcher closure: the shortest path to the target.
#   2 — everything else.
# `heapq` breaks ties alphabetically via the module name in the tuple.
def tier(n):
    if n in DONE:
        return 0
    if n in closure:
        return 1
    return 2

remaining = {n: set(deps_of[n]) for n in nodes}
heap = [(tier(n), n) for n in nodes if not remaining[n]]
heapq.heapify(heap)
order = []
while heap:
    _, n = heapq.heappop(heap)
    order.append(n)
    for dependent in sorted(rdeps[n]):
        remaining[dependent].discard(n)
        if not remaining[dependent]:
            heapq.heappush(heap, (tier(dependent), dependent))

cycle_nodes = sorted(n for n in nodes if remaining[n])

os.makedirs(OUT_DIR, exist_ok=True)
dot_path = os.path.join(OUT_DIR, "module-dependencies.dot")
with open(dot_path, "w", encoding="utf-8") as fh:
    fh.write("// Hale module dependency graph (intra-Hale imports only).\n")
    fh.write("// Generated from `import Hale.*` statements under Hale/.\n")
    fh.write("// A -> B means module A imports module B.\n")
    fh.write("digraph HaleModules {\n")
    fh.write("  rankdir=LR;\n")
    fh.write('  node [shape=box, style=rounded, fontsize=9, fontname="Helvetica"];\n')
    fh.write('  edge [color="#888888", arrowsize=0.6];\n')
    for n in sorted(nodes):
        if n in modules:
            fh.write(f'  "{n}";\n')
        else:
            fh.write(f'  "{n}" [color="#cc0000", style="rounded,dashed"];\n')
    for mod in sorted(edges):
        for dep in sorted(edges[mod]):
            fh.write(f'  "{mod}" -> "{dep}";\n')
    fh.write("}\n")

with open("/tmp/topo_order.txt", "w") as fh:
    fh.write("\n".join(order))
with open("/tmp/cycle.txt", "w") as fh:
    fh.write("\n".join(cycle_nodes))

# ---- Write Markdown ----
n_edges = sum(len(v) for v in edges.values())
missing = sorted(n for n in nodes if n not in modules)
md_path = os.path.join(OUT_DIR, "module-dependencies.md")
with open(md_path, "w", encoding="utf-8") as fh:
    w = fh.write
    w("# Hale module dependencies\n\n")
    w("Dependency graph and topological order of every module under "
      "[`Hale/`](../../hale/Hale), derived from the `import Hale.*` "
      "statements in each source file (imports inside comments/docstrings "
      "are ignored).\n\n")
    w("An edge **A → B** means *module A imports module B*, so **B must be "
      "built before A**.\n\n")
    w("## Summary\n\n")
    w(f"- **Modules (nodes):** {len(nodes)}\n")
    w(f"- **Source files scanned:** {len(modules)}\n")
    w(f"- **Dependency edges:** {n_edges}\n")
    w(f"- **Cycles (strongly-connected components > 1):** "
      f"{len(cycle_nodes)} → the graph is a DAG.\n")
    if missing:
        w(f"- **Imported but no source file found:** {len(missing)} "
          f"({', '.join('`'+m+'`' for m in missing)})\n")
    w("\n## Graph\n\n")
    w("The full Graphviz source is in "
      "[`module-dependencies.dot`](module-dependencies.dot); a rendered "
      "version is in [`module-dependencies.svg`](module-dependencies.svg). "
      "Regenerate either with:\n\n")
    w("```sh\n")
    w("python3 docs/depgraph.py            # rebuild .dot + .md\n")
    w("dot -Tsvg docs/module-dependencies.dot -o docs/module-dependencies.svg\n")
    w("```\n\n")
    w("## Topologically sorted modules\n\n")
    w("Each module is listed after all modules it imports. The order is "
      "**prioritised to reach "
      "`Hale.Network.Network.Socket.EventDispatcher` as early as possible**: "
      "already-ported modules (commented out) come first, then "
      "EventDispatcher's remaining dependency chain, then everything else. "
      "Within a tier, ordering is alphabetical.\n\n")
    for i, m in enumerate(order, 1):
        if m in DONE:
            w(f"<!-- {i}. `{m}` -->\n")
        else:
            w(f"{i}. `{m}`\n")
    w("\n")
    if cycle_nodes:
        w("## Modules in cycles (not sortable)\n\n")
        for m in cycle_nodes:
            w(f"- `{m}`\n")
print("md:", md_path)

print({
    "n_files": len(modules),
    "n_nodes": len(nodes),
    "n_edges": sum(len(v) for v in edges.values()),
    "n_sorted": len(order),
    "n_cycle": len(cycle_nodes),
})
print("dot:", dot_path)
