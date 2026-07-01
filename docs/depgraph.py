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
    "Hale.IpRoute.Data.IP",   # ported to Linen/Data/IP.lean (namespace Data.IP): IPv4/IPv6/IP + AddrRange4/6 (bounded mask proof) + isMatchedTo + parseIPv4/parseCIDR4; pure, verbatim
    "Hale.IpRoute",   # aggregator: re-exports Data.IP; covered by linen's root; no file
    "Hale.Jose.Crypto.JOSE.FFI",   # ported to Linen/Crypto/JOSE/FFI.lean + ffi/jose.c (linen_jose_* OpenSSL bindings: HMAC/RSA-verify/EC-verify/key-build/base64url); OpenSSL via pkg-config in lakefile (nativeLinkArgs = libpq++openssl), CI installs libssl-dev
    "Hale.Jose.Crypto.JOSE.Types",   # ported to Linen/Crypto/JOSE/Types.lean (namespace Crypto.JOSE): JWSAlgorithm/ECCurve/JWKKeyType/JWKKeyMaterial/JWK (coherence proof)/ClaimsSet/JWSHeader/JWTValidationSettings/JwtError + laws; kept local Repr ByteArray (stdlib lacks it); pure, verbatim
    "Hale.Jose.Crypto.JOSE.JWK",   # ported to Linen/Crypto/JOSE/JWK.lean (namespace Crypto.JOSE.JWK): parseOctKey + toDerPublicKey over the OpenSSL FFI; #eval IO tests exercise the real base64url FFI; verbatim
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
