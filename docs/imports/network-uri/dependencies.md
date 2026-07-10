# `network-uri` module dependencies

Topological order of every module of the
[`network-uri`](https://hackage.haskell.org/package/network-uri) Hackage package,
source at [haskell/network-uri](https://github.com/haskell/network-uri), imported
into `linen` per [AGENTS.md](../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

The package exposes three modules:

- **`Network.URI`** (~1500 lines) — the RFC 3986 URI type, parser, renderer,
  percent-encoding, and relative-URI resolution. This is the only one `cdp` (or
  anything else in `linen`) needs, and the only one ported here.
- `Network.URI.Lens` — `lens`-compatible field lenses over `URI`/`URIAuth`. Not
  ported: `linen` has no `lens` port, and Lean's own field-projection/`{ x with
  ... }` update syntax already covers the same need directly.
- `Network.URI.Static` — GHC Template Haskell quasi-quoters for compile-time URI
  literals. Not ported: Lean's metaprogramming model doesn't map onto TH, and
  nothing consumes this outside example code.

`Network.URI`'s own parser is built on `parsec` (a general parser-combinator
library), listed in the package's `build-depends`. Per AGENTS.md's
stdlib-substitution rule, `parsec` itself is not ported — the grammar is
reimplemented directly as a hand-rolled recursive-descent parser over
`List Char`/`String` (structural recursion, no `partial`), since Parsec is an
implementation detail of the upstream parser, not part of its public API.

Deprecated upstream functions (`parseabsoluteURI`, `escapeString`, `reserved`,
`unreserved`, and the pre-3.0 field-accessor aliases `scheme`/`authority`/`path`/
`query`/`fragment`) are not ported, matching this project's practice of skipping
superseded/duplicate surface area.

## Topologically sorted modules

Ported to `Linen/Network/URI.lean` (namespace `Network.URI`) — see the module's
own docstring for the two documented simplifications (bracketed IP-literal hosts
accepted at the character-class level rather than RFC 3986's full IPv6 grammar;
`unEscapeString` decodes each `%XX` to its raw byte rather than reassembling
multi-byte UTF-8).

<!-- 1. `Network.URI` -->
