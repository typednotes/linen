# RFCs

What `linen` implements is mostly specified by IETF documents. This page maps
them to the modules that implement them, records the foundational specs
everything here rests on, and collects a few that are worth reading for their
own sake.

Two conventions:

- A module that **implements** a specification cites it in its own
  doc-comment. This page is the index, not the authority — the module is.
- Where a specification has been **obsoleted**, both numbers appear. Citing
  only the famous one is how a reference page goes quietly out of date.

Canonical text for any of these: `https://www.rfc-editor.org/rfc/rfcNNNN`.
The Best Current Practice index is at
<https://www.rfc-editor.org/standards#BCP>.

---

## Foundations

These are not implemented here — the operating system provides them — but
everything in `Linen.Network` depends on their behaviour, and the phrasing of
`Linen.Data.IP` follows their address semantics.

| RFC | Title | Bearing on `linen` |
| --- | --- | --- |
| [791](https://www.rfc-editor.org/rfc/rfc791) | Internet Protocol | IPv4 addressing and the 32-bit layout `Data.IP` stores as a `UInt32` |
| [793](https://www.rfc-editor.org/rfc/rfc793) | Transmission Control Protocol | The original TCP. **Obsoleted by [9293](https://www.rfc-editor.org/rfc/rfc9293)** (2022), which is the one to read; 793 remains the reference everyone cites |
| [1122](https://www.rfc-editor.org/rfc/rfc1122) | Requirements for Internet Hosts — Communication Layers | What a host must do, as opposed to what the wire format is. The source of the robustness principle, and of most of the socket behaviour `Network.Socket`'s lifecycle states model |
| [8200](https://www.rfc-editor.org/rfc/rfc8200) | Internet Protocol, Version 6 | IPv6 addressing; `Data.IP` stores an address as a pair of `UInt64` |
| [8446](https://www.rfc-editor.org/rfc/rfc8446) | The Transport Layer Security Protocol Version 1.3 | The protocol `Network.TLS` speaks. `linen` does not implement it: `ffi/tls.c` binds OpenSSL, which does |

## Implemented here

Grouped by subsystem. Every entry is cited in the implementing module.

### HTTP

| RFC | Title | Module |
| --- | --- | --- |
| [9110](https://www.rfc-editor.org/rfc/rfc9110) | HTTP Semantics | `Network.HTTP.*` |
| [7231](https://www.rfc-editor.org/rfc/rfc7231) | HTTP/1.1 Semantics and Content (obsoleted by 9110) | `Network.HTTP.*` |
| [9113](https://www.rfc-editor.org/rfc/rfc9113) | HTTP/2 | `Network.HTTP2.*` |
| [7541](https://www.rfc-editor.org/rfc/rfc7541) | HPACK: Header Compression for HTTP/2 | `Network.HTTP2.HPACK` |
| [9114](https://www.rfc-editor.org/rfc/rfc9114) | HTTP/3 | `Network.HTTP3.*` |
| [9204](https://www.rfc-editor.org/rfc/rfc9204) | QPACK: Field Compression for HTTP/3 | `Network.HTTP3.QPACK` |
| [5789](https://www.rfc-editor.org/rfc/rfc5789) | PATCH Method for HTTP | `Network.HTTP.Types` |
| [6265](https://www.rfc-editor.org/rfc/rfc6265) | HTTP State Management (cookies) | `Network.HTTP.Cookie` |
| [6455](https://www.rfc-editor.org/rfc/rfc6455) | The WebSocket Protocol | `Network.WebSockets.*` |

### QUIC

| RFC | Title | Module |
| --- | --- | --- |
| [9000](https://www.rfc-editor.org/rfc/rfc9000) | QUIC: A UDP-Based Multiplexed and Secure Transport | `Network.QUIC.*` |
| [9369](https://www.rfc-editor.org/rfc/rfc9369) | QUIC Version 2 | `Network.QUIC.*` |

### Identity and tokens

| RFC | Title | Module |
| --- | --- | --- |
| [7515](https://www.rfc-editor.org/rfc/rfc7515) | JSON Web Signature (JWS) | `Crypto.JOSE.JWS` |
| [7517](https://www.rfc-editor.org/rfc/rfc7517) | JSON Web Key (JWK) | `Crypto.JOSE.JWK` |
| [7518](https://www.rfc-editor.org/rfc/rfc7518) | JSON Web Algorithms (JWA) | `Crypto.JOSE.*` |
| [7519](https://www.rfc-editor.org/rfc/rfc7519) | JSON Web Token (JWT) | `Crypto.JOSE.JWT` |
| [7523](https://www.rfc-editor.org/rfc/rfc7523) | JWT Profile for OAuth 2.0 Client Authentication and Authorization Grants | `Cloud.Credentials.Gcp` — the JWT-bearer grant that turns a service-account key into an access token |
| [6749](https://www.rfc-editor.org/rfc/rfc6749) | The OAuth 2.0 Authorization Framework | `Network.OAuth2.*` |
| [6750](https://www.rfc-editor.org/rfc/rfc6750) | OAuth 2.0 Bearer Token Usage | `Network.OAuth2.*`, `Cloud.Auth` |
| [7636](https://www.rfc-editor.org/rfc/rfc7636) | PKCE for OAuth Public Clients | `Network.OAuth2.*` |
| [8628](https://www.rfc-editor.org/rfc/rfc8628) | OAuth 2.0 Device Authorization Grant | `Network.OAuth2.*` |

### Formats and encodings

| RFC | Title | Module |
| --- | --- | --- |
| [8259](https://www.rfc-editor.org/rfc/rfc8259) | JSON | `Data.Json.*` |
| [3986](https://www.rfc-editor.org/rfc/rfc3986) | URI Generic Syntax | `Network.URI`, `Network.HTTP.Types.URI` |
| [4648](https://www.rfc-editor.org/rfc/rfc4648) | Base16, Base32, Base64 Encodings | `Data.Base64`, `Data.Hex` |
| [3339](https://www.rfc-editor.org/rfc/rfc3339) | Date and Time on the Internet: Timestamps | `Data.Time.*` |
| [4180](https://www.rfc-editor.org/rfc/rfc4180) | Common Format for CSV Files | `Data.Csv` |
| [1950](https://www.rfc-editor.org/rfc/rfc1950) | ZLIB Compressed Data Format | `Codec.Zlib` |
| [1321](https://www.rfc-editor.org/rfc/rfc1321) | The MD5 Message-Digest Algorithm | `Crypto.Hash.MD5` |
| [8536](https://www.rfc-editor.org/rfc/rfc8536) | The TZif Time Zone Information Format | `Data.Time.Zone` |
| [1123](https://www.rfc-editor.org/rfc/rfc1123) | Requirements for Internet Hosts — Application and Support | Hostname syntax; HTTP date format |
| [850](https://www.rfc-editor.org/rfc/rfc850) | Standard for Interchange of USENET Messages | The obsolete HTTP date format a server must still parse |
| [1918](https://www.rfc-editor.org/rfc/rfc1918) | Address Allocation for Private Internets | `Data.IP` — the private ranges |

## Not implemented, and why they are here anyway

`linen` speaks to networks; it does not build them. These specify the layers
underneath and are listed because knowing where the library stops is part of
knowing what it does.

| RFC | Title | Note |
| --- | --- | --- |
| [3031](https://www.rfc-editor.org/rfc/rfc3031) | Multiprotocol Label Switching Architecture | Forwarding beneath IP. Nothing here is aware of it |
| [4271](https://www.rfc-editor.org/rfc/rfc4271) | A Border Gateway Protocol 4 (BGP-4) | Inter-domain routing. `Data.IP`'s longest-prefix-match lookup is the same *operation* a router performs, on a table `linen` does not participate in building |
| [7938](https://www.rfc-editor.org/rfc/rfc7938) | Use of BGP for Routing in Large-Scale Data Centers | Why the network a `Cloud` call crosses is shaped the way it is |

## Worth reading

Not specifications of anything. They explain why the specifications look the
way they do, which is harder to find and more durable than any single protocol.

- **[RFC 970](https://www.rfc-editor.org/rfc/rfc970) — On Packet Switches With
  Infinite Storage** (Nagle, 1985). That adding buffer does not remove
  congestion but converts it into delay. The argument behind every timeout and
  queue bound in this library; `Cloud.Transport`'s retry policy and
  `Control.Concurrent.QSem`'s bounds are both instances of the lesson.
- **[RFC 5218](https://www.rfc-editor.org/rfc/rfc5218) — What Makes for a
  Successful Protocol?** Why protocols succeed, and the hazard of "wild
  success": a design that works beyond its assumptions gets used beyond them.
  Relevant to any library that must keep working when its guarantees are
  leaned on harder than intended.
- **[RFC 8890](https://www.rfc-editor.org/rfc/rfc8890) — The Internet is for
  End Users.** That architectural choices are political ones, and whose
  interests a protocol serves when they conflict.
- **[BCP 9](https://www.rfc-editor.org/info/bcp9) — The Internet Standards
  Process** ([RFC 2026](https://www.rfc-editor.org/rfc/rfc2026) and its
  updates). What "Proposed Standard" and "Internet Standard" actually mean,
  which is the difference between a specification being agreed and being
  merely written down.
- **[BCP 95](https://www.rfc-editor.org/info/bcp95) — A Mission Statement for
  the IETF** ([RFC 3935](https://www.rfc-editor.org/rfc/rfc3935)). "Rough
  consensus and running code", stated by the body that means it.

## Adding to this page

When a module starts implementing a specification, cite it in that module's
doc-comment and add a row here. When a specification is obsoleted, keep both
numbers: the new one is what to implement, the old one is what the rest of the
world will keep calling it.
