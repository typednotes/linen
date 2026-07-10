# `pdf-toolbox-document` module dependencies

Topological order of every module of the
[`pdf-toolbox-document`](https://hackage.haskell.org/package/pdf-toolbox-document)
Hackage package, source at [Yuras/pdf-toolbox](https://github.com/Yuras/pdf-toolbox)
(`document/` subdirectory), imported into `linen` per
[AGENTS.md](../../AGENTS.md)'s Hackage-import convention. Depends on
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md) and
[`pdf-toolbox-content`](../PdfToolboxContent/dependencies.md).

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

All 11 exposed modules are ported. `Pdf.Document.Types` is a one-line
pass-through re-export of `Pdf.Core.Types` upstream — in the Lean port this is
just a direct re-export/alias, not a duplicated definition.

Two places in this package walk the underlying PDF object graph recursively
with **no cycle guard in upstream Haskell**: `Pdf.Document.PageNode.pageNodePageByNum`/
`loadPageNode` (descending the page tree via untrusted `/Kids`/`/Count`
fields) and `Pdf.Document.Page`'s `mediaBoxRec` (ascending via `/Parent`) and
`dictXObjects` (descending nested Form-XObject `/Resources/XObject`
dictionaries, where `XObject` is itself a genuinely self-referential type,
`XObject` containing a `Map Name XObject` of its own children). Because the
recursion is driven by live, adversarially-controllable PDF data rather than
a bounded Lean-side inductive structure, no purely structural termination
measure exists over the raw object graph — a malicious/malformed PDF's
`Kids`/`Parent`/nested-XObject graph can be cyclic, and upstream simply loops
forever on such input. Per AGENTS.md's rule against dodging genuine
termination proofs with fuel, the Lean port instead threads an explicit
*visited-`Ref`-set* through each of these three walks: termination follows
from "the set of distinct object references in a file is finite and strictly
grows on each new visit," which is a real, provable measure (not a fuel
parameter standing in for one), and doubles as a fix for the same
unbounded-recursion class of bug upstream has.

`Pdf.Document.Page.combinedContent`'s hand-rolled `Streams.fromGenerator`
producer/consumer coroutine (chaining multiple content streams into one) is
ported as a plain, eagerly-concatenated `Data.PDF.Stream.InputStream` (see
[`IoStreams`](../IoStreams/dependencies.md)'s scope note on why linen's
streams are buffer-resident, not incrementally generator-driven) —
observationally equivalent for this consumer, not a behavior change.

`Pdf.Document.Info`'s six near-identical single-string-field accessors
(`infoTitle`/`infoAuthor`/`infoSubject`/`infoKeywords`/`infoCreator`/
`infoProducer`) are ported through one shared parametrized helper rather than
six copies of the same lookup/deref/decode logic — a direct de-duplication of
upstream's own repetition, not new abstraction beyond what the task needs.

## External dependencies

Beyond what `linen`/`pdf-toolbox-core`/`pdf-toolbox-content` already cover
(`base`, `bytestring`, `text`, `containers`, `io-streams`): none — no further
new Hackage prerequisites. `Data.IORef` (the per-`Pdf`-handle mutable object
cache) maps to a plain `IO.Ref`.

## Topologically sorted modules

<!-- 1. `Pdf.Document.Types` -->
<!-- 2. `Pdf.Document.Internal.Types` -->
<!-- 3. `Pdf.Document.Internal.Util` -->
<!-- 4. `Pdf.Document.Pdf` -->
<!-- 5. `Pdf.Document.Document` -->
<!-- 6. `Pdf.Document.Info` -->
<!-- 7. `Pdf.Document.Catalog` -->
<!-- 8. `Pdf.Document.PageNode` -->
<!-- 9. `Pdf.Document.FontDict` -->
<!-- 10. `Pdf.Document.Page` -->
<!-- 11. `Pdf.Document` — package aggregator -->
