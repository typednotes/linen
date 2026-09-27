/-
  `Control.Reactive` — typed reactive graphs: DAGs of observables

  A reactive graph is a DAG of **observables** (ReactiveX): inputs are
  streams of events, and every node is a stream of its own, computed by an
  operator from earlier ones. The graph itself is an observable of its inputs,
  and composes like one.

  ```lean
  def sheet : Reactive Id V (Observable Nat) := do
    node clicks ← subject Nat
    node doubled ← clicks.map (· * 2)
    node total ← doubled.scan 0 (· + ·)
    node quiet ← total.debounceTime 100
    combineLatest (fun (a b : Nat) => a + b) quiet doubled

  -- every node's stream, over virtual time
  sheet.graph!.run [.next clicks 0 1, .next clicks 30 2, .complete clicks 500]
  ```

  The library is three modules, re-exported here:

  - `Control.Reactive.Graph` — the data: operators (`Op`), nodes, the function
    table and labels, with the proofs every graph carries (`WellFormed`:
    acyclic, arities right, functions registered; `Labelled`: labels unique).
  - `Control.Reactive.Builder` — writing graphs in the `Reactive` monad:
    `subject`, the operators (`map`, `filter`, `scan`, `take`, `skip`,
    `distinctUntilChanged`, `merge`, `mergeWith`, `combineLatest`,
    `withLatestFrom`, `zip`, `throttleTime`, `debounceTime`, `delay`),
    graphs as operators (`Operator`), labels (`node x ← e`, `scope`).
  - `Control.Reactive.Run` — the meaning: occurrences in, every node's stream
    out (`Trace`), over virtual time, incrementally (`Session`) or at once
    (`run`).

  Serialisation and drawing are in `Control.Reactive.Json` and
  `Control.Reactive.Graphviz`.

  ## Design and naming

  - **Names are ReactiveX's** (RxJS, rxRust): `Observable`, `Subject`,
    `Notification` (`next`/`error`/`complete`), and the operators above, with
    their usual meaning.
  - **Semantics are reactive-banana's**: time advances in instants; in an
    instant each node, visited in topological order, emits at most one value
    (then possibly ends), so there are no glitches and simultaneity is
    explicit (`merge` is left-biased, `mergeWith f` combines). Runs are pure
    and deterministic, which makes a recorded log of occurrences a complete
    description of a run — replayed against a new version of a graph, it
    brings that version up to date.
  - **Time is virtual**: occurrences carry timestamps, timed operators
    (`throttleTime`, `debounceTime`, `delay`) are exact and replayable, and a
    run ends by letting time pass until no timer is left.
  - Like observablehq's notebooks, nodes are named after the identifiers that
    define them (`node x ← e`), and a graph can be drawn (`Graphviz`) and
    stored (`Json`).
  - Every node is a hot, shared observable: it is computed once per instant
    and read by all its dependents, so ReactiveX's cold/hot and `share`
    distinctions do not arise.
  - **Why not `Control.Arrow`.** Arrows are the classic abstraction for static
    dataflow, but are written point-free without `proc` notation, which Lean
    lacks; `Reactive` names intermediate streams with ordinary `do`-notation.
  - **Why not linen's `Data.Stream`.** streamly's streams are pull-based and
    linear; a reactive graph is push-based and shares every node among its
    dependents. `Data.Stream` remains the way to produce occurrences from an
    effectful source.
-/
import Linen.Control.Reactive.Graph
import Linen.Control.Reactive.Builder
import Linen.Control.Reactive.Run
