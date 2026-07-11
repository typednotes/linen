/-
  Linen.Data.Array.Shaped.Operators.Traversal — generic array traversal

  Ported from Haskell's `Data.Array.Repa.Operators.Traversal` (package
  `repa`). Unstructured traversal builds a new `Delayed` array by combining
  the extents and elements of one to four source arrays, given lookup
  functions into each.
-/

import Linen.Data.Array.Shaped.Repr.Delayed

namespace Data.Array.Shaped

/-- Unstructured traversal. -/
def traverse {arr sh sh' e1 e2} [Shape sh] [Inhabited e1] [Source arr]
    (a : arr sh e1) (transExtent : sh → sh') (newElem : (sh → e1) → sh' → e2) :
    Delayed sh' e2 :=
  fromFunction (transExtent (Source.extent a)) (newElem (fun ix => index a ix))

/-- Unstructured traversal, without bounds checking. -/
def unsafeTraverse {arr sh sh' e1 e2} [Shape sh] [Inhabited e1] [Source arr]
    (a : arr sh e1) (transExtent : sh → sh') (newElem : (sh → e1) → sh' → e2) :
    Delayed sh' e2 :=
  fromFunction (transExtent (Source.extent a)) (newElem (fun ix => unsafeIndex a ix))

/-- Unstructured traversal over two arrays at once. -/
def traverse2 {arr1 arr2 sh1 sh2 sh3 e1 e2 e3}
    [Shape sh1] [Shape sh2] [Inhabited e1] [Inhabited e2] [Source arr1] [Source arr2]
    (a1 : arr1 sh1 e1) (a2 : arr2 sh2 e2)
    (transExtent : sh1 → sh2 → sh3) (newElem : (sh1 → e1) → (sh2 → e2) → sh3 → e3) :
    Delayed sh3 e3 :=
  fromFunction (transExtent (Source.extent a1) (Source.extent a2))
    (newElem (fun ix => index a1 ix) (fun ix => index a2 ix))

/-- Unstructured traversal over two arrays at once, without bounds checking. -/
def unsafeTraverse2 {arr1 arr2 sh1 sh2 sh3 e1 e2 e3}
    [Shape sh1] [Shape sh2] [Inhabited e1] [Inhabited e2] [Source arr1] [Source arr2]
    (a1 : arr1 sh1 e1) (a2 : arr2 sh2 e2)
    (transExtent : sh1 → sh2 → sh3) (newElem : (sh1 → e1) → (sh2 → e2) → sh3 → e3) :
    Delayed sh3 e3 :=
  fromFunction (transExtent (Source.extent a1) (Source.extent a2))
    (newElem (fun ix => unsafeIndex a1 ix) (fun ix => unsafeIndex a2 ix))

/-- Unstructured traversal over three arrays at once. -/
def traverse3 {arr1 arr2 arr3 sh1 sh2 sh3 sh4 e1 e2 e3 e4}
    [Shape sh1] [Shape sh2] [Shape sh3]
    [Inhabited e1] [Inhabited e2] [Inhabited e3]
    [Source arr1] [Source arr2] [Source arr3]
    (a1 : arr1 sh1 e1) (a2 : arr2 sh2 e2) (a3 : arr3 sh3 e3)
    (transExtent : sh1 → sh2 → sh3 → sh4)
    (newElem : (sh1 → e1) → (sh2 → e2) → (sh3 → e3) → sh4 → e4) :
    Delayed sh4 e4 :=
  fromFunction (transExtent (Source.extent a1) (Source.extent a2) (Source.extent a3))
    (newElem (fun ix => index a1 ix) (fun ix => index a2 ix) (fun ix => index a3 ix))

/-- Unstructured traversal over three arrays at once, without bounds checking. -/
def unsafeTraverse3 {arr1 arr2 arr3 sh1 sh2 sh3 sh4 e1 e2 e3 e4}
    [Shape sh1] [Shape sh2] [Shape sh3]
    [Inhabited e1] [Inhabited e2] [Inhabited e3]
    [Source arr1] [Source arr2] [Source arr3]
    (a1 : arr1 sh1 e1) (a2 : arr2 sh2 e2) (a3 : arr3 sh3 e3)
    (transExtent : sh1 → sh2 → sh3 → sh4)
    (newElem : (sh1 → e1) → (sh2 → e2) → (sh3 → e3) → sh4 → e4) :
    Delayed sh4 e4 :=
  fromFunction (transExtent (Source.extent a1) (Source.extent a2) (Source.extent a3))
    (newElem (fun ix => unsafeIndex a1 ix) (fun ix => unsafeIndex a2 ix)
      (fun ix => unsafeIndex a3 ix))

/-- Unstructured traversal over four arrays at once. -/
def traverse4 {arr1 arr2 arr3 arr4 sh1 sh2 sh3 sh4 sh5 e1 e2 e3 e4 e5}
    [Shape sh1] [Shape sh2] [Shape sh3] [Shape sh4]
    [Inhabited e1] [Inhabited e2] [Inhabited e3] [Inhabited e4]
    [Source arr1] [Source arr2] [Source arr3] [Source arr4]
    (a1 : arr1 sh1 e1) (a2 : arr2 sh2 e2) (a3 : arr3 sh3 e3) (a4 : arr4 sh4 e4)
    (transExtent : sh1 → sh2 → sh3 → sh4 → sh5)
    (newElem : (sh1 → e1) → (sh2 → e2) → (sh3 → e3) → (sh4 → e4) → sh5 → e5) :
    Delayed sh5 e5 :=
  fromFunction
    (transExtent (Source.extent a1) (Source.extent a2) (Source.extent a3) (Source.extent a4))
    (newElem (fun ix => index a1 ix) (fun ix => index a2 ix) (fun ix => index a3 ix)
      (fun ix => index a4 ix))

/-- Unstructured traversal over four arrays at once, without bounds checking. -/
def unsafeTraverse4 {arr1 arr2 arr3 arr4 sh1 sh2 sh3 sh4 sh5 e1 e2 e3 e4 e5}
    [Shape sh1] [Shape sh2] [Shape sh3] [Shape sh4]
    [Inhabited e1] [Inhabited e2] [Inhabited e3] [Inhabited e4]
    [Source arr1] [Source arr2] [Source arr3] [Source arr4]
    (a1 : arr1 sh1 e1) (a2 : arr2 sh2 e2) (a3 : arr3 sh3 e3) (a4 : arr4 sh4 e4)
    (transExtent : sh1 → sh2 → sh3 → sh4 → sh5)
    (newElem : (sh1 → e1) → (sh2 → e2) → (sh3 → e3) → (sh4 → e4) → sh5 → e5) :
    Delayed sh5 e5 :=
  fromFunction
    (transExtent (Source.extent a1) (Source.extent a2) (Source.extent a3) (Source.extent a4))
    (newElem (fun ix => unsafeIndex a1 ix) (fun ix => unsafeIndex a2 ix)
      (fun ix => unsafeIndex a3 ix) (fun ix => unsafeIndex a4 ix))

end Data.Array.Shaped
