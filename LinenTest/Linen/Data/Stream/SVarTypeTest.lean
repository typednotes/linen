import Linen.Data.Stream.SVarType

open Data.Stream

-- The default state has no yield limit and the magic buffer/thread ceilings.
#guard defState.yieldLimit == none
#guard getMaxBuffer defState == Limit.limited 1500
#guard getMaxThreads defState == Limit.limited 1500
#guard getInspectMode defState == false

-- `setYieldLimit` clamps non-positive to `0` and clears on `none`.
#guard getYieldLimit (setYieldLimit (some 10) defState) == some 10
#guard getYieldLimit (setYieldLimit (some (-3)) defState) == some 0
#guard getYieldLimit (setYieldLimit none defState) == none

-- `setMaxBuffer`: negative → unlimited, zero → default, positive → limited.
#guard getMaxBuffer (setMaxBuffer (-1) defState) == Limit.unlimited
#guard getMaxBuffer (setMaxBuffer 0 defState) == Limit.limited 1500
#guard getMaxBuffer (setMaxBuffer 42 defState) == Limit.limited 42

-- `adaptState` resets the one-shot yield limit but keeps persistent config.
#guard getYieldLimit (adaptState (setYieldLimit (some 5) (setMaxBuffer 9 defState))) == none
#guard getMaxBuffer (adaptState (setMaxBuffer 9 defState)) == Limit.limited 9

-- Latency clamps non-positive to `none`; inspect mode is a one-way switch.
#guard getStreamLatency (setStreamLatency 100 defState) == some 100
#guard getStreamLatency (setStreamLatency 0 defState) == none
#guard getInspectMode (setInspectMode defState) == true
