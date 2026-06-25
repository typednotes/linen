/-
  Linen test suite root.

  Mirrors the `Linen/` source tree under `Tests/Linen/`. Each module asserts
  correctness with `#guard`, so building this library runs every check.

      lake build Tests
-/
import Tests.Linen.Control.ApplicativeTest
import Tests.Linen.Control.ConcurrentTest
import Tests.Linen.Control.AutoUpdateTest
import Tests.Linen.Control.CategoryTest
import Tests.Linen.Control.Concurrent.ChanTest
import Tests.Linen.Control.Concurrent.GreenTest
import Tests.Linen.Control.Concurrent.MVarTest
import Tests.Linen.Control.Concurrent.QSemTest
import Tests.Linen.Control.Concurrent.QSemNTest
import Tests.Linen.Control.MonadTest
import Tests.Linen.Data.BifunctorTest
import Tests.Linen.Data.BitsTest
import Tests.Linen.Data.BoolTest
import Tests.Linen.Data.CharTest
import Tests.Linen.Data.FunctorTest
import Tests.Linen.Data.Json.TypesTest
import Tests.Linen.Data.Json.EncodeTest
import Tests.Linen.Data.Json.DecodeTest
import Tests.Linen.System.Console.AnsiTest
import Tests.Linen.Network.Socket.TypesTest
import Tests.Linen.Network.Socket.FFITest
import Tests.Linen.Network.SocketTest
import Tests.Linen.Network.Socket.EventDispatcherTest
