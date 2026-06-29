/-
  Linen test suite root.

  Mirrors the `Linen/` source tree under `Tests/Linen/`. Each module asserts
  correctness with `#guard`, so building this library runs every check.

      lake build Tests
-/
import Tests.Linen.Control.ApplicativeTest
import Tests.Linen.Control.ArrowTest
import Tests.Linen.Control.ConcurrentTest
import Tests.Linen.Control.ExceptionTest
import Tests.Linen.Control.AutoUpdateTest
import Tests.Linen.Control.CategoryTest
import Tests.Linen.Control.Concurrent.ChanTest
import Tests.Linen.Control.Concurrent.GreenTest
import Tests.Linen.Control.Concurrent.MVarTest
import Tests.Linen.Control.Concurrent.QSemTest
import Tests.Linen.Control.Concurrent.QSemNTest
import Tests.Linen.Control.MonadTest
import Tests.Linen.Data.Base64Test
import Tests.Linen.Data.BifunctorTest
import Tests.Linen.Data.ByteStringTest
import Tests.Linen.Data.ByteString.Char8Test
import Tests.Linen.Data.ByteString.LazyTest
import Tests.Linen.Data.ByteString.Lazy.Char8Test
import Tests.Linen.Data.ByteString.ShortTest
import Tests.Linen.Data.ByteString.BuilderTest
import Tests.Linen.Data.CaseInsensitiveTest
import Tests.Linen.Data.Conduit.Internal.PipeTest
import Tests.Linen.Data.Configurator.TypesTest
import Tests.Linen.Data.ConfiguratorTest
import Tests.Linen.Data.BitsTest
import Tests.Linen.Data.BoolTest
import Tests.Linen.Data.FoldableTest
import Tests.Linen.Data.CharTest
import Tests.Linen.Data.ComplexTest
import Tests.Linen.Data.FixedTest
import Tests.Linen.Data.FunctionTest
import Tests.Linen.Data.FunctorTest
import Tests.Linen.Data.IxTest
import Tests.Linen.Data.ListTest
import Tests.Linen.Data.List.NonEmptyTest
import Tests.Linen.Data.NewtypeTest
import Tests.Linen.Data.OrdTest
import Tests.Linen.Data.ProxyTest
import Tests.Linen.Data.RatTest
import Tests.Linen.Data.StringTest
import Tests.Linen.Data.TraversableTest
import Tests.Linen.Data.UniqueTest
import Tests.Linen.Data.VoidTest
import Tests.Linen.Data.Json.TypesTest
import Tests.Linen.Data.Json.EncodeTest
import Tests.Linen.Data.Json.DecodeTest
import Tests.Linen.System.Console.AnsiTest
import Tests.Linen.System.ExitTest
import Tests.Linen.Network.HTTP.ChunkedTest
import Tests.Linen.Network.Socket.TypesTest
import Tests.Linen.Network.Socket.FFITest
import Tests.Linen.Network.SocketTest
import Tests.Linen.Network.Socket.EventDispatcherTest
