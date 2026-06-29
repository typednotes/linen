# Hale module dependencies

Dependency graph and topological order of every module under [`Hale/`](../../hale/Hale), derived from the `import Hale.*` statements in each source file (imports inside comments/docstrings are ignored).

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Summary

- **Modules (nodes):** 344
- **Source files scanned:** 344
- **Dependency edges:** 675
- **Cycles (strongly-connected components > 1):** 0 → the graph is a DAG.

## Graph

The full Graphviz source is in [`module-dependencies.dot`](module-dependencies.dot); a rendered version is in [`module-dependencies.svg`](module-dependencies.svg). Regenerate either with:

```sh
python3 docs/depgraph.py            # rebuild .dot + .md
dot -Tsvg docs/module-dependencies.dot -o docs/module-dependencies.svg
```

## Topologically sorted modules

Each module is listed after all modules it imports. The order is **prioritised to reach `Hale.Network.Network.Socket.EventDispatcher` as early as possible**: already-ported modules (commented out) come first, then EventDispatcher's remaining dependency chain, then everything else. Within a tier, ordering is alphabetical.

<!-- 1. `Hale.Aeson.Data.Aeson.Types` -->
<!-- 2. `Hale.Aeson.Data.Aeson.Decode` -->
<!-- 3. `Hale.Aeson.Data.Aeson.Encode` -->
<!-- 4. `Hale.Aeson.Data.Aeson` -->
<!-- 5. `Hale.Aeson` -->
<!-- 6. `Hale.AnsiTerminal.System.Console.ANSI` -->
<!-- 7. `Hale.AnsiTerminal` -->
<!-- 8. `Hale.AutoUpdate.Control.AutoUpdate` -->
<!-- 9. `Hale.AutoUpdate` -->
<!-- 10. `Hale.Base.Control.Applicative` -->
<!-- 11. `Hale.Base.Control.Category` -->
<!-- 12. `Hale.Base.Control.Concurrent.MVar` -->
<!-- 13. `Hale.Base.Control.Concurrent.Chan` -->
<!-- 14. `Hale.Base.Control.Concurrent.QSem` -->
<!-- 15. `Hale.Base.Control.Concurrent.QSemN` -->
<!-- 16. `Hale.Base.Control.Concurrent.Green` -->
<!-- 17. `Hale.Base.Control.Concurrent.Scheduler` -->
<!-- 18. `Hale.Base.Control.Concurrent` -->
<!-- 19. `Hale.Base.Control.Monad` -->
<!-- 20. `Hale.Base.Data.Bifunctor` -->
<!-- 21. `Hale.Base.Data.Bits` -->
<!-- 22. `Hale.Base.Data.Bool` -->
<!-- 23. `Hale.Base.Data.Char` -->
<!-- 24. `Hale.Base.Data.Complex` -->
<!-- 25. `Hale.Base.Data.Either` -->
<!-- 26. `Hale.Base.Control.Arrow` -->
<!-- 27. `Hale.Base.Control.Exception` -->
<!-- 28. `Hale.Base.Data.Function` -->
<!-- 29. `Hale.Base.Data.Functor.Compose` -->
<!-- 30. `Hale.Base.Data.Functor.Const` -->
<!-- 31. `Hale.Base.Data.Functor.Contravariant` -->
<!-- 32. `Hale.Base.Data.Functor.Identity` -->
<!-- 33. `Hale.Base.Data.Functor.Product` -->
<!-- 34. `Hale.Base.Data.Functor.Sum` -->
<!-- 35. `Hale.Base.Data.IORef` -->
<!-- 36. `Hale.Base.Data.Ix` -->
<!-- 37. `Hale.Base.Data.List.NonEmpty` -->
<!-- 38. `Hale.Base.Data.Foldable` -->
<!-- 39. `Hale.Base.Data.List` -->
<!-- 40. `Hale.Base.Data.Maybe` -->
<!-- 41. `Hale.Base.Data.Newtype` -->
<!-- 42. `Hale.Base.Data.Ord` -->
<!-- 43. `Hale.Base.Data.Proxy` -->
<!-- 44. `Hale.Base.Data.Ratio` -->
<!-- 45. `Hale.Base.Data.Fixed` -->
<!-- 46. `Hale.Base.Data.String` -->
<!-- 47. `Hale.Base.Data.Traversable` -->
<!-- 48. `Hale.Base.Data.Tuple` -->
<!-- 49. `Hale.Base.Data.Unique` -->
<!-- 50. `Hale.Base.Data.Void` -->
<!-- 51. `Hale.Base.System.Environment` -->
<!-- 52. `Hale.Base.System.Exit` -->
<!-- 53. `Hale.Base.System.IO` -->
<!-- 54. `Hale.Base` -->
<!-- 55. `Hale.Base64.Data.ByteString.Base64` -->
<!-- 56. `Hale.Base64` -->
<!-- 57. `Hale.BsbHttpChunked.Network.HTTP.Chunked` -->
<!-- 58. `Hale.BsbHttpChunked` -->
<!-- 59. `Hale.ByteString.Data.ByteString.Internal` -->
<!-- 60. `Hale.ByteString.Data.ByteString` -->
<!-- 61. `Hale.ByteString.Data.ByteString.Char8` -->
<!-- 62. `Hale.ByteString.Data.ByteString.Lazy.Internal` -->
<!-- 63. `Hale.ByteString.Data.ByteString.Lazy` -->
<!-- 64. `Hale.ByteString.Data.ByteString.Lazy.Char8` -->
<!-- 65. `Hale.ByteString.Data.ByteString.Short` -->
<!-- 66. `Hale.ByteString.Data.ByteString.Builder` -->
<!-- 67. `Hale.ByteString` -->
<!-- 68. `Hale.CaseInsensitive.Data.CaseInsensitive` -->
<!-- 69. `Hale.CaseInsensitive` -->
<!-- 70. `Hale.Conduit.Data.Conduit.Internal.Pipe` -->
<!-- 71. `Hale.ConfiguratorPg.Data.Configurator.Types` -->
<!-- 72. `Hale.ConfiguratorPg.Data.Configurator` -->
<!-- 73. `Hale.ConfiguratorPg` -->
<!-- 74. `Hale.Network.Network.Socket.Types` -->
<!-- 75. `Hale.Network.Network.Socket.FFI` -->
<!-- 76. `Hale.Network.Network.Socket` -->
<!-- 77. `Hale.Network.Network.Socket.EventDispatcher` -->
78. `Hale.Containers.Data.IntMap`
79. `Hale.Containers.Data.Map`
80. `Hale.Containers.Data.Map.Strict`
81. `Hale.Containers.Data.Set`
82. `Hale.Containers`
83. `Hale.Cookie.Web.Cookie`
84. `Hale.Cookie`
85. `Hale.DataDefault.Data.Default`
86. `Hale.DataDefault`
87. `Hale.DataFrame.DataFrame.Internal.Types`
88. `Hale.DataFrame.DataFrame.IO.CSV`
89. `Hale.DataFrame.DataFrame.Internal.Column`
90. `Hale.DataFrame.DataFrame.Display`
91. `Hale.DataFrame.DataFrame.Operations.Join`
92. `Hale.DataFrame.DataFrame.Operations.Sort`
93. `Hale.DataFrame.DataFrame.Operations.Statistics`
94. `Hale.DataFrame.DataFrame.Operations.Aggregation`
95. `Hale.DataFrame.DataFrame.Operations.Subset`
96. `Hale.DataFrame.DataFrame.Operations.Transform`
97. `Hale.DataFrame.DataFrame`
98. `Hale.DataFrame`
99. `Hale.FastLogger.System.Log.FastLogger`
100. `Hale.FastLogger`
101. `Hale.Hasql.Database.PostgreSQL.LibPQ.Types`
102. `Hale.Hasql.Database.PostgreSQL.LibPQ`
103. `Hale.Hasql.Hasql.Connection`
104. `Hale.Hasql.Hasql.Encoders`
105. `Hale.Hasql.Hasql.Session`
106. `Hale.Hasql.Hasql.Decoders`
107. `Hale.Hasql.Hasql.Pool`
108. `Hale.Hasql.Hasql.Statement`
109. `Hale.Hasql`
110. `Hale.Http2.Network.HTTP2.Frame.Types`
111. `Hale.Http2.Network.HTTP2.Frame.Decode`
112. `Hale.Http2.Network.HTTP2.Frame.Encode`
113. `Hale.Http2.Network.HTTP2.HPACK.Huffman`
114. `Hale.Http2.Network.HTTP2.HPACK.Table`
115. `Hale.Http2.Network.HTTP2.HPACK.Decode`
116. `Hale.Http2.Network.HTTP2.HPACK.Encode`
117. `Hale.Http2.Network.HTTP2.Types`
118. `Hale.Http2.Network.HTTP2.Stream`
119. `Hale.Http2.Network.HTTP2.FlowControl`
120. `Hale.Http2.Network.HTTP2.Server`
121. `Hale.Http2`
122. `Hale.Http3.Network.HTTP3.Error`
123. `Hale.Http3.Network.HTTP3.Frame`
124. `Hale.Http3.Network.HTTP3.QPACK.Table`
125. `Hale.Http3.Network.HTTP3.QPACK.Decode`
126. `Hale.Http3.Network.HTTP3.QPACK.Encode`
127. `Hale.HttpDate.Network.HTTP.Date`
128. `Hale.HttpDate`
129. `Hale.HttpTypes.Network.HTTP.Types.Header`
130. `Hale.HttpTypes.Network.HTTP.Types.Method`
131. `Hale.HttpTypes.Network.HTTP.Types.Status`
132. `Hale.HttpTypes.Network.HTTP.Types.URI`
133. `Hale.HttpTypes.Network.HTTP.Types.Version`
134. `Hale.HttpTypes`
135. `Hale.HttpClient.Network.HTTP.Client.Types`
136. `Hale.HttpClient.Network.HTTP.Client.Request`
137. `Hale.HttpClient.Network.HTTP.Client.Response`
138. `Hale.IpRoute.Data.IP`
139. `Hale.IpRoute`
140. `Hale.Jose.Crypto.JOSE.FFI`
141. `Hale.Jose.Crypto.JOSE.Types`
142. `Hale.Jose.Crypto.JOSE.JWK`
143. `Hale.Jose.Crypto.JOSE.JWS`
144. `Hale.Jose.Crypto.JOSE.JWT`
145. `Hale.Jose`
146. `Hale.MimeTypes.Network.Mime`
147. `Hale.MimeTypes`
148. `Hale.Mtl.Control.Monad.Except`
149. `Hale.Mtl.Control.Monad.Reader`
150. `Hale.Mtl.Control.Monad.State`
151. `Hale.Mtl.Control.Monad.Trans`
152. `Hale.Mtl`
153. `Hale.Network.Network.Socket.Blocking`
154. `Hale.Network.Network.Socket.ByteString`
155. `Hale.Network`
156. `Hale.OptParse.Options.Applicative.Types`
157. `Hale.OptParse.Options.Applicative.Builder`
158. `Hale.OptParse.Options.Applicative.Extra`
159. `Hale.OptParse.Options.Applicative`
160. `Hale.OptParse`
161. `Hale.PostgREST.PostgREST.ApiRequest.Preferences`
162. `Hale.PostgREST.PostgREST.Auth.Types`
163. `Hale.PostgREST.PostgREST.Auth`
164. `Hale.PostgREST.PostgREST.Cache.Sieve`
165. `Hale.PostgREST.PostgREST.Config.JSPath`
166. `Hale.PostgREST.PostgREST.Config.PgVersion`
167. `Hale.PostgREST.PostgREST.Config.Proxy`
168. `Hale.PostgREST.PostgREST.Cors`
169. `Hale.PostgREST.PostgREST.Debounce`
170. `Hale.PostgREST.PostgREST.Listener`
171. `Hale.PostgREST.PostgREST.Logger`
172. `Hale.PostgREST.PostgREST.MediaType`
173. `Hale.PostgREST.PostgREST.Network`
174. `Hale.PostgREST.PostgREST.RangeQuery`
175. `Hale.PostgREST.PostgREST.Response`
176. `Hale.PostgREST.PostgREST.Response.GucHeader`
177. `Hale.PostgREST.PostgREST.Response.Performance`
178. `Hale.PostgREST.PostgREST.SchemaCache.Identifiers`
179. `Hale.PostgREST.PostgREST.ApiRequest.Types`
180. `Hale.PostgREST.PostgREST.Config`
181. `Hale.PostgREST.PostgREST.Config.Database`
182. `Hale.PostgREST.PostgREST.Error.Types`
183. `Hale.PostgREST.PostgREST.Error`
184. `Hale.PostgREST.PostgREST.MainTx`
185. `Hale.PostgREST.PostgREST.Plan.Types`
186. `Hale.PostgREST.PostgREST.Query.SqlFragment`
187. `Hale.PostgREST.PostgREST.SchemaCache.Relationship`
188. `Hale.PostgREST.PostgREST.Plan.ReadPlan`
189. `Hale.PostgREST.PostgREST.Plan.MutatePlan`
190. `Hale.PostgREST.PostgREST.SchemaCache.Representations`
191. `Hale.PostgREST.PostgREST.SchemaCache.Routine`
192. `Hale.PostgREST.PostgREST.Plan.CallPlan`
193. `Hale.PostgREST.PostgREST.SchemaCache.Table`
194. `Hale.PostgREST.PostgREST.SchemaCache`
195. `Hale.PostgREST.PostgREST.AppState`
196. `Hale.PostgREST.PostgREST.Metrics`
197. `Hale.PostgREST.PostgREST.Admin`
198. `Hale.PostgREST.PostgREST.Observation`
199. `Hale.PostgREST.PostgREST.TimeIt`
200. `Hale.PostgREST.PostgREST.Unix`
201. `Hale.PostgREST.PostgREST.Version`
202. `Hale.PostgREST.PostgREST.App`
203. `Hale.PostgREST.PostgREST.CLI`
204. `Hale.PostgREST.PostgREST.Response.OpenAPI`
205. `Hale.PostgREST`
206. `Hale.QUIC.Network.QUIC.Types`
207. `Hale.QUIC.Network.QUIC.Config`
208. `Hale.QUIC.Network.QUIC.Connection`
209. `Hale.QUIC.Network.QUIC.Client`
210. `Hale.QUIC.Network.QUIC.Server`
211. `Hale.QUIC.Network.QUIC.Stream`
212. `Hale.Http3.Network.HTTP3.Server`
213. `Hale.Http3`
214. `Hale.QUIC`
215. `Hale.Recv.Network.Socket.Recv`
216. `Hale.Recv`
217. `Hale.ResourceT.Control.Monad.Trans.Resource`
218. `Hale.ResourceT`
219. `Hale.Conduit.Data.Conduit.Internal.Conduit`
220. `Hale.Conduit.Data.Conduit.Combinators`
221. `Hale.Conduit.Data.Conduit`
222. `Hale.Conduit`
223. `Hale.STM.Control.Monad.STM`
224. `Hale.STM.Control.Concurrent.STM.TVar`
225. `Hale.STM.Control.Concurrent.STM.TMVar`
226. `Hale.STM.Control.Concurrent.STM.TQueue`
227. `Hale.STM`
228. `Hale.Scientific.Data.Scientific`
229. `Hale.Scientific`
230. `Hale.SimpleSendfile.Network.Sendfile`
231. `Hale.SimpleSendfile`
232. `Hale.StreamingCommons.Data.Streaming.Network`
233. `Hale.StreamingCommons`
234. `Hale.TLS.Network.TLS.Types`
235. `Hale.TLS.Network.TLS.Context`
236. `Hale.TLS`
237. `Hale.HttpClient.Network.HTTP.Client.Connection`
238. `Hale.HttpClient.Network.HTTP.Client.Redirect`
239. `Hale.HttpClient`
240. `Hale.HttpConduit.Network.HTTP.Client.Conduit`
241. `Hale.HttpConduit.Network.HTTP.Simple`
242. `Hale.HttpConduit`
243. `Hale.Req.Network.HTTP.Req`
244. `Hale.Req`
245. `Hale.Text.Data.Text`
246. `Hale.Text.Data.Text.Encoding`
247. `Hale.Text`
248. `Hale.Time.Data.Time.Clock`
249. `Hale.Time`
250. `Hale.TimeManager.System.TimeManager`
251. `Hale.TimeManager`
252. `Hale.UnixCompat.System.Posix.Compat`
253. `Hale.UnixCompat`
254. `Hale.UnliftIO.Control.Monad.IO.Unlift`
255. `Hale.UnliftIO`
256. `Hale.Vault.Data.Vault`
257. `Hale.Vault`
258. `Hale.Vector.Data.Vector`
259. `Hale.Vector`
260. `Hale.WAI.Network.Wai.Internal`
261. `Hale.WAI.Network.Wai`
262. `Hale.WAI`
263. `Hale.WaiAppStatic.WaiAppStatic.Types`
264. `Hale.WaiAppStatic.WaiAppStatic.Storage.Filesystem`
265. `Hale.WaiAppStatic.Network.Wai.Application.Static`
266. `Hale.WaiAppStatic`
267. `Hale.WaiExtra.Network.Wai.EventSource`
268. `Hale.WaiExtra.Network.Wai.EventSource.EventStream`
269. `Hale.WaiExtra.Network.Wai.Header`
270. `Hale.WaiExtra.Network.Wai.Middleware.AcceptOverride`
271. `Hale.WaiExtra.Network.Wai.Middleware.AddHeaders`
272. `Hale.WaiExtra.Network.Wai.Middleware.Approot`
273. `Hale.WaiExtra.Network.Wai.Middleware.Autohead`
274. `Hale.WaiExtra.Network.Wai.Middleware.CleanPath`
275. `Hale.WaiExtra.Network.Wai.Middleware.CombineHeaders`
276. `Hale.WaiExtra.Network.Wai.Middleware.ForceDomain`
277. `Hale.WaiExtra.Network.Wai.Middleware.ForceSSL`
278. `Hale.WaiExtra.Network.Wai.Middleware.Gzip`
279. `Hale.WaiExtra.Network.Wai.Middleware.HealthCheckEndpoint`
280. `Hale.WaiExtra.Network.Wai.Middleware.HttpAuth`
281. `Hale.WaiExtra.Network.Wai.Middleware.Jsonp`
282. `Hale.WaiExtra.Network.Wai.Middleware.Local`
283. `Hale.WaiExtra.Network.Wai.Middleware.MethodOverride`
284. `Hale.WaiExtra.Network.Wai.Middleware.MethodOverridePost`
285. `Hale.WaiExtra.Network.Wai.Middleware.RealIp`
286. `Hale.WaiExtra.Network.Wai.Middleware.RequestLogger`
287. `Hale.WaiExtra.Network.Wai.Middleware.RequestLogger.JSON`
288. `Hale.WaiExtra.Network.Wai.Middleware.RequestSizeLimit`
289. `Hale.WaiExtra.Network.Wai.Middleware.RequestSizeLimit.Internal`
290. `Hale.WaiExtra.Network.Wai.Middleware.Rewrite`
291. `Hale.WaiExtra.Network.Wai.Middleware.Routed`
292. `Hale.WaiExtra.Network.Wai.Middleware.Select`
293. `Hale.WaiExtra.Network.Wai.Middleware.StreamFile`
294. `Hale.WaiExtra.Network.Wai.Middleware.StripHeaders`
295. `Hale.WaiExtra.Network.Wai.Middleware.Timeout`
296. `Hale.WaiExtra.Network.Wai.Middleware.ValidateHeaders`
297. `Hale.WaiExtra.Network.Wai.Middleware.Vhost`
298. `Hale.WaiExtra.Network.Wai.Parse`
299. `Hale.WaiExtra.Network.Wai.Request`
300. `Hale.WaiExtra.Network.Wai.Test`
301. `Hale.WaiExtra.Network.Wai.Test.Internal`
302. `Hale.WaiExtra.Network.Wai.UrlMap`
303. `Hale.WaiExtra`
304. `Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.LRU`
305. `Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.ParseURL`
306. `Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.Types`
307. `Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer.Manager`
308. `Hale.WaiHttp2Extra.Network.Wai.Middleware.Push.Referer`
309. `Hale.WaiHttp2Extra`
310. `Hale.WaiLogger.Network.Wai.Logger`
311. `Hale.WaiLogger`
312. `Hale.Warp.Network.Wai.Handler.Warp.Counter`
313. `Hale.Warp.Network.Wai.Handler.Warp.Date`
314. `Hale.Warp.Network.Wai.Handler.Warp.HashMap`
315. `Hale.Warp.Network.Wai.Handler.Warp.Header`
316. `Hale.Warp.Network.Wai.Handler.Warp.PackInt`
317. `Hale.Warp.Network.Wai.Handler.Warp.ReadInt`
318. `Hale.Warp.Network.Wai.Handler.Warp.Request`
319. `Hale.Warp.Network.Wai.Handler.Warp.Settings`
320. `Hale.Warp.Network.Wai.Handler.Warp.Response`
321. `Hale.Warp.Network.Wai.Handler.Warp.Run`
322. `Hale.Warp.Network.Wai.Handler.Warp.Types`
323. `Hale.Warp.Network.Wai.Handler.Warp.Conduit`
324. `Hale.Warp.Network.Wai.Handler.Warp.IO`
325. `Hale.Warp.Network.Wai.Handler.Warp.SendFile`
326. `Hale.Warp.Network.Wai.Handler.Warp.Internal`
327. `Hale.Warp.Network.Wai.Handler.Warp.WithApplication`
328. `Hale.Warp.Network.Wai.Handler.Warp`
329. `Hale.Warp`
330. `Hale.WarpQUIC.Network.Wai.Handler.WarpQUIC`
331. `Hale.WarpQUIC`
332. `Hale.WarpTLS.Network.Wai.Handler.WarpTLS`
333. `Hale.WarpTLS.Network.Wai.Handler.WarpTLS.Internal`
334. `Hale.WarpTLS`
335. `Hale.WebSockets.Network.WebSockets.Types`
336. `Hale.WebSockets.Network.WebSockets.Frame`
337. `Hale.WebSockets.Network.WebSockets.Connection`
338. `Hale.WebSockets.Network.WebSockets.Handshake`
339. `Hale.WebSockets`
340. `Hale.WaiWebSockets.Network.Wai.Handler.WebSockets`
341. `Hale.WaiWebSockets`
342. `Hale.Word8.Data.Word8`
343. `Hale.Word8`
344. `Hale`

