# servant-reachable (work in progress)

This package provides the `Reachable` type family which, when applied to a
`servant` router, throws a type error if any portion of the specified endpoints
is not reachable. For example, an endpoint is not reachable if it has the same
path and verb as a preceding endpoint. `Reachable` takes the `Content-Type` and
`Accept` headers into account as well: an endpoint with specific content types
is considered reachable if there are no preceding endpoints with the same path
and verb and which does not expect any of the same content types.

Example:
```haskell
main :: IO ()
main = run 8080 $ serve (Proxy @(Reachable Api)) server

type Api = "one" :> Get '[PlainText] Text
      :<|> "two" :> ReqBody '[JSON] String :> Post '[PlainText] Text

server :: Server Api
server = ...
```
