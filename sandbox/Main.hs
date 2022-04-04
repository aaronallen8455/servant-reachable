{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
import           Servant
import           Servant.Server
import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Debug.Trace

import           Servant.Reachable

main :: IO ()
main = run 8084 $ serve (Proxy @Api) server

type Api = Reachable (
  "one" :> ( ReqBody '[JSON] Bool :> GetNoContent
        :<|> ReqBody '[PlainText] String :> GetNoContent
        :<|> GetNoContent
           )
                     )

server :: Server Api
server = a :<|> b :<|> c where
  a _ = trace "a" $! pure NoContent
  b _ = trace "b" $! pure NoContent
  c = trace "c" $! pure NoContent
--   one = trace "one" undefined
  -- two _ = pure "two"
