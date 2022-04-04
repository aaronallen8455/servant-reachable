{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
import           Servant
import           Servant.Server
import           Data.Proxy
import           Network.Wai.Handler.Warp

import           Servant.Reachable
import           Debug.Trace

main :: IO ()
main = run 8084 $ serve (Proxy @Api) server

type Api = -- Reachable (
  "one" :> ( ReqBody '[JSON] Bool :> Raw
        :<|> Get '[JSON] String
           )
                     --)

server :: Server Api
server = a :<|> b where -- :<|> one where -- :<|> two where
  a _ = undefined
  b = pure "b"
--   one = trace "one" undefined
  -- two _ = pure "two"
