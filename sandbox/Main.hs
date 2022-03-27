{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
import           Servant
import           Servant.Server
import           Data.Proxy
import           Network.Wai.Handler.Warp

import           Servant.API.Reachable

main :: IO ()
main = run 8084 $ serve (Proxy @Api) server

type Api = Reachable (
  "one" :> ( Capture "a" String :> Get '[JSON] String
        :<|> ReqBody '[JSON] String :> Capture "oof" Int :> Get '[JSON] String
        -- :<|> Capture "goof" String :> Get '[JSON] String
           )
                     )

server :: Server Api
server = a :<|> one where -- :<|> two where
  a _ = pure "a"
  one _ _ = pure "one"
  -- two _ = pure "two"
