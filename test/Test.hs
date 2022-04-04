module Test where

import           Servant.Reachable

-- has to be run with `cabal exec cabal test`

-- $setup
-- >>> import Servant.API
-- >>> import Servant.Reachable
-- >>> import Data.Proxy

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> "two" :> Get '[JSON] String
--          :<|> "one" :> "two" :> Get '[JSON] String
--        ))
--   )
-- :}
-- <BLANKLINE>
-- ...
--     • Conflicting paths detected: /one/two/
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> "two" :> Get '[PlainText] String
--          :<|> "one" :> "two" :> Get '[JSON] String
--        ))
--   )
-- :}
-- Proxy

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> "two" :> Get '[PlainText, JSON] String
--          :<|> "one" :> "two" :> Get '[JSON, OctetStream] String
--        ))
--   )
-- :}
-- <BLANKLINE>
-- ...
--     • Conflicting paths detected: /one/two/
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> "two" :> Get '[JSON] String
--          :<|> "one" :> "three" :> Post '[JSON] String
--          :<|> "one" :> "two" :> Get '[JSON] String
--        ))
--   )
-- :}
-- <BLANKLINE>
-- ...
--     • Conflicting paths detected: /one/two/
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> Capture "cap" Bool :> Get '[JSON] String
--          :<|> "one" :> "two" :> Get '[JSON] String
--        ))
--   )
-- :}
-- <BLANKLINE>
-- ...
--     • Conflicting paths detected: /one/two/
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> "two" :> Get '[JSON] String
--          :<|> "one" :> Capture "cap" Bool :> Get '[JSON] String
--        ))
--   )
-- :}
-- Proxy

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> ReqBody '[JSON] Bool :> Get '[JSON] String
--          :<|> "one" :> ReqBody '[PlainText] :> Get '[JSON] String
--        ))
--   )
-- :}
-- Proxy

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               "one" :> ReqBody '[JSON] Bool :> Get '[JSON] String
--          :<|> "one" :> ReqBody '[JSON] :> Get '[PlainText] String
--        ))
--   )
-- :}
-- Proxy

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               Raw
--          :<|> ReqBody '[JSON] Bool :> Get '[PlainText] String
--        ))
--   )
-- :}
-- <BLANKLINE>
-- ...
--     • Conflicting paths detected: /
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (
--               Capture "test" Bool :> Get '[JSON] String
--          :<|> "one" :> "two" :> Get '[JSON] String
--          :<|> "one" :> Get '[JSON] String
--        ))
--   )
-- :}
-- <BLANKLINE>
-- ...
--     • Conflicting paths detected: /one/
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (Get '[] String)))
-- :}
-- <BLANKLINE>
-- ...
--     • Empty 'Accept' list: /
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)

-- $
-- >>> :{
--   (Proxy :: Proxy (Reachable (ReqBody '[] Bool :> Get '[PlainText] String)))
-- :}
-- <BLANKLINE>
-- ...
--     • Empty 'Content-Type' list: /
--     • When checking the inferred type
--         it :: Proxy (TypeError ...)
