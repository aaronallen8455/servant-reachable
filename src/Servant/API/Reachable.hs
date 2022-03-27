{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Servant.API.Reachable
  ( Reachable
  ) where

import           Data.Kind
import           GHC.TypeLits
import           Servant.API

type Trie = [Node]
data Node = MkNode PathComponent [Node]

-- For the Verb constructor, will need to be indifferent to the ordering of
-- the content types. Have some way of sorting them?
data PathComponent
  = PathCapture -- ^ Matches any path component, i.e. 'Capture'
  | Specific Symbol -- ^ Matches only the given string
  | Body [Type] -- ^ Request body with specific content types
  | End (Maybe PathVerb) -- TODO (Maybe [Type])
  -- ^ The HTTP verb for an endpoint plus content types of request body if
  -- there is a request body

type Path = [PathComponent]

data PathVerb =
  MkPathVerb
    StdMethod -- ^ method
    [Type] -- ^ content types

type family ToPath (component :: k) :: Maybe PathComponent

type instance ToPath (string :: Symbol) = Just (Specific string)
type instance ToPath (Capture' mods label ty) = Just PathCapture
type instance ToPath (Verb method statusCode contentTypes a)
  = Just (End (Just (MkPathVerb method contentTypes)))
type instance ToPath (NoContentVerb method)
  = Just (End (Just (MkPathVerb method '[])))
type instance ToPath (UVerb method contentTypes as)
  = Just (End (Just (MkPathVerb method contentTypes)))
type instance ToPath (Stream method status framing contentType a)
  = Just (End (Just (MkPathVerb method '[contentType])))
type instance ToPath (ReqBody' mods contentTypes a)
  = Just (Body contentTypes)
type instance ToPath Raw = Just (End Nothing)
-- No-ops
type instance ToPath (Description sym) = Nothing
type instance ToPath (AuthProtect tag) = Nothing
type instance ToPath (Summary sym) = Nothing
type instance ToPath RemoteHost = Nothing
type instance ToPath IsSecure = Nothing
type instance ToPath Vault = Nothing
type instance ToPath (WithNamedContext name subContext) = Nothing

type family Reachable (api :: Type) :: Type where
  Reachable api = ResolveEither (Reachable' '[] '[] api) api

type family ResolveEither (either :: Either a b) (x :: a) :: a where
  ResolveEither (Left err) _ = err
  ResolveEither (Right _) x = x

type family Reachable' (routes :: Trie) (path :: Path) (api :: Type)
    :: Either Type Trie  where
  Reachable' routes path (component :> rest) =
    Reachable' routes (AppendMaybe (ToPath component) path) rest
  Reachable' routes path (a :<|> b) =
    BindEither (Reachable' routes path a) path b
    -- Reachable' (Reachable' routes path a) path b
  Reachable' routes path end =
    CheckPath routes (Reverse (AppendMaybe (ToPath end) path))

type family BindEither (inp :: Either Type Trie) path next :: Either Type Trie where
  BindEither (Left err) path next = Left err
  BindEither (Right trie) path next = Reachable' trie path next

type family CheckPath (routes :: Trie) (path :: Path) :: Either Type Trie where
  CheckPath routes path = CheckPath' '[] routes path

-- TODO the position of the ReqBody in a path shouldn't matter
-- ReqBody a :> "string"
-- == "string" :> ReqBody a
-- Perhaps propagate the reqBody info up to construction of the End component?
type family CheckPath' (visited :: Trie) (routes :: Trie) (path :: Path) :: Either Type Trie where
  -- If both are Ends, check that the verb is different
  CheckPath' visited (MkNode (End mVerb) '[] ': trieRest)
                     (End mVerb ': pathRest)
    = Left (TypeError (Text "same route"))

--   -- Check that body content types are disjoint
--   CheckPath' visited (MkNode (Body cts) inner ': trieRest)
--                      (Body cts ': pathRest) -- TODO order of cts shouldn't matter
--     = _

  -- Captures overshadow other captures
  CheckPath' visited (MkNode PathCapture inner ': trieRest)
                     (PathCapture ': pathRest)
    = Left (TypeError (Text "too many captures")) -- Need the full path here

  -- Descend if path components match
  CheckPath' visited (MkNode comp inner ': trieRest)
                     (comp ': pathRest)
    = Descend (CheckPath inner pathRest) visited comp trieRest

  -- PathCapture overshadows Specific
  CheckPath' visited (MkNode PathCapture inner ': trieRest)
                     (Specific sym ': pathRest)
    = Left (TypeError (Text "...")) -- Need the full path here

  -- Recurse if node doesn't match
  CheckPath' visited (MkNode comp1 inner ': trieRest) (comp2 ': pathRest)
    = CheckPath' (MkNode comp1 inner ': visited) trieRest (comp2 ': pathRest)

  -- Insert node if there's no matching nodes
  CheckPath' visited '[] (comp ': pathRest)
    = Insert (CheckPath '[] pathRest) comp visited

  CheckPath' '[] '[] '[] = Right '[]

type family Descend either visited comp trieRest where
  Descend (Left err) v c t = Left err
  Descend (Right trie) visited comp trieRest =
    Right (Append (Reverse visited) (MkNode comp trie ': trieRest))

type family Insert either comp visited where
  Insert (Left err) c v = Left err
  Insert (Right trie) comp visited =
    Right (Reverse (MkNode comp trie ': visited))

type family AppendMaybe (ma :: Maybe a) (as :: [a]) :: [a] where
  AppendMaybe Nothing as = as
  AppendMaybe (Just a) as = a ': as

-- | Used for displaying a path in an error message
type family PathToSymbol (path :: Path) :: Symbol where
  PathToSymbol (Specific string ': rest) =
    AppendSymbol (AppendSymbol "/" string) (PathToSymbol rest)
  PathToSymbol (PathCapture ': rest) =
    AppendSymbol "/{*}" (PathToSymbol rest)
  PathToSymbol other = ""

type family Snd (tuple :: (k1, k2)) :: k2 where
  Snd '(a, b) = b

type family Reverse xs where
  Reverse xs = Reverse' '[] xs

type family Reverse' acc xs where
  Reverse' acc '[] = acc
  Reverse' acc (x ': xs) = Reverse' (x ': acc) xs

type family Append (xs :: [a]) (ys :: [a]) where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- Type family will need to return multiple things: an updated Trie as well as
-- the API type

-- What components have an effect on routing?
-- Fragment?
-- QueryParam? only if required?
-- Content types?
-- Auth?

-- It appears that Captures will go to the next handler if parsing fails, unless
-- the Lenient modifier is present.
-- So in that case, will need to keep a list of the parsing targets for the
-- capture at that position and if one reoccurs, then that is an error

-- Since there's no way to know if the parsing of two types is equivalent, we
-- can't garuantee that one capture is covering another. Two options:
-- 1) Check for equivalence of types in captures and fail if there are duplicates.
-- This is really ad-hoc and breaks down easily in the event that the first
-- capture has a text type that never fails to parse.
-- 2) be pessimistic and fail if the only differenciator is a Capture component.
-- This does not reflect the actual behavior of servant but gives false negatives
-- rather than false positives. Routers shouldn't be relying on this behavior
-- anyways - parsers are poor control flow

-- Content type is significant

-- How to deal with NamedRoutes?
