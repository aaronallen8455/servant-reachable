{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Servant.API.Reachable
  ( Reachable
  , ToPath(..)
  ) where

import           Data.Kind
import           Data.Type.Bool (If)
import           GHC.TypeLits
import           Servant.API

type Trie = [Node]
data Node = MkNode PathComponent [Node]

-- For the Verb constructor, will need to be indifferent to the ordering of
-- the content types. Have some way of sorting them?
data PathComponent
  = PathCapture -- ^ Matches any path component, i.e. 'Capture'
  | Specific Symbol -- ^ Matches only the given string
  -- | Body [Type] -- ^ Request body with specific content types
  | End (Maybe PathVerb) (Maybe ContentTypes)
  -- ^ The HTTP verb for an endpoint plus content types of request body if
  -- there is a request body

type Path = [PathComponent]

data PathVerb =
  MkPathVerb
    StdMethod -- ^ method
    (Maybe ContentTypes) -- ^ Accept header content types. 'Nothing' for 'GetNoContent'.

type ContentTypes = [Type]

type family ToPath (component :: k) (cts :: Maybe ContentTypes)
  :: (Maybe PathComponent, Maybe ContentTypes)

type instance ToPath (string :: Symbol) _ = '(Just (Specific string), Nothing)
type instance ToPath (Capture' mods label ty) _ = '(Just PathCapture, Nothing)
type instance ToPath (ReqBody' mods contentTypes a) _
  = '(Nothing, Just contentTypes)
-- verbs
type instance ToPath (Verb method statusCode contentTypes a) cts
  = '(Just (End (Just (MkPathVerb method (Just contentTypes))) cts), Nothing)
type instance ToPath (NoContentVerb method) cts
  = '(Just (End (Just (MkPathVerb method Nothing)) cts), Nothing)
type instance ToPath (UVerb method contentTypes as) cts
  = '(Just (End (Just (MkPathVerb method (Just contentTypes))) cts), Nothing)
type instance ToPath (Stream method status framing contentType a) cts
  = '(Just (End (Just (MkPathVerb method (Just '[contentType]))) cts), Nothing)
type instance ToPath Raw cts = '(Just (End Nothing cts), Nothing)
-- No-ops
type instance ToPath (Description sym) _ = '(Nothing, Nothing)
type instance ToPath (AuthProtect tag) _ = '(Nothing, Nothing)
type instance ToPath (Summary sym) _ = '(Nothing, Nothing)
type instance ToPath RemoteHost _ = '(Nothing, Nothing)
type instance ToPath IsSecure _ = '(Nothing, Nothing)
type instance ToPath Vault _ = '(Nothing, Nothing)
type instance ToPath (WithNamedContext name subContext) _ = '(Nothing, Nothing)

type family Reachable (api :: Type) :: Type where
  Reachable api = ResolveEither (Reachable' '[] '[] Nothing api) api

type family ResolveEither (either :: Either a b) (x :: a) :: a where
  ResolveEither (Left err) _ = err
  ResolveEither (Right _) x = x

type family Reachable' (routes :: Trie) (path :: Path) (cts :: Maybe ContentTypes) (api :: Type)
    :: Either Type Trie  where
  Reachable' routes path cts (component :> rest) =
    AddComponent (ToPath component cts) routes path cts rest
  Reachable' routes path cts (a :<|> b) =
    BindEither (Reachable' routes path cts a) path cts b
  Reachable' routes path cts end =
    AddEnd (ToPath end cts) routes path

type family AddComponent component routes path cts rest where
  AddComponent '(Nothing, Nothing) routes path cts rest =
    Reachable' routes path cts rest
  AddComponent '(Nothing, Just cts) routes path _ rest =
    Reachable' routes path (Just cts) rest
  AddComponent '(Just c, Just cts) routes path _ rest =
    Reachable' routes (c ': path) (Just cts) rest
  AddComponent '(Just c, Nothing) routes path cts rest =
    Reachable' routes (c ': path) cts rest

-- TODO instead of using Either, could have the result be () and then have a
-- type family that matches on () in order to produce the final result.
-- Would the resulting type error be as nice?

type family AddEnd (component :: (Maybe PathComponent, Maybe ContentTypes))
                   routes
                   (path :: Path) where
  AddEnd '(Nothing, _) routes path =
    Left (TypeError (Text "Invalid path ending"))
  AddEnd '(Just c, _) routes path =
    CheckPath routes (Reverse (c ': path))

type family BindEither (inp :: Either Type Trie) path cts next :: Either Type Trie where
  BindEither (Left err) path cts next = Left err
  BindEither (Right trie) path cts next = Reachable' trie path cts next

type family CheckPath (routes :: Trie) (path :: Path) :: Either Type Trie where
  CheckPath routes path = CheckPath' '[] routes path

-- Cases to test
-- 1st has content types that don't intersect 2nd's content types - check
-- 1st has content types but 2nd does not - check
-- 1st has no content types but 2nd does not - fail
-- 1st has accept types that don't intersect 2nd's accept types
--  ^ this has some edge cases but should probably fail for simplicity.
-- 1st has accept types but 2nd does not
-- 1st has no accept types but 2nd does

-- TODO the position of the ReqBody in a path shouldn't matter
-- ReqBody a :> "string"
-- == "string" :> ReqBody a
-- Perhaps propagate the reqBody info up to construction of the End component?
type family CheckPath' (visited :: Trie) (routes :: Trie) (path :: Path) :: Either Type Trie where
  -- If both are Ends, check that the verb is different
--   CheckPath' visited (MkNode (End mVerb cts) '[] ': trieRest)
--                      (End mVerb cts ': pathRest)
--     = Left (TypeError (Text "same route"))

  CheckPath' visited (MkNode (End Nothing cts0) '[] ': trieRest)
                     '[End Nothing cts1]
    = Left (TypeError (Text "same route"))

  -- If both have content types, check that they are disjoint
  CheckPath' visited
    (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts0)) desc ': trieRest)
    '[End (Just (MkPathVerb meth acceptTys1)) (Just cts1)]
    = If (Disjoint cts0 cts1)
         (CheckPath'
            (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts0)) desc ': visited)
            trieRest
            '[End (Just (MkPathVerb meth acceptTys1)) (Just cts1)])
         (CheckAcceptTypes acceptTys0 acceptTys1
            (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts0)) desc ': visited)
            trieRest
            '[End (Just (MkPathVerb meth acceptTys1)) (Just cts1)])

  -- If the first has no content types. To be valid the accept types must be
  -- the distinguishing factor.
  CheckPath' visited
    (MkNode (End (Just (MkPathVerb meth acceptTys0)) Nothing) desc ': trieRest)
    '[End (Just (MkPathVerb meth acceptTys1)) cts]
    = CheckAcceptTypes acceptTys0 acceptTys1
        (MkNode (End (Just (MkPathVerb meth acceptTys0)) Nothing) desc ': visited)
        trieRest
        '[End (Just (MkPathVerb meth acceptTys1)) cts]

  CheckPath' visited
    (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts)) desc ': trieRest)
    '[End (Just (MkPathVerb meth acceptTys1)) Nothing]
    = CheckPath'
        (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts)) desc ': visited)
        trieRest
        '[End (Just (MkPathVerb meth acceptTys1)) Nothing]

  -- Edge cases with Raw may not be worth exploring
--   CheckPath' visited (MkNode (End Nothing (Just cts0)) '[] ': trieRest)
--                      '[End Nothing (Just cts1)]
--     = If (Disjoint cts0 cts1)
--         ( CheckPath' (MkNode (End Nothing (Just cts0)) '[] ': visited)
--                      trieRest
--                      '[End Nothing (Just cts1)]
--         )
--         (Left (TypeError (Text "same route")))
-- 
--   -- 
--   CheckPath' visited (MkNode (End Nothing Nothing) '[] ': trieRest)
--                      '[End Nothing (Just cts)]
--     = CheckPath' (MkNode (End Nothing Nothing) '[] ': visited)
--                  trieRest
--                  '[End Nothing (Just cts)]

--   CheckPath' visited (MkNode (End (Just (MkPathVerb meth mAccept)) 

--   -- Check that body content types are disjoint
--   CheckPath' visited (MkNode (Body cts) inner ': trieRest)
--                      (Body cts ': pathRest) -- TODO order of cts shouldn't matter
--     = _

  -- Captures overshadow other captures
--   CheckPath' visited (MkNode PathCapture inner ': trieRest)
--                      (PathCapture ': pathRest)
--     = Left (TypeError (Text "too many captures")) -- Need the full path here

  -- Descend if path components match
  CheckPath' visited (MkNode comp inner ': trieRest)
                     (comp ': pathRest)
    = Descend (CheckPath inner pathRest) visited comp trieRest

  -- PathCapture overshadows Specific
  CheckPath' visited (MkNode PathCapture inner ': trieRest)
                     (Specific sym ': pathRest)
    = HandleCapture
        (CheckPath inner pathRest)
        (MkNode PathCapture inner ': visited)
        trieRest
        (Specific sym ': pathRest)

  -- Recurse if node doesn't match
  CheckPath' visited (MkNode comp1 inner ': trieRest) (comp2 ': pathRest)
    = CheckPath' (MkNode comp1 inner ': visited) trieRest (comp2 ': pathRest)

  -- Insert node if there's no matching nodes
  CheckPath' visited '[] (comp ': pathRest)
    = Insert (CheckPath '[] pathRest) comp visited

  CheckPath' '[] '[] '[] = Right '[]

-- If content types are such that the accept types need to be differencianted
-- then this should fail if both are nothing.
type family CheckAcceptTypes acceptTys0 acceptTys1 visited trieRest path where
  CheckAcceptTypes (Just at0) (Just at1) visited trieRest path =
    If (Disjoint at0 at1)
       (CheckPath' visited trieRest path)
       (Left (TypeError (Text "Fail")))

  -- If we got here, that means
  CheckAcceptTypes Nothing Nothing visited trieRest path =
    Left (TypeError (Text "Fail"))

  -- If a NoContent endpoint follows one with accept headers then continue.
  CheckAcceptTypes (Just at0) Nothing visited trieRest path =
    CheckPath' visited trieRest path

  CheckAcceptTypes Nothing (Just at1) vistied trieRest path =
    Left (TypeError (Text "Fail"))

-- If a Specific is in the same position as a previous capture and it didn't
-- conflict, then throw the result away and continue recursing through the trie
-- nodes so that the new branch is created in the correct place.
type family HandleCapture rec visited trieRest path where
  HandleCapture (Left err) v t p = Left err
  HandleCapture (Right _) visited trieRest path =
    CheckPath' visited trieRest path

type family Descend either visited comp trieRest where
  Descend (Left err) v c t = Left err
  Descend (Right trie) visited comp trieRest =
    Right (Append (Reverse visited) (MkNode comp trie ': trieRest))

type family Insert either comp visited where
  Insert (Left err) c v = Left err
  Insert (Right trie) comp visited =
    Right (Reverse (MkNode comp trie ': visited))

type family Disjoint (as :: [k]) (bs :: [k]) where
  Disjoint '[] _ = True
  Disjoint (a ': as) bs =
    If (Elem a bs)
      False
      (Disjoint as bs)

type family Elem (a :: k) (as :: [k]) where
  Elem x '[] = False
  Elem x (x ': _) = True
  Elem x (y ': xs) = Elem x xs

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
