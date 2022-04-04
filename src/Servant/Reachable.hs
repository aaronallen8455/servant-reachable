{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Servant.Reachable
  ( Reachable
  , ToPath(..)
  ) where

import           Data.Kind
import           Data.Type.Bool (If)
import           GHC.TypeLits
import           Servant.API
import           Servant.API.Generic (ToServantApi)

type Trie = [Node]
data Node = MkNode PathComponent [Node]

-- For the Verb constructor, will need to be indifferent to the ordering of
-- the content types. Have some way of sorting them?
data PathComponent
  = PathCapture -- ^ Matches any path component, i.e. 'Capture'
  | PathCaptureAll
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
type instance ToPath (CaptureAll sym a) _ = '(Just PathCaptureAll, Nothing)
type instance ToPath (ReqBody' mods contentTypes a) _
  = '(Nothing, Just contentTypes)
type instance ToPath (StreamBody' mods framing ctype a) _
  = '(Nothing, Just '[ctype])
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
type instance ToPath HttpVersion _ = '(Nothing, Nothing)
type instance ToPath RemoteHost _ = '(Nothing, Nothing)
type instance ToPath (QueryParam' mods sym a) _ = '(Nothing, Nothing)
type instance ToPath (QueryParams sym a) _ = '(Nothing, Nothing)
type instance ToPath (QueryFlag sym) _ = '(Nothing, Nothing)
type instance ToPath (Header' mods sym a) _ = '(Nothing, Nothing)
type instance ToPath IsSecure _ = '(Nothing, Nothing)
type instance ToPath (Fragment a) _ = '(Nothing, Nothing)
type instance ToPath (AuthProtect tag) _ = '(Nothing, Nothing)
type instance ToPath (BasicAuth realm usr) _ = '(Nothing, Nothing)

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
  Reachable' routes path cts EmptyAPI =
    Right routes
  Reachable' routes path cts (NamedRoutes api) =
    Reachable' routes path cts (ToServantApi api)
  Reachable' routes path cts (WithNamedContext n s subApi) =
    Reachable' routes path cts subApi
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

type family AddEnd (component :: (Maybe PathComponent, Maybe ContentTypes))
                   routes
                   (path :: Path) where
  AddEnd '(Nothing, _) routes path =
    Left (TypeError (Text "Invalid path ending"))
  AddEnd '(Just c, _) routes path =
    CheckPath
      routes
      (Reverse (c ': path))

type family BindEither (inp :: Either Type Trie) path cts next :: Either Type Trie where
  BindEither (Left err) path cts next = Left err
  BindEither (Right trie) path cts next = Reachable' trie path cts next

type family CheckPath (routes :: Trie) (path :: Path) :: Either Type Trie where
  CheckPath routes path =
    CheckPath'
      '[]
      routes
      path
      (PathToSymbol path)

type family InvalidPathError (pathSym :: Symbol) where
  InvalidPathError pathSym =
    TypeError (Text "Conflicting paths detected: " :<>: Text pathSym)

type family CheckPath' (visited :: Trie) (routes :: Trie) (path :: Path) (pathSym :: Symbol)
    :: Either Type Trie where

  -- | Fail if a PathCaptureAll is followed by anything other than the end of the path
  CheckPath' visited trie (PathCaptureAll ': a ': b ': c) pathSym
    = Left (TypeError (Text "Cannot have path components after a CaptureAll: " :<>: Text pathSym))

  -- | Fail if the endpoint has no acceptable content types
  CheckPath' visited trie '[End verb (Just '[])] pathSym
    = Left (TypeError (Text "Empty 'Content-Type' list: " :<>: Text pathSym))
  CheckPath' visited trie '[End (Just (MkPathVerb v (Just '[]))) cts] pathSym
    = Left (TypeError (Text "Empty 'Accept' list: " :<>: Text pathSym))

  -- | Raw endpoints shadow any other end if the content types are not disjoint
  CheckPath' visited (MkNode (End Nothing (Just cts0)) '[] ': trieRest)
                     '[End verb (Just cts1)]
                     pathSym
    = If (Disjoint cts0 cts1)
         (CheckPath' visited trieRest
                     '[End verb (Just cts1)]
                     pathSym)
         (Left (InvalidPathError pathSym))

  CheckPath' visited (MkNode (End Nothing Nothing) '[] ': trieRest)
                     '[End verb cts1]
                     pathSym
    = Left (InvalidPathError pathSym)

  -- If both have content types, check that they are disjoint
  CheckPath' visited
    (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts0)) desc ': trieRest)
    '[End (Just (MkPathVerb meth acceptTys1)) (Just cts1)]
    pathSym
    = If (Disjoint cts0 cts1)
         (CheckPath'
            (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts0)) desc ': visited)
            trieRest
            '[End (Just (MkPathVerb meth acceptTys1)) (Just cts1)]
            pathSym)
         (CheckAcceptTypes acceptTys0 acceptTys1
            (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts0)) desc ': visited)
            trieRest
            '[End (Just (MkPathVerb meth acceptTys1)) (Just cts1)]
            pathSym)

  -- If the first has no content types. To be valid the accept types must be
  -- the distinguishing factor.
  CheckPath' visited
    (MkNode (End (Just (MkPathVerb meth acceptTys0)) Nothing) desc ': trieRest)
    '[End (Just (MkPathVerb meth acceptTys1)) cts]
    pathSym
    = CheckAcceptTypes acceptTys0 acceptTys1
        (MkNode (End (Just (MkPathVerb meth acceptTys0)) Nothing) desc ': visited)
        trieRest
        '[End (Just (MkPathVerb meth acceptTys1)) cts]
        pathSym

  CheckPath' visited
    (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts)) desc ': trieRest)
    '[End (Just (MkPathVerb meth acceptTys1)) Nothing]
    pathSym
    = CheckPath'
        (MkNode (End (Just (MkPathVerb meth acceptTys0)) (Just cts)) desc ': visited)
        trieRest
        '[End (Just (MkPathVerb meth acceptTys1)) Nothing]
        pathSym

  -- Descend if path components match
  CheckPath' visited (MkNode comp inner ': trieRest)
                     (comp ': pathRest)
                     pathSym
    = Descend (CheckPath' '[] inner pathRest pathSym) visited comp trieRest

  -- PathCapture overshadows Specific
  CheckPath' visited (MkNode PathCapture inner ': trieRest)
                     (Specific sym ': pathRest)
                     pathSym
    = HandleCapture
        (CheckPath' '[] inner pathRest pathSym)
        (MkNode PathCapture inner ': visited)
        trieRest
        (Specific sym ': pathRest)
        pathSym

  CheckPath' visited (MkNode PathCaptureAll inner ': trieRest)
                     path
                     pathSym
    = HandleCaptureAll visited inner trieRest path pathSym

  -- Recurse if node doesn't match
  CheckPath' visited (MkNode comp1 inner ': trieRest) (comp2 ': pathRest) pathSym
    = CheckPath' (MkNode comp1 inner ': visited) trieRest (comp2 ': pathRest) pathSym

  -- Insert node if there's no matching nodes
  CheckPath' visited '[] (comp ': pathRest) pathSym
    = Insert (CheckPath' '[] '[] pathRest pathSym) comp visited

  CheckPath' '[] '[] '[] _ = Right '[]

-- If content types are such that the accept types need to be differencianted
-- then this should fail if both are nothing.
type family CheckAcceptTypes acceptTys0 acceptTys1 visited trieRest path pathSym
    :: Either Type Trie where
  CheckAcceptTypes (Just at0) (Just at1) visited trieRest path pathSym =
    If (Disjoint at0 at1)
       (CheckPath' visited trieRest path pathSym)
       (Left (InvalidPathError pathSym))

  -- If we got here, that means
  CheckAcceptTypes Nothing Nothing visited trieRest path pathSym =
    Left (InvalidPathError pathSym)

  -- If a NoContent endpoint follows one with accept headers then continue.
  CheckAcceptTypes (Just at0) Nothing visited trieRest path pathSym =
    CheckPath' visited trieRest path pathSym

  CheckAcceptTypes Nothing (Just at1) vistied trieRest path pathSym =
    Left (InvalidPathError pathSym)

-- If a Specific is in the same position as a previous capture and it didn't
-- conflict, then throw the result away and continue recursing through the trie
-- nodes so that the new branch is created in the correct place.
type family HandleCapture rec visited trieRest path pathSym where
  HandleCapture (Left err) v t p ps = Left err
  HandleCapture (Right _) visited trieRest path pathSym =
    CheckPath' visited trieRest path pathSym

type family HandleCaptureAll visited inner trieRest path pathSym where
  HandleCaptureAll visited inner trieRest path pathSym
    = HandleCaptureAll'
        (CheckPath' '[] inner (DropNonEnd path) pathSym)
        visited
        inner
        trieRest
        path
        pathSym

type family HandleCaptureAll' either visited inner trieRest path pathSym where
  HandleCaptureAll' (Left err) v i t p ps = Left err
  HandleCaptureAll' (Right trie) visited inner trieRest path pathSym
    = CheckPath' (MkNode PathCaptureAll inner ': visited) trieRest path pathSym

type family DropNonEnd (path :: [PathComponent]) where
  DropNonEnd '[End v cts] = '[End v cts]
  DropNonEnd (_ ': rest) = DropNonEnd rest

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
  PathToSymbol other = "/"

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
