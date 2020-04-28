{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.AST.Meta
  ( module HSLox.AST.Meta
  , Identity (..), Compose (..)
  ) where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind

-- HKD
type f ~> g = forall a. f a -> g a
class FFunctor (t :: (Type -> Type) -> Type) where
  ffmap :: (Functor f, Functor g) => (f ~> g) -> t f -> t g

-- AST Meta stuff
newtype SomeAST astNode =
  SomeAST { runSomeAST :: forall a
                        . (forall f. ( AsIdentity f
                                     , FFunctor astNode
                                     ) => astNode f -> a)
                        -> a }

mkSomeAST :: (AsIdentity f, FFunctor astNode) => astNode f -> SomeAST astNode
mkSomeAST fnAst = SomeAST $ \k -> k fnAst
{-# INLINE mkSomeAST #-}

astContent :: SomeAST astNode -> astNode Identity
astContent ast = runSomeAST ast (ffmap asIdentity)
{-# INLINE astContent #-}

class Functor f => AsIdentity f where
  asIdentity :: f ~> Identity

content :: AsIdentity f => f c -> c
content = runIdentity . asIdentity
{-# INLINE content #-}

type WithMeta meta f = Compose ((,) meta) f

withMeta :: meta -> f a -> WithMeta meta f a
withMeta meta fa = Compose (meta, fa)

class HasMeta meta f where
  meta :: forall a. f a -> meta

instance AsIdentity Identity where
  asIdentity = id
  {-# INLINE asIdentity #-}

instance HasMeta () f where
  meta _ = ()

instance AsIdentity ((,) a) where
  asIdentity = Identity . snd
  {-# INLINE asIdentity #-}

instance (AsIdentity f, AsIdentity g) => AsIdentity (Compose g f) where
  asIdentity gf = asIdentity . runIdentity . asIdentity . getCompose $ gf
  {-# INLINE asIdentity #-}

instance HasMeta a g => HasMeta a (Compose g f) where
  meta = meta . getCompose
  {-# INLINE meta #-}

instance (AsIdentity g, HasMeta a f) => HasMeta a (Compose g f) where
  meta = meta . runIdentity . asIdentity . getCompose
  {-# INLINE meta #-}
