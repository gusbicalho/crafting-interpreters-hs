{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}
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
class Functor f => AsIdentity f where
  asIdentity :: f ~> Identity

content :: AsIdentity f => f c -> c
content = runIdentity . asIdentity
{-# INLINE content #-}

data WithMeta meta f a = WithMeta { withMetaMeta :: meta, withMetaContent :: f a }
  deriving (Show, Functor, Foldable, Traversable)

withMeta :: meta -> f a -> WithMeta meta f a
withMeta = WithMeta

class HasMeta meta f where
  meta :: forall a. f a -> meta

instance AsIdentity Identity where
  asIdentity = id
  {-# INLINE asIdentity #-}

instance AsIdentity f => AsIdentity (WithMeta meta f) where
  asIdentity = asIdentity . withMetaContent
  {-# INLINE asIdentity #-}

instance {-# OVERLAPPABLE #-} HasMeta a f => HasMeta a (WithMeta meta f) where
  meta = meta . withMetaContent
  {-# INLINE meta #-}

instance {-# OVERLAPPING #-} HasMeta meta (WithMeta meta f) where
  meta = withMetaMeta
  {-# INLINE meta #-}
