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

-- AST Meta stuff
class Functor f => AsIdentity f where
  asIdentity :: f a -> Identity a

content :: AsIdentity f => f c -> c
content = runIdentity . asIdentity
{-# INLINE content #-}

data WithMeta meta f a = WithMeta { withMetaMeta :: meta, withMetaContent :: f a }
  deriving stock (Show, Functor, Foldable, Traversable)

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
