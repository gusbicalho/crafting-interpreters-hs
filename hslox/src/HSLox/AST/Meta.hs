{-# LANGUAGE StrictData #-}

module HSLox.AST.Meta (
  WithMeta,
  pattern NoMeta,
  withMeta,
  content,
  meta,
  getMetaItem,
  addMetaItem,
  HasMetaItem,
) where

pattern NoMeta :: a -> WithMeta () a
pattern NoMeta a = WithMeta () a

{-# COMPLETE NoMeta #-}

data WithMeta meta a = WithMeta {meta :: meta, content :: a}
  deriving stock (Show, Functor, Foldable, Traversable)

withMeta :: meta -> a -> WithMeta meta a
withMeta = WithMeta

getMetaItem ::
  forall itemType meta x.
  HasMetaItem itemType meta =>
  WithMeta meta x ->
  itemType
getMetaItem = getMetaItem' . meta

addMetaItem :: item -> WithMeta meta x -> WithMeta (item, meta) x
addMetaItem item v = v{meta = (item, meta v)}

class HasMetaItem itemType meta where
  getMetaItem' :: meta -> itemType

instance HasMetaItem itemType itemType where
  getMetaItem' = id

instance {-# OVERLAPPING #-} HasMetaItem itemType (itemType, more) where
  getMetaItem' = fst

instance
  {-# OVERLAPPABLE #-}
  HasMetaItem itemType more =>
  HasMetaItem itemType (differentItemType, more)
  where
  getMetaItem' = getMetaItem' . snd
