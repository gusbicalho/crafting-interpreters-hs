module HSLox.AST.Meta
  ( module HSLox.AST.Meta
  , Identity (..)
  ) where

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

astContent :: SomeAST astNode -> astNode Identity
astContent ast = runSomeAST ast (ffmap asIdentity)

class Functor f => AsIdentity f where
  asIdentity :: f ~> Identity

content :: AsIdentity f => f c -> c
content = runIdentity . asIdentity

class HasMeta meta f where
  meta :: forall a. f a -> meta

instance AsIdentity Identity where
  asIdentity = id

instance HasMeta () f where
  meta _ = ()

instance AsIdentity ((,) a) where
  asIdentity = Identity . snd

instance HasMeta a ((,) a) where
  meta = fst
