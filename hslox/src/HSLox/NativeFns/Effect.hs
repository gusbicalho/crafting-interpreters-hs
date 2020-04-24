{-# LANGUAGE UndecidableInstances #-}
module HSLox.NativeFns.Effect
  ( Algebra, Has
  , module HSLox.NativeFns.Effect
  ) where

import Control.Algebra
import Data.Kind
import qualified Data.Text as T

data NativeFns (m :: Type -> Type) k where
  Clock :: NativeFns m Integer
  PrintText :: T.Text -> NativeFns m ()

clock :: Has NativeFns sig m => m Integer
clock = send Clock

printText :: Has NativeFns sig m => T.Text -> m ()
printText test = send (PrintText test)
