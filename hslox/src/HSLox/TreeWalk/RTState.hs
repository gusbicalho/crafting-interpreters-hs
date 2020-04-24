{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.RTState
  ( RTState (..)
  , module HSLox.TreeWalk.RTState
  ) where

import Control.Effect.Error
import Control.Effect.State
import Data.Function
import qualified Data.Map.Strict as Map
import HSLox.Cells.Effect
import HSLox.Token (Token (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.Runtime
import qualified HSLox.Util as Util

newEnv :: forall cell sig m
        . Has (Cells cell) sig m
       => m (RTEnv cell)
newEnv = RTEnv <$> newCell @cell Map.empty

newState :: forall cell sig m
          . Has (Cells cell) sig m
         => m (RTState cell)
newState = RTState <$> newEnv @cell <*> pure Nothing

addAsChildFrame :: RTEnv cell -> RTState cell -> RTState cell
addAsChildFrame env state =
  let newFrame = RTFrame env (rtStateLocalFrame state)
  in state { rtStateLocalFrame = Just newFrame }

atNewChildEnv :: forall cell sig m
               . Has (Cells cell) sig m
              => RTState cell -> m (RTState cell)
atNewChildEnv state = addAsChildFrame <$> newEnv @cell <*> pure state

overFrameEnv :: (RTEnv cell -> RTEnv cell) -> RTFrame cell -> RTFrame cell
overFrameEnv f frame =
  frame { rtFrameEnv = f (rtFrameEnv frame)}

overCurrentEnv :: (RTEnv cell -> RTEnv cell) -> RTState cell -> RTState cell
overCurrentEnv f state =
  case rtStateLocalFrame state of
    Just frame -> state { rtStateLocalFrame = Just (overFrameEnv f frame) }
    Nothing -> state { rtStateGlobalEnv = f (rtStateGlobalEnv state)}

currentEnv :: RTState cell -> RTEnv cell
currentEnv state = case rtStateLocalFrame state of
  Just frame -> rtFrameEnv frame
  Nothing -> rtStateGlobalEnv state

globalEnv :: RTState cell -> RTEnv cell
globalEnv = rtStateGlobalEnv

bindNameToFreshCell :: Has (Cells cell) sig m
                    => Has (State (RTState cell)) sig m
                    => BindingName
                    -> m (RTCell cell)
bindNameToFreshCell name = do
  cell <- RTCell <$> newCell ValNil
  bindings <- rtEnvBindings . currentEnv <$> get
  updateCell (Map.insert name cell) bindings
  pure cell

bindingInCurrentEnv :: Has (Cells cell) sig m
                    => BindingName
                    -> RTState cell
                    -> m (Maybe (RTCell cell))
bindingInCurrentEnv name = fmap (Map.lookup name)
                         . readCell
                         . rtEnvBindings
                         . currentEnv

assignRTCell :: Has (Cells cell) sig m
             => RTCell cell -> RTValue cell -> m ()
assignRTCell (RTCell cell) val = writeCell val cell

readRTCell :: Has (Cells cell) sig m
           => RTCell cell -> m (RTValue cell)
readRTCell (RTCell cell) = readCell cell

defineM :: Has (Cells cell) sig m
        => Has (State (RTState cell)) sig m
        => BindingName -> RTValue cell -> m ()
defineM name val = do
  state <- get
  cell <- bindingInCurrentEnv name state
  cell <- case cell of
            Nothing   -> bindNameToFreshCell name
            Just cell -> pure cell
  assignRTCell cell val

getBoundCell :: Has (Cells cell) sig m
             => BindingName
             -> RTState cell
             -> m (Maybe (RTCell cell))
getBoundCell name state
    = (go `onJust` rtStateLocalFrame state) `orMaybe` lookup (rtStateGlobalEnv state)
  where
    orMaybe :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
    orMaybe ma mb = do
      a <- ma
      case a of
        Just a -> pure $ Just a
        Nothing -> mb
    onJust :: Applicative f => (t -> f (Maybe a)) -> Maybe t -> f (Maybe a)
    _ `onJust` Nothing = pure Nothing
    f `onJust` (Just a) = f a
    go :: Has (Cells cell) sig m => RTFrame cell -> m (Maybe (RTCell cell))
    go (RTFrame env parent) = do
      cell <- lookup env
      case cell of
        Just cell -> pure $ Just cell
        Nothing -> go `onJust` parent
    lookup :: Has (Cells cell) sig m => RTEnv cell -> m (Maybe (RTCell cell))
    lookup env = Map.lookup name <$> readCell (rtEnvBindings env)

getBoundValueM :: forall cell sig m
                . Has (Cells cell) sig m
               => Has (State (RTState cell)) sig m
               => Has (Throw RTError) sig m
               => Token -> m (RTValue cell)
getBoundValueM tk = do
  state <- get @(RTState cell)
  let name = tokenLexeme tk
  cell <- getBoundCell name state
  case cell of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."

atParentEnv :: RTState cell -> Maybe (RTEnv cell, RTState cell)
atParentEnv state = case rtStateLocalFrame state of
  Nothing -> Nothing
  Just frame -> Just (rtFrameEnv frame, state { rtStateLocalFrame = rtFrameEnclosing frame })

assignM :: forall cell sig m
         . Has (Cells cell) sig m
        => Has (State (RTState cell)) sig m
        => Has (Throw RTError) sig m
        => Token -> RTValue cell -> m ()
assignM tk val = do
  state <- get
  let name = tokenLexeme tk
  cell <- getBoundCell name state
  case cell of
    Just cell -> assignRTCell cell val
    Nothing -> RTError.throwRT tk $ "Undefined variable '"
                                 <> name
                                 <> "'."

finallyOnErrorOrReturn :: forall cell sig m a
                        . Has (Error (RTReturn cell)) sig m
                       => Has (Error RTError) sig m
                       => m a -> m () -> m a
finallyOnErrorOrReturn action restore = do
  v <- action
        & (`catchError` \(e :: RTError) -> do
            restore
            throwError e)
        & (`catchError` \(e :: (RTReturn cell)) -> do
            restore
            throwError e)
  restore
  pure v

runInChildEnv :: forall cell sig m
               . Has (Cells cell) sig m
              => Has (Error RTError) sig m
              => Has (Error (RTReturn cell)) sig m
              => Has (State (RTState cell)) sig m
              => m () -> m ()
runInChildEnv action = do
    Util.modifyM @(RTState cell) atNewChildEnv
    action
      `finally` restoreParent
  where
    finally = finallyOnErrorOrReturn @cell
    restoreParent = Util.modifyM @(RTState cell) $ \s -> do
      case atParentEnv s of
        Just (_, s) -> pure s
        Nothing -> newState

runInChildEnvOf :: forall cell sig m a
                 . Has (Cells cell) sig m
                => Has (Error RTError) sig m
                => Has (Error (RTReturn cell)) sig m
                => Has (State (RTState cell)) sig m
                => Maybe (RTFrame cell) -> m a -> m a
runInChildEnvOf frame action = do
    currentFrame <- gets @(RTState cell) rtStateLocalFrame
    Util.modifyM $ \state -> do
      env <- newEnv
      pure $ state { rtStateLocalFrame = Just $ RTFrame env frame }
    action
      `finally` restore currentFrame
  where
    finally = finallyOnErrorOrReturn @cell
    restore frame = modify $ \state -> state { rtStateLocalFrame = frame }

envStack :: Has (State (RTState cell)) sig m => m [RTEnv cell]
envStack = do
    state <- get
    pure $ rtStateGlobalEnv state : go [] (rtStateLocalFrame state)
  where
    go acc Nothing = acc
    go acc (Just frame) = go (rtFrameEnv frame : acc) (rtFrameEnclosing frame)
