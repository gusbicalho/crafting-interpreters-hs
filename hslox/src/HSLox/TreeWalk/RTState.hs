{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.RTState
  ( RTState (..)
  , module HSLox.TreeWalk.RTState
  ) where

import Control.Effect.Error
import Control.Effect.State
import Control.Applicative
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HSLox.NativeFns.Effect (Cells (..), newCell, writeCell, readCell)
import HSLox.Token (Token (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.Runtime

newEnv :: RTEnv cell
newEnv = RTEnv Map.empty

newState :: RTState cell
newState = RTState newEnv Nothing

addAsChildFrame :: RTEnv cell -> RTState cell -> RTState cell
addAsChildFrame env state =
  let newFrame = RTFrame env (rtStateLocalFrame state)
  in state { rtStateLocalFrame = Just newFrame }

atNewChildEnv :: RTState cell -> RTState cell
atNewChildEnv state = addAsChildFrame newEnv state

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

overBindings :: (Map BindingName (RTCell cell) -> Map BindingName (RTCell cell)) -> RTEnv cell -> RTEnv cell
overBindings f env = env { rtEnvBindings = f (rtEnvBindings env) }

bindNameToFreshCell :: Has (Cells cell) sig m
                    => Has (State (RTState cell)) sig m
                    => BindingName
                    -> m (RTCell cell)
bindNameToFreshCell name = do
  cell <- RTCell <$> newCell ValNil
  modify $ overCurrentEnv . overBindings $ Map.insert name cell
  pure cell

bindingInCurrentEnv :: BindingName
                    -> RTState cell
                    -> Maybe (RTCell cell)
bindingInCurrentEnv name = Map.lookup name . rtEnvBindings . currentEnv

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
  cell <- case bindingInCurrentEnv name state of
            Nothing   -> bindNameToFreshCell name
            Just cell -> pure cell
  assignRTCell cell val

getBoundCell :: BindingName -> RTState cell -> Maybe (RTCell cell)
getBoundCell name state
    = (go =<< rtStateLocalFrame state) <|> lookup (rtStateGlobalEnv state)
  where
    go (RTFrame env parent) = lookup env <|> (go =<< parent)
    lookup env = Map.lookup name (rtEnvBindings env)

getBoundValueM :: forall cell sig m
                . Has (Cells cell) sig m
               => Has (State (RTState cell)) sig m
               => Has (Throw RTError) sig m
               => Token -> m (RTValue cell)
getBoundValueM tk = do
  env <- get @(RTState cell)
  let name = tokenLexeme tk
  case getBoundCell name env of
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
  case getBoundCell (tokenLexeme tk) state of
    Just cell -> assignRTCell cell val
    Nothing -> RTError.throwRT tk $ "Undefined variable '"
                                 <> tokenLexeme tk
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
    modify @(RTState cell) atNewChildEnv
    action
      `finally` restoreParent
  where
    finally = finallyOnErrorOrReturn @cell
    restoreParent = modify @(RTState cell) $ maybe newState snd . atParentEnv

runInChildEnvOf :: forall cell sig m a
                 . Has (Error RTError) sig m
                => Has (Error (RTReturn cell)) sig m
                => Has (State (RTState cell)) sig m
                => Maybe (RTFrame cell) -> m a -> m a
runInChildEnvOf frame action = do
    currentFrame <- gets @(RTState cell) rtStateLocalFrame
    modify $ \state -> state { rtStateLocalFrame = Just $ RTFrame newEnv frame }
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
