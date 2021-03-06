{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.RTState
  ( RTState, newState
  , RTFrame, localFrame
  , runInChildEnv, runInChildEnvOf, childFrameWithBinding
  , defineM, assignM, assignAtM, getBoundValueM, getBoundValueAtM
  ) where

import Control.Applicative
import Control.Effect.Error
import Control.Effect.State
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HSLox.Cells.Effect
import HSLox.Token (Token (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.Runtime

newEnv :: RTEnv cell
newEnv = RTEnv Map.empty

newState :: RTState cell
newState = RTState newEnv Nothing

localFrame :: RTState cell -> Maybe (RTFrame cell)
localFrame = rtStateLocalFrame

addAsChildFrame :: RTEnv cell -> RTState cell -> RTState cell
addAsChildFrame env state =
  let newFrame = RTFrame env (rtStateLocalFrame state)
  in state { rtStateLocalFrame = Just newFrame }

atNewChildEnv :: RTState cell -> RTState cell
atNewChildEnv state = addAsChildFrame newEnv state

overBindings :: (Map BindingName (RTCell cell) -> Map BindingName (RTCell cell)) -> RTEnv cell -> RTEnv cell
overBindings f env =
  env { rtEnvBindings = f (rtEnvBindings env)}

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
bindingInCurrentEnv name = Map.lookup name
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
  cell <- case bindingInCurrentEnv name state of
            Nothing   -> bindNameToFreshCell name
            Just cell -> pure cell
  assignRTCell cell val

getBoundCell :: BindingName
             -> RTState cell
             -> Maybe (RTCell cell)
getBoundCell name state
    = (go =<< rtStateLocalFrame state) <|> lookup (rtStateGlobalEnv state)
  where
    go :: RTFrame cell -> Maybe (RTCell cell)
    go (RTFrame env parent) = do
      case lookup env of
        Just cell -> Just cell
        Nothing -> go =<< parent
    lookup :: RTEnv cell -> Maybe (RTCell cell)
    lookup env = Map.lookup name (rtEnvBindings env)

getBoundValueM :: forall cell sig m
                . Has (Cells cell) sig m
               => Has (State (RTState cell)) sig m
               => Has (Throw RTError) sig m
               => Token -> m (RTValue cell)
getBoundValueM tk = do
  state <- get @(RTState cell)
  let name = tokenLexeme tk
  case getBoundCell name state of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."

getGlobalCell :: BindingName
              -> RTState cell
              -> Maybe (RTCell cell)
getGlobalCell name state =
  Map.lookup name . rtEnvBindings . rtStateGlobalEnv $ state

getLocalCell :: BindingName
             -> Int
             -> Maybe (RTFrame cell)
             -> Maybe (RTCell cell)
getLocalCell _ _ Nothing = Nothing
getLocalCell name 0 (Just frame) =
  Map.lookup name . rtEnvBindings . rtFrameEnv $ frame
getLocalCell name distance (Just frame) =
  getLocalCell name (distance - 1) (rtFrameEnclosing frame)

getBoundValueAtM :: forall cell sig m
                  . Has (Cells cell) sig m
                 => Has (State (RTState cell)) sig m
                 => Has (Throw RTError) sig m
                 => Token -> Maybe Int -> m (RTValue cell)
getBoundValueAtM tk Nothing = do
  let name = tokenLexeme tk
  cell <- gets @(RTState cell) $ getGlobalCell name
  case cell of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."
getBoundValueAtM tk (Just distance) = do
  let name = tokenLexeme tk
  cell <- gets @(RTState cell) $ getLocalCell name distance . rtStateLocalFrame
  case cell of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined local '" <> name <> "'."

assignAtM :: forall cell sig m
           . Has (Cells cell) sig m
          => Has (State (RTState cell)) sig m
          => Has (Throw RTError) sig m
          => Token -> Maybe Int -> RTValue cell -> m ()
assignAtM tk Nothing val = do
  let name = tokenLexeme tk
  cell <- gets @(RTState cell) $ getGlobalCell name
  case cell of
    Just cell -> assignRTCell cell val
    Nothing -> RTError.throwRT tk $ "Undefined variable '"
                                 <> name
                                 <> "'."
assignAtM tk (Just distance) val = do
  let name = tokenLexeme tk
  cell <- gets @(RTState cell) $ getLocalCell name distance . rtStateLocalFrame
  case cell of
    Just cell -> assignRTCell cell val
    Nothing -> RTError.throwRT tk $ "Undefined local '"
                                 <> name
                                 <> "'."

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
  case getBoundCell name state of
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
               . Has (Error RTError) sig m
              => Has (Error (RTReturn cell)) sig m
              => Has (State (RTState cell)) sig m
              => m () -> m ()
runInChildEnv action = do
    modify @(RTState cell) atNewChildEnv
    action
      `finally` restoreParent
  where
    finally = finallyOnErrorOrReturn @cell
    restoreParent = modify @(RTState cell) $ \s ->
      case atParentEnv s of
        Just (_, s) -> s
        Nothing -> newState

childFrameWithBinding :: forall cell sig m
                       . Has (Cells cell) sig m
                      => Has (State (RTState cell)) sig m
                      => BindingName -> RTValue cell -> m (RTFrame cell)
childFrameWithBinding name val = do
  frame <- gets @(RTState cell) localFrame
  cell <- RTCell <$> newCell val
  let env = newEnv @cell & overBindings (Map.insert name cell)
  pure $ RTFrame env frame

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
