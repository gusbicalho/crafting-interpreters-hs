{-# LANGUAGE AllowAmbiguousTypes #-}

module HSLox.TreeWalk.RTState (
  Runtime.RTState,
  newState,
  Runtime.RTFrame,
  localFrame,
  runInChildEnv,
  runInChildEnvOf,
  childFrameWithBinding,
  defineM,
  assignM,
  assignAtM,
  getBoundValueM,
  getBoundValueAtM,
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Error (Error, Throw)
import Control.Effect.Error qualified as Error
import Control.Effect.State (State)
import Control.Effect.State qualified as State
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import HSLox.Cells.Effect (Cells)
import HSLox.Cells.Effect qualified as Cells
import HSLox.Token (Token (..))
import HSLox.TreeWalk.RTError qualified as RTError
import HSLox.TreeWalk.Runtime qualified as Runtime

newEnv :: Runtime.RTEnv cell
newEnv = Runtime.RTEnv Map.empty

newState :: Runtime.RTState cell
newState = Runtime.RTState newEnv Nothing

localFrame :: Runtime.RTState cell -> Maybe (Runtime.RTFrame cell)
localFrame = Runtime.rtStateLocalFrame

addAsChildFrame :: Runtime.RTEnv cell -> Runtime.RTState cell -> Runtime.RTState cell
addAsChildFrame env state =
  let newFrame = Runtime.RTFrame env (Runtime.rtStateLocalFrame state)
   in state{Runtime.rtStateLocalFrame = Just newFrame}

atNewChildEnv :: Runtime.RTState cell -> Runtime.RTState cell
atNewChildEnv = addAsChildFrame newEnv

overBindings :: (Map Runtime.BindingName (Runtime.RTCell cell) -> Map Runtime.BindingName (Runtime.RTCell cell)) -> Runtime.RTEnv cell -> Runtime.RTEnv cell
overBindings f env =
  env{Runtime.rtEnvBindings = f (Runtime.rtEnvBindings env)}

overFrameEnv :: (Runtime.RTEnv cell -> Runtime.RTEnv cell) -> Runtime.RTFrame cell -> Runtime.RTFrame cell
overFrameEnv f frame =
  frame{Runtime.rtFrameEnv = f (Runtime.rtFrameEnv frame)}

overCurrentEnv :: (Runtime.RTEnv cell -> Runtime.RTEnv cell) -> Runtime.RTState cell -> Runtime.RTState cell
overCurrentEnv f state =
  case Runtime.rtStateLocalFrame state of
    Just frame -> state{Runtime.rtStateLocalFrame = Just (overFrameEnv f frame)}
    Nothing -> state{Runtime.rtStateGlobalEnv = f (Runtime.rtStateGlobalEnv state)}

currentEnv :: Runtime.RTState cell -> Runtime.RTEnv cell
currentEnv state = case Runtime.rtStateLocalFrame state of
  Just frame -> Runtime.rtFrameEnv frame
  Nothing -> Runtime.rtStateGlobalEnv state

bindNameToFreshCell ::
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Runtime.BindingName ->
  m (Runtime.RTCell cell)
bindNameToFreshCell name = do
  cell <- Runtime.RTCell <$> Cells.newCell Runtime.ValNil
  State.modify $ overCurrentEnv . overBindings $ Map.insert name cell
  pure cell

bindingInCurrentEnv ::
  Runtime.BindingName ->
  Runtime.RTState cell ->
  Maybe (Runtime.RTCell cell)
bindingInCurrentEnv name =
  Map.lookup name
    . Runtime.rtEnvBindings
    . currentEnv

assignRTCell ::
  Has (Cells cell) sig m =>
  Runtime.RTCell cell ->
  Runtime.RTValue cell ->
  m ()
assignRTCell (Runtime.RTCell cell) val = Cells.writeCell val cell

readRTCell ::
  Has (Cells cell) sig m =>
  Runtime.RTCell cell ->
  m (Runtime.RTValue cell)
readRTCell (Runtime.RTCell cell) = Cells.readCell cell

defineM ::
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Runtime.BindingName ->
  Runtime.RTValue cell ->
  m ()
defineM name val = do
  state <- State.get
  cell <- case bindingInCurrentEnv name state of
    Nothing -> bindNameToFreshCell name
    Just cell -> pure cell
  assignRTCell cell val

getBoundCell ::
  Runtime.BindingName ->
  Runtime.RTState cell ->
  Maybe (Runtime.RTCell cell)
getBoundCell name state =
  (go =<< Runtime.rtStateLocalFrame state) <|> lookup (Runtime.rtStateGlobalEnv state)
 where
  go :: Runtime.RTFrame cell -> Maybe (Runtime.RTCell cell)
  go (Runtime.RTFrame env parent) = do
    case lookup env of
      Just cell -> Just cell
      Nothing -> go =<< parent
  lookup :: Runtime.RTEnv cell -> Maybe (Runtime.RTCell cell)
  lookup env = Map.lookup name (Runtime.rtEnvBindings env)

getBoundValueM ::
  forall cell sig m.
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  m (Runtime.RTValue cell)
getBoundValueM tk = do
  state <- State.get @(Runtime.RTState cell)
  let name = tokenLexeme tk
  case getBoundCell name state of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."

getGlobalCell ::
  Runtime.BindingName ->
  Runtime.RTState cell ->
  Maybe (Runtime.RTCell cell)
getGlobalCell name = Map.lookup name . Runtime.rtEnvBindings . Runtime.rtStateGlobalEnv

getLocalCell ::
  Runtime.BindingName ->
  Int ->
  Maybe (Runtime.RTFrame cell) ->
  Maybe (Runtime.RTCell cell)
getLocalCell _ _ Nothing = Nothing
getLocalCell name 0 (Just frame) =
  Map.lookup name . Runtime.rtEnvBindings . Runtime.rtFrameEnv $ frame
getLocalCell name distance (Just frame) =
  getLocalCell name (distance - 1) (Runtime.rtFrameEnclosing frame)

getBoundValueAtM ::
  forall cell sig m.
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Maybe Int ->
  m (Runtime.RTValue cell)
getBoundValueAtM tk Nothing = do
  let name = tokenLexeme tk
  cell <- State.gets @(Runtime.RTState cell) $ getGlobalCell name
  case cell of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."
getBoundValueAtM tk (Just distance) = do
  let name = tokenLexeme tk
  cell <- State.gets @(Runtime.RTState cell) $ getLocalCell name distance . Runtime.rtStateLocalFrame
  case cell of
    Just cell -> readRTCell cell
    Nothing -> RTError.throwRT tk $ "Undefined local '" <> name <> "'."

assignAtM ::
  forall cell sig m.
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Maybe Int ->
  Runtime.RTValue cell ->
  m ()
assignAtM tk Nothing val = do
  let name = tokenLexeme tk
  cell <- State.gets @(Runtime.RTState cell) $ getGlobalCell name
  case cell of
    Just cell -> assignRTCell cell val
    Nothing ->
      RTError.throwRT tk $
        "Undefined variable '"
          <> name
          <> "'."
assignAtM tk (Just distance) val = do
  let name = tokenLexeme tk
  cell <- State.gets @(Runtime.RTState cell) $ getLocalCell name distance . Runtime.rtStateLocalFrame
  case cell of
    Just cell -> assignRTCell cell val
    Nothing ->
      RTError.throwRT tk $
        "Undefined local '"
          <> name
          <> "'."

atParentEnv :: Runtime.RTState cell -> Maybe (Runtime.RTEnv cell, Runtime.RTState cell)
atParentEnv state = case Runtime.rtStateLocalFrame state of
  Nothing -> Nothing
  Just frame -> Just (Runtime.rtFrameEnv frame, state{Runtime.rtStateLocalFrame = Runtime.rtFrameEnclosing frame})

assignM ::
  forall cell sig m.
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Runtime.RTValue cell ->
  m ()
assignM tk val = do
  state <- State.get
  let name = tokenLexeme tk
  case getBoundCell name state of
    Just cell -> assignRTCell cell val
    Nothing ->
      RTError.throwRT tk $
        "Undefined variable '"
          <> name
          <> "'."

finallyOnErrorOrReturn ::
  forall cell sig m a.
  Has (Error (Runtime.RTReturn cell)) sig m =>
  Has (Error Runtime.RTError) sig m =>
  m a ->
  m () ->
  m a
finallyOnErrorOrReturn action restore = do
  v <-
    action
      & ( `Error.catchError`
            \(e :: Runtime.RTError) -> do
              restore
              Error.throwError e
        )
      & ( `Error.catchError`
            \(e :: (Runtime.RTReturn cell)) -> do
              restore
              Error.throwError e
        )
  restore
  pure v

runInChildEnv ::
  forall cell sig m.
  Has (Error Runtime.RTError) sig m =>
  Has (Error (Runtime.RTReturn cell)) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  m () ->
  m ()
runInChildEnv action = do
  State.modify @(Runtime.RTState cell) atNewChildEnv
  action
    `finally` restoreParent
 where
  finally = finallyOnErrorOrReturn @cell
  restoreParent = State.modify @(Runtime.RTState cell) $ \s ->
    case atParentEnv s of
      Just (_, s) -> s
      Nothing -> newState

childFrameWithBinding ::
  forall cell sig m.
  Has (Cells cell) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Runtime.BindingName ->
  Runtime.RTValue cell ->
  m (Runtime.RTFrame cell)
childFrameWithBinding name val = do
  frame <- State.gets @(Runtime.RTState cell) localFrame
  cell <- Runtime.RTCell <$> Cells.newCell val
  let env = newEnv @cell & overBindings (Map.insert name cell)
  pure $ Runtime.RTFrame env frame

runInChildEnvOf ::
  forall cell sig m a.
  Has (Error Runtime.RTError) sig m =>
  Has (Error (Runtime.RTReturn cell)) sig m =>
  Has (State (Runtime.RTState cell)) sig m =>
  Maybe (Runtime.RTFrame cell) ->
  m a ->
  m a
runInChildEnvOf frame action = do
  currentFrame <- State.gets @(Runtime.RTState cell) Runtime.rtStateLocalFrame
  State.modify $ \state -> state{Runtime.rtStateLocalFrame = Just $ Runtime.RTFrame newEnv frame}
  action
    `finally` restore currentFrame
 where
  finally = finallyOnErrorOrReturn @cell
  restore frame = State.modify $ \state -> state{Runtime.rtStateLocalFrame = frame}
