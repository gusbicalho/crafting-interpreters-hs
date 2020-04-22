{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.RTState where

import Control.Effect.Error
import Control.Effect.State
import Control.Applicative
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import HSLox.Token (Token (..))
import HSLox.TreeWalk.RTError (RTError (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.RTReturn (RTReturn (..))
import HSLox.TreeWalk.RTValue (RTValue (..))

type BindingName = T.Text

newtype RTEnv = RTEnv { rtEnvBindings :: Map BindingName RTValue }
  deriving (Show)

data RTFrame = RTFrame { rtFrameEnv :: RTEnv
                       , rtFrameEnclosing :: Maybe RTFrame
                       }

data RTState = RTState { rtStateGlobalEnv :: RTEnv
                       , rtStateLocalFrame :: Maybe RTFrame
                       }

newEnv :: RTEnv
newEnv = RTEnv Map.empty

newState :: RTState
newState = RTState newEnv Nothing

addAsChildFrame :: RTEnv -> RTState -> RTState
addAsChildFrame env state =
  let newFrame = RTFrame env (rtStateLocalFrame state)
  in state { rtStateLocalFrame = Just newFrame }

atNewChildEnv :: RTState -> RTState
atNewChildEnv state = addAsChildFrame newEnv state

overFrameEnv :: (RTEnv -> RTEnv) -> RTFrame -> RTFrame
overFrameEnv f frame =
  frame { rtFrameEnv = f (rtFrameEnv frame)}

overCurrentEnv :: (RTEnv -> RTEnv) -> RTState -> RTState
overCurrentEnv f state =
  case rtStateLocalFrame state of
    Just frame -> state { rtStateLocalFrame = Just (overFrameEnv f frame) }
    Nothing -> state { rtStateGlobalEnv = f (rtStateGlobalEnv state)}

currentEnv :: RTState -> RTEnv
currentEnv state = case rtStateLocalFrame state of
  Just frame -> rtFrameEnv frame
  Nothing -> rtStateGlobalEnv state

globalEnv :: RTState -> RTEnv
globalEnv = rtStateGlobalEnv

overBindings :: (Map BindingName RTValue -> Map BindingName RTValue) -> RTEnv -> RTEnv
overBindings f env = env { rtEnvBindings = f (rtEnvBindings env) }

bindNameToValue :: BindingName -> RTValue -> RTState -> RTState
bindNameToValue name val = overCurrentEnv . overBindings $ Map.insert name val

defineM :: Has (State RTState) sig m => BindingName -> RTValue -> m ()
defineM name val = modify $ bindNameToValue name val

getBoundValue :: BindingName -> RTState -> Maybe RTValue
getBoundValue name state
    = (go =<< rtStateLocalFrame state) <|> lookup (rtStateGlobalEnv state)
  where
    go (RTFrame env parent) = lookup env <|> (go =<< parent)
    lookup env = Map.lookup name (rtEnvBindings env)

getBoundValueM :: Has (State RTState) sig m
               => Has (Throw RTError) sig m
               => Token -> m RTValue
getBoundValueM tk = do
  env <- get @RTState
  let name = tokenLexeme tk
  case getBoundValue name env of
    Just val -> pure val
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."

atParentEnv :: RTState -> Maybe (RTEnv, RTState)
atParentEnv state = case rtStateLocalFrame state of
  Nothing -> Nothing
  Just frame -> Just (rtFrameEnv frame, state { rtStateLocalFrame = rtFrameEnclosing frame })

assign :: Token -> RTValue -> RTState -> Maybe RTState
assign tk val state = go state
  where
    go state =
      if found state
      then Just $ change state
      else do
        (local, parent) <- atParentEnv state
        parent <- go parent
        pure $ addAsChildFrame local parent
    found state = currentEnv state
                & rtEnvBindings
                & Map.lookup (tokenLexeme tk)
                & maybe False (const True)
    change state = bindNameToValue (tokenLexeme tk) val state

assignM :: Has (State RTState) sig m
        => Has (Throw RTError) sig m
        => Token -> RTValue -> m ()
assignM tk val = do
  env <- get
  case assign tk val env of
    Just env -> put env
    Nothing -> RTError.throwRT tk $ "Undefined variable '"
                                 <> tokenLexeme tk
                                 <> "'."

finallyOnErrorOrReturn :: Has (Error RTReturn) sig m
                       => Has (Error RTError) sig m
                       => m a -> m () -> m a
finallyOnErrorOrReturn action restore = do
  v <- action
        & (`catchError` \(e :: RTError) -> do
            restore
            throwError e)
        & (`catchError` \(e :: RTReturn) -> do
            restore
            throwError e)
  restore
  pure v

runInChildEnv :: Has (Error RTError) sig m
              => Has (Error RTReturn) sig m
              => Has (State RTState) sig m
              => m () -> m ()
runInChildEnv action = do
    modify atNewChildEnv
    action
      `finallyOnErrorOrReturn` restoreParent
  where
    restoreParent = modify $ maybe newState snd . atParentEnv

runInChildEnvOf :: Has (Error RTError) sig m
                => Has (Error RTReturn) sig m
                => Has (State RTState) sig m
                => Maybe RTFrame -> m a -> m a
runInChildEnvOf frame action = do
    currentFrame <- gets rtStateLocalFrame
    modify $ \state -> state { rtStateLocalFrame = Just $ RTFrame newEnv frame }
    action
      `finallyOnErrorOrReturn` restore currentFrame
  where
    restore frame = modify $ \state -> state { rtStateLocalFrame = frame }

envStack :: Has (State RTState) sig m => m [RTEnv]
envStack = do
    state <- get
    pure $ rtStateGlobalEnv state : go [] (rtStateLocalFrame state)
  where
    go acc Nothing = acc
    go acc (Just frame) = go (rtFrameEnv frame : acc) (rtFrameEnclosing frame)
