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
import HSLox.TreeWalk.RTValue (RTValue (..))

type BindingName = T.Text

data RTEnv = RTEnv { rtEnvBindings :: Map BindingName RTValue
                   }

data RTState = RTState { rtStateGlobalEnv :: RTEnv
                       , rtStateLocalEnvs :: [RTEnv]
                       }

newEnv :: RTEnv
newEnv = RTEnv Map.empty

newState :: RTState
newState = RTState newEnv []

atNewChildEnv :: RTState -> RTState
atNewChildEnv state = state { rtStateLocalEnvs = newEnv : rtStateLocalEnvs state }

overCurrentEnv :: (RTEnv -> RTEnv) -> RTState -> RTState
overCurrentEnv f state =
  case rtStateLocalEnvs state of
    (env : envs) -> state { rtStateLocalEnvs = f env : envs }
    [] -> state { rtStateGlobalEnv = f (rtStateGlobalEnv state)}

currentEnv :: RTState -> RTEnv
currentEnv state = case rtStateLocalEnvs state of
  (env : _) -> env
  [] -> rtStateGlobalEnv state

overBindings :: (Map BindingName RTValue -> Map BindingName RTValue) -> RTEnv -> RTEnv
overBindings f env = env { rtEnvBindings = f (rtEnvBindings env) }

bindNameToValue :: BindingName -> RTValue -> RTState -> RTState
bindNameToValue name val = overCurrentEnv . overBindings $ Map.insert name val

defineM :: Has (State RTState) sig m => BindingName -> RTValue -> m ()
defineM name val = modify $ bindNameToValue name val

getBoundValue :: BindingName -> RTState -> Maybe RTValue
getBoundValue name state
    = (go $ rtStateLocalEnvs state) <|> lookup (rtStateGlobalEnv state)
  where
    go [] = Nothing
    go (env : envs) = lookup env <|> go envs
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
atParentEnv state = case rtStateLocalEnvs state of
  [] -> Nothing
  (env : envs) -> Just (env, state { rtStateLocalEnvs = envs })

assign :: Token -> RTValue -> RTState -> Maybe RTState
assign tk val state = go state
  where
    go state =
      if found state
      then Just $ change state
      else do
        (local, parent) <- atParentEnv state
        parent <- go parent
        pure $ parent { rtStateLocalEnvs = local : rtStateLocalEnvs parent }
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

runInChildEnv :: Has (Throw RTError) sig m
              => Has (Catch RTError) sig m
              => Has (State RTState) sig m
              => m () -> m ()
runInChildEnv action = do
    modify atNewChildEnv
    action `catchError` \e -> do
      restoreParent
      throwError @RTError e
    restoreParent
  where
    restoreParent = modify $ maybe newState snd . atParentEnv
