module HSLox.TreeWalk.RTEnv where

import Control.Effect.Throw
import Control.Effect.State
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import HSLox.Token (Token (..))
import HSLox.TreeWalk.RTError (RTError (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.RTValue (RTValue (..))

type BindingName = T.Text

data RTEnv = RTEnv { rtEnvBindings :: Map BindingName RTValue
                   , rtEnvEnclosing :: Maybe RTEnv
                   }

newEnv :: RTEnv
newEnv = RTEnv Map.empty Nothing

newChildEnv :: RTEnv -> RTEnv
newChildEnv parent = RTEnv Map.empty (Just parent)

parentEnv :: RTEnv -> Maybe RTEnv
parentEnv = rtEnvEnclosing

overBindings :: (Map BindingName RTValue -> Map BindingName RTValue) -> RTEnv -> RTEnv
overBindings f env = env { rtEnvBindings = f (rtEnvBindings env) }

bindNameToValue :: BindingName -> RTValue -> RTEnv -> RTEnv
bindNameToValue name val = overBindings $ Map.insert name val

defineM :: Has (State RTEnv) sig m => BindingName -> RTValue -> m ()
defineM name val = modify $ bindNameToValue name val

getBoundValue :: BindingName -> RTEnv -> Maybe RTValue
getBoundValue name env =
  Map.lookup name (rtEnvBindings env)
  <|> (getBoundValue name =<< (rtEnvEnclosing env))

getBoundValueM :: Has (State RTEnv) sig m
               => Has (Throw RTError) sig m
               => Token -> m RTValue
getBoundValueM tk = do
  env <- get @RTEnv
  let name = tokenLexeme tk
  case getBoundValue name env of
    Just val -> pure val
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."

assign :: Token -> RTValue -> RTEnv -> Maybe RTEnv
assign tk val env = do
  let name = tokenLexeme tk
  let bindings = rtEnvBindings env
  case Map.lookup name bindings of
    Just _ -> Just $ bindNameToValue name val env
    Nothing -> do
      parent <- rtEnvEnclosing env
      parent <- assign tk val parent
      pure env { rtEnvEnclosing = Just parent }

assignM :: Has (State RTEnv) sig m
        => Has (Throw RTError) sig m
        => Token -> RTValue -> m ()
assignM tk val = do
  env <- get
  case assign tk val env of
    Just env -> put env
    Nothing -> RTError.throwRT tk $ "Undefined variable '"
                                 <> tokenLexeme tk
                                 <> "'."
