module HSLox.TreeWalk.RTEnv where

import Control.Effect.Throw
import Control.Effect.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import HSLox.Token (Token (..))
import HSLox.TreeWalk.RTError (RTError (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.RTValue (RTValue (..))

type BindingName = T.Text

data RTEnv = RTEnv { rtEnvBindings :: Map BindingName RTValue }

newEnv :: RTEnv
newEnv = RTEnv Map.empty

define :: BindingName -> RTValue -> RTEnv -> RTEnv
define name val env = RTEnv $ Map.insert name val (rtEnvBindings env)

defineM :: Has (State RTEnv) sig m => BindingName -> RTValue -> m ()
defineM name val = modify $ define name val

getBoundValue :: BindingName -> RTEnv -> Maybe RTValue
getBoundValue name env = Map.lookup name (rtEnvBindings env)

getBindingValueM :: Has (State RTEnv) sig m
                 => Has (Throw RTError) sig m
                 => Token -> m RTValue
getBindingValueM tk = do
  env <- get @RTEnv
  let name = tokenLexeme tk
  case getBoundValue name env of
    Just val -> pure val
    Nothing -> RTError.throwRT tk $ "Undefined variable '" <> name <> "'."
