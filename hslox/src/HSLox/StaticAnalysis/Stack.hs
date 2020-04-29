module HSLox.StaticAnalysis.Stack
  ( Stack
  , emptyStack, push, pop, pop_, peek, overPeek, overPeekA
  ) where

newtype Stack a = Stack [a]
  deriving (Show)
  deriving newtype (Foldable)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push scope (Stack scopes) = Stack (scope : scopes)

pop :: Stack a -> (Maybe a, Stack a)
pop s@(Stack []) = (Nothing, s)
pop (Stack (x:xs)) = (Just x, Stack xs)

pop_ :: Stack a -> Stack a
pop_ s@(Stack []) = s
pop_ (Stack (_:xs)) = Stack xs

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

overPeek :: (a -> a) -> Stack a -> Stack a
overPeek _ s@(Stack []) = s
overPeek f (Stack (x : xs)) = Stack (f x : xs)

overPeekA :: Applicative m => (a -> m a) -> Stack a -> m (Stack a)
overPeekA _ s@(Stack []) = pure s
overPeekA f (Stack (x : xs)) = do
  fx <- f x
  pure $ Stack (fx : xs)
