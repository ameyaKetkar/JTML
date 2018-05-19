module StateMonad where

--
-- The State monad
--

data State s a = State (s -> (a,s))

instance Functor (State s) where
  fmap f (State c) = State (\s->let (x,s') = c s in (f x,s'))

instance Applicative (State s) where
  pure x = State (\s -> (x,s))
  (State f) <*> (State c) = State (\s->let (g,s') = f s
                                           (y,s'') = c s'
                                        in (g y, s''))

instance Monad (State s) where
  return x = State (\s->(x,s))
  State c >>= f = State (\s->let (x,s') = c s
                                 State d = f x
                             in d s')

runState :: State s a -> s -> (a,s)
runState (State f) = f

runWith :: s -> State s a -> (a,s)
runWith = flip runState

-- exec = runState
-- execWith = runWith

onState :: (s -> s) -> State s ()
onState f = State $ \s->((),f s)

fromState :: (s -> a) -> State s a
fromState f = State $ \s->(f s,s)

readState :: State s s
readState = State $ \s->(s,s)

init :: s -> State s ()
init s = State $ const ((),s)