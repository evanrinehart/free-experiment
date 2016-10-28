{-# LANGUAGE RankNTypes #-}
module View where

newtype Pid a = Pid { unPid :: Int } deriving (Show)
data Processes = Procs { procsLookup :: forall a . Pid a -> Maybe a }
newtype View a = View { runView :: Processes -> a }

instance Functor View where
  fmap f (View l) = View (f . l)

instance Applicative View where
  pure x = View (const x)
  View lf <*> View lx = View (lf <*> lx)

instance Monoid a => Monoid (View a) where
  mempty = pure mempty
  View l `mappend` View r = View (l `mappend` r)

instance Monad View where
  return = pure
  v >>= f = vjoin (fmap f v)

vjoin :: View (View a) -> View a
vjoin vv = View $ \ps ->
  let v = runView vv ps in runView v ps

viewPid :: Pid a -> View (Maybe a)
viewPid pid = View (\ps -> procsLookup ps pid)

