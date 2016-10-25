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

viewPid :: Pid a -> View (Maybe a)
viewPid pid = View (\l -> procsLookup l pid)

