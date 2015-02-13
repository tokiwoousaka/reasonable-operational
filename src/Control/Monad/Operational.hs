{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Operational where
import Control.Monad
import Control.Applicative
 
--
-- Coyoneda

data CoYoneda f x where 
  CoYoneda :: (b -> x) -> f b -> CoYoneda f x
 
instance Functor (CoYoneda f) where
  fmap f (CoYoneda g v) = CoYoneda (f . g) v

liftCoYoneda :: f a -> CoYoneda f a
liftCoYoneda = CoYoneda id

----
-- Free

data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
  fmap f x = x >>= return . f

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure
  Free x >>= f = Free $ fmap (>>= f) x
  Pure x >>= f = f x

liftF :: Functor f => f r -> Free f r
liftF cmd = Free (fmap Pure cmd)

----
-- Operational

newtype Program f a = Program { toFree :: Free (CoYoneda f) a }

instance Functor (Program f) where
  fmap f = Program . fmap f . toFree

instance Applicative (Program f) where
  pure = return
  (<*>) = ap

instance Monad (Program f) where
  return = Program . return 
  x >>= f = Program $ toFree x >>= toFree . f

singleton :: f a -> Program f a
singleton = Program . liftF . liftCoYoneda

interpret :: forall instr m b. Monad m => (forall a. instr a -> m a) -> Program instr b -> m b
interpret g (Program (Free (CoYoneda f x))) = g x >>= interpret g . Program . f
interpret _ (Program (Pure a)) = return a
