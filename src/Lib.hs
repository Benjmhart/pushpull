{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where 
  fmap g (Free fx) = Free (fmap g <$> fx)
  fmap g (Pure x) = Pure (g x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Free ff) <*> fx = Free $(<*> fx) <$> ff
  (Pure f) <*> (Free fx) = Free $ fmap f <$> fx
  (Pure f) <*> (Pure x) = Pure $f x

instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= g = g x
  Free fx >>= g = Free ((>>= g) <$> fx)

infixr 0 ~> 
type f ~> g = forall x. f x -> g x

freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM phi (Pure x) = Pure x
freeM phi (Free fx) = Free $ phi (freeM phi <$> fx)

monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx
