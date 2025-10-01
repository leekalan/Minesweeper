{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module State where

import Data.Bifunctor

import Operators

newtype Monad m => StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sa) = StateT $ sa ~> fmap (first f)

instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ (a,) ~> return

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (<*>) (StateT sf) (StateT sa) = StateT $ \s -> do
        (f, s') <- sf s
        first f <$> sa s'

instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (>>=) (StateT sa) f = StateT $ \s -> do
        (a, s') <- sa s
        runStateT (f a) s'

getT :: Monad m => StateT s m s
getT = StateT $ \s -> return (s, s)

putT :: Monad m => s -> StateT s m ()
putT s = StateT $ const $ return ((), s)

mutT :: Monad m => (s -> s) -> StateT s m ()
mutT f = StateT $ \s -> return ((), f s)

getsT :: Monad m => (s -> a) -> StateT s m a
getsT f = fmap f getT

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State sa) = State $ sa ~> first f

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (a,)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State sf) (State sa) = State $ \s ->
        let (f, s') = sf s
        in  first f $ sa s'

instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State sa) f = State $ \s ->
        let (a, s') = sa s
        in  runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

mut :: Monad m => (s -> s) -> StateT s m ()
mut f = StateT $ \s -> return ((), f s)

gets :: (s -> a) -> State s a
gets f = fmap f get

lift :: Monad m => State s a -> StateT s m a
lift (State sa) = StateT $ return . sa

liftState :: Monad m => m a -> StateT s m a
liftState m = StateT $ \s -> do
    a <- m
    return (a, s)