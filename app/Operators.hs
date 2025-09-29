{-# LANGUAGE RankNTypes #-}

module Operators (
    tapM, (=>>), (<~), (~>),
    compose2, chain2, (!<~), (~>!),
    (<~|), (|~>), (|<~), (~>|),
    (<<|), (|>>), (|<<), (>>|),
    forOrElse, forElseOr,
) where

tapM :: Monad m => m a -> (a -> m b) -> m a
tapM m f = m >>= (\x -> f x >> return x)

infixl 1 =>>
(=>>) :: Monad m => m a -> (a -> m b) -> m a
(=>>) = tapM

infixr 9 <~
(<~) :: (b -> c) -> (a -> b) -> a -> c
(<~) g f x = g $ f x

infixr 9 ~>
(~>) :: (a -> b) -> (b -> c) -> a -> c
(~>) f g x = g $ f x

type Rank2Fn a b m = forall s . a -> m s b
type Rank2Consumer a b m = (forall s . m s a) -> b

compose2 :: Rank2Consumer b c m -> Rank2Fn a b m -> a -> c
compose2 g f x = g $ f x

chain2 :: Rank2Fn a b m -> Rank2Consumer b c m -> a -> c
chain2 f g x = g $ f x

infixr 9 !<~
(!<~) :: Rank2Consumer b c m -> Rank2Fn a b m -> a -> c
(!<~) = compose2

infixr 9 ~>!
(~>!) :: Rank2Fn a b m -> Rank2Consumer b c m -> a -> c
(~>!) = chain2

infixr 9 <~|
(<~|) :: Functor f => (b -> c) -> f (a -> b) -> a -> f c
(<~|) g f x = fmap (($ x) ~> g) f

infixr 9 |~>
(|~>) :: Functor f => f (a -> b) -> (b -> c) -> a -> f c
(|~>) = flip (<~|)

infixr 9 |<~
(|<~) :: Functor f => f (b -> c) -> (a -> b) -> a -> f c
(|<~) g f x = fmap ($ f x) g

infixr 9 ~>|
(~>|) :: Functor f => (a -> b) -> f (b -> c) -> a -> f c
(~>|) = flip (|<~)

infixr 9 <<|
(<<|) :: Functor f => (a -> b) -> f a -> f b
(<<|) = fmap

infixr 9 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap

infixr 9 |<<
(|<<) :: Functor f => f (a -> b) -> a -> f b
(|<<) f a = fmap ($ a) f

infixr 9 >>|
(>>|) :: Functor f => a -> f (a -> b) -> f b
(>>|) = flip (|<<)

forOrElse :: Maybe a -> (a -> b) -> b -> b
forOrElse Nothing _ b = b
forOrElse (Just a) f _ = f a

forElseOr :: Maybe a -> b -> (a -> b) -> b
forElseOr Nothing b _ = b
forElseOr (Just a) _ f = f a