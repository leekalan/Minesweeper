{-# LANGUAGE MultiParamTypeClasses #-}
module Utils.Field ( GetField, getField, SetField, setField ) where

class GetField a b where
  getField :: a -> b

class SetField a b where
  setField :: b -> a -> a
