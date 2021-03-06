-- | A simple result type. Similar to the 'Either' type, but tuned for results with an error type.
module ConditionalRestriction.Result
  ( Result (..),
    fromResult,
  )
where

import Data.Bifunctor (Bifunctor, bimap)

-- | The 'Result' type consists of an error type @e@ and a success type @a@.
data Result e a
  = Err e
  | Ok a
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Err e) = Err e

instance Bifunctor Result where
  bimap f _ (Err e) = Err $ f e
  bimap _ f (Ok x) = Ok $ f x

instance Applicative (Result e) where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok (f x)
  (Err f) <*> _ = Err f
  _ <*> (Err x) = Err x

instance Monad (Result e) where
  return = pure
  (Ok x) >>= f = f x
  (Err x) >>= _ = Err x

-- | 'Result' equivalent to 'Data.Maybe.fromMaybe'.
fromResult :: a -> Result e a -> a
fromResult _ (Ok x) = x
fromResult x (Err _) = x
