module Util.Monad where
import Control.Monad (foldM)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f = foldr (\x -> (<*>) $ (||) <$> f x) (return False)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f = foldr (\x -> (<*>) $ (&&) <$> f x) (return False)

-- from: https://hackage.haskell.org/package/extra-1.7.10/docs/src/Control.Monad.Extra.html#ifM
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

-- from: https://hackage.haskell.org/package/extra-1.7.10/docs/src/Control.Monad.Extra.html#findM
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
