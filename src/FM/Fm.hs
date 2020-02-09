{-# LANGUAGE DeriveFunctor #-}
module FM.Fm where

import           Data.IORef
import qualified Data.Map   as M
import           Data.UUID  (UUID)

data Storage k =
    Persist UUID Int k
  | Fetch UUID (Maybe Int -> k)
  deriving stock (Functor)

data Free (f:: * -> *) (k :: *) =
  Pure k |
  Impure (f (Free f k))

instance Functor f => Functor (Free f) where
  fmap f (Pure k)   = Pure $ f k
  fmap f (Impure c) = Impure (fmap (fmap f) c)

instance Functor f => Applicative (Free f) where
  pure a = Pure a
  (<*>) func (Pure a)   = fmap (\f -> f a) func
  (<*>) func (Impure c) = Impure (fmap (\f -> func <*> f) c)

instance Functor f => Monad (Free f) where
  Pure k >>= f = f k
  Impure c >>= f = Impure $ fmap (\x -> x >>= f) c

interpretFree ::
  Monad m
  => (forall x. f x -> m x)
  -> Free f a
  -> m a
interpretFree _ (Pure a)   = pure a
interpretFree f (Impure c) = f c >>= interpretFree f

persist :: UUID -> Int -> Free Storage ()
persist uuid i = Impure (Persist uuid i (Pure ()))

fetch :: UUID -> Free Storage (Maybe Int)
fetch uuid = Impure (Fetch uuid (\mi -> Pure mi))

-- | take Int, fetch existing Int (if does not exist, default to zero)
-- | add them, store the result, return result as text
doStuff :: UUID -> Int -> Free Storage String
doStuff uuid i = do
  maybeOld <- fetch uuid
  let
    oldI = maybe 0 id maybeOld
    newI = oldI + i
  persist uuid newI
  pure ("New value: " ++ (show newI))

type InMemStorage = M.Map UUID Int

interpret :: IORef InMemStorage -> Storage a -> IO a
interpret ioRef (Persist uuid i k) = do
  modifyIORef ioRef (M.insert uuid i)
  pure k
interpret ioRef (Fetch uuid kFunc) = do
  inmem <- readIORef ioRef
  let maybeI = M.lookup uuid inmem
  pure $ kFunc maybeI
