{-# LANGUAGE DeriveFunctor #-}
module FM.Fm where

import           Data.IORef
import qualified Data.Map   as M
import           Data.UUID  (UUID)

data Storage k =
    Done k
  | Persist UUID Int (Storage k)
  | Fetch UUID (Maybe Int -> Storage k)
  deriving stock (Functor)

instance Applicative Storage where
  pure a = Done a
  (<*>) func (Done a)              = fmap (\f -> f a) func
  (<*>) func (Persist uuid i next) = Persist uuid i (func <*> next)

instance Monad Storage where
  (Done a) >>= f = f a
  (Persist uuid i next) >>= f = Persist uuid i (next >>= f)
  (Fetch uuid nextFunc) >>= f = Fetch uuid (\mi -> (nextFunc mi) >>= f)

persist :: UUID -> Int -> Storage ()
persist uuid i = Persist uuid i (Done ())

fetch :: UUID -> Storage (Maybe Int)
fetch uuid = Fetch uuid pure

-- | take Int, fetch existing Int (if does not exist, default to zero)
-- | add them, store the result, return result as text
doStuff :: UUID -> Int -> Storage String
doStuff uuid i = do
  maybeOld <- fetch uuid
  let
    oldI = maybe 0 id maybeOld
    newI = oldI + i
  persist uuid newI
  pure ("New value: " ++ (show newI))

type InMemStorage = M.Map UUID Int

interpret :: IORef InMemStorage -> Storage a -> IO a
interpret ioRef (Done a) = pure a
interpret ioRef (Persist uuid i next) =
  (modifyIORef ioRef (M.insert uuid i)) *> (interpret ioRef next)
interpret ioRef (Fetch uuid nextFunc) = do
  inmem <- readIORef ioRef
  let maybeI = M.lookup uuid inmem
  interpret ioRef (nextFunc maybeI)
