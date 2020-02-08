{-# LANGUAGE DeriveFunctor #-}
module FM.Fm where

import           Data.IORef
import qualified Data.Map   as M
import           Data.UUID  (UUID)

data Storage k =
    Done k
  | Persist UUID Int (Storage k)
  -- | Fetch UUID (Maybe Int -> Storage k)
  deriving stock (Functor, Eq, Show)

instance Applicative Storage where
  pure a = Done a
  (<*>) func (Done a)              = fmap (\f -> f a) func
  (<*>) func (Persist uuid i next) = Persist uuid i (func <*> next)

-- | take Int, store it once, story it twice, return +1 as text
doStuff :: UUID -> Int -> Storage String
doStuff uuid i =
  (Persist uuid newI
      (Persist uuid newI
          (Done $ "New value: " ++ (show newI))
      )
  )
  where
    newI = i + 1

type InMemStorage = M.Map UUID Int

interpret :: IORef InMemStorage -> Storage a -> IO a
interpret ioRef (Done a) = pure a
interpret ioRef (Persist uuid i next) =
  (modifyIORef ioRef (M.insert uuid i)) *> (interpret ioRef next)
