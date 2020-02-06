module FM.Fm where

import           Data.IORef
import qualified Data.Map   as M
import           Data.UUID  (UUID)

data Storage =
  Persist UUID Int
  deriving stock (Eq, Show)

-- | take Int, store it once, story it twice, return +1 as text
doStuff :: UUID -> Int -> ([Storage], String)
doStuff uuid i =
  ( [ (Persist uuid newI)
    , (Persist uuid newI)
    ]
  , "New value: " ++ (show newI)
  )
  where
    newI = i + 1

type InMemStorage = M.Map UUID Int

interpret :: IORef InMemStorage -> ([Storage], a) -> IO a
interpret ioRef (actions, i) = do
  traverse perform actions
  return i
  where
    perform (Persist uuid pi) = modifyIORef ioRef (M.insert uuid pi)
