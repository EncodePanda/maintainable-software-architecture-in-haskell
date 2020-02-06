module Main where

import           Data.IORef
import qualified Data.Map     as M
import           Data.UUID.V4 (nextRandom)
import           FM.Fm

main :: IO ()
main = do
  ioRef <- newIORef $ M.empty
  uuid <- nextRandom
  res <- interpret ioRef (doStuff uuid 10)
  putStrLn res
