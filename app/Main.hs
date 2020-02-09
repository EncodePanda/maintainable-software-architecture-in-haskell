module Main where

import           Data.IORef
import qualified Data.Map     as M
import           Data.UUID.V4 (nextRandom)
import           FM.Fm

main :: IO ()
main = do
  uuid <- nextRandom
  ioRef <- newIORef $ M.singleton uuid 5
  res <- interpretFree (interpret ioRef) (program uuid)
  putStrLn res

program uuid = do
  doStuff uuid 10
  doStuff uuid 20
