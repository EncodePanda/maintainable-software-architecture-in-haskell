{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.CdrStore where

import           BS.Types       (AccountId)
import qualified Data.Map       as M
import           Data.UUID      (UUID)
import           Polysemy
import           Polysemy.State

data CallType = Voice | Sms

newtype Duration = Duration { unDuration :: Int }
  deriving stock (Show, Eq)
  deriving newtype (Num)

data Cdr = Cdr
  { uuid         :: UUID
  , accountId    :: AccountId
  , callType     :: CallType
  , callDuration :: Duration
  }

data CdrStore m a where
  FetchCdrs :: AccountId -> CdrStore m [Cdr]

makeSem ''CdrStore

type CdrMap = M.Map AccountId [Cdr]

runCdrStore ::
     Member (State CdrMap) r
  => Sem (CdrStore ': r) a
  -> Sem r a
runCdrStore = interpret $ \case
  FetchCdrs accountId -> gets (\m -> m M.! accountId)
