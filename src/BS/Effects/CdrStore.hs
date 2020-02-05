{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.CdrStore where

import           BS.Types  (AccountId)
import           Data.UUID (UUID)
import           Polysemy

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
  StoreCdr  :: AccountId -> Cdr -> CdrStore m ()
  FetchCdrs :: AccountId -> CdrStore m [Cdr]

makeSem ''CdrStore
