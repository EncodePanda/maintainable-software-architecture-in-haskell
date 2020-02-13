{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.Crm where

import           BS.Types       (AccountId, Address, Cent)
import qualified Data.Map       as M
import           Data.Text
import           Polysemy
import           Polysemy.State

data Plan = Plan
  { voiceCost :: Cent
  , smsCost   :: Cent
  }

data Profile = Profile
  { firstName :: Text
  , lastName  :: Text
  , address   :: Address
  , plan      :: Plan
  }

data Crm m a where
  GetProfile :: AccountId -> Crm m Profile

makeSem ''Crm

type CrmMap = M.Map AccountId Profile

runCrm ::
     Member (State CrmMap) r
  => Sem (Crm ': r) a
  -> Sem r a
runCrm = interpret $ \case
  GetProfile accountId -> gets (\m -> m M.! accountId)
