{-# LANGUAGE TemplateHaskell #-}
module BS.Effects.Crm where

import           BS.Types  (AccountId, Address, Cent)
import           Data.Text
import           Polysemy

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
