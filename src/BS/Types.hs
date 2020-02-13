module BS.Types where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

newtype AccountId  = AccountId { unAccountId :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

newtype InvoiceNumber = InvoiceNumber { unInvoiceNumber :: Text }
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

type Cent = Int

zeroCents :: Cent
zeroCents = 0

data Address = Address
  { street  :: Text
  , house   :: Text
  , num     :: Text
  , city    :: Text
  , country :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON Address
instance ToJSON Address

data FullName = FullName
  { first :: Text
  , last  :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON FullName
instance ToJSON FullName

data Invoice = Invoice
  { invoiceNumber   :: InvoiceNumber
  , fullName        :: FullName
  , deliveryAddress :: Address
  , total           :: Cent
  }
  deriving stock (Show, Generic)

instance FromJSON Invoice
instance ToJSON Invoice
