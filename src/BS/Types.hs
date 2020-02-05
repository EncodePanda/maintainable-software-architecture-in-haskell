module BS.Types where

import           Data.Text

newtype AccountId  = AccountId { unAccountId :: Int }
  deriving (Show, Eq)

newtype InvoiceNumber = InvoiceNumber { unInvoiceNumber :: Text }
  deriving (Show, Eq)

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

data FullName = FullName
  { first :: Text
  , last  :: Text
  }

data Invoice = Invoice
  { invoiceNumber   :: InvoiceNumber
  , fullName        :: FullName
  , deliveryAddress :: Address
  , total           :: Cent
  }
