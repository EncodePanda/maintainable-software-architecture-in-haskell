module BS.GenerateInvoice where

import           BS.Effects.CdrStore
import           BS.Effects.Crm
import           BS.Effects.InvoiceStore
import           BS.Types
import           Polysemy

generateInvoice ::
     Member CdrStore r
  => Member Crm r
  => Member InvoiceStore r
  => AccountId
  -> Sem r Invoice
generateInvoice accId = do
  invNumber   <- genNextInvoiceNumber accId
  profile     <- getProfile accId
  cdrs        <- fetchCdrs accId
  let invoice = mkInvoice invNumber profile cdrs
  storeInvoice accId invoice
  return invoice

mkInvoice ::
     InvoiceNumber
  -> Profile
  -> [Cdr]
  -> Invoice
mkInvoice invNum Profile {..} cdrs = Invoice
  { invoiceNumber = invNum
  , fullName = FullName firstName lastName
  , deliveryAddress= address
  , total = foldr cost zeroCents cdrs
  }
  where
    cost (Cdr _ _ Voice (Duration duration)) acc = acc + (voiceCost plan * duration)
    cost (Cdr _ _ Sms (Duration amount)) acc = acc + (smsCost plan * amount)
