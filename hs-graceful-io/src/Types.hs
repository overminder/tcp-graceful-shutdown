module Types where

data ReceiverArgs
  = ReceiverArgs
  { raBsLen :: Int
  , raReportPer :: Int
  }

data SenderArgs 
  = SenderArgs
  { saBsLen :: Int
  , saClients :: Int
  }

data TcpArgs
  = TcpArgs
  { taHost :: String
  , taPort :: Int
  , taIsServer :: Bool
  , taSoLinger :: Maybe Int
  }
