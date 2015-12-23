module Types where

data ReceiverArgs
  = ReceiverArgs
  { raBsLen :: Int
  , raReportPer :: Int
  }
  deriving (Show)

data SenderArgs 
  = SenderArgs
  { saBsLen :: Int
  , saClients :: Int
  }
  deriving (Show)

data TcpArgs
  = TcpArgs
  { taHost :: String
  , taPort :: Int
  , taIsServer :: Bool
  , taSoLinger :: Maybe Int
  }
  deriving (Show)
