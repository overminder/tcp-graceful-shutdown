module Main where

import Data.Maybe (fromJust)
import System.Environment (getEnv, lookupEnv)

import Lib
import Types

main :: IO ()
main = do
  remoteHost <- lookupEnv "REMOTE_HOST"
  port <- read <$> getEnv "PORT"
  bsLen <- read <$> getEnv "BS_LEN"
  nClients <- read <$> getEnv "MAX_CLIENTS"
  isReceiver <- read <$> getEnv "IS_RECEIVER"
  soLinger <- read <$> getEnv "SO_LINGER"
  let
    rArgs = ReceiverArgs {
      raBsLen = bsLen,
      raReportPer = 10
    }
    sArgs = SenderArgs {
      saBsLen = bsLen,
      saClients = nClients
    }
    tArgs = TcpArgs {
      taHost = if isReceiver then "0.0.0.0" else fromJust remoteHost,
      taPort = port,
      taIsServer = isReceiver,
      taSoLinger = if soLinger >= 0 then Just soLinger else Nothing
    }
  if isReceiver
    then serveReceiver rArgs tArgs
    else runSender sArgs tArgs
