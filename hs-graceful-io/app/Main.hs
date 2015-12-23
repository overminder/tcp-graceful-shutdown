module Main where

import Data.Maybe (fromJust)
import System.Environment (getEnv, lookupEnv)

import Lib
import Types

readEnv name = read <$> getEnv name

main :: IO ()
main = do
  remoteHost <- lookupEnv "REMOTE_HOST"
  port <- readEnv "PORT"
  bsLen <- readEnv "BS_LEN"
  nClients <- readEnv "MAX_CLIENTS"
  isReceiver <- readEnv "IS_RECEIVER"
  soLinger <- readEnv "SO_LINGER"
  gracefulShutdown <- readEnv "TCP_GRACEFUL_SHUTDOWN"
  let
    rArgs = ReceiverArgs {
      raBsLen = bsLen,
      raReportPer = 10
    }
    sArgs = SenderArgs {
      saBsLen = bsLen,
      saClients = nClients,
      saReportPer = 10
    }
    tArgs = TcpArgs {
      taHost = if isReceiver then "0.0.0.0" else fromJust remoteHost,
      taPort = port,
      taIsServer = isReceiver,
      taSoLinger = if soLinger >= 0 then Just soLinger else Nothing,
      taGracefulShutdown = gracefulShutdown
    }
  if isReceiver
    then do
      putStrLn $ "serveReceiver: " ++ show rArgs ++ "; " ++ show tArgs
      serveReceiver rArgs tArgs
    else do
      putStrLn $ "runSender: " ++ show sArgs ++ "; " ++ show tArgs
      runSender sArgs tArgs
