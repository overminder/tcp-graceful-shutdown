module Lib
    ( serveReceiver
    , runSender
    ) where

import Pipes
import Pipes.Network.TCP.Safe
import Control.Exception
import Control.Monad (forever, when, replicateM)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Network.Socket (shutdown, ShutdownCmd(..))
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Word

import Types

connectTA (TcpArgs {..}) = connect taHost (show taPort)
serveTA (TcpArgs {..}) = serve (Host taHost) (show taPort)

runSender :: SenderArgs -> TcpArgs -> IO () 
runSender (SenderArgs {..}) tArgs@(TcpArgs {..}) = do
  putStrLn $ "Connecting to " ++ taHost ++ ":" ++ show taPort
  connectedRef <- newTVarIO 0
  let payload = B.replicate saBsLen (0 :: Word8)
  ts <- replicateM saClients $ async $ runSafeT $ do
    connectTA tArgs $ \ (sock, _) -> lift $ do
      connected <- atomically $ do
        modifyTVar' connectedRef (+1)
        readTVar connectedRef
      when (connected `mod` saReportPer == 0) $ do
        putStrLn $ "Connected: " ++ show (connected, saClients)
      atomically $ do
        connected <- readTVar connectedRef
        when (connected < saClients) retry
      runEffect $ yield payload >-> toSocket sock
      res <- try $ do
        when taGracefulShutdown $ do
          shutdown sock ShutdownSend
          runEffect $ fromSocket sock 4096 >-> expectEof
          shutdown sock ShutdownReceive
      case res of
        Left (e :: SomeException) -> putStrLn $ "XXX: toSocket: " ++ show e
        Right () -> return ()
  mapM_ wait ts

expectEof = do
  absurd <- await
  lift $ putStrLn "XXX: expectEof: Should not reach here."

serveReceiver :: ReceiverArgs -> TcpArgs -> IO ()
serveReceiver rArgs tArgs@(TcpArgs {..}) = do
  statRef <- newMVar (0, 0)
  putStrLn $ "Serving at " ++ taHost ++ ":" ++ show taPort
  runSafeT $ serveTA tArgs $ \ (sock, addr) -> do
    totalReadRef <- newIORef 0
    res <- try $ do
      runEffect $ fromSocket sock 4096 >-> gatherStat totalReadRef
      when taGracefulShutdown $ shutdown sock ShutdownSend
    case res of
      Left (e :: SomeException) -> putStrLn $ "XXX: fromSocket: " ++ show e
      Right () -> return ()
    totalRead <- readIORef totalReadRef
    reportReceiverStat rArgs totalRead statRef
    threadDelay 1500000

gatherStat :: IORef Int -> Consumer B.ByteString IO ()
gatherStat totalReadRef = forever $ go
 where
  go = do
    nRead <- B.length <$> await
    lift (modifyIORef' totalReadRef (+ nRead)) >> go

reportReceiverStat :: ReceiverArgs -> Int -> MVar (Int, Int) -> IO ()
reportReceiverStat (ReceiverArgs {..}) totalRead statRef = do
  let ok = raBsLen == totalRead
  stat@(_, totalCount) <- modifyMVar statRef $ \ (okCount, totalCount) -> do
    let
      okCount' = (if ok then (+ 1) else id) $ okCount
      newStat = (okCount', totalCount + 1)
    return (newStat, newStat)
  when (totalCount `mod` raReportPer == 0) $ do
    putStrLn $ "[reportStat] counts = " ++ show stat ++ " ok = " ++ show ok ++
               "; bsCount = " ++ show (totalRead, raBsLen)
