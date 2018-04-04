module Utils where

import Control.Concurrent           (threadDelay)
import Control.Distributed.Process
import Control.Monad                (forM_)
import Types

logDebug :: String -> Process ()
logDebug msg = liftIO . putStrLn $ msg

sleep :: Int -> Process ()
sleep s = liftIO $ threadDelay $ s * 1000000

broadcast :: [ProcessId] -> Protocol -> Process ()
broadcast pids msg = forM_ pids $ \pid -> send pid msg