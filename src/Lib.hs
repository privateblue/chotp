{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( run
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad                                        (forM)
import Data.Binary
import Data.List                                            (sortOn)
import Data.Typeable
import GHC.Generics
import System.Environment                                   (getArgs)
import System.Random
import Types
import Utils

agent :: Process ()
agent = do
  Start pids seed <- expect
  myPid           <- getSelfPid
  logInfo $ (show myPid) ++ " Started"
  rngPid          <- spawnLocal $ rng myPid $ mkStdGen seed
  listen myPid pids rngPid [] 0
  logInfo "Done"
  where
    rng :: ProcessId -> StdGen -> Process ()
    rng agentPid gen = do
      maybeStop <- expectTimeout 1000000 :: Process (Maybe Protocol)
      case maybeStop of
        Just Stop -> pure ()
        Nothing -> do
          let (r, gen') = randomR (0, 1) gen
          send agentPid (Gen r)
          rng agentPid gen'

    listen :: ProcessId
           -> [ProcessId]
           -> ProcessId
           -> [(Double, Int, ProcessId)]
           -> Int
           -> Process ()
    listen myPid pids rngPid history ts = do
      msg <- expect :: Process Protocol
      case msg of
        Gen d -> do
          let nts = ts + 1
          logInfo $ (show nts) ++ ". Sending " ++ (show d)
          broadcast pids (Msg d nts myPid)
          listen myPid pids rngPid ((d, nts, myPid) : history) nts
        Msg d mts fromPid | fromPid == myPid -> do
          logInfo "Ignoring own message"
          listen myPid pids rngPid history ts
        Msg d mts fromPid -> do
          let nts = if (mts >= ts) then mts + 1 else ts + 1
          logInfo $ (show nts) ++ ". Received " ++ (show d) ++ " sent by " ++ (show fromPid) ++ " at " ++ (show mts)
          listen myPid pids rngPid ((d, mts, fromPid) : history) nts
        Stop -> do
          logInfo "Stopping RNG"
          send rngPid Stop
          listen myPid pids rngPid history ts
        Kill -> do
          let res = result $ sortOn (\(_,t,_) -> t) history
          logInfo $ "\nRESULT: " ++ (show res) ++ "\n"

    result :: [(Double, Int, ProcessId)] -> (Int, Double)
    result history =
      let l       = length history
          values  = map (\(v,_,_) -> v) history
          indices = map fromIntegral [1..l]
          summa   = sum $ zipWith (*) indices values
      in (l, summa)

remotable ['agent]

master :: Backend -> Int -> Int -> Int -> [NodeId] -> Process ()
master backend sendFor waitFor withSeed agents = do
  logInfo "Spawning agents..."
  pids <- forM agents $ \nodeId -> spawn nodeId $(mkStaticClosure 'agent)
  logInfo "Starting messages..."
  broadcast pids (Start pids withSeed)
  sleep sendFor
  logInfo "Stopping messages..."
  broadcast pids Stop
  sleep waitFor
  logInfo "Halting agents..."
  broadcast pids Kill
  terminateAllSlaves backend

run :: IO ()
run = do
  args <- getArgs
  let remoteTable = __remoteTable initRemoteTable
  case args of
    ["agent", host, port] -> do
      backend <- initializeBackend host port remoteTable
      startSlave backend
    ["master", host, port, sendFor, waitFor, withSeed] -> do
      backend <- initializeBackend host port remoteTable
      let sf = read sendFor
          wf = read waitFor
          ws = read withSeed
      startMaster backend (master backend sf wf ws)