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
import Data.List                                            (sort)
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
  logDebug $ (show myPid) ++ " Started"
  genPid          <- spawnLocal $ gen myPid $ mkStdGen seed
  listen myPid pids genPid [] 0
  logDebug "Done"
  where
    gen :: ProcessId -> StdGen -> Process ()
    gen agentPid g = do
      maybeStop <- expectTimeout 1000 :: Process (Maybe Protocol)
      case maybeStop of
        Just Stop -> pure ()
        Nothing -> do
          let (r, g') = randomR (0, 1) g
          send agentPid (Gen r)
          gen agentPid g'

    listen :: ProcessId
           -> [ProcessId]
           -> ProcessId
           -> [Entry]
           -> Timestamp
           -> Process ()
    listen myPid pids genPid history t = do
      msg <- expect :: Process Protocol
      case msg of
        Gen d -> do
          let nt = t + 1
          logDebug $ (show nt) ++ ". Sending " ++ (show d)
          broadcast pids (Msg d nt myPid)
          listen myPid pids genPid history nt
        Msg d mt fromPid -> do
          let nt   = if (mt >= t) then mt + 1 else t + 1
              entry = Entry d mt fromPid
          logDebug $ (show nt) ++ ". Received " ++ (show d)
                    ++ " sent by " ++ (show fromPid) ++ " at " ++ (show mt)
          listen myPid pids genPid (entry : history) nt
        Stop -> do
          let nt = t + 1
          logDebug $ (show nt) ++ ". Stopping Generator"
          send genPid Stop
          listen myPid pids genPid history nt
        Kill -> do
          let res = result $ sort history
          logDebug $ "\nRESULT: " ++ (show res) ++ "\n"

    result :: [Entry] -> (Int, Double)
    result history =
      let l       = length history
          values  = map _eValue history
          indices = map fromIntegral [1..l]
          summa   = sum $ zipWith (*) indices values
      in (l, summa)

remotable ['agent]

master :: Backend -> MasterConfig -> [NodeId] -> Process ()
master backend (MasterConfig sendFor waitFor withSeed) agents = do
  logDebug "Spawning agents..."
  pids <- forM agents $ \nodeId -> spawn nodeId $(mkStaticClosure 'agent)
  logDebug "Starting messages..."
  broadcast pids (Start pids withSeed)
  sleep sendFor
  logDebug "Stopping messages..."
  broadcast pids Stop
  sleep waitFor
  logDebug "Halting agents..."
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
          mc = MasterConfig sf wf ws
      startMaster backend (master backend mc)