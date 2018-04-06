module Main where

import Data.Semigroup       ((<>))
import Lib
import Options.Applicative
import Types

main :: IO ()
main = run =<< execParser configInfo
  where
    configInfo = info (config <**> helper) $
      fullDesc <>
      progDesc "CH/OTP solution"

    config :: Parser Config
    config = subparser $
      command "agent" (info agentConfig (progDesc "Run as agent")) <>
      command "master" (info masterConfig (progDesc "Run as master"))

    agentConfig :: Parser Config
    agentConfig
      = AgentConfig <$> host <*> port

    masterConfig :: Parser Config
    masterConfig
      = MasterConfig <$> host <*> port <*> sendFor <*> waitFor <*> withSeed

    host :: Parser Host
    host = strOption $ (long "host")

    port :: Parser Port
    port = strOption $ (long "port")

    sendFor :: Parser SendFor
    sendFor = option auto $ (long "send-for")

    waitFor :: Parser WaitFor
    waitFor = option auto $ (long "wait-for")

    withSeed :: Parser WithSeed
    withSeed = option auto $ (long "with-seed")

