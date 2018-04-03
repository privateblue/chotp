{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics

data Protocol
  = Start [ProcessId] Int
  | Gen Double
  | Msg Double Int ProcessId
  | Stop
  | Kill
  deriving (Typeable, Generic, Show)

instance Binary Protocol