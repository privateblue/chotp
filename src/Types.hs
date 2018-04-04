{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics

data Protocol
  = Start [ProcessId] WithSeed
  | Gen Double
  | Msg Double Timestamp ProcessId
  | Stop
  | Kill
  deriving (Typeable, Generic, Show)

instance Binary Protocol

data MasterConfig
  = MasterConfig SendFor WaitFor WithSeed

type Timestamp = Int

type SendFor = Int

type WaitFor = Int

type WithSeed = Int

data Entry
  = Entry {_eValue :: Double, _eTimestamp :: Timestamp, _ePid :: ProcessId}
  deriving (Eq)

instance Ord Entry where
  (<=) (Entry v t _) (Entry v' t' _) = if (t == t') then v <= v' else t <= t'