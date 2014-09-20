{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Maybe
import Data.Word
import Network.Pcap

import qualified Data.ByteString as B

import TCP

type Port = Word16
type SeqNo = Word32
