-- Infinity.hs
-- Mathijs Saey
-- This module provides an abstraction to deal with
-- infinity, regardless of the data type

module Infinity  where

data Inf e = INF | NI e deriving (Read, Show, Eq)

instance (Ord e) => Ord (Inf e) where
	compare (NI x) (NI y) = compare x y 
	compare INF INF = EQ
	compare INF _ = GT
	compare _ INF = LT
