-- Base.hs
-- Mathijs Saey
-- This module contains the base pacman abstraction layer

module Pacman.Base (
	PacmanField, PMGraph, 
	emptyField, getPlaces, getTunnelDelay, 
	insertPlace, insertTunnel, calculatePath) where

import Data.Char
import Data.Maybe

import Debug.Trace

import Graph.Dijkstra
import Graph.BinarySearchTree

import Pacman.GeneralParser

----------------
-- Data types --
----------------

type PMTunnel = Int
type PMLocation = String
type Ghost = Character
type Pacman = Character
type PMGraph = BSTGraph PMLocation PMTunnel

-- A pacmanField contains the graph, pacman and the ghosts
data PacmanField = PF {
	graph 	:: PMGraph,
	pacman 	:: Pacman,
	ghosts	:: [Ghost]
} deriving (Show)

-- A charachter is either at a certain location,
-- or moving towards a location, it always keeps
-- track of the path that it's following
data Character = UnDef | Loc {
	path :: [PMLocation],
	location :: PMLocation
} | Mov {
	path :: [PMLocation],
	time :: Int
} deriving (Show)

-----------------------
-- Abstraction layer --
-----------------------

emptyField :: PMGraph
emptyField = empty :: PMGraph

getPlaces :: PMGraph -> [PMLocation]
getPlaces g = nodes g

getTunnelDelay ::  PMGraph -> (PMLocation, PMLocation) -> Maybe PMTunnel
getTunnelDelay g n = edge g n 

insertPlace ::  PMGraph -> PMLocation -> PMGraph
insertPlace g l = insertNode g l

insertTunnel ::  PMGraph -> (PMLocation,PMLocation) -> PMTunnel -> PMGraph
insertTunnel g n e = insertEdge g n e

calculatePath ::  PMGraph -> PMLocation -> PMLocation -> Maybe [PMLocation]
calculatePath g from to = dijkstra g from to 

---------------
-- Functions --
---------------

-- Makes a character move along it's path
updateChar :: PMGraph -> Character -> Character
updateChar _ UnDef 			= UnDef
updateChar _ (Loc [] l) 	= Loc [] l 
updateChar _ (Mov (x:xs) 1) = Loc xs x
updateChar _ (Mov xs t) 	= Mov xs $ t - 1
updateChar g (Loc (x:xs) l) = Mov xs $ fromJust $ getTunnelDelay g (l,x)

-- Set a new destination for the character, the path to this 
-- destination is calculated by the dijkstra algorithm
-- if the charachter is moving, we calculate the path starting
-- from the node it's going to
--getPath ::  PMGraph -> Character -> PMLocation -> Character
--getPath g (Loc _ l) d = Loc (tail $ dijkstra g l d) l
--getPath g (Mov (x:xs) t) d = Mov (x:(tail $ dijkstra g x d)) t


-----------------------
-- Low level parsing --
-----------------------

-- Based on exercise session 9

sep :: Parser ()
sep = many (sat (flip elem [' ', '\n'])) >> return ()

word :: Parser String
word = sep >> some (sat isLetter)

number :: Parser Int
number = sep >> some (sat isDigit) >>= (\s -> return $ read s)

keyword :: String -> Parser ()
keyword s = sep >> string s >> return ()

------------------------
-- High level parsing --
------------------------

-- This parser is based on the one seen in 
-- exercise session 9. The main difference is
-- that is passes the graf along down the calls

instance Read PacmanField where
	readsPrec _ s = apply graph s where
		token = word >>= (\s -> return s)
		--boolean = bool >>= (\b -> return b) 
		natural = number >>= (\i -> return i)
		--attrs = do
		--	keyword "["
		--	lst <- some attr
		--	keyword "]"
		--attr = do 
		--	tag <- token
		--	keyword "="
		--	val <- name -- Account for different types
		chain ls = do -- Make this work
			name <- token
			orelse
				(do 
					keyword "--"
					return $ chain (name:ls))
				(return ls)
		node (PF g p l) = trace "parsing node" $ do 
			name <- token
			keyword ";"
			return $ PF (insertPlace g name) p l
		edge (PF g p l) = trace "parsing edge" $ do
			nodes <- chain []
			keyword "["
			keyword "value"
			keyword "="
			delay <- natural
			keyword "]"
			keyword ";"
			return $ PF (insertTunnel g ((head nodes),(head $ tail nodes)) delay) p l
		stmt f 	= do
			res <- (node f) `orelse` (edge f)
			return res
		stmts f = do
			res <- stmt f
			orelse
				(stmts res)
				(return res)
		graph = do
			keyword "graph"
			name <- token
			keyword "{"
			res <- stmts $ PF emptyField UnDef []
			keyword "}"
			return res 