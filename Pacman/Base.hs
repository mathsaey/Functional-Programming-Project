-- Base.hs
-- Mathijs Saey
-- This module contains the base pacman abstraction layer

module Pacman.Base (
	PMTunnel, PMLocation,
	PacmanField(..), PMGraph, 
	Character(..), Pacman, Ghost,
	emptyField, getPlaces, getTunnelDelay, 
	insertPlace, insertTunnel, calculatePath) where

import Data.Maybe
import Graph.Dijkstra
import Graph.BinarySearchTree

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
data Character = Loc {
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

---------------------------
-- Ghost search strategy --
---------------------------

findPaths :: PMGraph -> PMLocation -> PMLocation -> [[PMLocation]]
findPaths g from to 
	| from == to = []
	| toNodes == [] 
	| otherwise =  result where
		toNodes = getToNodes g to





---------------
-- Functions --
---------------

-- Makes a character move along it's path
updateChar :: PMGraph -> Character -> Character
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

