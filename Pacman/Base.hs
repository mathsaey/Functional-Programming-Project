-- Base.hs
-- Mathijs Saey
-- This module contains the base pacman abstraction layer

module Pacman.Base where 
	--PMTunnel, PMLocation,
	--Character(..), Pacman, Ghost,
	--PacmanField(..), PMGraph,PacmanPath(..),
	--emptyField, getPlaces, getTunnelDelay, 
	--insertPlace, insertTunnel, calculatePath, getAllPaths) where

import Data.Maybe
import Data.Array

import Infinity
import Graph.Kernel
import Graph.Dijkstra
import Graph.TreeSearch
import Graph.BinarySearchTree

----------------
-- Data types --
----------------

type PMTunnel = Int
type PMLocation = String
type PMGraph = BSTGraph PMLocation PMTunnel

type Pacman = Character
type Ghost = Character

-- A pacmanpath contains a path that pacman might follow
-- and a boolean that indicates if a ghost is currently 
-- trying to block it
data PacmanPath = PmP {
	isBlocked 	:: Bool, 
	fullPath 	:: [PMLocation]
} deriving (Show)

-- A pacmanField contains the graph, pacman and the ghosts
data PacmanField = PF {
	graph 	:: PMGraph,
	pacman 	:: Pacman,
	ghosts	:: [Ghost],
	paths	:: (Array Int PacmanPath)
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

getAllPaths :: PMGraph -> PMLocation -> PMLocation -> [[PMLocation]] 
getAllPaths g n1 n2 = findPaths g n1 n2

---------------------------
-- Ghost search strategy --
---------------------------

-- Sets the blocked value for every path in a given list
setPath :: PacmanField -> [Int] -> Bool -> PacmanField
setPath (PF gr pa gh ps) idx val = PF gr pa gh $ 
	ps // [(i,j) | i <- idx, j <- [PmP val $ fullPath (ps ! i)]] 

claimPath :: PacmanField -> [Int] -> PacmanField
claimPath g idx = setPath g idx True

releasePath :: PacmanField -> [Int] -> PacmanField
releasePath g idx = setPath g idx False

checkPath :: PacmanField -> Int -> Bool
checkPath (PF gr pa gh ps) idx = isBlocked $ ps ! idx

-- Check how many paths we can block by occupying a node
checkBlocks ::  PacmanField -> PMLocation -> Int
checkBlocks (PF gr pa gh ps) loc = foldl (\acc x -> if loc `elem` (fullPath x) then acc + 1 else acc) 0 $ elems ps 

-- Get the index of every path that contains a given node
getIndices ::  PacmanField -> PMLocation -> [Int]
getIndices (PF gr pa gh ps) loc = foldl (\ls (idx, pmp) -> if loc `elem` (fullPath pmp) then idx:ls else ls) [] $ assocs ps 

-- Block all paths that contain a given node
blockNode :: PacmanField -> PMLocation -> PacmanField
blockNode f l = claimPath f $ getIndices f l 

unBlockNode :: PacmanField -> PMLocation -> PacmanField
unBlockNode f loc = refreshPaths $ releasePath f $ getIndices f loc

-- Refreshes the locks on the path. Opens up every path, then closes 
-- every path that is blocked by a ghost
refreshPaths :: PacmanField -> PacmanField
refreshPaths (PF gr pa gh ps) = foldl 
	(\acc (Loc _ l) -> blockNode acc l) 
	(releasePath (PF gr pa gh ps) $ indices ps) 
	gh 

-- Finds the nearest node in a path
findNearestNode'' ::  PMGraph -> PacmanPath -> PMLocation -> (Inf PMTunnel, PMLocation)
findNearestNode'' _ (PmP True p) l = (INF,l)
findNearestNode'' g (PmP False p) l = foldl (\(weight, ls) x -> 
	let weight' = if path /= Nothing 
					then NI $ getPathWeight g (fromJust path)
					else INF
					 where
					 	path = calculatePath g l x 
	in if weight' < weight 
		then (weight', x) 
		else (weight, ls)) (INF, l) p

-- Selects the nearest node from all the unclaimed paths
findNearestNode' :: PMGraph -> [PacmanPath] -> PMLocation -> [PMLocation]
findNearestNode' g ls loc = snd $ foldl 
	(\(weight, dest) x -> case () of 
	  _	| weight' == weight 	-> (weight, node:dest)
		| weight' < weight 		-> (weight', [node])
		| otherwise 			-> (weight, dest) 
		where
	     	res = findNearestNode'' g x loc
	     	weight' = fst $ res
	     	node = snd $ res) 
	(INF, [loc])
	ls

-- Selects the node that blocks most possible paths from the result of findNearestNode'
findNearestNode :: PacmanField -> [PacmanPath] -> PMLocation -> [PMLocation]
findNearestNode f ls loc = fromJust $ dijkstra (graph f) loc dest where
	nodes = findNearestNode' (graph f) ls loc
	res = foldl (\(paths, node) x -> let paths' = checkBlocks f x in
					if paths < paths' then (paths', x) else (paths, node))
				(0, loc) nodes
	dest = snd $ res

---------------------
-- Pacman strategy --
---------------------

findPacmanPath :: PacmanField -> PacmanField
findPacmanPath (PF gr pa gh ps) = PF gr pacman gh ps
	where 
		idx = foldl (\acc (idx, pmp) -> if not $ isBlocked pmp then idx else acc) (-1) $ assocs ps
		pacman = if idx /= (-1) 
					then (Loc (fullPath (ps ! idx)) (location pa))
					else pa

-----------------------
-- General Functions --
-----------------------

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

