-- Base.hs
-- Mathijs Saey
-- This module contains the base pacman abstraction layer

module Pacman.Base where 
	--PMTunnel, PMLocation,
	--Character(..), Pacman, Ghost,
	--PacmanField(..), PMGraph,PacmanPath(..),
	--emptyField, getPlaces, getTunnelDelay, 
	--insertPlace, insertTunnel, calculatePath, getAllPaths) where

import Debug.Trace

import Data.Maybe
import Data.Array

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import System.IO.Unsafe

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
-- We construct these paths when initialising the Pacmanfield
-- right after parsing
data PacmanPath = PmP {
	isBlocked 	:: Bool, 
	fullPath 	:: [PMLocation]
} deriving (Show)

-- A pacmanField contains the gamestate
data PacmanField = PF {
	graph 	:: PMGraph,
	pacman 	:: Pacman,
	ghosts	:: [Ghost],
	paths	:: (Array Int PacmanPath)
} deriving (Show)

-- A charachter is either at a certain locaction,
-- or moving towards a locaction, it always keeps
-- track of the path that it's following
-- it also keeps track of the pacmanpath
-- it's following/trying to block
data Character = Loc {
	path 	:: [PMLocation],
	pmpIdx 	:: [Int],
	loc 	:: PMLocation
} | Mov {
	path 	:: [PMLocation],
	pmpIdx 	:: [Int],
	time 	:: Int
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

-----------------------------
-----------------------------
---- Game Implementation ----
-----------------------------
-----------------------------

------------------
-- Path Locking --
------------------

-- Sets the blocked value for every path in a given list
setPath :: PacmanField -> [Int] -> Bool -> PacmanField
setPath (PF gr pa gh ps) idx val = PF gr pa gh $ 
	ps // [(i,j) | i <- idx, j <- [PmP val $ fullPath (ps ! i)]] 

-- Sets every path in the idx list as closed
claimPath :: PacmanField -> [Int] -> PacmanField
claimPath g idx = setPath g idx True

-- Sets every path in the idx list as open
releasePath :: PacmanField -> [Int] -> PacmanField
releasePath g idx = setPath g idx False

-- Checks if a given path is blocked
checkPath :: PacmanField -> Int -> Bool
checkPath (PF gr pa gh ps) idx = isBlocked $ ps ! idx

-- Get the index of every path that contains a given node
getIndices ::  PacmanField -> PMLocation -> [Int]
getIndices (PF gr pa gh ps) loc = foldl (\ls (idx, pmp) -> if loc `elem` (fullPath pmp) then idx:ls else ls) [] $ assocs ps 

-- Block all paths that contain a given node
blockNode :: PacmanField -> PMLocation -> PacmanField
blockNode f l = claimPath f $ getIndices f l 

-- Unblock all paths that contain a given node
unBlockNode :: PacmanField -> PMLocation -> PacmanField
unBlockNode f loc = refreshPaths $ releasePath f $ getIndices f loc

-- Refreshes the locks on the path. Opens up every path, then closes 
-- every path that is blocked by a ghost
refreshPaths :: PacmanField -> PacmanField
refreshPaths (PF gr pa gh ps) = foldl 
	(\acc (Loc _ _ l) -> blockNode acc l) 
	(releasePath (PF gr pa gh ps) $ indices ps) 
	gh 

-------------------------------
-- Path selection procedures --
-------------------------------

-- Check how many paths we can block by occupying a node
checkBlocks ::  PacmanField -> PMLocation -> Int
checkBlocks (PF gr pa gh ps) loc = foldl (\acc x -> 
	if loc `elem` (fullPath x) && not (isBlocked x) 
		then acc + 1 else acc) 0 $ elems ps 


-- Finds the nearest node in a path, we disregard blocked paths
findNearestPathNode'' ::  PMGraph -> PacmanPath -> PMLocation -> (Inf PMTunnel, PMLocation)
findNearestPathNode'' _ (PmP True p) l = (INF,l)
findNearestPathNode'' g (PmP False p) l = foldl (\(weight, ls) x -> 
	let weight' = if path /= Nothing 
					then NI $ getPathWeight g (fromJust path)
					else INF
					 where
					 	path = calculatePath g l x 
	in if weight' < weight 
		then (weight', x) 
		else (weight, ls)) (INF, l) p

-- Selects the nearest node(s) from all the unclaimed paths
findNearestPathNode' :: PMGraph -> [PacmanPath] -> PMLocation -> [PMLocation]
findNearestPathNode' g ls loc = snd $ foldl 
	(\(weight, dest) x -> case () of 
	  _	| weight' == weight 	-> (weight, node:dest)
		| weight' < weight 		-> (weight', [node])
		| otherwise 			-> (weight, dest) 
		where
	     	res = findNearestPathNode'' g x loc
	     	weight' = fst $ res
	     	node = snd $ res) 
	(INF, [loc])
	ls

-- Selects the node that blocks most possible paths from the result of findNearestPathNode'
findNearestPathNode :: PacmanField -> PMLocation -> [PMLocation]
findNearestPathNode f loc = fromJust $ dijkstra (graph f) loc dest where
	nodes = findNearestPathNode' (graph f) (elems $ paths f) loc
	res = foldl (\(paths, node) x -> let paths' = checkBlocks f x in
					if paths < paths' then (paths', x) else (paths, node))
				(0, loc) nodes
	dest = snd $ res

orientGhost :: PacmanField -> Ghost -> Ghost
orientGhost (PF _ _ _ _) (Mov pa pm t) = Mov pa pm t
orientGhost (PF gr pa gh ps) (Loc path indices l) = Loc newPath idx l where
	newPath = findNearestPathNode (PF gr pa gh ps) l
	idx = getIndices (PF gr pa gh ps) $ last newPath


---------------
-- Threading --
---------------

insertField :: PacmanField -> STM (TVar PacmanField)
insertField f = newTVar f

getField :: TVar PacmanField -> STM PacmanField
getField tvar = readTVar tvar

setGhosts :: TVar PacmanField -> TVar Int -> Ghost -> STM ()
setGhosts tField tChecker ghost = do 
	oldField <- getField tField
	if null (ghosts oldField)
		then retry
		else do
			newField <- return $ PF (graph oldField) (pacman oldField) (ghost:(ghosts oldField)) (paths oldField)
			reduceUpdateChecker tChecker
			writeTVar tField newField

-- The update check keeps track of the amount of ghosts
-- that are still calculating their route
createUpdateChecker :: Int -> STM (TVar Int)
createUpdateChecker i = newTVar i

getUpdateChecker :: TVar Int -> STM Int
getUpdateChecker tvar = readTVar tvar

setUpdateChecker :: TVar Int ->  Int -> STM ()
setUpdateChecker tvar i = writeTVar tvar i 

reduceUpdateChecker :: TVar Int -> STM ()
reduceUpdateChecker tvar = do
	val <- (getUpdateChecker tvar)
	newVal <- return $ val - 1
	setUpdateChecker tvar newVal

-- Blocks the thread until the shared resource reaches 0
checkUpdateChecker :: TVar Int -> STM ()
checkUpdateChecker tvar = trace "hum" $ do 
	ctr <- getUpdateChecker tvar
	if ctr == 0
		then trace "maybe" $ writeTVar tvar 0
		else retry

calculateGhost :: TVar PacmanField -> TVar Int -> PacmanField -> Ghost -> IO()
calculateGhost tField tChecker field ghost = trace "getting ghosts" $ atomically $ setGhosts tField tChecker $ orientGhost field ghost

startGame :: PacmanField -> IO(PacmanField)
startGame (PF gr pa gh ps) = do
	field <- atomically $ insertField (PF gr pa [] ps)
	check <- atomically $ createUpdateChecker (length gh)
	return $! map (\x -> trace "forking" $ forkIO $ calculateGhost field check (PF gr pa gh ps) x) gh
	putStrLn "Filling thread terminated."
	return $! checkUpdateChecker check
	putStrLn "looool"
	traceT check $ readTVarIO field

-- DEBUG
traceT x f = trace (show $ unsafePerformIO $ readTVarIO x) f

---------------------
-- Pacman strategy --
---------------------

findPacmanPath :: PacmanField -> PacmanField
findPacmanPath (PF gr pa gh ps) = PF gr pacman gh ps
	where 
		idx = foldl (\acc (idx, pmp) -> if not $ isBlocked pmp then idx else acc) (-1) $ assocs ps
		pacman = if idx /= (-1) 
					then (Loc (fullPath (ps ! idx)) [idx] (loc pa))
					else pa

-----------------------
-- General Functions --
-----------------------

-- Makes a character move along it's path
updateChar :: PMGraph -> Character -> Character
updateChar _ (Loc [] i l) 		= Loc [] i l 
updateChar _ (Mov (x:xs) i 1)	= Loc xs i x
updateChar _ (Mov xs i t) 		= Mov xs i $ t - 1
updateChar g (Loc (x:xs) i l) 	= Mov xs i $ fromJust $ getTunnelDelay g (l,x)

-- Set a new destination for the character, the path to this 
-- destination is calculated by the dijkstra algorithm
-- if the charachter is moving, we calculate the path starting
-- from the node it's going to
--getPath ::  PMGraph -> Character -> PMLocation -> Character
--getPath g (Loc _ l) d = Loc (tail $ dijkstra g l d) l
--getPath g (Mov (x:xs) t) d = Mov (x:(tail $ dijkstra g x d)) t

