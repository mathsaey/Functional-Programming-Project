-- Layout.hs
-- Mathijs Saey
-- This module contains the implementation of the Force-directed layout algorithm

module Graph.Layout  where

import Data.Maybe
import Graph.Kernel
import System.Random

kineticMinConst = 1
repulsionConst = 8.998 * (10^9)
stringConst = -0.2

data (Springnode n) = SN {
	node 	:: n, 
	xPos 	:: Float,
	yPos	:: Float,
	xSpeed	:: Float,
	ySpeed	:: Float,
	xNet	:: Float,
	yNet	:: Float
} deriving (Show)

initSpringNode :: (RandomGen r) => n -> r -> ((Springnode n), r)
initSpringNode node gen = (SN node xPos yPos 0.0 0.0 0.0 0.0, newGen) where 
	splitGen = split gen
	rand1 = next $ fst splitGen
	rand2 = next $ snd splitGen
	xPos = fromIntegral $ fst rand1
	yPos = fromIntegral $ fst rand2
	newGen = snd rand1

returnNodePosition :: (Springnode n) -> (n, Float, Float)
returnNodePosition n = (node n, xPos n, yPos n)

calculateDistance ::  (Springnode n) -> (Springnode n) -> Float
calculateDistance n1 n2 = sqrt ((xPos n1 - xPos n2)^2 + (yPos n1 - yPos n2)^2)

-- Updates the position and speed of a node depending on the net force on the node
updateNode :: (Springnode n) -> ((Springnode n), Float)
updateNode n = ((SN (node n) newXpos newYpos newXspeed newYspeed 0.0 0.0), displacement)
	where 
		newXpos = xPos n + newXspeed
		newYpos = yPos n + newYspeed
		newXspeed = xSpeed n + xNet n
		newYspeed = ySpeed n + yNet n
		displacement = (xPos n - newXpos)^2 + (yPos n - newYpos)^2


-- Adds force to a node
addForce :: (Springnode n) -> Float -> Float -> (Springnode n)
addForce (SN node xPos yPos xSpeed ySpeed xNet yNet) xForce yForce =
		 SN node xPos yPos xSpeed ySpeed (xNet + xForce) (yNet + yForce)

-- Calculates the force on the x and y value of a node given the force and node exercising the force
calculateForce :: (Springnode n) -> (Springnode n) -> Float -> (Springnode n)
calculateForce n1 n2 force = addForce n1 xdist ydist where  
	angle = atan $ (abs (yPos n1 - yPos n2)) / (abs (xPos n1 - xPos n2))
	xdist = (cos angle) * force
	ydist = (sin angle) * force

-- Repulses a node based on the Coulombs law
repulseNode :: (Eq n) => (Springnode n) -> (Springnode n) -> (Springnode n)
repulseNode n1 n2 
	| (node n1) == (node n2) = n1
	| otherwise = calculateForce n1 n2 (repulsionConst/((calculateDistance n1 n2)^2))

-- Attracts a node based on the law of hooke
attractNode :: (Num e, Eq n) => (Springnode n) -> (Springnode n) -> e -> (Springnode n)
attractNode n1 n2 weight 
	| (node n1) == (node n2) = n1
	| otherwise =  calculateForce n1 n2 (-1.0 * (calculateDistance n1 n2) * stringConst)

-- Get the effect of all other nodes and edge on a given node
calculateEffect :: (Graph g n e, Num e, Eq e, Eq n) => g -> [(Springnode n)] -> (Springnode n) -> (Springnode n)
calculateEffect graph ls n = attractedN where
	repulsedN = foldl (\acc x -> repulseNode acc x) n ls
	attractedN = foldl (\acc x -> let e = edge graph ((node n),(node x)) in
									if e == Nothing then acc
									else attractNode acc x (fromJust e))
						 repulsedN ls

layoutLoop :: (Graph g n e, Num e, Eq e, Eq n) => g -> [Springnode n] -> Int -> [Springnode n]
layoutLoop graph nodes ctr
	| kineticEn < kineticMinConst = nodes
	| otherwise = layoutLoop graph newNodesl (ctr + 1) 
	where
		movedN = map (\x -> calculateEffect graph nodes x) nodes
		updatedN = foldl (\(ls, kin) x -> let pair = updateNode x
								in ((fst pair):ls, kin + (snd pair)))
							([], 0) nodes
		kineticEn = snd updatedN
		newNodesl = fst updatedN

layout :: (Num e, Graph g n e, RandomGen r, Eq n, Eq e) => g -> r -> [(n, Float, Float)]
layout graph rand =
		map returnNodePosition $
		layoutLoop 
		graph
		(fst $ foldl 
			(\(ls, gen) x -> 
				let n = initSpringNode x gen
				in (((fst n):ls), snd n))
			([], rand)
			(nodes graph))
		0