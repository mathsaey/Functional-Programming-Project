-- Layout.hs
-- Mathijs Saey
-- This module contains the implementation of the Force-directed layout algorithm

module Graph.Layout (layout) where

import Data.Maybe
import Graph.Kernel
import System.Random

stringLengthConst = 3
kineticMinConst = 0.5
timeStepConst = 0.1
repulsionConst = 12
dampingConst = 0.4
stringConst = 0.0001

data (Springnode n) = SN {
	node 	:: n, 
	xPos 	:: Float,
	yPos	:: Float,
	xSpeed	:: Float,
	ySpeed	:: Float,
	xNet	:: Float,
	yNet	:: Float
} deriving (Show)

initSpringNode :: n -> Float -> Float -> (Springnode n)
initSpringNode node r1 r2 = SN node r1 r2 0.0 0.0 0.0 0.0 

returnNodePosition :: (Springnode n) -> (n, Float, Float)
returnNodePosition n = (node n, xPos n, yPos n)

calculateDistance ::  (Springnode n) -> (Springnode n) -> Float
calculateDistance n1 n2 = sqrt (((abs $ xPos n1) - (abs $ xPos n2))^2 + ((abs $ yPos n1) - (abs $ yPos n2))^2)

-- Updates the position and speed of a node depending on the net force on the node
updateNode :: (Springnode n) -> ((Springnode n), Float)
updateNode n = ((SN (node n) newXpos newYpos 0.0 0.0 0.0 0.0), displacement)
	where 
		newXpos = xPos n + (newXspeed * timeStepConst)
		newYpos = yPos n + (newYspeed * timeStepConst)
		newXspeed = xSpeed n + (xNet n * timeStepConst * dampingConst)
		newYspeed = ySpeed n + (yNet n * timeStepConst * dampingConst)
		displacement = sqrt $ (abs newXspeed)^2 + (abs newYspeed)^2

-- Adds force to a node
addForce :: (Springnode n) -> Float -> Float -> (Springnode n)
addForce (SN node xPos yPos xSpeed ySpeed xNet yNet) xForce yForce =
		 SN node xPos yPos xSpeed ySpeed (xNet + xForce) (yNet + yForce)

-- Calculates the force on the x and y value of a node given the force and node exercising the force
calculateForce :: (Show n) => (Springnode n) -> (Springnode n) -> Float -> (Springnode n)
calculateForce n1 n2 force = addForce n1 xdir ydir where  
	angle = atan $ (yPos n1 - yPos n2)/(xPos n1 - xPos n2)
	xForce = (cos angle) * force
	yForce = (sin angle) * force
	xdir = getdistance (xPos n1) (xPos n2) xForce
	ydir = getdistance (yPos n1) (yPos n2) yForce

-- Calculates how to move the nodes given the nodes positions and a force
-- A negative force should bring nodes together while a positive forces them apart
getdistance :: Float -> Float -> Float -> Float
getdistance x1 x2 force 
	| x1 < x2 = if force < 0
		then abs force 
		else -1 * abs force
	| otherwise = if force < 0
		then -1 * abs force
		else abs force 

-- Repulses a node based on the Coulombs law
repulseNode :: (Show n) => (Eq n) => (Springnode n) -> (Springnode n) -> (Springnode n)
repulseNode n1 n2 
	| (node n1) == (node n2) = n1
	| otherwise = calculateForce n1 n2 (repulsionConst/((calculateDistance n1 n2)^2))

-- Attracts a node based on the law of hooke
attractNode :: (Num e, Eq n, Show n) => (Springnode n) -> (Springnode n) -> e -> (Springnode n)
attractNode n1 n2 weight 
	| (node n1) == (node n2) = n1
	| otherwise = calculateForce n1 n2 (-1 * ((calculateDistance n1 n2) - stringLengthConst) * stringConst)

-- Get the effect of all other nodes and edge on a given node
calculateEffect :: (Graph g n e, Num e, Eq e, Eq n, Show n) => g -> [(Springnode n)] -> (Springnode n) -> (Springnode n)
calculateEffect graph ls n = attractedN where
	repulsedN = foldl (\acc x -> repulseNode acc x) n ls
	attractedN = foldl (\acc x -> let e = edge graph ((node n),(node x)) in
									if e == Nothing then acc
									else attractNode acc x (fromJust e))
						 repulsedN ls

layoutLoop :: (Graph g n e, Num e, Eq e, Eq n, Show n) => g -> [Springnode n] -> [Springnode n]
layoutLoop graph nodes
	| kineticEn < kineticMinConst = nodes
	| otherwise = layoutLoop graph newNodesl
	where
		movedN = map (\x -> calculateEffect graph nodes x) nodes
		updatedN = foldl (\(ls, kin) x -> let pair = updateNode x
								in ((fst pair):ls, kin + (snd pair)))
							([], 0) movedN
		kineticEn = snd updatedN
		newNodesl = fst updatedN

layout :: (Num e, Graph g n e, RandomGen r, Eq n, Eq e, Show n, Show r) => g -> r -> [(n, Float, Float)]
layout graph rand =
		map returnNodePosition $
		layoutLoop 
		graph
		(fst $ foldl 
			(\(ls, (r1:r2:rs)) x -> 
				let n = initSpringNode x r1 r2
				in ((n:ls), rs))
			([], randomRs (1,10) rand)
			(nodes graph))