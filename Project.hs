-- Layout.hs
-- Mathijs Saey
-- This module contains the top level of the project

import Data.List
import Data.Maybe

import Text.Printf
import System.Random

import Pacman.Base
import Pacman.DotParser

import Graph.Layout
import Graph.LatexDraw
import Graph.BinarySearchTree

-- DEBUG

t0 = foldl (\acc x -> insertNode acc x) (empty :: (BSTGraph Int Int)) [1..10]
t1 = insertEdge t0 (1,2) 1
t2 = insertEdge t1 (2,3) 1
t3 = insertEdge t2 (3,4) 1
t4 = insertEdge t3 (4,5) 1
t5 = insertEdge t4 (1,5) 2

x = t4
y = t5

cycles = insertEdge x (3,1) 1

gen = mkStdGen 32242342

filePath = "pacmanfield.txt"
loadField = (read $ getGameFileContents filePath) :: PacmanField

-- DEBUG


