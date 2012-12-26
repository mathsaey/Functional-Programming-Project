module Pacman.DotParser where

import Data.Char
import Data.Maybe
import Debug.Trace

import Graph.Dijkstra

import Pacman.Base
import Pacman.GeneralParser


-- A temporary version of the datatypes used for parsing
data ParsingStr = UnDef | D String deriving (Show)
data ParsingPacman = PP ParsingStr ParsingStr deriving (Show)
data ParsingField = PF' PMGraph ParsingPacman [Ghost]

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

----------------------------
-- Extra parser functions --
----------------------------

-- Converts a parsing field back to a regular pacmanfield
convertField :: ParsingField -> PacmanField
convertField (PF' g pm ls) = PF g (convertPacman g pm) ls 

-- Attempts to convert a ParsingPacman to a regular pacman
convertPacman :: PMGraph -> ParsingPacman -> Pacman
convertPacman _ (PP UnDef UnDef) = error "Parse error: pacman does not have a source or destination" 
convertPacman _ (PP UnDef _) = error "Parse error: pacman does not have a source" 
convertPacman _ (PP _ UnDef) = error "Parse error: pacman does not have a goal"
convertPacman g (PP (D s) (D d))  = Loc (tail path) s where
	maybePath = (dijkstra g s d)
	path = if maybePath /= Nothing 
			then fromJust maybePath
			else error "Parse error: There is no valid path between the start and end location of pacman"

-- This is just a call to the normal insertTunnel function
-- the only difference is that it will raise an error when the
-- nodes do not exist rather then just ignoring the call 
insertParseTunnel ::  PMGraph -> (PMLocation,PMLocation) -> PMTunnel -> PMGraph
insertParseTunnel g (n1,n2) e 
	| notElem n1 $ getPlaces g = error er
	| notElem n2 $ getPlaces g = error er
	| otherwise =  insertTunnel g (n1,n2) e 
	where er = "Parse error: Undefined node reference"

-- Attempts to set the source for pacman
setPacmanSource :: ParsingPacman -> PMLocation -> ParsingPacman
setPacmanSource (PP UnDef d) s = PP (D s) d
setPacmanSource (PP _ _) _ = error "Parse error: There is already a source location for pacman!"

setPacmanDest :: ParsingPacman -> PMLocation -> ParsingPacman
setPacmanDest (PP s UnDef) d = PP s (D d)
setPacmanDest (PP _ _) _ = error "Parse error: There is already a target location for pacman!"
------------------------
-- High level parsing --
------------------------

-- This parser is based on the one seen in 
-- exercise session 9. The main difference is
-- that is passes the graf along down the calls

instance Read PacmanField where
	readsPrec _ s = map (\(x,s) -> (convertField x, s)) $ apply graph s where
		token = word >>= (\s -> return s)
		--boolean = bool >>= (\b -> return b) 
		natural = number >>= (\i -> return i)
		attrs f = do
			return f
		chain ls = do
			keyword "--"
			name <- token
			(chain (name:ls)) `orelse` (return (name:ls))
		node (PF' g p l) = do 
			name <- token
			atributes <- attrs $ PF' (insertPlace g name) p l
			noatributes <- return $ PF' (insertPlace g name) p l
			keyword ";"
			(return $ atributes) `orelse` (return $ noatributes)
		edge (PF' g p l) = do
			name <- token
			nodes <- chain []
			keyword "["
			keyword "value"
			keyword "=" 
			delay <- natural
			keyword "]"
			keyword ";"
			return $ PF' (insertParseTunnel g (name,(head nodes)) delay) p l
		stmt f = do
			res <- (edge f) `orelse` (node f)
			return res
		stmts f = do
			res <- stmt f
			(stmts res) `orelse` (return res)
		graph = do
			keyword "graph"
			name <- token
			keyword "{"
			res <- stmts $ PF' emptyField (PP UnDef UnDef) []
			keyword "}"
			return res 