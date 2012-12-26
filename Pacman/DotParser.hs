-- DotParser.hs
-- Mathijs Saey
-- This module contains the read implementation as well as the file handling

module Pacman.DotParser(PacmanField) where

import System.IO
import Data.Char
import Data.Maybe

import Graph.Dijkstra

import Pacman.Base
import Pacman.GeneralParser

-- A temporary version of the datatypes used for parsing
data ParsingStr = UnDef | D String deriving (Show)
data ParsingPacman = PP ParsingStr ParsingStr deriving (Show)
data ParsingField = PF' PMGraph ParsingPacman [Ghost]

------------------
-- File reading --
------------------

filePath = "pacmanfield.txt"

main = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    putStr $ show $ (read contents :: PacmanField)
    hClose handle

-----------------------
-- Low level parsing --
-----------------------

-- Based on exercise session 9

sep :: Parser ()
sep = many (sat (flip elem [' ', '\n', '\t'])) >> return ()

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

-- Attempts to set the source for pacman
setPacmanSource :: ParsingPacman -> PMLocation -> ParsingPacman
setPacmanSource (PP UnDef d) s = PP (D s) d
setPacmanSource (PP _ _) _ = error "Parse error: There is already a source location for pacman!"

setPacmanDest :: ParsingPacman -> PMLocation -> ParsingPacman
setPacmanDest (PP s UnDef) d = PP s (D d)
setPacmanDest (PP _ _) _ = error "Parse error: There is already a target location for pacman!"

-- This is just a call to the normal insertTunnel function
-- the only difference is that it will raise an error when the
-- nodes do not exist rather then just ignoring the call 
insertParseTunnel ::  PMGraph -> (PMLocation,PMLocation) -> PMTunnel -> PMGraph
insertParseTunnel g (n1,n2) e 
	| notElem n1 $ getPlaces g = error er
	| notElem n2 $ getPlaces g = error er
	| otherwise =  insertTunnel g (n1,n2) e 
	where er = "Parse error: Undefined node reference"

-- Adds n ghosts with a set source
addGhosts ::  ParsingField -> PMLocation -> Int -> ParsingField
addGhosts (PF' g p ls) loc n = PF' g p (newGhosts ++ ls) where
	newGhosts =  foldl (\acc x -> ((Loc [] loc):acc)) [] [1..n]

------------------------
-- High level parsing --
------------------------

-- This parser is based on the one seen in 
-- exercise session 9. The main difference is
-- that is passes the graf along down the calls

instance Read PacmanField where
	readsPrec _ s = map (\(x,s) -> (convertField x, s)) $ apply graph s where
		token = word >>= (\s -> return s)
		natural = number >>= (\i -> return i)
		boolean s = (s == "True") || (s == "true") -- read only accepts True as a valid boolean
		ghosts f n = do
			keyword "ghost"
			keyword "="
			amount <- natural
			return $ addGhosts f n amount
		source (PF' g p l) n = do
			keyword "source"
			keyword "="
			bool <- token
			return $ if (boolean bool)
					then PF' g (setPacmanSource p n) l
					else PF' g p l
		target (PF' g p l) n = do
			keyword "target"
			keyword "="
			bool <- token
			return $ if (boolean bool)
					then PF' g (setPacmanDest p n) l
					else PF' g p l
		attr f n = (ghosts f n) `orelse` (target f n) `orelse` (source f n)
		inattrs f n = do
			res <- attr f n
			(inattrs res n) `orelse` (return res)
		attrs f n = do
			keyword "["
			res <- inattrs f n
			keyword "]"
			return res
		chain ls = do
			keyword "--"
			name <- token
			(chain (name:ls)) `orelse` (return (name:ls))
		node (PF' g p l) = do 
			name <- token
			res <- (orelse 
				(attrs (PF' (insertPlace g name) p l) name)
				(return $ PF' (insertPlace g name) p l))
			keyword ";"
			return res
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