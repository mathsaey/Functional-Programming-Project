-- DotParser.hs
-- Mathijs Saey
-- This module contains the read implementation as well as the file handling

module Pacman.DotParser(PacmanField, openGameFile, getGameFileContents) where

import Data.Char
import Data.Array
import Data.Maybe

import System.IO
import System.IO.Unsafe

import Pacman.Base
import Pacman.GeneralParser

-- A temporary version of the datatypes used for parsing
data ParsingStr = UnDef | D String deriving (Show)
data ParsingField = PF' PMGraph ParsingPacman [Ghost]

data ParsingPacman = PP {
	source :: ParsingStr,
	target :: ParsingStr 
} deriving (Show)

------------------
-- File reading --
------------------

openGameFile :: String -> IO String
openGameFile filePath = do
    contents <- readFile filePath
    return contents

-- HERE BE DRAGONS --
getGameFileContents :: String -> String
getGameFileContents filePath = unsafePerformIO $ openGameFile filePath

-----------------------
-- Low level parsing --
-----------------------

-- Based on exercise session 9

sep :: Parser ()
sep = many (sat (flip elem [' ', '\n', '\t'])) >> return ()

word :: Parser String
word = sep >> some (sat isLetter `orelse` sat isDigit)

number :: Parser Int
number = sep >> some (sat isDigit) >>= (\s -> return $ read s)

keyword :: String -> Parser ()
keyword s = sep >> string s >> return ()

----------------------------
-- Extra parser functions --
----------------------------

-- Converts a parsing field back to a regular pacmanfield
convertField :: ParsingField -> PacmanField
convertField (PF' g pm ls) = PF g (convertPacman g pm) ls $ getPaths g pm

-- Calculate the paths Pacman can take
getPaths ::  PMGraph -> ParsingPacman -> (Array Int PacmanPath)
getPaths g (PP (D source) (D target)) =  listArray (1, length paths) $
											map (\x -> PmP False x) paths where
												paths = getAllPaths g source target

-- Attempts to convert a ParsingPacman to a regular pacman
convertPacman :: PMGraph -> ParsingPacman -> Pacman
convertPacman _ (PP UnDef UnDef) = error "Parse error: pacman does not have a source or destination" 
convertPacman _ (PP UnDef _) = error "Parse error: pacman does not have a source" 
convertPacman _ (PP _ UnDef) = error "Parse error: pacman does not have a goal"
convertPacman g (PP (D s) (D d))  = Loc (tail path) [(-1)] s where
	maybePath = (calculatePath g s d)
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

-- Add a bidirectional tunnel between 2 nodes, if they exist
insertParseTunnel ::  PMGraph -> (PMLocation,PMLocation) -> PMTunnel -> PMGraph
insertParseTunnel g (n1,n2) e 
	| notElem n1 $ getPlaces g = error er
	| notElem n2 $ getPlaces g = error er
	| otherwise =  insertTunnel (insertTunnel g (n1,n2) e) (n2,n1) e
	where er = "Parse error: Undefined node reference"

-- Adds n ghosts with a set source
addGhosts ::  ParsingField -> PMLocation -> Int -> ParsingField
addGhosts (PF' g p ls) loc n = PF' g p (newGhosts ++ ls) where
	newGhosts =  foldl (\acc x -> ((Loc [] [(-1)] loc):acc)) [] [1..n]

------------------------
-- High level parsing --
------------------------

-- This parser is based on the one seen in 
-- exercise session 9. The main difference is
-- that is passes the graph along with the calls

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
			return $ PF' (fst (foldl 
								(\(g, n1) n2 -> ((insertParseTunnel g (n1, n2) delay), n2)) 
								(g, name) (reverse nodes)))
						 p l 
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