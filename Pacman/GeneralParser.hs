-- GeneralParser.hs
-- This module contains the general parser

-- This code was implemented by Wolfgang De Meuter
-- for the Functional programming course

module Pacman.GeneralParser (Parser, apply, sat, char, string, some, many, orelse) where

import Control.Monad

newtype Parser a = P (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (P f) s = f s

instance Monad Parser where
  -- Let the input intact
  return x = P (\s -> [(x,s)])
  -- Combine two parsers
  p >>= f = P (\s1 -> concat (map (\(x,s2) -> apply (f x) s2) (apply p s1)))

instance MonadPlus Parser where
	mzero = zero
	mplus = plus

------------------------
-- Parser combinators --
------------------------

-- Combine all the possible reading
plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = P (\s -> (apply p1 s) ++ (apply p2 s))

-- Try the first parser, in case of failure use the second parser
orelse :: Parser a -> Parser a -> Parser a
orelse p1 p2 = P (\s -> let x = (apply p1 s)
                        in if null x
                           then apply p2 s
                           else x)

-- Use the same parser again and again until failure
many :: Parser a -> Parser [a]
many p = orelse
           (do x <- p
               xs <- many p
               return $ x:xs)
           (return [])

-- Idem many, but require at least one parsing
some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            return $ x:xs

--------------------
-- Simple Parsers --
--------------------

-- The failure parser
zero :: Parser a
zero = P (\s -> [])

-- Eat a character
item :: Parser Char
item = P f where f [] = []
                 f (c:cs) = [(c,cs)]

-- Eat a character only if it satisfies
sat :: (Char -> Bool) -> Parser Char
sat f = item >>= (\c -> if f c then return c else zero)

-- Try to eat the given character
char :: Char -> Parser ()
char c = sat (==c) >> return ()

-- Try to eat the given string
string :: String -> Parser ()
string (c:cs) = char c >> string cs
string [] = return ()
