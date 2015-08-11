module Sound.Morse
  ( encodeString
  , encodeMorse
  , morseTable
  , fromChar
  , morseToBool
  , Morse(..)
  ) where

import Data.List (intersperse)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import Control.Arrow (first)

import qualified Data.Map as Map

data Morse = Dit | Dah | Pause deriving (Show, Read, Eq)

morseTable :: Map.Map Char [Morse]
morseTable = Map.fromList $ lowers ++ uppers ++ symbols ++ digits
  where
    lowers = first toLower <$> uppers
    uppers = [ ('A', [Dit, Dah])
             , ('B', [Dah, Dit, Dit, Dit])
             , ('C', [Dah, Dit, Dah, Dit])
             , ('D', [Dah, Dit, Dit])
             , ('E', [Dit])
             , ('F', [Dit, Dit, Dah, Dit])
             , ('G', [Dah, Dah, Dit])
             , ('H', [Dit, Dit, Dit, Dit])
             , ('I', [Dit, Dit])
             , ('J', [Dit, Dah, Dah, Dah])
             , ('K', [Dah, Dit, Dah])
             , ('L', [Dit, Dah, Dit, Dit])
             , ('M', [Dah, Dah])
             , ('N', [Dah, Dit])
             , ('O', [Dah, Dah, Dah])
             , ('P', [Dit, Dah, Dah, Dit])
             , ('Q', [Dah, Dah, Dit, Dah])
             , ('R', [Dit, Dah, Dit])
             , ('S', [Dit, Dit, Dit])
             , ('T', [Dah])
             , ('U', [Dit, Dit, Dah])
             , ('V', [Dit, Dit, Dit, Dah])
             , ('W', [Dit, Dah, Dah])
             , ('X', [Dah, Dit, Dit, Dah])
             , ('Y', [Dah, Dit, Dah, Dah])
             , ('Z', [Dah, Dah, Dit, Dit])
             , ('À', [Dit, Dah, Dah, Dit, Dah])
             , ('Å', [Dit, Dah, Dah, Dit, Dah])
             , ('Ä', [Dit, Dah, Dit, Dah])
             , ('È', [Dit, Dah, Dit, Dit, Dah])
             , ('É', [Dit, Dit, Dah, Dit, Dit])
             , ('Ö', [Dah, Dah, Dah, Dit])
             , ('Ü', [Dit, Dit, Dah, Dah])
             , ('ß', [Dit, Dit, Dit, Dah, Dah, Dit, Dit])
             ]
    digits = [ ('0', [Dah, Dah, Dah, Dah, Dah])
             , ('1', [Dit, Dah, Dah, Dah, Dah])
             , ('2', [Dit, Dit, Dah, Dah, Dah])
             , ('3', [Dit, Dit, Dit, Dah, Dah])
             , ('4', [Dit, Dit, Dit, Dit, Dah])
             , ('5', [Dit, Dit, Dit, Dit, Dit])
             , ('6', [Dah, Dit, Dit, Dit, Dit])
             , ('7', [Dah, Dah, Dit, Dit, Dit])
             , ('8', [Dah, Dah, Dah, Dit, Dit])
             , ('9', [Dah, Dah, Dah, Dah, Dit])
             ]
    symbols = [ ('.', [Dit, Dah, Dit, Dah, Dit, Dah])
              , (',', [Dah, Dah, Dit, Dit, Dah, Dah])
              , (':', [Dah, Dah, Dah, Dit, Dit, Dit])
              , (';', [Dah, Dit, Dah, Dit, Dah, Dit])
              , ('?', [Dit, Dit, Dah, Dah, Dit, Dah])
              , ('-', [Dah, Dit, Dit, Dit, Dit, Dah])
              , ('_', [Dit, Dit, Dah, Dah, Dit, Dah])
              , ('(', [Dah, Dit, Dah, Dah, Dit])
              , (')', [Dah, Dit, Dah, Dah, Dit, Dah])
              , ('\'', [Dit, Dah, Dah, Dah, Dah, Dit])
              , ('=', [Dah, Dit, Dit, Dit, Dah])
              , ('+', [Dit, Dah, Dit, Dah, Dit])
              , ('/', [Dah, Dit, Dit, Dah, Dit])
              , ('@', [Dit, Dah, Dah, Dit, Dah, Dit])
              , (' ', [Pause])
              , ('\EOT', [Dit, Dah, Dit, Dah, Dit])
              ]

fromChar :: Char -> [Morse]
fromChar = fromMaybe [] . (`Map.lookup` morseTable)

boolFilter :: [a] -> [a] -> Int -> [Bool] -> [a]
boolFilter tone silence dit (x:xs)
  = let tone' = case x of
                  True  -> tone
                  False -> silence
    in take dit tone' ++ boolFilter (drop dit tone) (drop dit silence) dit xs
boolFilter _ _ _ _ = []

morseToBool :: [Morse] -> [Bool]
morseToBool (Dit:xs) = True : morseToBool xs
morseToBool (Dah:xs) = True : True : True : morseToBool xs
morseToBool (Pause:xs) = False : morseToBool xs
morseToBool _ = []

encodeMorse :: [a] -> [a] -> Int -> [Morse] -> [a]
encodeMorse tone silence dit
  = boolFilter tone silence dit . morseToBool

encodeString :: [a] -> [a] -> Int -> String -> [a]
encodeString tone silence dit
  = concat
    . (encodeMorse tone silence dit
      . (++ [Pause, Pause, Pause])
      . intersperse Pause
      . fromChar <$>)

