module Main (main)
where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, stdin)
import Data.Foldable (fold)

import qualified Data.ByteString.Builder as BB

import Sound.Morse as M

sinusTone :: Floating a => a -> a -> [a]
sinusTone rate freq = sin . (*(2*pi*freq/rate)) . fromInteger <$> [0..]

silence :: Num a => [a]
silence = repeat 0

encodeS16 :: RealFrac a => [a] -> BB.Builder
encodeS16 = fold . (BB.int16BE . floor . (*32767) <$>)

main = do
  hSetBuffering stdin NoBuffering
  getContents >>= BB.hPutBuilder stdout . encodeS16 . M.encodeString sinus silence dit
  where
    dit = 800
    sinus = sinusTone 8000 800

