module Main where


import System.Environment
import System.Random
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as S

main :: IO ()
main = do 
  { [ v ] <- getArgs
  ; let numOfByte = read v
        randGen   = getStdRandom (randomR (32,126))
  ; mapM_ (\i -> do 
              { j <- randGen
              ; S.putStr (S.singleton (chr j))
              }
          ) [ 0 .. numOfByte ]
  ; S.putStr (S.pack "(650) 253-0001")
  }
                    