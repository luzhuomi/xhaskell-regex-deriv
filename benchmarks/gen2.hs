module Main where


import System.Environment
import System.Random
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as S

main :: IO ()
main = do 
  { [ str, size ] <- getArgs
  ; let numOfByte = (read size)
  ; mapM_ (\i -> S.putStr (S.pack str)
          ) [ 0 .. numOfByte ]
  }
                    
