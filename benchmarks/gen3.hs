module Main where


import System.Environment
import System.Random
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as S

main :: IO ()
main = do 
  { [ size ] <- getArgs
  ; let fact = (read size)
  ; mapM_ (\i -> S.putStr (S.pack "a")
          ) [ 0 .. 2^fact ]
  ; S.putStr (S.pack "bc")
  }
                    
