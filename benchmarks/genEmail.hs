module Main where


import System.Environment
import System.Random
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as S

main :: IO ()
main = do 
  { [ size ] <- getArgs
  ; let fact = (read size)
        charGen   = getStdRandom (randomR (97,122))
        intGen    = getStdRandom (randomR (3,10))
  ; mapM_ (\i -> do  
              { i1 <- intGen 
              ; i2 <- intGen 
              ; i3 <- intGen
              ; [str1,str2,str3] <- mapM (\l -> randStr l charGen) [i1,i2,i3]
              ; putStr (str1++"@"++str2++"."++str3++",")
              }
          ) [ 0 .. fact-1 ]
  ; do 
    { i1 <- intGen 
    ; i2 <- intGen 
    ; i3 <- intGen
    ; [str1,str2,str3] <- mapM (\l -> randStr l charGen) [i1,i2,i3]
    ; putStr (str1++"@"++str2++"."++str3)
    }
  }
       
                    

randStr :: Int -> IO Int -> IO [Char]
randStr 0 gen = return []
randStr n gen = do 
  a <- gen 
  let c = chr a
  cs <- randStr (n-1) gen
  return (c:cs)