module Main where

import System
import System.IO
import System.Process 
import System.Locale
import System.Posix.Process
import System.Posix.Files
import System.Posix.Directory

import qualified Text.Regex.PDeriv.ByteString.LeftToRightD as R
import qualified Data.ByteString.Char8 as S

import Data.Char

pat = S.pack "([0-9\\.]*) user"

isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure _ = False

timeExec :: FilePath -> R.Regex -> String -> String -> IO ()
timeExec fp r exec arg = do 
  { (ec,stdin,stderr) <- readProcessWithExitCode "time" [exec,arg] []
  ; if isExitFailure ec 
    then do print "failed"
            print (show (ec,stdin,stderr))
    else do { print stdin 
            ; case parse r (S.pack $ stripCR stderr) of
                { Just (t:_) -> let ln = (joinWith (S.pack "\t") ((S.pack arg) : [t])) `S.append` (S.pack "\n")
                                in S.appendFile fp ln
                ; _          -> print "not matched" }
            ; putStr stderr }
  }
                       
stripCR :: String -> String                       
stripCR [] = []
stripCR ('\n':xs) = stripCR xs
stripCR (x:xs) = x:(stripCR xs)

joinWith :: S.ByteString -> [S.ByteString] -> S.ByteString
joinWith _ [] = S.empty
joinWith _ [x] = x
joinWith d (x:xs) = x `S.append` d `S.append` (joinWith d xs)

    
parse compiled s = case R.regexec compiled s of 
                     (Right (Just (_,_,_,l))) -> Just l
                     _ -> Nothing                     
                   
main :: IO ()
main = do 
  { (exec:logfile:rest) <- getArgs
  ; let compiled = case R.compile R.defaultCompOpt R.defaultExecOpt pat of
                     Left _  -> error " compilation failed . "
                     Right r -> r
        params = parseArgs rest
  -- ; S.writeFile logfile S.empty
  ; mapM_ (\x -> do { timeExec logfile compiled exec x } ) params
  }
    where 
      parseArgs :: [String] -> [String]
      parseArgs args = 
          case args of
            [l, u, inc ] | all (\a -> all isDigit a) args -> 
                             let 
                                 l' = read l
                                 u' = read u
                                 inc' = read inc
                                 params :: [ String ]
                                 params = map show [l', l'+inc' .. u' ]
                             in params
            _ -> args
    