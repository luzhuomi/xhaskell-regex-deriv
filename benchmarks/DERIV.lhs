> module Main where


> import Text.Regex.Deriv.ByteString

> import System.Environment
> import Data.Maybe
> import qualified Data.ByteString.Char8 as S



> parse compiled s = case regexec compiled s of 
>                      (Right (Just (_,_,_,l))) -> Just l
>                      _ -> Nothing


> main :: IO ()
> main = do 
>   { [ p, x ] <- getArgs
>   ; let pat = S.pack p
>         compiled = {-# SCC "main/compile" #-} case compile defaultCompOpt defaultExecOpt pat of
>                    Left _  -> error " compilation failed . "
>                    Right r -> r
>         -- ls = S.pack "abc"
>   ; ls <- {-# SCC "main/readFile" #-} S.readFile x
>   ; let input = S.lines ls
>   ; if (length input) > 0 
>     then do 
>       { let result = {-# SCC "main/parse" #-} parse compiled (head $ S.lines ls)
>       ; putStrLn (show result)
>       }
>     else putStrLn "The input file is empty"
>   }
