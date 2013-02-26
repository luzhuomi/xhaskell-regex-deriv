> module Main where


> import Text.Regex.TDFA.ByteString
> import Text.Regex.TDFA

> import System.Environment
> import Data.Maybe
> import qualified Data.ByteString.Char8 as S



> parse compiled s = case regexec compiled s of 
>                      (Right (Just (_,_,_,l))) -> Just l
>                      _ -> Nothing


> main :: IO ()
> main = do 
>   { [ p, x ] <- getArgs
>   ; let pat  = S.pack p
>         compiled = case compile defaultCompOpt defaultExecOpt pat of
>                    Left _  -> error " compilation failed . "
>                    Right r -> r
>         -- ls = S.pack "abc"
>   ; ls <- S.readFile x
>   ; let result = parse compiled (head $ S.lines ls)
>   ; putStrLn (show result)
>   }
