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
>   ; let input = if S.null ls  
>                 then S.empty 
>                 else head $ S.lines ls
>         result = parse compiled input
>   ; putStrLn (show result)
>   }
