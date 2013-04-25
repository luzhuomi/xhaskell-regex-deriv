> module Main where

> import System.Environment
> import Text.Regex.Posix.ByteString  
> import Data.Maybe
> import qualified Data.ByteString.Char8 as S



> parse compiled s = 
>     do { res <- regexec compiled s
>        ; case res of 
>          { (Right (Just (_,_,_,l))) -> return (Just l)
>          ;  _ -> return Nothing
>          }
>        }


> main :: IO ()
> main = do 
>   { [ p, x ] <- getArgs
>   ; let pat = S.pack p                  
>   ; (Right compiled) <- compile compExtended execBlank pat
>   ; ls <- S.readFile x
>   ; let input = S.lines ls
>   ; if (length input) > 0 
>     then do 
>      	{ result <- parse compiled (head $ S.lines ls) 
>      	; putStrLn $ show result
>	}
>     else putStrLn  "The input file is empty"
>   }
