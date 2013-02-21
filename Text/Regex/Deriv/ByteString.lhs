> {- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2009, BSD License -}

A bytestring implementation of POSIX reg exp pattern matching using derivative

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 


> module Text.Regex.Deriv.ByteString 
>     ( Regex
>     , CompOption(..)
>     , ExecOption(..)
>     , defaultCompOpt
>     , defaultExecOpt
>     , compile
>     , execute
>     , regexec
>     ) where 

The re-exports

> import Text.Regex.Deriv.ByteString.Posix ( Regex
>                                          , CompOption(..)
>                                          , ExecOption(..)
>                                          , defaultCompOpt
>                                          , defaultExecOpt
>                                          , compile
>                                          , execute
>                                          , regexec
>                                          ) 

