> {- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2013, BSD License -}

A bytestring implementation of reg exp pattern matching using partial derivative / derivative
The POSIX matching policy is implemented by following the 'structure' of the reg-exp.
The pattern is follow annotated. 
We do not break part the sub-pattern of the original reg, they are always grouped under the same var pattern.


> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     BangPatterns, 
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 


> module Text.Regex.Deriv.ByteString.Posix
>     ( Regex
>     , CompOption(..)
>     , ExecOption(..)
>     , defaultCompOpt
>     , defaultExecOpt
>     , compile
>     , execute
>     , regexec
>     ) where 

> import Prelude hiding (Word)
> import System.IO.Unsafe
> import Data.IORef
> import qualified Data.HashTable.IO as H
> import qualified Data.Hashable as Ha


> import Data.List 
> import Data.Char (ord)
> import GHC.Int
> import GHC.Arr 
> import qualified Data.IntMap as IM
> import qualified Data.ByteString.Char8 as S
> import qualified Data.Map as M

> import Control.Monad

> import Text.Regex.Base(RegexOptions(..),RegexLike(..),MatchArray)


> import Text.Regex.Deriv.RE 
> import Text.Regex.Deriv.Common (IsPhi(..), IsEpsilon(..))
> import Text.Regex.Deriv.Pretty (Pretty(..))
> import Text.Regex.Deriv.Common (Range(..), Letter, PosEpsilon(..), my_hash, my_lookup, GFlag(..), IsGreedy(..), preBinder, subBinder, mainBinder)
> -- import Text.Regex.Deriv.IntPattern (Pat(..), toBinder, Binder(..), strip, listifyBinder, Key(..))
> import Text.Regex.Deriv.IntPattern (Pat(..), toBinder, Binder(..), strip, listifyBinder)
> import Text.Regex.Deriv.Parse
> import qualified Text.Regex.Deriv.Dictionary as D (Dictionary(..), Key(..), insertNotOverwrite, lookupAll, empty, isIn, nub, member, lookup, insert) 



> logger io = unsafePerformIO io

> type SRange = (Int,[Range])

> type CarryForward = IM.IntMap [Range] -- we only keep the first choice (i.e. the posix)  hence it is always mapping one variable to one env
> emptyCF = IM.empty


> combineCF :: CarryForward -> CarryForward -> CarryForward
> combineCF cf1 cf2 = IM.foldWithKey (\k r cf -> cf `seq` k `seq` r `seq` updateIfExist k r cf) cf2 cf1
>                          
> updateIfExist :: IM.Key -> [Range] -> CarryForward -> CarryForward
> updateIfExist k !r !cf = 
>    case IM.lookup k cf of   
>    { Just !r' -> IM.update (\_ -> Just (combineRange r r')) k cf
>    ; Nothing -> IM.insert k r cf }

> combineRange :: [Range] -> [Range] -> [Range]
> -- combineRange rs1 rs2 = rs1 -- this caused the bug of (.)* returning the first match instead of the last.
> combineRange ((r1@(Range b1 e1)):rs1) ((r2@(Range b2 e2)):rs2) 
>   | b1 == b2 = if e1 >= e2 
>                then [r1]
>                else [r2]
>   | b1 > b2 = [r1]
>   | otherwise = [r2]
> combineRange [] rs2 = rs2
> combineRange rs1 [] = rs1

> {-
> combineRange ((r1@(Range b1 e1)):rs1) ((r2@(Range b2 e2)):rs2) 
>   | b1 == b2 && e1 >= e2 = -- keeping all the discontinuated binding of p* 
>                            let rs = combineRange rs1 rs2
>                            in rs `seq` (r1:rs) 
>   | b1 == b2 && e2 >= e1 = -- keeping all the discontinuated binding of p*
>                            let rs = combineRange rs1 rs2
>                            in rs `seq` (r2:rs)
>   | b1 == e2+1 = -- keeping all the discontinuated binding of p* 
>                  let r = Range b2 e1
>                      rs = combineRange rs1 rs2
>                  in r `seq` rs `seq` (r:rs)
>   | b2 == e1+1 = -- keeping all the discontinuated binding of p* 
>                  let r = Range b1 e2
>                      rs = combineRange rs1 rs2
>                  in r `seq` rs `seq` (r:rs)
>   | b1 > e2+1 = -- keeping all the discontinuated binding of p*
>                 -- (Range b2 e2):(combineRange (r1:rs1) rs2)
>                 let rs1' = (r1:rs1)                      
>                     rs = rs1' `seq` combineRange rs1' rs2
>                 in  rs `seq` (r2:rs) 
>   | b2 > e1+1 = -- keeping all the discontinuated binding of p*
>                 -- (Range b1 e1):(combineRange rs1 (r2:rs2))
>                 let rs2' = r2:rs2
>                     rs = rs2' `seq` combineRange rs1 rs2'
>                 in rs `seq` (r1:rs) 
>   | otherwise = error ("unhandled combineRange " ++ r1 ++ " vs " ++ r2) 
> -}



> combineCFs :: [CarryForward] -> CarryForward 
> combineCFs cfs = foldl' (\cf1 cf2 -> cf1 `combineCF` cf2) emptyCF cfs

> insertCF :: SRange -> CarryForward -> CarryForward 
> insertCF (x,r) cf = IM.insert x r cf


> data SBinder = SChoice [SBinder] CarryForward                
>           | SPair SBinder SBinder CarryForward              
>           | SVar SRange SBinder CarryForward
>           | SStar CarryForward -- no need to store any SBinder, but 
>           | SRE CarryForward
>    deriving Show

> toSBinder :: Pat -> SBinder
> toSBinder (PVar x w p) = SVar (x,[]) (toSBinder p) emptyCF
> toSBinder (PE rs) = SRE emptyCF
> toSBinder (PStar p g) = SStar emptyCF
> toSBinder (PPair p1 p2) = SPair (toSBinder p1) (toSBinder p2) emptyCF
> toSBinder (PChoice ps g) = SChoice (map toSBinder ps) emptyCF



The invariance: 
The shapes of the input/output Pat and SBinder should be identical.


> dPat0C :: Pat -> Char -> [(Pat, Int -> SBinder -> SBinder)]
> dPat0C p c =  dPat0 p c 
> {- memoization
> dPat0C p c = 
>   case {-# SCC "dPat0C/lookupDCache" #-} lookupDCache p c of  
>    { Nothing -> let r = {-# SCC "dPat0C/dPat0" #-} dPat0 p c 
>                     io = r `seq` {-# SCC "dPat0C/insertDCache" #-} insertDCache p c r
>                 in io `seq` r
>    ; Just r -> r }  
> -}

> dPat0 :: Pat -> Char -> [(Pat, Int -> SBinder -> SBinder)] -- the result is always singleton or empty
> dPat0 y@(PVar x w p) l = 
>    do { (!p',!f) <- dPat0C p l  
>       ; let f' !i !sb = {-# SCC "dPat0/f0" #-} case sb of 
>                       { SVar (!v,!r) !sb' !cf -> let sb'' = {-# SCC "dPat0/f0/sb''" #-} f i sb' 
>                                                      r' =  {-# SCC "dPat0/f0/updateRange" #-} updateRange i r
>                                                      r'' = {-# SCC "dPat0/f0/updateRange2" #-} r' `seq` r' 
>                                                  in {-# SCC "dPat0/f0/in" #-}  sb'' `seq` r'' `seq` {-# SCC "dPat0/f0/in2" #-}  SVar (v, r') sb'' cf 
>                       -- ; senv -> error $ "invariance is broken: " ++ show y ++ " vs " ++ show senv 
>                       }
>       ; (!p'',!f'') <-  {-# SCC "dPat0/simpFix0" #-} simpFix (PVar x w p')
>       ; if  {-# SCC "dPat0/eq" #-} (p'' == (PVar x w p')) 
>         then return (PVar x w p', f')
>         else return (p'', (\i sb -> {-# SCC "dPat0/f0'" #-} 
>                                     let sb' = sb `seq` i `seq` f' i sb
>                                     in sb' `seq` (f'' i sb')))
>       }
> dPat0 (PE rs) l = 
>    let pds' = (concatMap (\r -> partDeriv r l) rs)
>        pds = pds' `seq` nub pds' 
>    in pds `seq` 
>       if null pds then mzero
>       else return (PE pds, (\_ !sb -> {-# SCC "dPat0/id0" #-}  sb) )
> dPat0 (PStar p g) l = 
>    do { (!p', !f) <- dPat0C p l        
>       ; let emp = toSBinder p                     
>       ; emp `seq` 
>         return (PPair p' (PStar p g), (\i sb -> {-# SCC "dPat0/f1" #-} i `seq` sb `seq` 
>                     case sb of { SStar !cf -> let sb' =  f i emp 
>                                               in sb' `seq` 
>                                                  SPair sb' sb cf} ) ) 
>       }
> dPat0 (PPair !p1 !p2) l 
>    | (posEpsilon (strip p1)) =
>       let pf1 = dPat0C p1 l                           
>           pf2 = dPat0C p2 l
>       in case (pf1, pf2) of
>       { ([], []) -> mzero
>       ; ([], [(!p2',!f2')]) ->
>          let rm = extract p1
>              f !i !sb = {-# SCC "dPat0/f3" #-} case sb of 
>                 { SPair !sb1 !sb2 !cf -> 
>                      let sb1' = {-# SCC "dPat0/f3/rm" #-}rm sb1  
>                          cf' = {-# SCC "dPat0/f3/cf'" #-}sb1' `seq` combineCF sb1' cf
>                          sb2' = {-# SCC "dPat0/f3/sb2'" #-}f2' i sb2
>                      in cf' `seq` sb2' `seq` {-# SCC "dPat0/f3/carryForward" #-} carryForward cf' sb2' }
>          in do { (!p2'',!f2'') <- {-# SCC "dPat0/simpFix1" #-} simpFix p2'
>                ; if p2'' == p2'
>                  then return (p2', f)
>                  else return (p2'', \i sb -> {-# SCC "dPat0/f4" #-} 
>                                              let sb' = i `seq` sb `seq` f i sb
>                                              in sb' `seq` (f2'' i sb'))
>                }
>       ; ([(!p1',!f1')], []) -> -- todo
>          let f !i !sb = case sb of 
>                 { SPair !sb1 !sb2 !cf -> {-# SCC "dPat0/f5" #-} 
>                                          let sb1' =  f1' i sb1 
>                                          in sb1' `seq` 
>                                             SPair sb1' sb2 cf }
>          in do { (!p1'',!f1'') <- {-# SCC "dPat0/simpFix2" #-} simpFix (PPair p1' p2)
>                ; if (p1'' == (PPair p1' p2))
>                  then return (PPair p1' p2, f)
>                  else return (p1'', \i sb -> {-# SCC "dPat0/f6" #-} 
>                                              let sb' = i `seq` sb `seq` f i sb
>                                              in sb' `seq` (f1'' i sb')) 
>                }
>       ; _ | isGreedy p1 -> do 
>         { (!p1',!f1) <- pf1
>         ; (!p2',!f2) <- pf2
>         ; let rm = extract p1
>               f !i !sb = {-# SCC "dPat0/f7" #-} case sb of
>                 { SPair !sb1 !sb2 !cf ->
>                     let sb1' =  {-# SCC "dPat0/f7/rm" #-} rm sb1
>                         sb1'' = {-# SCC "dPat0/f7/f1" #-} f1 i sb1
>                         cf' = {-# SCC "dPat0/f7/combineCF" #-}sb1' `seq` sb1' `combineCF` cf
>                         sb2' = {-# SCC "dPat0/f7/f2" #-}f2 i sb2
>                         sb2'' = {-# SCC "dPat0/f7/carryForward" #-}sb2' `seq` cf' `seq` carryForward cf' sb2'
>                     in {-# SCC "dPat0/f7/in" #-} sb1'' `seq` cf `seq` sb2 `seq` sb2'' `seq` SChoice [ SPair sb1'' sb2 cf, sb2'' ] emptyCF }
>         ; (!p',!f') <- {-# SCC "dPat0/simpFix3" #-}  simpFix (PChoice [PPair p1' p2, p2'] Greedy) 
>         ; if (p' == (PChoice [PPair p1' p2, p2'] Greedy))
>           then return (PChoice [PPair p1' p2, p2'] Greedy, f)
>           else return (p', \i sb -> {-# SCC "dPat0/f8" #-} 
>                                     let sb' = i `seq` sb `seq` f i sb 
>                                     in sb' `seq`  (f' i sb'))          
>         }
>           | otherwise -> do 
>         { (!p1',!f1) <- pf1
>         ; (!p2',!f2) <- pf2
>         ; let rm = extract p1
>               f !i !sb = {-# SCC "dPat0/f9" #-} case sb of
>                 { SPair !sb1 !sb2 !cf ->
>                     let sb1' = rm sb1
>                         sb2' = f2 i sb2
>                         cf1' = sb1' `seq` cf `seq` 
>                                sb1' `combineCF` cf
>                         sb1'' = f1 i sb1
>                         sb2'' = cf1' `seq` sb2' `seq` carryForward cf1' sb2'
>                     in  sb2'' `seq` sb1'' `seq` sb2 `seq` 
>                        SChoice [sb2'',  SPair sb1'' sb2 cf ] emptyCF }
>         ; (!p',!f') <- {-# SCC "dPat0/simpFix4" #-} simpFix (PChoice [p2' , PPair p1' p2] Greedy) 
>         ; if (p' == (PChoice [p2' , PPair p1' p2] Greedy))
>           then return (PChoice [p2' , PPair p1' p2] Greedy, f)
>           else return (p', \i sb -> {-# SCC "dPat0/f10" #-} let sb' =  i `seq` sb `seq` f i sb
>                                     in sb' `seq` (f' i sb'))          
>         }
>       }
>    | otherwise =
>       do { (!p1',!f1) <- dPat0C p1 l
>          ; let f !i !sb = {-# SCC "dPat0/f11" #-} case sb of { SPair !sb1 !sb2 !cf -> 
>                                                                let sb1' = f1 i sb1
>                                                                in sb1' `seq` sb2 `seq` SPair sb1' sb2 cf } 
>          ; (!p',!f') <- {-# SCC "dPat0/simpFix5" #-} simpFix (PPair p1' p2)
>          ; if (p' == (PPair p1' p2))
>            then return (PPair p1' p2, f)
>            else return (p', \i sb -> {-# SCC "dPat0/f12" #-} 
>                                      let sb' =  i `seq` sb `seq` f i sb 
>                                      in sb' `seq` (f' i sb'))
>          }
> dPat0 (PChoice [] g) l = mzero
> dPat0 y@(PChoice [!p] g) l = do
>       { (!p',!f') <- dPat0C p l
>       ; let f !i !sb = {-# SCC "dPat0/f13" #-} 
>                        case sb of { SChoice [!sb'] !cf -> let sb'' =  (f' i sb')  in sb'' `seq` carryForward cf sb''
>                                   ; senv -> error $ "invariance is broken: " ++ pretty y ++ " vs "  ++ show senv 
>                                   }
>       ; (!p'',!f'') <- {-# SCC "dPat0/simpFix6" #-} simpFix p'
>       ; if (p'' == p')
>         then return (p', f)
>         else return (p'', \i sb -> {-# SCC "dPat0/f14" #-} 
>                                    let sb' = i `seq` sb `seq` f i sb 
>                                    in sb' `seq` (f'' i sb'))                     
>       }
> dPat0 (PChoice !ps g) l = 
>    let pfs = map (\p -> p `seq` dPat0C p l) ps
>        nubPF :: [[(Pat, Int -> SBinder -> SBinder)]] -> [(Pat, Int -> SBinder -> SBinder)] 
>        nubPF pfs = {-# SCC "dPat0/nubPF" #-}  nub2Choice pfs M.empty 
>    in do 
>    { (!p,!f) <- pfs `seq` nubPF pfs
>    ; (!p',!f') <- {-# SCC "dPat0/simpFix7" #-} simpFix p
>    ; if (p' == p) 
>      then return (p, f)
>      else return (p', \i sb -> {-# SCC "dPat0/f15" #-} 
>                                let sb' =  i `seq` sb `seq` f i sb 
>                                in sb' `seq`  (f' i sb')) 
>    }



nub2Choice: turns a list of pattern x coercion pairs into a pchoice and a func, duplicate patterns (hence conflicting matches) are removed.


e.g. 

*Text.Regex.Deriv.ByteString.Posix> matchInner [(testp, toSBinder testp)] (zip "ABAA" [1..])
[((0:(1:(2:(3:({(9:|[([<'A','C'>,'C'])]|),(9:|['C']|)}))))),
 SVar (0,[Range 1 4]) (SVar (1,[Range 1 4]) (SVar (2,[Range 1 4]) (SVar (3,[Range 1 4]) (SChoice [SVar (9,[]) (SRE (fromList [])) (fromList [(5,[Range 1 4]),(7,[Range 1 1]),(8,[Range 2 4])])
                                                                                                 ,SVar (9,[Range 4 4]) (SRE (fromList [])) (fromList [(5,[Range 1 3]),(7,[Range 1 2]),(8,[Range 3 3])])] (fromList [])) (fromList [])) (fromList [])) (fromList [])) (fromList []))]


We match one more C,
The intermediate step was 


[((0:(1:(2:(3:({(9:|[([<>]|),(9:|[<>]|)}))))),
 SVar (0,[Range 1 4]) (SVar (1,[Range 1 4]) (SVar (2,[Range 1 4]) (SVar (3,[Range 1 4]) (SChoice [SVar (9,[Range 5 5]) (SRE (fromList [])) (fromList [(5,[Range 1 4]),(7,[Range 1 1]),(8,[Range 2 4])])
                                                                                                 ,SVar (9,[Range 4 5]) (SRE (fromList [])) (fromList [(5,[Range 1 3]),(7,[Range 1 2]),(8,[Range 3 3])])] (fromList [])) (fromList [])) (fromList [])) (fromList [])) (fromList []))]



*Text.Regex.Deriv.ByteString.Posix> matchInner [(testp, toSBinder testp)] (zip "ABAAC" [1..])
[((0:(1:(2:(3:(9:|[<>]|))))),
 SVar (0,[Range 1 5]) (SVar (1,[Range 1 5]) (SVar (2,[Range 1 5]) (SVar (3,[Range 1 5]) (SVar (9,[Range 5 5]) (SRE (fromList [])) (fromList [(5,[Range 1 4]),(7,[Range 1 1]),(8,[Range 2 4])])) (fromList [])) (fromList [])) (fromList [])) (fromList []))]





The first arg is a list of list of pair, because of the list monad generated by dPat0, each non-empty sub list is a singleton list.
The resulting func accept a SChoice pattern (cf to the input list of pattern). 




-----------------------------------
{}, d |-nub PChoice {}, \i -> id


pfs .... todo
--------------------------------------------
{[]}\cup pfs , d |-nub PChoice {}, \i -> id

> nub2Choice :: [[(Pat, Int -> SBinder -> SBinder)]] -> M.Map Pat (Int -> SBinder -> SBinder) -> [(Pat, Int -> SBinder -> SBinder)] -- the return type is a singleton list.
> nub2Choice [] pDict = return (PChoice [] Greedy, (\i !sb -> {-# SCC "nubChoice/id0" #-} sb )) -- the base case is the identity
> nub2Choice ([]:pfs) pDict = do 
>      { (PChoice !ps !g, !f'') <- nub2Choice pfs pDict
>      ; let f' !i !sb = {-# SCC "nub2Choice/f1" #-} case sb of
>            { SChoice (s:ss) !cf -> ss `seq`
>                 f'' i $! (SChoice ss cf)
>            ; _ -> error "nub2Choice coercion is applied to a non SChoice"
>            }
>      ; return  (PChoice ps g, f')
>      }                                  
> nub2Choice ([(!p,!f)]:pfs) !pDict  -- recall the invarance of nub2Choice and dPat0, the return value of f shares the same shape of p
>   | isPhi (strip p) || {-# SCC "nub2Choice/member" #-} p `M.member` pDict = do  -- simplification
>      { (PChoice !ps !g, !f'') <- nub2Choice pfs pDict
>      ; let f' !i !sb =  {-# SCC "nub2Choice/f2" #-} case sb of
>            { SChoice (s:ss) !cf -> ss `seq` 
>                 f'' i $! (SChoice ss cf)
>            ; _ -> error "nub2Choice coercion is applied to a non SChoice"
>            }
>      ; return (PChoice ps g, f')
>      } 
>   | otherwise = 
>     case p of 
>       { PChoice !ps' !g' -> do 
>         { let fs' :: [Int -> SBinder -> SBinder]
>               fs' = repeat (\i !sb -> {-# SCC "nubChoice/id1" #-}  sb ) --  identity functions
>               pfs' = zip ps' fs'
>               pfs'' = pfs' `seq` map (\x -> [x]) pfs'
>               pfs''' = pfs'' `seq` pfs'' ++ pfs
>         --;  (p', f'') <- nub2Choice ((map (\x -> [x]) (zip ps' fs'))++pfs) pDict 
>         ; (!p', !f'') <- pfs''' `seq` nub2Choice pfs''' pDict
>         ; let f' !i !sb = {-# SCC "nub2Choice/f3" #-} case sb of  
>                         { (SChoice (s:ss) !cf) -> s `seq` ss `seq`
>                             case (f i s) of
>                             { SChoice !ss'' !cf' -> let ss''' = map (\x -> x `seq` carryForward cf' x) ss''
>                                                         ss'''' = ss''' `seq`  (ss''' ++ ss)
>                                                     in ss'''' `seq` f'' i $ (SChoice ss'''' cf) 
>                             ; _ -> error "nub2Choice coercion is applied to a non SChoice" }
>                         ;  _ -> error "nub2Choice coercion is applied to a non SChoice" }
>         ; return (p', f')
>         }
>       ; _ ->   
>         do
>         { let pDict' = M.insert p f pDict
>         ; (PChoice !ps !g, !f'') <- pfs `seq` pDict' `seq` nub2Choice pfs pDict'
>         ; let f' !i !sb = {-# SCC "nub2Choice/f4" #-} case sb of
>                         { SChoice (s:ss) !cf -> s `seq` ss `seq` 
>                              let (SChoice !ss' !cf') = f'' i $ (SChoice ss cf)
>                                  s' = f i s
>                                  ss'' = s' `seq` ss' `seq` (s':ss')
>                              in ss'' `seq` cf' `seq` SChoice ss'' cf'
>                         ; _ -> error "nub2Choice coercion is applied to a non SChoice"
>                         }
>         ; let ps' = (p:ps)
>         ; ps' `seq` return (PChoice ps' g, f')
>         }
>       }

unsafe cache

> {- 16 6
> dPat0Cache :: IORef (M.Map (Char,Pat) [(Pat, Int -> SBinder -> SBinder)])
> dPat0Cache = unsafePerformIO $ do { cref <- newIORef M.empty 
>                                   ; return cref }

> insertDCache :: Pat -> Char -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> insertDCache p c r = unsafePerformIO $ do { m <- {-# SCC "insertDCache/readIORef" #-} readIORef dPat0Cache
>                                           ; let m' = {-# SCC "insertDCache/insert" #-} M.insert (c,p) r m
>                                           ; {-# SCC "insertDCache/writeIORef" #-} writeIORef dPat0Cache m' }

> lookupDCache :: Pat -> Char -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupDCache p c = unsafePerformIO $ do { m <- {-# SCC "lookupDCache/readIORef" #-} readIORef dPat0Cache
>                                         ; case {-# SCC "lookupDCache/lookup" #-} M.lookup (c,p) m of
>                                           { Nothing -> return Nothing 
>                                           ; Just x  -> return (Just x) } }
> 

> 
> 
> simpCache :: IORef (M.Map Pat [(Pat, Int -> SBinder -> SBinder)])
> simpCache = unsafePerformIO $ do { cref <- newIORef M.empty 
>                                  ; return cref }

> insertSCache :: Pat -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> insertSCache p r = unsafePerformIO $ do { m <- readIORef simpCache
>                                         ; let m' = M.insert p r m
>                                         ; writeIORef simpCache m' }

> lookupSCache :: Pat -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupSCache p = unsafePerformIO $ do { m <- readIORef simpCache
>                                       ; case M.lookup p m of
>                                         { Nothing -> return Nothing 
>                                         ; Just x  -> return (Just x) } }
> -}


> 

> instance Ha.Hashable Pat where
>    hashWithSalt salt (PVar x _ p) = Ha.hashWithSalt salt (Ha.hashWithSalt 1 $ Ha.hashWithSalt (Ha.hash x) p)
>    hashWithSalt salt (PPair p1 p2) = Ha.hashWithSalt salt $ Ha.hashWithSalt 3 $ Ha.hashWithSalt (Ha.hash p1) p2
>    hashWithSalt salt (PPlus p1 p2) = Ha.hashWithSalt salt $ Ha.hashWithSalt 5 $ Ha.hashWithSalt (Ha.hash p1) p2
>    hashWithSalt salt (PStar p1 g) = Ha.hashWithSalt salt $ Ha.hashWithSalt 7 $ Ha.hashWithSalt (Ha.hash g) p1
>    hashWithSalt salt (PE rs) = Ha.hashWithSalt salt $ Ha.hashWithSalt 11 $ Ha.hash rs                         
>    hashWithSalt salt (PChoice ps g) = Ha.hashWithSalt salt $ Ha.hashWithSalt 13 $ Ha.hashWithSalt (Ha.hash g) ps
>    hashWithSalt salt (PEmpty p) = Ha.hashWithSalt salt $ Ha.hashWithSalt 17 $ p


> instance Ha.Hashable GFlag where 
>    hashWithSalt salt Greedy = Ha.hashWithSalt salt (19::Int)
>    hashWithSalt salt NotGreedy = Ha.hashWithSalt salt (23::Int)

> instance Ha.Hashable RE where
>    hashWithSalt salt Empty = Ha.hashWithSalt salt (29::Int)
>    hashWithSalt salt (L x) = Ha.hashWithSalt salt (Ha.hashWithSalt 31 (Ha.hash x))
>    hashWithSalt salt (Choice rs g) = Ha.hashWithSalt salt (Ha.hashWithSalt 37 (Ha.hashWithSalt (Ha.hash g) rs))
>    hashWithSalt salt (Seq r1 r2) = Ha.hashWithSalt salt (Ha.hashWithSalt 41 (Ha.hashWithSalt (Ha.hash r1) r2))
>    hashWithSalt salt (Star r g) = Ha.hashWithSalt salt (Ha.hashWithSalt 43 (Ha.hashWithSalt (Ha.hash g) r))
>    hashWithSalt salt Any = Ha.hashWithSalt salt (47 ::Int)
>    hashWithSalt salt (Not cs) = Ha.hashWithSalt salt (Ha.hashWithSalt 53 cs)


> {-
> type HashTable k v = H.BasicHashTable k v   --9.7 5.9
                                                     
> 
> dPat0Cache :: IORef (HashTable (Char,Pat) [(Pat, Int -> SBinder -> SBinder)])
> dPat0Cache = unsafePerformIO $ do { ht <- H.new
>                                   ; cref <- newIORef ht
>                                   ; return cref }


> insertDCache :: Pat -> Char -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> insertDCache p c r = unsafePerformIO $ do { ht <- {-# SCC "insertDCache/readIORef" #-} readIORef dPat0Cache
>                                           ; {-# SCC "insertDCache/insert" #-} H.insert ht (c,p) r 
>                                           ; {-# SCC "insertDCache/writeIORef" #-} writeIORef dPat0Cache ht }


> lookupDCache :: Pat -> Char -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupDCache p c = unsafePerformIO $ do { ht <- {-# SCC "lookupDCache/readIORef" #-} readIORef dPat0Cache
>                                         ; {-# SCC "lookupDCache/lookup" #-} H.lookup ht (c,p) }
> 

> 
> simpCache :: IORef (HashTable Pat [(Pat, Int -> SBinder -> SBinder)])
> simpCache = unsafePerformIO $ do { ht <- H.new
>                                  ; cref <- newIORef ht
>                                  ; return cref }

> insertSCache :: Pat -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> insertSCache p r = unsafePerformIO $ do { ht <- readIORef simpCache
>                                         ; H.insert ht p r
>                                         ; writeIORef simpCache ht }

> lookupSCache :: Pat -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupSCache p = unsafePerformIO $ do { ht <- readIORef simpCache
>                                       ; H.lookup ht p }                                         
> -}



> -- slowest
> dPat0Cache :: IORef (IM.IntMap [(Pat, Int -> SBinder -> SBinder)])
> dPat0Cache = unsafePerformIO $ do { cref <- newIORef IM.empty 
>                                   ; return cref }

> insertDCache :: Pat -> Char -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> insertDCache p c r = unsafePerformIO $ do { m <- readIORef simpCache
>                                           ; let m' = IM.insert (Ha.hash (c,p)) r m
>                                           ; writeIORef simpCache m' }

> lookupDCache :: Pat -> Char -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupDCache p c = unsafePerformIO $ do { m <- readIORef simpCache
>                                         ; case IM.lookup (Ha.hash (c,p)) m of
>                                            { Nothing -> return Nothing 
>                                            ; Just x  -> return (Just x) } }

> simpCache :: IORef (IM.IntMap [(Pat, Int -> SBinder -> SBinder)])
> simpCache = unsafePerformIO $ do { cref <- newIORef IM.empty 
>                                  ; return cref }

> insertSCache :: Pat -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> insertSCache p r = unsafePerformIO $ do { m <- readIORef simpCache
>                                         ; let m' = IM.insert (Ha.hash p) r m
>                                         ; writeIORef simpCache m' }

> lookupSCache :: Pat -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupSCache p = unsafePerformIO $ do { m <- readIORef simpCache
>                                        ; case IM.lookup (Ha.hash p) m of
>                                          { Nothing -> return Nothing 
>                                          ; Just x  -> return (Just x) } }

> {-
> mkKey :: Pat -> Int
> mkKey p = let s = show p
>           in foldl' (\i c -> i*31 + (ord c)) 0 s                
> -}

> {- collision
> simpCache :: IORef (D.Dictionary [(Pat, Int -> SBinder -> SBinder)])
> simpCache = unsafePerformIO $ do { cref <- newIORef D.empty 
>                                  ; return cref }

> cache :: Pat -> [(Pat, Int -> SBinder -> SBinder)] -> ()
> cache p r = unsafePerformIO $ do { m <- readIORef simpCache
>                                  ; let m' = D.insertNotOverwrite p r m
>                                  ; writeIORef simpCache m' }

> lookupCache :: Pat -> Maybe [(Pat, Int -> SBinder -> SBinder)]
> lookupCache p = unsafePerformIO $ do { m <- readIORef simpCache
>                                      ; case D.lookup p m of
>                                        { Nothing -> return Nothing 
>                                        ; Just x  -> return (Just x) } }
> -}
> 

simplification


> simpFix :: Pat -> [(Pat, Int -> SBinder -> SBinder)]
> simpFix p = 
>   simpC p 
> -- simpFix' p (\i -> id) -- simpfix' seems not neccessary

> simpFix' p f = 
>   case simp p of
>   { [] -> []  
>   ; [(!p',!f')] ->
>      if p' == p
>      then [(p,f)]
>      else simpFix' p' (\i sb -> let sb' =  i `seq` sb `seq` f i sb
>                                 in sb' `seq`  (f' i sb))
>   }


> simpC :: Pat -> [(Pat, Int -> SBinder -> SBinder)]
> simpC p = simp p 
> {- memoization
> simpC p = 
>   case lookupSCache p of 
>    { Nothing -> let r = simp p  
>                     io = insertSCache p r
>                 in io `seq` r
>    ; Just r -> r }
> -}

invariance: input / outoput of Int -> SBinder -> SBinder agree with simp's Pat input/ output

> simp :: Pat -> [(Pat, Int -> SBinder -> SBinder)] -- the output list is singleton 
> simp (PVar !x w !p) = do
>   { (!p',!f') <- simpC p
>   ; case p' of
>     { _ | p == p' -> return (PVar x w p,\_ !sb -> {-# SCC "simp/id0" #-}  sb)
>         | isPhi (strip p') -> mzero
>         | otherwise -> let f i sb = {-# SCC "simp/f0" #-} i `seq` sb `seq` case sb of 
>                                     { SVar !vr !sb' !cf -> let sb'' = f' i sb' 
>                                                            in sb'' `seq` SVar vr sb'' cf
>                                     }
>                        in return (PVar x w p', f)
>     }
>   }
> simp y@(PPair !p1 !p2) = do
>    { (!p1',!f1') <- simpC p1
>    ; (!p2',!f2') <- simpC p2
>    ; case (p1',p2') of       
>      { _ | isPhi p1' || isPhi p2' -> mzero
>          | isEpsilon p1'          -> 
>              let !rm = extract p1
>                  f !i !sb = {-# SCC "simp/f1" #-} case sb of
>                     { SPair !sb1 !sb2 !cf -> let cf' = {-# SCC "simp/f1/rm" #-} rm sb1 
>                                                  cf'' =  {-# SCC "simp/f1/combineCF" #-} cf' `seq` cf' `combineCF` cf
>                                                  sb2' = {-# SCC "simp/f1/f2'" #-}sb2 `seq` f2' i sb2
>                                              in {-# SCC "simp/f1/in" #-} cf'' `seq` sb2' `seq` carryForward cf'' sb2' }
>              in return (p2',f)
>          | isEpsilon p2'          ->
>              let !rm = extract p2
>                  f !i !sb = {-# SCC "simp/f2" #-} case sb of 
>                     { SPair !sb1 !sb2 !cf -> let cf' = rm sb2 
>                                                  cf'' = cf' `seq` (cf' `combineCF` cf) 
>                                                  sb1' = f1' i sb1 
>                                              in cf'' `seq` sb1' `seq` carryForward cf'' sb1' }
>              in return (p1',f)
>          | otherwise              ->
>              let f !i !sb = {-# SCC "simp/f3" #-} case sb of
>                     { SPair !sb1 !sb2 !cf -> let sb1' = (f1' i sb1)
>                                                  sb2' = (f2' i sb2)
>                                              in sb1' `seq` sb2' `seq` SPair sb1' sb2' cf
>                     ; senv -> error $ "invariance broken: " ++ pretty y ++ " vs " ++ show senv }
>              in return (PPair p1' p2', f)
>       }
>    }
> simp (PChoice [] g) = mzero
> simp (PChoice [!p] !g) = do 
>    { (!p',!f') <- simpC p
>    ; if isPhi p' 
>      then mzero
>      else 
>       let f !i !sb = {-# SCC "simp/f4" #-}
>                      case sb of { SChoice [!sb'] !cf -> let sb'' = f' i sb' 
>                                                         in sb'' `seq` carryForward cf sb'' }
>       in return (p',f)
>    }
> simp (PChoice !ps !g) = 
>    let pfs = map simpC ps  
>        nubPF :: [[(Pat, Int -> SBinder -> SBinder)]] -> [(Pat, Int -> SBinder -> SBinder)] 
>        nubPF pfs = nub2Choice pfs M.empty
>    in pfs `seq` {-# SCC "simp/nubPF" #-} nubPF pfs
> simp p = return (p,\_ !sb -> {-# SCC "simp/id1" #-} sb)


> carryForward :: CarryForward -> SBinder -> SBinder
> carryForward sr (SVar (v, r) sb' cf) = let cf' = combineCF cf sr
>                                        in cf' `seq` SVar (v, r) sb' cf'
> carryForward sr (SRE cf) = let cf' = combineCF cf sr in cf' `seq` SRE cf'
> carryForward sr (SStar cf) = let cf' = combineCF cf sr in cf' `seq` SStar cf'
> carryForward sr (SPair sb1 sb2 cf) =  let cf' = combineCF cf sr in cf' `seq` SPair sb1 sb2 cf'
> carryForward sr (SChoice sbs cf) =  let cf' = combineCF cf sr in cf' `seq` SChoice sbs cf'
> carryForward sr sb2 = error ("trying to carry forward into a non-annotated pattern binder " ++ (show sb2))


> instance Ord Pat where
>   compare (PVar x1 _ p1) (PVar x2 _ p2) 
>      | x1 == x2 = {-# SCC "compare1" #-}  compare p1 p2
>      | otherwise = {-# SCC "compare2" #-} compare x1 x2
>   compare (PE r1) (PE r2) = {-# SCC "compare3" #-} compare r1 r2 
>   compare (PStar p1 g1) (PStar p2 g2) = let r = {-# SCC "compare4" #-} compare g1 g2 in case r of 
>      { EQ -> compare p1 p2
>      ; _  -> r }
>   compare (PPair p1 p2) (PPair p3 p4) = let r = {-# SCC "compare5" #-} compare p1 p3 in case r of 
>      { EQ -> {-# SCC "compare6" #-} compare p2 p4
>      ; _  -> r }
>   compare (PChoice ps1 g1) (PChoice ps2 g2) = let r = {-# SCC "compare7" #-} compare g1 g2 in case r of
>      { EQ -> compare ps1 ps2
>      ; _  -> r }
>   compare p1 p2 = {-# SCC "compare8" #-} compare (assignInt p1) (assignInt p2) 
>     where assignInt (PVar _ _ _) = 0
>           assignInt (PE _) = 1
>           assignInt (PStar _ _) = 2
>           assignInt (PPair _ _) = 3
>           assignInt (PChoice _ _) = 4


extract a carry forward from the sbinder

> extract :: Pat -> SBinder -> CarryForward
> extract (PVar !x w !p) (SVar (_,!b) !sb !cf)
>      | posEpsilon (strip p) = let cf' = extract p sb
>                                   cf'' = cf' `seq` insertCF (x,b) cf'
>                               in  cf'' `seq` (cf'' `combineCF` cf)
>      | otherwise = IM.empty -- cf?
> extract (PE rs) (SRE !cf) = cf
> extract (PStar p g) (SStar !cf) = cf
> extract (PPair !p1 !p2) (SPair !sb1 !sb2 !cf) = let cf1 = (extract p1 sb1) 
>                                                     cf2 = (extract p2 sb2) 
>                                                     cf' = cf1 `seq` cf2 `seq` (combineCF cf1 cf2)
>                                                  in  cf' `seq` (cf' `combineCF` cf)
> extract (PChoice !ps !g) (SChoice !sbs !cf) = let psbs = zip ps sbs 
>                                                   cf' = psbs `seq` (combineCFs $! map (\(!p,!sb) -> extract p sb) psbs) 
>                                               in  cf' `seq` (cf' `combineCF` cf)
> extract p sb = error $ "Error: trying to extract" ++ (show sb) ++ " from " ++ (show p)


> updateRange :: Int -> [Range] -> [Range]
> updateRange !pos (rs_@((Range !b !e):rs)) = 
>           let e' =  e + 1    
>           in e' `seq` case e' of
>              _ | pos == e' -> let r = Range b e' in r `seq` rs `seq` (r:rs)
>                | pos > e'  -> -- keeping all the discontinuated binding of p*
>                               -- let r = Range pos pos in r `seq` (r:rs_)
>                               -- only keep the last binding
>                               let r = Range pos pos in r `seq` [r]
>                | otherwise -> error "impossible, the current letter position is smaller than the last recorded letter" 
> updateRange !pos [] = let r = Range pos pos in r `seq` [r]


> matchInner :: [(Pat, SBinder)] -> [(Char,Int)] -> [(Pat, SBinder)]
> matchInner pb [] = pb
> matchInner pb (l:ls) = 
>   do { (p,sb) <- pb  
>      ; (p',f) <- dPat0 p (fst l)
>      ; matchInner [(p', f (snd l) sb)] ls 
>      }


> type Env = [SRange]

> match :: Pat -> [Char] -> [Env]
> match p w = do { (p',sb) <- matchInner [(p, toSBinder p)] (zip w [1..])  
>                ; sbinderToEnv p' sb }

> posixMatch :: Pat -> [Char] -> Maybe Env
> posixMatch p w = case match p w of
>   { (e:_) -> Just e ; _ -> Nothing }


> match2 :: (Pat,FollowBy,IM.IntMap()) -> [Char] -> [MatchArray]
> match2 (p,fb,posixBinder) w = 
>   map (\env -> sbinderToMatchArray (length w) fb posixBinder (IM.fromList env)) (match p w)


get all envs from the sbinder

> sbinderToEnv :: Pat -> SBinder -> [Env]
> sbinderToEnv p sb = 
>   let cfs = {-# SCC "sbinderToEnv/sbinderToEnv'" #-} sbinderToEnv' p sb
>   in map IM.toList cfs

> sbinderToEnv' :: Pat -> SBinder -> [CarryForward]
> sbinderToEnv' _ (SChoice [] _) = []
> sbinderToEnv' (PChoice (p:ps) g) (SChoice (sb:sbs) cf) 
>   | posEpsilon (strip p) = 
>   do { cf' <- sbinderToEnv' p sb
>      ; cf `seq` cf' `seq` return (combineCF cf cf') }
>   | otherwise = sbinderToEnv' (PChoice ps g) (SChoice sbs cf)
> sbinderToEnv' (PPair p1 p2) (SPair sb1 sb2 cf) =
>   do { cf1 <- sbinderToEnv' p1 sb1 
>      ; cf2 <- sbinderToEnv' p2 sb2
>      ; cf1 `seq` cf2 `seq` cf `seq` return (combineCFs [cf1,cf2,cf]) }
> sbinderToEnv' (PVar x _ p) (SVar sr sb cf) 
>   | posEpsilon (strip p) = do { cf' <- sbinderToEnv' p sb
>                               ; let cf'' = cf' `seq` sr `seq` insertCF sr cf'
>                               ; cf `seq` cf'' `seq` return (cf `combineCF` cf'') }
>   | otherwise = []
> sbinderToEnv' (PStar _ _) (SStar cf) = return cf
> sbinderToEnv' (PE _) (SRE cf) = return cf
> sbinderToEnv' p sb = error $ (pretty p) ++ " and " ++ (show sb)


> type DfaTable = IM.IntMap (Int, Int -> SBinder -> SBinder, SBinder -> [Env])


> compilePat :: Pat -> (DfaTable,SBinder, SBinder -> [Env], [Int])
> compilePat p = let (t, sb, toEnv, finals) = buildDfaTable p 
>                in (t, sb, toEnv, finals)


> buildDfaTable :: Pat -> (DfaTable, SBinder, SBinder -> [Env], [Int])
> buildDfaTable p = 
>   let sig = sigmaRE (strip p)
>       init_dict = M.insert p 0 M.empty
>       (delta, mapping) = {-# SCC "buildDfaTable/builder" #-}  builder sig [] init_dict 0 [p] -- 0 is already used by p
>       delta' = delta
>       table = {-# SCC "buildDfaTable/table" #-} IM.fromList (map (\ (s,c,d,f,sb2env) -> (my_hash s c, (d,f,sb2env))) delta')
>       finals = [] -- final is not needed, see the Arg below  --  map snd (filter (\(p,i) -> posEpsilon $! strip p) ( M.toList mapping))
>   in (table, toSBinder p, sbinderToEnv p, finals)

testing 

> testp =
>    -- let (Right (pp,posixBnd)) = parsePatPosix "(.)*" 
>    -- let (Right (pp,posixBnd)) = parsePatPosix "(...?.?)*" 
>    -- let (Right (pp,posixBnd)) = parsePatPosix "^(((A|AB)(BAA|A))(AC|C))$" 
>    -- let (Right (pp,posixBnd)) = parsePatPosix "^((A)|(AB)|(B))*$" 
>    -- let (Right (pp,posixBnd)) = parsePatPosix "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$"-- "X(.?){1,8}Y"
>    let (Right (pp,posixBnd)) = parsePatPosix "^[XY]*X([XY]?){1,2}Y[XY]*$"
>    -- let (Right (pp,posixBnd)) = parsePatPosix "^.*X(.?){1,4}Y.*$"
>    -- let (Right (pp,posixBnd)) = parsePatPosix "X(.?){1,4}Y"         
>    in pp


> testp2 = 
>    let (Right (pp,posixBnd)) = parsePatPosix "X(.?){1,3}Y"
>        fb                    = followBy pp
>    in (pp,fb,posixBnd)

> testp3 = 
>    let sig = sigmaRE (strip testp)
>        init_dict = M.insert testp (0::Int) M.empty
>        (delta, mapping) = builder sig [] init_dict (0::Int) [testp]
>    in (delta,mapping)



let sig = sigmaRE (strip testp)
let init_dict = M.insert testp (0::Int) M.empty

let (delta, mapping) = builder sig [] init_dict (0::Int) [testp]



let (allStates, delta, mapping) = builder sig [] [] init_dict (0::Int) [testp]
mapM_ (\p -> putStrLn (show p)) (sort allStates)



> builder :: [Char] 
>         -> [ (Int,Char,Int,Int -> SBinder -> SBinder, SBinder -> [Env] ) ] 
>         -> M.Map Pat Int
>         -> Int
>         -> [Pat]
>         -> ([ (Int,Char,Int,Int -> SBinder -> SBinder, SBinder -> [Env] ) ], M.Map Pat Int)
> builder sig acc_delta dict max_id curr_pats 
>    | null curr_pats = (acc_delta, dict)
>    | otherwise = 
>       let 
>           new_delta      = {-# SCC "builder/new_delta" #-} [ p `seq` p' `seq` l `seq` f' `seq` g `seq` (p,l,p',f',g) | p <- curr_pats, 
>                                                              l <- sig, (p',f') <- {-# SCC "builder/dPat0" #-} dPat0 p l, let g = sbinderToEnv p'  ]
>           new_pats       = {-# SCC "builder/new_pats" #-} D.nub [ p' | (p,l,p',f',g) <- new_delta, not (p' `M.member` dict) ] -- todo D.nub might cause collision
>           (dict',max_id') = {-# SCC "builder/dict'" #-} foldl' (\(d,id) p -> (M.insert p (id+1) d, id + 1)) (dict,max_id) new_pats
>           acc_delta_next = {-# SCC "builder/acc_delta_next" #-} acc_delta ++ (map (\(p,l,p',f,g) -> (getId dict' p, l, getId dict' p', f, g)) new_delta)
>       in {- io `seq` -} builder sig  acc_delta_next dict' max_id' new_pats     
>  where getId :: M.Map Pat Int -> Pat -> Int 
>        getId m p = {-# SCC "getId/M.lookup" #-} case M.lookup p m of 
>                    { Just i -> i 
>                    ; Nothing -> error "getId failed: this should not happen" }

> type Word = S.ByteString

> execDfa :: Int -> DfaTable -> Word -> [(Int, SBinder, SBinder -> [Env])] -> [(Int,SBinder, SBinder -> [Env])] -- the list is always singleton, since it is a dfa
> execDfa cnt dfaTable w' [] = []
> execDfa cnt dfaTable w' currDfaStateSBinders = 
>      case {-# SCC "execDfa/uncons" #-} S.uncons w' of
>        Nothing -> currDfaStateSBinders
>        Just (l,w) -> 
>           let ((i,sb,_):_) = {-# SCC "execDfa/currDfaStateSBinders" #-} currDfaStateSBinders
>               k            = {-# SCC "execDfa/my_hash" #-} my_hash i l
>           in case {-# SCC "execDfa/IM.lookup" #-} IM.lookup k dfaTable of
>               { Nothing -> [] -- error (" k not found " ++ show i ++ " " ++  show l)
>               ; Just (j, f, sb2env) -> 
>                  let sb' = {-# SCC "execDfa/compute_sb" #-} {- cnt `seq` -} sb `seq` f cnt sb
>                      nextDfaStateSBinders =  {-# SCC "execDfa/nextDfaStateSBinders" #-} j `seq` sb' `seq` sb2env `seq` 
>                                              [(j, sb',sb2env)] 
>                      cnt' = {-# SCC "execDfa/cnt'" #-} cnt + 1
>                  in nextDfaStateSBinders `seq` cnt' `seq` w `seq` 
>                     execDfa cnt' dfaTable w nextDfaStateSBinders
>               }

x0 :: (x1 :: ( x2 :: (ab|a), x3 :: (baa|a)), x4 :: (ac|c))

> execPatMatch :: (DfaTable, SBinder, SBinder -> [Env], [Int], FollowBy, IM.IntMap ()) -> Word -> Maybe Env
> execPatMatch (dt, init_sbinder, init_sb2env, finals, _, posixBinder) w = 
>   let r = {-# SCC "execDfa" #-} dt `seq` execDfa 0 dt w [(0, init_sbinder, init_sb2env)]
>   in case r of 
>    { [] -> Nothing 
>    ; ((i,sb,sb2env):_) -> case (sb2env sb) of 
>          -- Arg: why not  i `elem` finals? 
>          -- No, we don't need to, because if the var pattern is not empty, the binding will not be extracted into the environment see sbinderToEnv
>                           { [] -> Nothing 
>                           ; (e:_) -> let e' = filter (\(x,_) -> x  `IM.notMember` posixBinder) e 
>                                      in Just e'
>                           } }


> p4 = PVar 0 [] (PPair (PVar 1 [] ((PPair p_x p_y))) p_z)
>    where p_x = PVar 2 [] (PE [(Choice [(L 'A'),(Seq (L 'A') (L 'B'))] Greedy)])      
>          p_y = PVar 3 [] (PE [(Choice [(Seq (L 'B') (Seq (L 'A') (L 'A'))), (L 'A')] Greedy)])
>          p_z = PVar 4 [] (PE [(Choice [(Seq (L 'A') (L 'C')), (L 'C')] Greedy)])


x0 :: ( x1 :: (  x2 :: (x3:: a | x4 :: ab) | x5 :: b)* )


> p3 = PVar 0 [] (PStar ( PVar 1 [] ( PChoice [(PVar 2 [] (PChoice [p3,p4] Greedy)), p5] Greedy)) Greedy)
>    where p3 = PVar 3 [] (PE [(L 'A')])
>          p4 = PVar 4 [] (PE [(Seq (L 'A') (L 'B'))])           
>          p5 = PVar 5 [] (PE [(L 'B')])

(X|Y)* X (X|Y)|() ((X|Y)|())|() Y (X|Y)*
(0:(1:(2:<(3:|[(['X','Y'])*]|),<|['X']|,<(-3:|[<([([(['X','Y']),<>])]),([([([(['X','Y']),<>])]),<>])>]|),<|['Y']|,(4:|[(['X','Y'])*]|)>>>>)))

> p0 = PVar 0 [] p1
>    where p1 = PVar 1 [] p2
>          p2 = PVar 2 [] (PPair p3 (PPair (PE [(L 'X')]) (PPair pn3 (PPair (PE [(L 'Y')]) p4))))
>          p3 = PVar 3 [] (PE [Star (Choice [L 'X', L 'Y'] Greedy) Greedy])
>          pn3 = PVar (-3) [] (PE [ Seq (Choice [(Choice [Choice [L 'X', L 'Y'] Greedy, Empty] Greedy)] Greedy) (Choice [(Choice [(Choice [Choice [L 'X', L 'Y'] Greedy, Empty] Greedy), Empty] Greedy)] Greedy)])
>          p4 = PVar 4 [] (PE [Star (Choice [L 'X', L 'Y'] Greedy) Greedy])

> -- | The Deriv backend spepcific 'Regex' type
> -- | the IntMap keeps track of the auxillary binder generated because of posix matching, i.e. all sub expressions need to be tag
> -- | the FollowBy keeps track of the order of the pattern binder 
> type Regex = (DfaTable, SBinder, SBinder -> [Env], [Int], FollowBy, IM.IntMap ())


-- todo: use the CompOption and ExecOption

> compile :: CompOption -- ^ Flags (summed together)
>         -> ExecOption -- ^ Flags (summed together) 
>         -> S.ByteString -- ^ The regular expression to compile
>         -> Either String Regex -- ^ Returns: the compiled regular expression
> compile compOpt execOpt bs =
>     case {-# SCC "compile/parsePatPosix" #-} parsePatPosix (S.unpack bs) of
>     Left err -> Left ("parseRegex for Text.Regex.Deriv.ByteString failed:"++show err)
>     Right (pat,posixBnd) -> 
>        Right (patToRegex pat posixBnd compOpt execOpt)


> patToRegex p posixBnd compOpt execOpt  =  
>     let (t, sb, toEnv, finals) = {-# SCC "patToRegex/compilePat" #-} compilePat p
>         fb = {-# SCC "patToRegex/followBy" #-} followBy p
>     in t `seq` sb `seq` toEnv `seq` finals `seq` fb `seq` posixBnd `seq` (t, sb, toEnv, finals, fb, posixBnd)



> execute :: Regex      -- ^ Compiled regular expression
>        -> S.ByteString -- ^ ByteString to match against
>        -> Either String (Maybe Env)
> execute r bs = Right (execPatMatch r bs)

> regexec :: Regex      -- ^ Compiled regular expression
>        -> S.ByteString -- ^ ByteString to match against
>        -> Either String (Maybe (S.ByteString, S.ByteString, S.ByteString, [S.ByteString]))
> regexec r bs =
>  case execPatMatch r bs of
>    Nothing -> Right Nothing
>    Just env ->
>      let pre = case lookup preBinder env of { Just e -> rg_collect_many bs e ; Nothing -> S.empty }
>          post = case lookup subBinder env of { Just e -> rg_collect_many bs e ; Nothing -> S.empty }
>          full_len = S.length bs
>          pre_len = S.length pre
>          post_len = S.length post
>          main_len = full_len - pre_len - post_len
>          main_and_post = S.drop pre_len bs
>          main = main_and_post `seq` main_len `seq` S.take main_len main_and_post
>          matched = map ((rg_collect_many bs) . snd) (filter (\(v,w) -> v > mainBinder && v < subBinder ) env)
>      in -- logger (print (show env)) `seq` 
>             Right (Just (pre,main,post,matched))

> rg_collect :: S.ByteString -> Range -> S.ByteString
> rg_collect w (Range i j) = S.take (j' - i' + 1) (S.drop i' w)
>	       where i' = fromIntegral i
>	             j' = fromIntegral j

> rg_collect_many w rs = foldl' S.append S.empty $ map (rg_collect w) rs


> -- | Control whether the pattern is multiline or case-sensitive like Text.Regex and whether to
> -- capture the subgroups (\1, \2, etc).  Controls enabling extra anchor syntax.
> data CompOption = CompOption {
>       caseSensitive :: Bool    -- ^ True in blankCompOpt and defaultCompOpt
>     , multiline :: Bool 
>   {- ^ False in blankCompOpt, True in defaultCompOpt. Compile for
>   newline-sensitive matching.  "By default, newline is a completely ordinary
>   character with no special meaning in either REs or strings.  With this flag,
>   inverted bracket expressions and . never match newline, a ^ anchor matches the
>   null string after any newline in the string in addition to its normal
>   function, and the $ anchor matches the null string before any newline in the
>   string in addition to its normal function." -}
>     , rightAssoc :: Bool       -- ^ True (and therefore Right associative) in blankCompOpt and defaultCompOpt
>     , newSyntax :: Bool        -- ^ False in blankCompOpt, True in defaultCompOpt. Add the extended non-POSIX syntax described in "Text.Regex.TDFA" haddock documentation.
>     , lastStarGreedy ::  Bool  -- ^ False by default.  This is POSIX correct but it takes space and is slower.
>                                -- Setting this to true will improve performance, and should be done
>                                -- if you plan to set the captureGroups execoption to False.
>     } deriving (Read,Show)

> data ExecOption = ExecOption  {
>   captureGroups :: Bool    -- ^ True by default.  Set to False to improve speed (and space).
>   } deriving (Read,Show)

> instance RegexOptions Regex CompOption ExecOption where
>     blankCompOpt =  CompOption { caseSensitive = True
>                                , multiline = False
>                                , rightAssoc = True
>                                , newSyntax = False
>                                , lastStarGreedy = False
>                                  }
>     blankExecOpt =  ExecOption { captureGroups = True }
>     defaultCompOpt = CompOption { caseSensitive = True
>                                 , multiline = True
>                                 , rightAssoc = True
>                                 , newSyntax = True
>                                 , lastStarGreedy = False
>                                   }
>     defaultExecOpt =  ExecOption { captureGroups = True }
>     setExecOpts e r = undefined
>     getExecOpts r = undefined 



> instance RegexLike Regex S.ByteString where 
> -- matchAll :: regex -> source -> [MatchArray]
>    matchAll = execPatMatchArray
> -- matchOnce :: regex -> source -> Maybe MatchArray
>    matchOnce = posixPatMatchArray
> -- matchCount :: regex -> source -> Int
> -- matchTest :: regex -> source -> Bool
> -- matchAllText :: regex -> source -> [MatchText source]
> -- matchOnceText :: regex -> source -> Maybe (source, MatchText source, source)
>     

> instance RegexLike Regex String where 
> -- matchAll :: regex -> source -> [MatchArray]
>    matchAll r s = execPatMatchArray r (S.pack s)
> -- matchOnce :: regex -> source -> Maybe MatchArray
>    matchOnce r s = posixPatMatchArray r (S.pack s)
> -- matchCount :: regex -> source -> Int
> -- matchTest :: regex -> source -> Bool
> -- matchAllText :: regex -> source -> [MatchText source]
> -- matchOnceText :: regex -> source -> Maybe (source, MatchText source, source)




> execPatMatchArray ::  (DfaTable, SBinder, SBinder -> [Env], [Int], FollowBy, IM.IntMap ()) -> Word -> [MatchArray]
> execPatMatchArray (dt, init_sbinder, init_sb2env, finals, fb, posixBinder)  w =
>   let r = execDfa 0 dt w [(0, init_sbinder, init_sb2env)]
>   in case r of 
>    { [] -> [] 
>    ; ((i,sb,sb2env):_) -> map (\ env -> sbinderToMatchArray (S.length w) fb posixBinder (IM.fromList env)) (sb2env sb) 
>    }

> updateEmptyBinder b fb = 
>     let 
>         up b (x,y) = case IM.lookup x b of 
>                      { Just (_:_) -> -- non-empty, nothing to do
>                        b
>                      ; Just [] ->  -- lookup the predecessor
>                        case IM.lookup y b of
>                        { Just r@(_:_) -> let i = snd (last r)
>                                          in IM.update (\_ -> Just [(i,i)]) x b
>                        ; _ -> b }
>                      ; Nothing -> b }
>     in foldl' up b fb

> sbinderToMatchArray l fb posixBnd b  = 
>     let -- b'        = updateEmptyBinder b fb
>         subPatB   = filter (\(x,_) -> x > mainBinder && x < subBinder && x `IM.notMember` posixBnd ) (listifyBinder b)
>         mbPrefixB = IM.lookup preBinder b
>         mbSubfixB = IM.lookup subBinder b
>         mainB     = case (mbPrefixB, mbSubfixB) of
>                       (Just [(Range _ x)], Just [(Range y _)]) -> (x, y - x)
>                       (Just [(Range _ x)], _)            -> (x, l - x)
>                       (_, Just [(Range y _)])            -> (0, y) 
>                       (_, _)                       -> (0, l)
>                       _                            -> error (show (mbPrefixB, mbSubfixB) ) 
>         rs        = map snd subPatB      
>         rs'       = map lastNonEmpty rs
>         io = logger (print $ "\n" ++ show rs ++ " || " ++ show rs' ++ "\n")
>     in -- io `seq` 
>        listToArray (mainB:rs')
>     where fromRange (Range b e) = (b, e-b+1) 
>           -- chris' test cases requires us to get the last result even if it is a reset point,
>           -- e.g. input:"aaa"	 pattern:"((..)|(.))*" expected match:"(0,3)(2,3)(-1,-1)(2,3)" note that (..) matches with [(0,2),(2,2)], we return [(2,2)]
>           lastNonEmpty [] = (-1,0)
>           lastNonEmpty rs = fromRange (last rs)

> listToArray l = listArray (0,length l-1) l

> posixPatMatchArray :: (DfaTable, SBinder, SBinder -> [Env], [Int], FollowBy, IM.IntMap ()) -> Word -> Maybe MatchArray
> posixPatMatchArray compiled w =
>      first (execPatMatchArray compiled w)
>   where
>     first (env:_) = return env
>     first _ = Nothing


> -- | from FollowBy, we recover the right result of the variable that bound (-1,-1) to fit Chris' test case
> 

> type FollowBy = [(Int,Int)]

> followBy :: Pat -> FollowBy
> followBy p = map (\p -> (snd p, fst p)) (fst $ buildFollowBy p ([],[]))

> -- | describe the "followedBy" relation between two pattern variable
> buildFollowBy :: Pat -> ([(Int,Int)], [Int]) -> ([(Int,Int)], [Int])
> buildFollowBy (PVar x w p) (acc, lefts) = let (acc', lefts') = buildFollowBy p (acc,lefts)
>                                           in ([ (l,x) | l <- lefts] ++ acc', [x])
> buildFollowBy (PE r) x                  = x
> buildFollowBy (PStar p g) (acc, lefts)  = buildFollowBy p (acc,lefts)
> buildFollowBy (PPair p1 p2) (acc, lefts) = let (acc',lefts') = buildFollowBy p1 (acc,lefts)
>                                            in buildFollowBy p2 (acc',lefts')
> buildFollowBy (PChoice ps _) (acc, lefts) = 
>   foldl' (\(acc', lefts') p -> let (acc'', lefts'') = buildFollowBy p (acc',lefts) -- all choice share the same lefts comming from the parent
>                               in (acc'', lefts' ++ lefts'')) (acc, []) ps
