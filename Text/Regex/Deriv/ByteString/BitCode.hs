{- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2013, BSD License -}

{- A bytestring implementation of reg exp pattern matching using partial derivative / derivative
The POSIX matching policy is implemented by following the 'structure' of the reg-exp.
The pattern is follow annotated. 
We do not break part the sub-pattern of the original reg, they are always grouped under the same var pattern.
-}

{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
    BangPatterns, 
    FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 

module Text.Regex.Deriv.ByteString.BitCode
       {-
       ( Regex
       , CompOption(..)
       , ExecOption(..)
       , defaultCompOpt
       , defaultExecOpt
       , compile
       , execute
       , regexec
       ) -} where 


import System.IO.Unsafe
import Data.IORef
import qualified Data.HashTable.IO as H
import qualified Data.Hashable as Ha


import Data.List 
import Data.Char (ord)
import GHC.Int
import GHC.Arr 
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M

import Control.Monad

import Text.Regex.Base(RegexOptions(..),RegexLike(..),MatchArray)


import Text.Regex.Deriv.RE 
import Text.Regex.Deriv.Common (IsPhi(..), IsEpsilon(..))
import Text.Regex.Deriv.Pretty (Pretty(..))
import Text.Regex.Deriv.Common (Range(..), Letter, PosEpsilon(..), my_hash, my_lookup, GFlag(..), IsGreedy(..), preBinder, subBinder, mainBinder)
-- import Text.Regex.Deriv.IntPattern (Pat(..), toBinder, Binder(..), strip, listifyBinder, Key(..))
import Text.Regex.Deriv.IntPattern (Pat(..), toBinder, Binder(..), strip, listifyBinder)
import Text.Regex.Deriv.Parse
import qualified Text.Regex.Deriv.Dictionary as D (Dictionary(..), Key(..), insertNotOverwrite, lookupAll, empty, isIn, nub, member, lookup, insert) 

logger io = unsafePerformIO io

type Path = [Int] 

emptyP = []

appP p1 p2 = p1 ++ p2



data U where
  Nil :: U
  EmptyU :: U
  Letter :: Char -> U
  LeftU :: U -> U
  RightU :: U -> U
  Pair :: (U,U) -> U
  List :: [U] -> U
  deriving Show


data SPath = SChoice Path [SPath]                 
           | SPair Path SPath SPath 
           | SStar Path SPath -- no need to store any SPath, but 
           | SL Path
           | SEps Path 
           deriving Show



mkEmpty :: RE -> SPath -> Path 
mkEmpty Empty = (\(SEps p) -> p)
mkEmpty (Choice [r1,r2] _ ) 
  | nullable r1 = let f = mkEmpty r1   
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (0 : f sp1))
  | nullable r2 = let f = mkEmpty r2
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (1 : f sp2))
mkEmpty (ChoiceInt [r1,r2]) 
  | nullable r1 = let f = mkEmpty r1
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (f sp1))
  | nullable r2 = let f = mkEmpty r2
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (f sp2))
mkEmpty (Seq r1 r2) = 
  let f1 = mkEmpty r1 
      f2 = mkEmpty r2
  in (\ (SPair p sp1 sp2) -> p ++ (f1 sp1) ++ (f2 sp2))
mkEmpty (Star r _) = (\(SStar p _) -> p ++ [1] )


prefix :: Path -> SPath -> SPath
prefix b (SEps p) = SEps (b `appP` p)
prefix b (SL p)   = SL (b `appP` p)
prefix b (SChoice p [sp1,sp2]) = SChoice (b `appP` p) [sp1, sp2]
prefix b (SPair p sp1 sp2) = SPair (b `appP` p) sp1 sp2
prefix b (SStar p sp) = SStar (b `appP` p) sp

nullable = posEpsilon
  
deriv :: RE -> Char -> (RE, SPath -> SPath)
deriv Phi l = (Phi, \_ -> error "phi should not possess any value")
deriv Empty l = (Phi, \_ -> error "")
deriv (L l') l
  | l == l'  = (Empty, \p -> p) 
  | otherwise = (Phi, \_ -> error "")
deriv (Choice [r1,r2] gf) l = let (r1',f1) = deriv r1 l
                                  (r2',f2) = deriv r2 l
                              in (Choice [r1',r2'] gf, \(SChoice p [sp1,sp2]) -> SChoice p [(f1 sp1),(f2 sp2)])
deriv (ChoiceInt [r1,r2]) l = let (r1',f1) = deriv r1 l
                                  (r2',f2) = deriv r2 l
                              in (ChoiceInt [r1', r2'], \(SChoice p [sp1,sp2]) -> SChoice p [(f1 sp1),(f2 sp2)])
deriv (Seq r1 r2) l = let (r1', f1) = deriv r1 l
                          (r2', f2) = deriv r2 l
                          f3 = mkEmpty r1 
                      in if nullable r1
                         then (ChoiceInt [(Seq r1' r2), r2'], (\(SPair p sp1 sp2) -> SChoice emptyP [(SPair p (f1 sp1) sp2), (prefix (p `appP` (f3 sp1)) (f2 sp2))]))
                         else (Seq r1' r2, (\(SPair p sp1 sp2) -> SPair p (f1 sp1) sp2))
deriv (Star r gf) l = let (r', f) = deriv r l 
                   in (Seq r' (Star r gf), \(SStar p sp) -> SPair (p `appP` [0]) (f sp) (SStar emptyP sp)) -- todo check
                      
                      
                      
simp :: RE -> (RE, SPath -> SPath)                      
simp (Seq Empty r)
  | isPhi r = (Phi, \sp -> error "")
  | otherwise = (r, \(SPair p1 (SEps p2) sp2) -> prefix (p1 `appP` p2) sp2)
simp (Seq r1 r2)                 
  | isPhi r1 || isPhi r2 = (Phi, \sp -> error "")
  | otherwise = let (r1', f1) = simp r1 
                    (r2', f2) = simp r2
                in (Seq r1' r2', \(SPair p sp1 sp2) -> SPair p (f1 sp1) (f2 sp2))
simp (Choice [(Choice [r1,r2] gf2), r3] gf1) =  -- (r1+r2)+r3 => (r1+(  push all the path info to the options under the choice
  (ChoiceInt [r1, ChoiceInt [r2,r3]], \(SChoice p1 [SChoice p2 [sp1, sp2], sp3]) ->
    SChoice emptyP [prefix (p1 `appP` p2 `appP` [0,0]) sp1, SChoice emptyP [prefix (p1 `appP` p2 `appP` [0,1]) sp2, prefix (p1 `appP` [1]) sp3]])
simp (Choice [r1,r2] gf) 
  | r1 == r2 = (r1, \(SChoice p [sp1,sp2]) -> prefix (p `appP` [0]) sp1)
  | isPhi r1 = (r2, \(SChoice p [sp1,sp2]) -> prefix (p `appP` [1]) sp2)
  | isPhi r2 = (r1, \(SChoice p [sp1,sp2]) -> prefix (p `appP` [0]) sp1)
  | otherwise = let (r1',f1) = simp r1
                    (r2',f2) = simp r2
                in (ChoiceInt [r1',r2'], \(SChoice p [sp1,sp2]) -> 
                     SChoice p [f1 sp1, f2 sp2])
simp (ChoiceInt [ChoiceInt [r1,r2], r3]) =    
  (ChoiceInt [r1, ChoiceInt [r2,r3]], \(SChoice p1 [SChoice p2 [sp1, sp2], sp3]) ->
    SChoice emptyP [prefix (p1 `appP` p2) sp1, SChoice emptyP [prefix (p1 `appP` p2) sp2, prefix p1 sp3]])
simp (ChoiceInt [r1,r2]) 
  | r1 == r2 = (r1, \(SChoice p [sp1,sp2]) -> prefix p sp1)
  | isPhi r1 = (r2, \(SChoice p [sp1,sp2]) -> prefix p sp2)
  | isPhi r2 = (r1, \(SChoice p [sp1,sp2]) -> prefix p sp1)
  | otherwise = let (r1',f1) = simp r1
                    (r2',f2) = simp r2
                in (ChoiceInt [r1',r2'], \(SChoice p [sp1,sp2]) -> 
                     SChoice p [f1 sp1, f2 sp2])
simp (Star Empty gf) = (Empty, \(SStar p1 (SEps p2)) -> SEps (p1 `appP` p2 `appP` [1])) -- how likely this will be applied when p1 and p2 are non-empty paths?
-- r** ==> r*
-- tricky, because r* path p, would need to be embedded into
--  0 p 1
-- That is, the outer Kleene star performs a single iteration
-- I'm quite sure we can do without this optimization.
-- simp (Star (Star r gf2) gf1) = (Star r gf1, \(SStar p1 (SStar p2 sp)) -> SStar (p1 `appP` p2) sp)
simp (Star r gf) | isPhi r = (Empty, \(SStar p sp) -> SEps (p `appP` [1]))
                 | otherwise = let (r', f) = simp r
                               in (Star r' gf, \(SStar p sp) -> SStar p (f sp))
                               
simp r = (r, \sp -> sp)                               
                               
         
simpFix :: RE -> (RE, SPath -> SPath)          
simpFix r = let (r', f) = simp r           
            in if r == r' 
               then (r, \sp -> sp)
               else let (r'', f') = simpFix r'
                    in (r'', f' . f)
                   
                   
                      
builder :: [Char] 
           -> [ (Int,Char,Int,SPath -> SPath) ]
           -> M.Map RE Int
           -> Int
           -> [RE]
           -> ([ (Int,Char,Int, SPath -> SPath) ], M.Map RE Int)
builder sig acc_delta dict max_id curr_res           
  | null curr_res = (acc_delta, dict)
  | otherwise     = 
    let new_delta = [ (r,l,r',f) | r <- curr_res, l <- sig, let (r',f) = deriv r l ] 
        new_res   = nub [ r' | (r,l,r',f) <- new_delta, not (r' `M.member` dict) ]
        (dict', max_id') = foldl' (\(d,id) r -> (M.insert r (id+1) d, id+1)) (dict, max_id) new_res
        acc_delta_next = acc_delta ++ (map (\(r,l,r',f) -> (getId dict' r, l, getId dict' r', f)) new_delta)
    in builder sig acc_delta_next dict' max_id' new_res
      where getId :: M.Map RE Int -> RE -> Int
            getId m r = case M.lookup r m of
              { Just i -> i
              ; Nothing -> error "getId failed: this should not happen" }
                        
-- todo: move to Common.lhs                        
instance Ha.Hashable GFlag where 
  hashWithSalt salt Greedy = Ha.hashWithSalt salt (19::Int)
  hashWithSalt salt NotGreedy = Ha.hashWithSalt salt (23::Int)

instance Ha.Hashable RE where
  hashWithSalt salt Empty = Ha.hashWithSalt salt (29::Int)
  hashWithSalt salt (L x) = Ha.hashWithSalt salt (Ha.hashWithSalt 31 (Ha.hash x))
  hashWithSalt salt (Choice rs g) = Ha.hashWithSalt salt (Ha.hashWithSalt 37 (Ha.hashWithSalt (Ha.hash g) rs))
  hashWithSalt salt (Seq r1 r2) = Ha.hashWithSalt salt (Ha.hashWithSalt 41 (Ha.hashWithSalt (Ha.hash r1) r2))
  hashWithSalt salt (Star r g) = Ha.hashWithSalt salt (Ha.hashWithSalt 43 (Ha.hashWithSalt (Ha.hash g) r))
  hashWithSalt salt Any = Ha.hashWithSalt salt (47 ::Int)
  hashWithSalt salt (Not cs) = Ha.hashWithSalt salt (Ha.hashWithSalt 53 cs)
  hashWithSalt salt (ChoiceInt rs) = Ha.hashWithSalt salt (Ha.hashWithSalt 59 rs)

                        
type DfaTable = IM.IntMap (Int, SPath -> SPath) 

buildDfaTable :: RE -> DfaTable
buildDfaTable r = 
  let sig = sigmaRE r
      init_dict = M.insert r 0 M.empty
      (delta, mapping) = builder sig [] init_dict 0 [r]
      table = IM.fromList (map (\(s,c,d,f) -> (my_hash s c, (d,f))) delta)
  in table
     
type Word = S.ByteString
     
execDfa :: DfaTable -> Word -> [(Int, SPath)] -> [(Int, SPath)] -- list is either singleton or null, since it is a DFA
execDfa dfaTable w' [] = []
execDfa dfaTable w' currStateSPaths = 
  case S.uncons w' of
    Nothing    -> currStateSPaths 
    Just (l,w) -> 
      let ((i,sp):_) = currStateSPaths
          k               = my_hash i l
      in case IM.lookup k dfaTable of 
        { Nothing -> [] 
        ; Just (j, f) ->
             let sp' = sp `seq` f sp
                 nextStateSPaths = j `seq` sp' `seq` [(j,sp')]
             in nextStateSPaths `seq` w `seq`
                execDfa dfaTable w nextStateSPaths
        }
         



retrieveEmpty :: RE -> SPath -> Path
retrieveEmpty Empty (SEps p) = p
retrieveEmpty (Choice [r1,r2] gf) (SChoice p [sp1, sp2]) 
  | nullable r1 = p ++ (0 : retrieveEmpty r1 sp1)
  | nullable r2 = p ++ (1 : retrieveEmpty r2 sp2)
retrieveEmpty (ChoiceInt [r1,r2]) (SChoice p [sp1, sp2]) 
  | nullable r1 = p ++ (retrieveEmpty r1 sp1)
  | nullable r2 = p ++ (retrieveEmpty r2 sp2)
retrieveEmpty (Seq r1 r2) (SPair p sp1 sp2)  = p `appP` (retrieveEmpty r1 sp1) `appP` (retrieveEmpty r2 sp2)
retrieveEmpty (Star r gf) (SStar p sp) = p `appP` [1] 


sDecode :: RE -> SPath -> U
sDecode r sp = let p = retrieveEmpty r sp 
               in decode r p
         
decode2 :: RE -> [Int] -> (U, [Int])
decode2 Phi bs = (Nil,bs)
decode2 Empty bs = (EmptyU,bs)
decode2 (L l) bs = (Letter l, bs)
decode2 (Choice [r1,r2] gf) (0:bs) = let (u,p) = decode2 r1 bs
                                     in (LeftU u, p)
decode2 (Choice [r1,r2] gf) (1:bs) = let (u,p) = decode2 r2 bs
                                     in (RightU u, p)
decode2 (Seq r1 r2) bs = let (u1,p1) = decode2 r1 bs
                             (u2,p2) = decode2 r2 p1
                         in (Pair (u1,u2), p2)
decode2 (sr@(Star r gf)) (0:bs) = let (u,p1) = decode2 r bs
                                      (List us,p2) = decode2 sr p1
                                  in (List (u:us), p2)
decode2 Star{} (1:bs) = (List [],bs)

decode :: RE -> [Int] -> U
decode r bs = let (u,p) = decode2 r bs
              in case p of
                   [] -> u
                   _  -> error "invalid bit coding"

-- assume strip p = r
extract :: Pat -> RE -> U -> [(Int,Word)]
extract (PVar i _ p) r u
  | strip p == r = [(i, flatten u)]
  | otherwise    = error "the structures of the pattern and regex are not in sync"
extract (PE rs) (Choice rs' _) u = [] -- not in used
extract (PStar p _) (Star r _) (List []) = []
extract (PStar p _) (Star r _) (List [u]) = extract p r u 
extract p'@(PStar p _) r'@(Star r _) (List (u:us)) = extract p' r' (List us) -- we only extract the last binding
extract (PPair p1 p2) (Seq r1 r2) (Pair (u1,u2)) = extract p1 r1 u1 ++ extract p2 r2 u2
extract (PChoice [p1,p2] _) (Choice [r1,r2] _) (LeftU u)  = extract p1 r1 u
extract (PChoice [p1,p2] _) (Choice [r1,r2] _) (RightU u) = extract p2 r2 u
extract (PEmpty p) Empty _ = [] -- not in used


extractSR :: Pat -> RE -> U -> Int -> ([(Int, Range)], Int)
extractSR (PVar i _ p) r u start_index 
  | strip p == r = let l = S.length $ flatten u 
                   in ([(i, Range start_index l)] , start_index + l)
  | otherwise    = error "the structures of the pattern and regex are not in sync"
extractSR (PE rs) (Choice rs' _) u start_index = ([],start_index) -- not in used
extractSR (PStar p _) (Star r _) (List []) start_index = ([], start_index)
extractSR (PStar p _) (Star r _) (List [u]) start_index = extractSR p r u start_index
extractSR p'@(PStar p _) r'@(Star r _) (List (u:us)) start_index = extractSR p' r' (List us) start_index -- we only extract the last binding
extractSR (PPair p1 p2) (Seq r1 r2) (Pair (u1,u2)) start_index =  
  let (l1, i1) = extractSR p1 r1 u1 start_index 
      (l2, i2) = extractSR p2 r2 u2 i2
  in (l1 ++ l2, i2)
extractSR (PChoice [p1,p2] _) (Choice [r1,r2] _) (LeftU u) start_index = extractSR p1 r1 u start_index
extractSR (PChoice [p1,p2] _) (Choice [r1,r2] _) (RightU u) start_index = extractSR p2 r2 u start_index
extractSR (PEmpty p) Empty _ start_index = ([],start_index) -- not in used



  


flatten :: U -> Word
flatten u = S.pack (flatten' u)

flatten' :: U -> [Char]
flatten' Nil = []
flatten' EmptyU = []
flatten' (Letter c) = [c]
flatten' (LeftU u) = flatten' u
flatten' (RightU u) = flatten' u
flatten' (Pair (u1,u2)) = flatten' u1 ++ flatten' u2
flatten' (List us) = concatMap flatten' us


compilePat :: Pat -> (DfaTable, Pat)
compilePat p = 
  let r = strip p 
      dfa = buildDfaTable r
  in (dfa, p)
     
{-
execPatMatch :: (DfaTable, Pat) -> Word -> Maybe Env
execPatMatch (dfa, p) w = 
  let 
-}