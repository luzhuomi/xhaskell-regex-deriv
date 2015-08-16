{- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2013, BSD License -}

{- A bytestring implementation of reg exp pattern matching using partial derivative / derivative
The POSIX matching policy is implemented by following the 'structure' of the reg-exp.
The pattern is follow annotated. 
We do not break part the sub-pattern of the original reg, they are always grouped under the same var pattern.

-- '.' the any pattern is not supported due to the decoding of the bit-encoding does not store the literals, the literals need to be retrieved from the regex
-}

{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
    BangPatterns, 
    FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 

module Text.Regex.Deriv.ByteString.BitCode
       ( Regex
       , CompOption(..)
       , ExecOption(..)
       , defaultCompOpt
       , defaultExecOpt
       , compile
       , execute
       , regexec
       ) where 


import Prelude hiding (Word)
import System.IO.Unsafe
import Data.IORef
import qualified Data.HashTable.IO as H
import qualified Data.Hashable as Ha


import qualified Data.Dequeue as DQ
import Data.List 
import Data.Maybe
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

{-
type Path = [Int] 
emptyP = []
appP p1 p2 =  ((++) $!p1) $! p2
zero = 0
one = 1
zeroP = [0]
oneP = [1]
zeroZeroP = [0,0]
zeroOneP = [0,1]
consP = (:)
headP (x:xs) = x
unconsP (x:xs) = Just (x,xs)
unconsP []     = Nothing

nullP [] = True
nullP (_:_) = False
-}

type Path = DQ.BankersDequeue Int

-- decompose the left path and cons them into the right
rightApp :: Path -> Path -> Path 
rightApp p1 p2 = case DQ.popBack p1 of
  { (Just x, p1') -> rightApp p1' (DQ.pushFront p2 x)
  ; (Nothing, _ ) -> p2
  }

-- decompose the right path and snoc them into the left
leftApp :: Path -> Path -> Path
leftApp p1 p2 = case DQ.popFront p2 of
  { (Just x, p2') -> leftApp (DQ.pushBack p1 x) p2'
  ; (Nothing, _ ) -> p1
  }
                 
smartApp :: Path -> Path -> Path
smartApp p1 p2 | DQ.length p1 > DQ.length p2 = leftApp p1 p2
               | otherwise                   = rightApp p1 p2

singleton :: Int -> Path
singleton i = DQ.pushFront DQ.empty i


emptyP = DQ.empty
appP p1 p2 = smartApp p1 p2
zero = 0
one = 1
zeroP = singleton zero
oneP = singleton one
zeroZeroP = DQ.fromList [zero,zero]
zeroOneP = DQ.fromList [zero,one]
consP = flip DQ.pushFront 
headP p = case DQ.first p of { Just x -> x; _ -> error "headP is applied to an empty path." }
unconsP p = case DQ.popFront p of
  { (Nothing, _ ) -> Nothing
  ; (Just x , p') -> Just (x, p')
  }
nullP = DQ.null


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
           | SPhi
           deriving Show
                    

-- build empty SPath from a RE
mkSPath :: RE -> SPath 
mkSPath Empty = SEps emptyP
mkSPath (L c) = SL emptyP
mkSPath Any = SL emptyP
mkSPath (Choice [r1,r2] _) = SChoice emptyP [mkSPath r1, mkSPath r2]
mkSPath (Choice [r] _) = SChoice emptyP [mkSPath r]
mkSPath (Seq r1 r2) = SPair emptyP (mkSPath r1) (mkSPath r2)
mkSPath (Star r _)  = SStar emptyP (mkSPath r)
mkSPath Phi = SPhi
mkSPath r = error ("mkSPath fail" ++ show r)

mkEmpty :: RE -> SPath -> Path 
mkEmpty Empty = (\x -> case x of { (SEps p) -> p })
mkEmpty (Choice [r1,r2] _ ) 
  | nullable r1 = let f = mkEmpty r1   
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (consP zero (f sp1)))
  | nullable r2 = let f = mkEmpty r2
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (consP one (f sp2)))
mkEmpty (Choice [r] _ ) 
  | nullable r = let f = mkEmpty r   
                  in (\(SChoice p [sp]) -> p `appP` (f sp))
mkEmpty (ChoiceInt [r1,r2]) 
  | nullable r1 = let f = mkEmpty r1
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (f sp1))
  | nullable r2 = let f = mkEmpty r2
                  in (\(SChoice p [sp1,sp2]) -> p `appP` (f sp2))

mkEmpty (Seq r1 r2) = 
  let f1 = mkEmpty r1 
      f2 = mkEmpty r2
  in (\ (SPair p sp1 sp2) -> p `appP` (f1 sp1) `appP` (f2 sp2))
mkEmpty (Star r _) = (\x -> case x of (SStar p _) -> p `appP` oneP 
                                      _ -> error ("mkEmptyStar is applied to " ++ show x)
                     )


prefix :: Path -> SPath -> SPath
prefix b (SEps p) = SEps $! (b `appP` p)
prefix b (SL p)   = SL $! (b `appP` p)
prefix b (SChoice p [sp1,sp2]) = 
  let !bp = b `appP` p
  in SChoice bp  $! [sp1, sp2]
prefix b (SChoice p [sp]) = let !bp = b `appP` p in SChoice bp $! [sp]
prefix b (SPair p sp1 sp2) = let !bp = b `appP` p in SPair bp sp1  sp2
prefix b (SStar p sp) = let !bp = b `appP` p in SStar bp sp
prefix b SPhi = SPhi
prefix b p = error $ "prefix error: b=" ++ (show b)  ++ " p=" ++ show p

nullable = posEpsilon
  
deriv :: RE -> Char -> (RE, SPath -> SPath)
deriv Phi l = (Phi, \_ -> SPhi)
deriv Empty l = (Phi, \_ -> SPhi)
deriv (L l') l
  | l == l'  = (Empty, \(SL p) -> (SEps p))
  | otherwise = (Phi, \_ -> SPhi)
deriv (Choice [r1,r2] gf) l = let (r1',!f1) = deriv r1 l
                                  (r2',!f2) = deriv r2 l
                              in r1' `seq` r2' `seq` (Choice [r1',r2'] gf, \(SChoice !p [!sp1,!sp2]) -> 
                                                       let !sp1' = f1 sp1 
                                                           !sp2' = f2 sp2
                                                       in SChoice p $! [sp1',sp2'])
deriv (Choice [r] gf) l = let (r',!f) = deriv r l                                  
                          in r' `seq` (Choice [r'] gf, \(SChoice !p [!sp]) ->  
                                        let !sp' = f sp 
                                        in SChoice p [sp'])
deriv (ChoiceInt [r1,r2]) l = let (r1',!f1) = deriv r1 l
                                  (r2',!f2) = deriv r2 l
                              in r1' `seq` r2' `seq` (ChoiceInt [r1', r2'], \(SChoice !p [!sp1,!sp2]) -> 
                                                       let !sp1' = f1 sp1
                                                           !sp2' = f2 sp2 
                                                       in SChoice p $! [sp1', sp2'])
deriv (Seq r1 r2) l = let (r1', !f1) = deriv r1 l
                          (r2', !f2) = deriv r2 l
                          f3 = mkEmpty r1 
                      in r1' `seq` r2' `seq` if nullable r1
                         then (ChoiceInt [(Seq r1' r2), r2'], (\(SPair !p !sp1 !sp2) -> 
                                                                let !sp1' = f1 sp1
                                                                    !sp2' = f2 sp2
                                                                    !p1'' = f3 sp1
                                                                    !p'   = (p `appP` p1'') 
                                                                    !sp' = prefix p' sp2'
                                                                    !spp12 = (SPair p sp1' sp2)
                                                                in SChoice emptyP $! [spp12, sp' ]))
                         else (Seq r1' r2, (\(SPair !p !sp1 !sp2) -> 
                                             let !sp1' = f1 sp1
                                             in SPair p sp1' sp2))
deriv (Star r gf) l = let (r', f) = deriv r l 
                   in r' `seq` (Seq r' (Star r gf), \(SStar !p !sp) -> 
                                 let !sp' = f sp
                                     !p' = (p `appP` zeroP)
                                  in SPair p' sp' $! (SStar emptyP sp)) -- todo check
deriv Any _ = (Empty, \(SL p) -> (SEps p))                   
deriv r l = error ("deriv failed: " ++ (show r) ++ "/" ++ (show l))
                      
                      
simp :: RE -> (RE, SPath -> SPath)                      
simp (Seq Empty r)
  | isPhi r = (Phi, \sp ->SPhi)
  | otherwise = (r, \(SPair !p1 (SEps !p2) !sp2) -> 
                  let !p12 = p1 `appP` p2
                  in prefix p12 sp2)
simp (Seq r1 r2)                 
  | isPhi r1 || isPhi r2 = (Phi, \sp -> SPhi)
  | otherwise = let (r1', !f1) = simp r1 
                    (r2', !f2) = simp r2
                in r1' `seq` r2' `seq` (Seq r1' r2', \(SPair !p !sp1 !sp2) -> 
                                         let !sp1' = f1 sp1
                                             !sp2' = f2 sp2 
                                         in SPair p sp1' sp2')
simp (Choice [(Choice [r1,r2] gf2), r3] gf1) =  -- (r1+r2)+r3 => (r1+(  push all the path info to the options under the choice
  (ChoiceInt [r1, ChoiceInt [r2,r3]], \(SChoice !p1 [SChoice !p2 [!sp1, !sp2], !sp3]) ->
    let !p' = p1 `appP` p2 `appP` zeroZeroP
        !sp1' =  prefix p' sp1
        !p'' = p1 `appP` p2 `appP` zeroOneP
        !sp2' = prefix p'' sp2
        !p''' = p1 `appP` oneP
        !sp3' = prefix p''' sp3
    in SChoice emptyP [ sp1', SChoice emptyP [ sp2', sp3']])
simp (Choice [r1,r2] gf) 
  | r1 == r2 = (r1, \(SChoice !p [!sp1,!sp2]) -> let !p' = (p `appP` zeroP) in prefix p' sp1)
  | isPhi r1 = (r2, \(SChoice !p [!sp1,!sp2]) -> let !p' = (p `appP` oneP) in prefix p' sp2)
  | isPhi r2 = (r1, \(SChoice !p [!sp1,!sp2]) -> let !p' = (p `appP` zeroP) in prefix p' sp1)
  | isJust (simpChoice r1 [] r2) = fromJust (simpChoice r1 [] r2)
  | otherwise = let (r1',!f1) = simp r1
                    (r2',!f2) = simp r2
                in r1' `seq` r2' `seq` (Choice [r1',r2'] gf, \(SChoice !p [!sp1,!sp2]) -> 
                                         let !sp1' = f1 sp1
                                             !sp2' = f2 sp2 
                                         in SChoice p [sp1', sp2'])
simp (ChoiceInt [ChoiceInt [r1,r2], r3]) =    
  (ChoiceInt [r1, ChoiceInt [r2,r3]], \(SChoice !p1 [SChoice !p2 [!sp1, !sp2], !sp3]) ->
    let 
      !p12 = (p1 `appP` p2)
      !sp1' = prefix p12 sp1  
      !sp2' = prefix p12 sp2
      !sp3' = prefix p1 sp3
    in SChoice emptyP [sp1', SChoice emptyP [sp2', sp3']])
simp (ChoiceInt [r1,r2]) 
  | r1 == r2 = (r1, \(SChoice !p [!sp1,!sp2]) -> prefix p sp1)
  | isPhi r1 = (r2, \(SChoice !p [!sp1,!sp2]) -> prefix p sp2)
  | isPhi r2 = (r1, \(SChoice !p [!sp1,!sp2]) -> prefix p sp1)
  | isJust (simpChoice r1 [] r2) = fromJust (simpChoice r1 [] r2)
  | otherwise = let (r1',!f1) = simp r1
                    (r2',!f2) = simp r2
                in r1' `seq` r2' `seq` (ChoiceInt [r1',r2'], \(SChoice !p [!sp1,!sp2]) -> 
                                         let !sp1' = f1 sp1
                                             !sp2' = f2 sp2
                                         in SChoice p [sp1', sp2'])
simp (Star Empty gf) = (Empty, \(SStar !p1 (SEps !p2)) -> 
                         let !p =  (p1 `appP` p2 `appP` oneP)
                         in SEps p) -- how likely this will be applied when p1 and p2 are non-empty paths?
-- r** ==> r*
-- tricky, because r* path p, would need to be embedded into
--  0 p 1
-- That is, the outer Kleene star performs a single iteration
-- I'm quite sure we can do without this optimization.
-- simp (Star (Star r gf2) gf1) = (Star r gf1, \(SStar p1 (SStar p2 sp)) -> SStar (p1 `appP` p2) sp)
simp (Star r gf) | isPhi r = (Empty, \(SStar !p !sp) -> let !p' = (p `appP` oneP) in  SEps p' )
                 | otherwise = let (r', !f) = simp r
                               in r' `seq` (Star r' gf, \(SStar !p !sp) -> let !sp' = f sp in SStar p sp')
simp (Choice [r] gf) = let (r',!f) = simp r                               
                       in r' `seq` (Choice [r'] gf, \(SChoice !p [!sp]) -> let !sp' = f sp in SChoice p [sp'])
                       -- in (r', \(SChoice p [sp]) -> prefix p $ f sp) 
simp r = (r, \sp -> sp)                               
                               

{-  simpChoice r1   (r2 +  ... + rn-1)    rn 
   where r2 ... rn -1 = alts
     case 1) r1 == rn  
       simplified to r1 +  r2 ... rn-1
       the proof terms coercing from  [| r1 +  r2 ... rn-1 + rn |] -> [| r1 +  r2 ... rn-1|]
         \x -> case x of 
           { SChoice p1 [sp1, [SChoice p2 [ ... SChoice pn-1 [spn-1, spn]]]] 
              -> SChoice p1 [sp1, [SChoice p2 [ ... pn-1+[0] `prefix` spn-1 ]]]
           }
    case 2)  otherwise 
      case rn = rn_a + rn_b
        case 2a) r1 == rn_a
         simplified to r1 + r2 ... rn-1 + rn_b
         the proof terms coercing from  [| r1 +  r2 ... rn-1 + rn_a +rn_b |] -> [| r1 +  r2 ... rn-1+ rn_b |]
         \x -> case x of 
            { SChoice p1 [sp1, [SChoice p2 [ ... SChoice pn [spn_a, spn_b]]]] 
              -> SChoice p1 [sp1, [SChoice p2 [ ... pn+[1] `prefix` spn_b]]]  
            }
        case 2b) otherwise 
         simpChoice r1 + r2 ... rn_a + rn_b where alts = r2 ... rn_a
     case rn =\= rn_a + rn_b 
       Nothing
-}
simpChoice :: RE -> [RE] -> RE -> Maybe (RE, SPath -> SPath)
simpChoice r1 alts rn 
  | r1 == rn = Just (mkChoice (r1:alts) Greedy, -- fixme
                     \v -> prefixNthChoiceLeft (length alts) v)
  | otherwise = case rn of 
    { Choice [rna,rnb] gf 
      | r1 == rna -> 
         Just (mkChoice ((r1:alts) ++ [rnb]) gf,
               \v -> prefixNthChoiceRight (length alts + 1) v)
    ; ChoiceInt [rna,rnb] 
      | r1 == rna -> 
         Just (mkChoiceInt ((r1:alts) ++ [rnb]) ,
               \v -> prefixNthChoiceRight (length alts + 1) v)
      | otherwise -> simpChoice r1 (alts ++ [rna]) rnb
    ; _ -> Nothing 
    }
  where mkChoice [r1,r2] gf = Choice [r1,r2] gf
        mkChoice (r:rs) gf  = Choice [r, mkChoice rs gf] gf
        
        mkChoiceInt [r1,r2] = ChoiceInt [r1,r2] 
        mkChoiceInt (r:rs)  = ChoiceInt [r, mkChoiceInt rs] 
        
        
prefixNthChoiceLeft :: Int -> SPath -> SPath
{-
prefixNthChoiceLeft 0 sp = sp
prefixNthChoiceLeft 1 (SChoice p [sp1,sp2]) = (p `appP` zeroP) `prefix` sp1
-}
prefixNthChoiceLeft 0 (SChoice p [sp1,sp2]) = (p `appP` zeroP) `prefix` sp1
prefixNthChoiceLeft n (SChoice p [sp1,sp2]) = SChoice p [sp1, prefixNthChoiceLeft (n-1) sp2]

prefixNthChoiceRight :: Int -> SPath -> SPath
{- 
prefixNthChoiceRight 0 sp = sp
prefixNthChoiceRight 1 (SChoice p [sp1,sp2]) = (p `appP` oneP) `prefix` sp2
-}
prefixNthChoiceRight 0 (SChoice p [sp1,sp2]) = (p `appP` oneP) `prefix` sp2
prefixNthChoiceRight n (SChoice p [sp1,sp2]) = SChoice p [sp1, prefixNthChoiceRight (n-1) sp2]

                           
         
simpFix :: RE -> (RE, SPath -> SPath)          
simpFix r = let (r', !f) = simp r           
                io = logger (print r >> print "======")
            in {- io `seq` -} r' `seq` if r == r' 
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
    let new_delta = sig `seq` curr_res `seq` [ r'' `seq` r `seq` g `seq` l `seq` (r,l,r'',  g) | r <- curr_res, l <- sig, let (r',f) = deriv r l, let (r'',f') = r' `seq` simpFix r', let g = f' . f] 
        new_res   = new_delta `seq` dict `seq` nub [ r' | (r,l,r',f) <- new_delta, not (r' `M.member` dict) ]
        (dict', max_id') = new_delta `seq` dict `seq`
          foldl' (\(d,id) r -> 
                   let io = logger (putStrLn $show  (r,id+1))
                   in {- io `seq` -} (M.insert r (id+1) d, id+1)) (dict, max_id) new_res
        acc_delta_next = new_delta `seq` acc_delta ++ (map (\(r,l,r',f) -> (getId dict' r, l, getId dict' r', f)) new_delta)
    in  dict' `seq` max_id' `seq` new_res `seq` builder sig acc_delta_next dict' max_id' new_res
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

buildDfaTable :: RE -> (DfaTable, IM.IntMap RE) -- the dfa table and the id-to-regex mapping
buildDfaTable r = 
  let sig = sigmaRE r
      init_dict = M.insert r 0 M.empty
      (delta, mapping) = sig `seq` init_dict `seq` builder sig [] init_dict 0 [r]
      table = delta`seq` IM.fromList (map (\(s,c,d,f) -> (my_hash s c, (d,f))) delta)
      r_mapping = mapping `seq` IM.fromList (map (\(x,y) -> (y,x)) (M.toList mapping))
      -- io = logger (mapM_ (\x -> putStrLn (show x)) (M.toList mapping))
  in table`seq` r_mapping `seq` (table,r_mapping)
     
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
                 -- io  = logger (putStrLn (show sp) >> putStrLn (show sp') >> putStrLn "=================")
                 nextStateSPaths = {- io `seq` -} j `seq` sp' `seq` [(j,sp')]
             in nextStateSPaths `seq` w `seq`
                execDfa dfaTable w nextStateSPaths
        }
         

execDfa2 :: IM.IntMap RE ->  DfaTable -> Word -> [(Int, SPath)] -> [(Int, SPath)] -- list is either singleton or null, since it is a DFA
execDfa2 im dfaTable w' [] = []
execDfa2 im dfaTable w' currStateSPaths = 
  case S.uncons w' of
    Nothing    -> currStateSPaths 
    Just (l,w) -> 
      let ((i,sp):_) = currStateSPaths
          k               = my_hash i l
      in case IM.lookup k dfaTable of 
        { Nothing -> [] 
        ; Just (j, f) ->
             let sp' = sp `seq` f sp
                 -- io  = logger (putStrLn (show (im IM.! i)) >> putStrLn (show sp) >> putStrLn (show (im IM.! j)) >> putStrLn (show sp') >> putStrLn "=================")
                 nextStateSPaths = {- io `seq` -} j `seq` sp' `seq` [(j,sp')]
             in nextStateSPaths `seq` w `seq`
                execDfa2 im dfaTable w nextStateSPaths
        }




match :: [(RE,SPath)] -> String -> [(RE,SPath)]
match [(r,sp)] (c:cs) = case deriv r c of
  { (r',f) -> let (r'',f') = simp r'
              in 
                 match [(r', f sp)] cs
  }
match [(r,sp)] [] = [(r,sp)]

match2 :: [(RE,SPath)] -> String -> [(RE,SPath)]
match2 [(r,sp)] (c:cs) = case deriv r c of
  { (r',f) -> let (r'',f') = simp r'
              in match2 [(r'', (f'. f) sp)] cs
  }
match2 [(r,sp)] [] = [(r,sp)]


retrieveEmpty :: RE -> SPath -> Path
retrieveEmpty Empty (SEps p) = p
retrieveEmpty (Choice [r1,r2] gf) (SChoice p [sp1, sp2]) 
  | nullable r1 = p `appP` (consP zero (retrieveEmpty r1 sp1))
  | nullable r2 = p `appP` (consP one (retrieveEmpty r2 sp2))
retrieveEmpty (Choice [r] gf) (SChoice p [sp]) 
  | nullable r = p `appP` (retrieveEmpty r sp)
retrieveEmpty (ChoiceInt [r1,r2]) (SChoice p [sp1, sp2]) 
  | nullable r1 = p `appP` (retrieveEmpty r1 sp1)
  | nullable r2 = p `appP` (retrieveEmpty r2 sp2)
retrieveEmpty (Seq r1 r2) (SPair p sp1 sp2)  = p `appP` (retrieveEmpty r1 sp1) `appP` (retrieveEmpty r2 sp2)
retrieveEmpty (Star r gf) (SStar p sp) = p `appP` oneP
retrieveEmpty r sr = error ("retrieveEmpty failed:" ++ show (posEpsilon r) ++ (show r) ++ (show sr))


decode2 :: RE -> Path -> (U, Path)
decode2 Phi bs = (Nil,bs)
decode2 Empty bs = (EmptyU,bs)
decode2 (L l) bs = (Letter l, bs)
decode2 sr@(Choice [r1,r2] gf) bs' = 
  case unconsP bs' of 
    Just (b, bs) | b == zero -> let (u,p) = decode2 r1 bs
                                in (LeftU u, p)
                 | b == one  -> let (u,p) = decode2 r2 bs
                                in (RightU u, p)
    Nothing -> error ("decode2 failed:" ++ show sr ++ show bs')
decode2 (Choice [r] gf) bs = decode2 r bs                                        
decode2 (Seq r1 r2) bs = let (u1,p1) = decode2 r1 bs
                             (u2,p2) = decode2 r2 p1
                         in (Pair (u1,u2), p2)
decode2 (sr@(Star r gf)) bs' = 
  case unconsP bs' of
    Just (b, bs) | b == zero -> let (u,p1) = decode2 r bs
                                    (List us,p2) = decode2 sr p1
                                in (List (u:us), p2)
                 | b == one -> (List [],bs)
    Nothing -> error ("decode2 failed:" ++ show sr ++ show bs')
decode2 sr bs = error ("decode2 failed:" ++ show sr ++ show bs)

decode :: RE -> Path -> U
decode r bs = let (u,p) = decode2 r bs
              in if nullP p 
                 then u
                 else error $ "invalid bit coding u=" ++ show u ++ " p=" ++ show p

-- assume strip p = r
extract :: Pat -> RE -> U -> [(Int,Word)]
extract (PVar i _ p) r u
  | strip p == r = [(i, flatten u)]
  | otherwise    = error ("the structures of the pattern and regex are not in sync" ++ show p ++ " vs " ++ show r)
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
                       (e,_) = extractSR p r u start_index
                   in ([(i, Range start_index l)]++e , start_index + l)
  | otherwise    = error ("the structures of the pattern and regex are not in sync" ++ show p ++ " vs " ++ show r)
extractSR (PE rs) (Choice rs' _) u start_index = ([],start_index) -- not in used
extractSR (PStar p _) (Star r _) (List []) start_index = ([], start_index)
extractSR (PStar p _) (Star r _) (List [u]) start_index = extractSR p r u start_index
extractSR p'@(PStar p _) r'@(Star r _) (List (u:us)) start_index = extractSR p' r' (List us) start_index -- we only extract the last binding
extractSR (PPair p1 p2) (Seq r1 r2) (Pair (u1,u2)) start_index =  
  let (l1, i1) = extractSR p1 r1 u1 start_index 
      (l2, i2) = extractSR p2 r2 u2 i1
  in (l1 ++ l2, i2)
extractSR (PChoice [p1,p2] _) (Choice [r1,r2] _) (LeftU u) start_index = extractSR p1 r1 u start_index
extractSR (PChoice [p1,p2] _) (Choice [r1,r2] _) (RightU u) start_index = extractSR p2 r2 u start_index
extractSR (PEmpty p) Empty _ start_index = ([],start_index) -- not in used
extractSR (PChoice [p] _) (Choice [r] _) u start_index = extractSR p r u start_index
extractSR p r u _ = error ("etractSR failed:" ++ (show p) ++ show r ++ show u)


  


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


compilePat :: Pat -> (DfaTable, Pat, IM.IntMap RE)
compilePat p = 
  let p' = normChoice p
      r = strip p'
      (dfa,im) = buildDfaTable r
  in (dfa, p', im)
     

-- normalize a+b+c to a+(b+c)                    

class NormChoice a where 
  normChoice :: a -> a 
  
  
instance NormChoice RE where  
  normChoice Empty = Empty
  normChoice (L c) = L c
  normChoice (Seq r1 r2) = Seq (normChoice r1) (normChoice r2)
  normChoice (Star r gf) = Star (normChoice r) gf
  normChoice Phi = Phi
  normChoice (Choice [r] gf) = Choice [normChoice r] gf
  normChoice (Choice [r1,r2] gf) = Choice [normChoice r1,normChoice r2] gf
  normChoice (Choice (r:rs) gf) = Choice [normChoice r, normChoice (Choice rs gf)] gf
  normChoice Any = Any
  
instance NormChoice Pat where  
  normChoice (PVar i rng p) = PVar i rng (normChoice p)
  normChoice (PE rs)        = PE (map normChoice rs)
  normChoice (PPair p1 p2)   = PPair (normChoice p1) (normChoice p2)
  normChoice (PChoice [p] gf) = PChoice [normChoice p] gf
  normChoice (PChoice [p1,p2] gf) = PChoice [normChoice p1, normChoice p2] gf
  normChoice (PChoice (p:ps) gf) = PChoice [normChoice p, normChoice (PChoice ps gf)] gf
  normChoice (PPlus p1 p2) = PPlus (normChoice p1) (normChoice p2)
  normChoice (PStar p gf) = PStar (normChoice p) gf
  normChoice (PEmpty p) = PEmpty (normChoice p)





type Env = [(Int,Range)]

execPatMatch :: (DfaTable, Pat, IM.IntMap RE) -> Word -> Maybe Env
execPatMatch (dfa, p, im) w = 
  let res = dfa `seq` p `seq` im `seq` execDfa dfa w [(0, mkSPath (strip p))]
  in case res of 
    { [] -> Nothing
    ; [ (i, sp) ] -> 
      let r'   = im IM.! i
          -- io   = logger (putStrLn (show i))          
          path = {- io `seq`-} r' `seq` sp `seq` retrieveEmpty r' sp
          r    = p `seq` strip p
          -- io   = logger (putStrLn (show path) >> putStrLn (show sp) >> putStrLn (show r'))
          parseTree = path `seq` decode r path
          -- io = logger (putStrLn (show path) >> putStrLn (show parseTree) >> putStrLn (show p))
          (env, _)  = {- io `seq` -} parseTree `seq` extractSR p r parseTree 0 
      in Just env
    }
     
     
p4 = PVar 0 [] (PPair (PVar 1 [] ((PPair p_x p_y))) p_z)
  where p_x = PVar 2 [] (PE [(Choice [(L 'A'),(Seq (L 'A') (L 'B'))] Greedy)])      
        p_y = PVar 3 [] (PE [(Choice [(Seq (L 'B') (Seq (L 'A') (L 'A'))), (L 'A')] Greedy)])
        p_z = PVar 4 [] (PE [(Choice [(Seq (L 'A') (L 'C')), (L 'C')] Greedy)])
     
        
        
-- x0 :: ( x1 :: (  x2 :: (x3:: a | x4 :: ab) | x5 :: b)* )
        
        
p3 = PVar 0 [] (PStar ( PVar 1 [] ( PChoice [(PVar 2 [] (PChoice [p3,p4] Greedy)), p5] Greedy)) Greedy)
  where p3 = PVar 3 [] (PE [(L 'A')])
        p4 = PVar 4 [] (PE [(Seq (L 'A') (L 'B'))])           
        p5 = PVar 5 [] (PE [(L 'B')])
        

p0 = PVar 0 [] (PChoice [PPair (PE [Empty]) (PPair (PStar (PVar 1 [] (PChoice [PPair (PVar 2 [] (PChoice [PPair (PVar 3 [] (PE [Choice [L 'A',Seq (L 'A') (L 'B')] Greedy ])) (PVar 4 [] (PE [Choice [Seq (Seq (L 'B') (L 'A')) (L 'A'),L 'A'] Greedy ]))] Greedy)) (PVar 5 [] (PE [Choice [Seq (L 'A') (L 'C'),L 'C'] Greedy]))] Greedy)) Greedy ) (PE [Empty]))] Greedy)

p1 = PVar 0 [] (PChoice [PPair (PE [Empty]) (PPair 

                                              (PVar 1 [] (PChoice [PPair (PVar 2 [] (PChoice [PPair (PVar 3 [] (PE [Choice [L 'A',Seq (L 'A') (L 'B')] Greedy ])) (PVar 4 [] (PE [Choice [Seq (Seq (L 'B') (L 'A')) (L 'A'),L 'A'] Greedy ]))] Greedy)) (PVar 5 [] (PE [Choice [Seq (L 'A') (L 'C'),L 'C'] Greedy]))] Greedy)) 
                                             
                                             (PE [Empty]))] Greedy)


p2 = PVar 0 [] (PPair (PE [Empty]) (PPair 

                                              (PVar 1 [] (PChoice [PPair (PVar 2 [] (PChoice [PPair (PVar 3 [] (PE [Choice [L 'A',Seq (L 'A') (L 'B')] Greedy ])) (PVar 4 [] (PE [Choice [Seq (Seq (L 'B') (L 'A')) (L 'A'),L 'A'] Greedy ]))] Greedy)) (PVar 5 [] (PE [Choice [Seq (L 'A') (L 'C'),L 'C'] Greedy]))] Greedy)) 
                                             
                                             (PE [Empty])) )

q2 = PVar 0 [] 
     (PVar 1 [] (PChoice [PPair (PVar 2 [] (PChoice [PPair (PVar 3 [] (PE [Choice [L 'A',Seq (L 'A') (L 'B')] Greedy ])) (PVar 4 [] (PE [Choice [Seq (Seq (L 'B') (L 'A')) (L 'A'),L 'A'] Greedy ]))] Greedy)) (PVar 5 [] (PE [Choice [Seq (L 'A') (L 'C'),L 'C'] Greedy]))] Greedy)) 
                                             



type Regex = (DfaTable, Pat, IM.IntMap RE)

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together) 
        -> S.ByteString -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt bs =
    case {-# SCC "compile/parsePatPosix" #-} parsePatPosix (S.unpack bs) of
    Left err -> Left ("parseRegex for Text.Regex.Deriv.ByteString failed:"++show err)
    Right (pat,posixBnd) -> 
       Right (compilePat pat)


execute :: Regex      -- ^ Compiled regular expression
       -> S.ByteString -- ^ ByteString to match against
       -> Either String (Maybe Env)
execute r bs = Right (execPatMatch r bs)

regexec :: Regex      -- ^ Compiled regular expression
       -> S.ByteString -- ^ ByteString to match against
       -> Either String (Maybe (S.ByteString, S.ByteString, S.ByteString, [S.ByteString]))
regexec r bs =
 case execPatMatch r bs of
   Nothing -> Right Nothing
   Just env ->
     let pre = case lookup preBinder env of { Just e -> rg_collect bs e ; Nothing -> S.empty }
         post = case lookup subBinder env of { Just e -> rg_collect bs e ; Nothing -> S.empty }
         full_len = S.length bs
         pre_len = S.length pre
         post_len = S.length post
         main_len = full_len - pre_len - post_len
         main_and_post = S.drop pre_len bs
         main = main_and_post `seq` main_len `seq` S.take main_len main_and_post
         matched = map ((rg_collect bs) . snd) (filter (\(v,w) -> v > mainBinder && v < subBinder ) env)
     in -- logger (print (show env)) `seq` 
            Right (Just (pre,main,post,matched))




rg_collect :: S.ByteString -> Range -> S.ByteString
rg_collect w (Range i j) = S.take (j' - i' + 1) (S.drop i' w)
  where i' = fromIntegral i
        j' = fromIntegral j

rg_collect_many w rs = foldl' S.append S.empty $ map (rg_collect w) rs


-- | Control whether the pattern is multiline or case-sensitive like Text.Regex and whether to
-- capture the subgroups (\1, \2, etc).  Controls enabling extra anchor syntax.
data CompOption = CompOption {
      caseSensitive :: Bool    -- ^ True in blankCompOpt and defaultCompOpt
    , multiline :: Bool 
  {- ^ False in blankCompOpt, True in defaultCompOpt. Compile for
  newline-sensitive matching.  "By default, newline is a completely ordinary
  character with no special meaning in either REs or strings.  With this flag,
  inverted bracket expressions and . never match newline, a ^ anchor matches the
  null string after any newline in the string in addition to its normal
  function, and the $ anchor matches the null string before any newline in the
  string in addition to its normal function." -}
    , rightAssoc :: Bool       -- ^ True (and therefore Right associative) in blankCompOpt and defaultCompOpt
    , newSyntax :: Bool        -- ^ False in blankCompOpt, True in defaultCompOpt. Add the extended non-POSIX syntax described in "Text.Regex.TDFA" haddock documentation.
    , lastStarGreedy ::  Bool  -- ^ False by default.  This is POSIX correct but it takes space and is slower.
                               -- Setting this to true will improve performance, and should be done
                               -- if you plan to set the captureGroups execoption to False.
    } deriving (Read,Show)

data ExecOption = ExecOption  {
  captureGroups :: Bool    -- ^ True by default.  Set to False to improve speed (and space).
  } deriving (Read,Show)

instance RegexOptions Regex CompOption ExecOption where
    blankCompOpt =  CompOption { caseSensitive = True
                               , multiline = False
                               , rightAssoc = True
                               , newSyntax = False
                               , lastStarGreedy = False
                                 }
    blankExecOpt =  ExecOption { captureGroups = True }
    defaultCompOpt = CompOption { caseSensitive = True
                                , multiline = True
                                , rightAssoc = True
                                , newSyntax = True
                                , lastStarGreedy = False
                                  }
    defaultExecOpt =  ExecOption { captureGroups = True }
    setExecOpts e r = undefined
    getExecOpts r = undefined 


choice r = Choice r Greedy

{-
r = choice [Seq (choice [ChoiceInt [Seq (choice [choice [Phi,ChoiceInt [Seq Phi (L 'B'),Empty]] ] ) (choice [choice [Seq (Seq (L 'B') (L 'A')) (L 'A'),L 'A'] ] )
                                   ,choice [choice [Seq (Seq Empty (L 'A')) (L 'A'),Phi] ] ]] ) (choice [choice [Seq (L 'A') (L 'C'),L 'C'] ] )]  


sp = SChoice [] [SPair [] (SChoice [] [SChoice [] [SPair [] (SChoice [] [SChoice [] [SPhi,SChoice [] [SPair [] SPhi (SL []),SEps []]]]) (SChoice [] [SChoice [] [SPair [] (SPair [] (SL []) (SL [])) (SL []),SL []]])                          
                                                  ,SChoice [0] [SChoice [] [SPair [] (SPair [] (SEps []) (SL [])) (SL []),SPhi]]]]) 
                 (SChoice [] [SChoice [] [SPair [] (SL []) (SL []),SL []]])]


-- (A|AB)(BB|B)

pp1 = PVar 1 [] (PE [r1])

r1 =  Seq (choice [L 'A', (Seq (L 'A') (L 'B'))]) (choice [(Seq (L 'B') (L 'B')), L 'B']) 

pp2 = PVar 1 [] (PE [r2])

r2 = Seq (choice [L 'A', Empty]) (choice [Empty, L 'A']) -}
  
  
-- A?A?  