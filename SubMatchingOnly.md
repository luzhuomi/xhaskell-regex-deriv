
```



> {-# LANGUAGE NoMonomorphismRestriction, GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances #-}

> import Data.List
> import Data.Maybe
> import Control.Monad.State
> import Test.QuickCheck 
> import qualified Text.PrettyPrint as P
> import qualified Data.IntMap as IM
> import qualified Data.Map as M

> -- | Normalized representation, 
> -- instead of r1 + r2 we use a list
> data Re l where
>  Choice :: l -> [Re l] -> Re l
>  Pair :: l -> Re l -> Re l -> Re l
>  Star :: l -> Re l -> Re l
>  Ch :: l -> Char -> Re l
>  Eps :: l -> Re l
>  Phi :: Re l
>  Range :: l -> (Int,Int) -> Re l -> Re l
>  deriving Show

> sigma :: Re l -> String
> sigma (Choice _ rs) = nub $ concat $ map sigma rs
> sigma (Pair _ r1 r2) = nub $ (sigma r1) ++ (sigma r2)
> sigma (Star _ r) = sigma r
> sigma (Ch _ c) = [c]
> sigma Eps{} = []
> sigma Phi = []
> sigma (Range _ _ r) = sigma r

> size :: Re l -> Integer
> size (Choice _ rs) = 1 + (sum $ map size rs)
> size (Pair _ r1 r2) = 1 + size r1 + size r2
> size (Star _ r) = 1 + size r
> size (Range _ _ r) = 1 + size r
> size _ = 1


> mprint r = putStrLn $ P.render $ pretty r
> mprintRs rs = unlines $ map (\(i,r)-> show i ++ ": " ++ (P.render $ pretty r)) $ zip [1..] rs


> class Pretty a where
>   pretty :: a -> P.Doc

> instance Pretty (Re l) where
>   pretty (Choice l []) = P.empty
>   pretty (Choice l [r]) = pretty r
>   pretty (Choice l rs) = P.parens $ foldl (\a -> \r -> a P.<+> (P.text "+") P.<+> (pretty r)) (pretty $ head rs) (tail rs)
>   pretty (Pair _ r1 r2) = P.parens $ (pretty r1) 
>                                     P.<> 
>                                     (P.text " , ")
>                                     P.<>
>                                     (pretty r2)
>   pretty (Star _ r) = (P.parens $ pretty r) P.<> (P.text "*")
>   pretty (Ch _ c) = P.text $ show c
>   pretty Eps{} = P.text  "eps"
>   pretty Phi = P.text "phi"
>   pretty (Range _ (i,j) r) = P.parens $ (pretty r) P.<> (P.text "{") 
>                                         P.<> (P.text $ show i)
>                                         P.<> (P.text ",")
>                                         P.<> (P.text $ show j)
>                                         P.<> (P.text "}")

> instance Pretty a => Pretty [a] where
>    pretty xs = P.vcat $ map pretty xs

> class Var l where
>   v :: Int -> l

------------------------------
------------------------------
-- POSIX specification

> type SubMatch = (Int, Maybe (Int,Int))

> type Word = [(Int,Char)]

> instance Var Int where
>   v x = x

> split2 :: Word -> [(Word,Word)]
> split2 [] = [ ([],[]) ]
> split2 (w@(c:ws)) = 
>        nub $ (w,[]) : ([],w) : 
>              (map (\(w1,w2) -> (c:w1,w2)) $ split2 ws)


> split :: Word -> [[Word]]
> split [] = [ [] ]
> split [c] = [ [[c]] ]
> split (w@(c:ws)) = 
>  [w]:[ take i w : xs | i <- [1..length ws], xs <- split (drop i w) ]

> posix' s r = posix (zip [1..] s) r

> posix :: Word -> Re Int -> [SubMatch]
> posix [] (Eps l) = [(l,Nothing)]
> posix [(i,c)] (Ch l c')
>   | c == c'   = [(l,Just (i,i))]
>   | otherwise = []
> posix w (Choice l rs) =
>   case (filter ([] /=) $ map (posix w) rs) of
>     (m:_) -> (l,range w) : m
>     []    -> []

> posix w (Pair l r1 r2) =
>   case (filter (\(_,m1,m2) -> m1 /= [] && m2 /= []) $  
>         map (\(w1,w2) -> ((w1,w2),posix w1 r1, posix w2 r2)) $ 
>         split2 w) of
>     [] -> []
>     ms -> let (_,m1,m2) = maximumBy 
>                             (\(p1,_,_)-> \(p2,_,_)->
>                                 if p1 < p2 then LT
>                                 else if p1 > p2 then GT
>                                 else EQ) 
>                             ms
>           in (l,range w) : m1 ++ m2
> posix [] (Star l r) = [(l,Nothing)] 
> posix w (Star l r) = 
>   case (filter (\xs -> all (\(_,m) -> m /= []) xs) $ 
>         map (\ws-> map (\w -> (w,posix w r)) ws) $ 
>         split w) of
>     [] -> []
>     ms -> let (_,m) = last $ maximumBy 
>                               (\xs-> \ys-> 
>                                   let ws1 = map snd xs
>                                       ws2 = map snd ys
>                                   in if ws1 < ws2 then LT
>                                      else if ws1 > ws2 then GT
>                                           else EQ) 
>                               ms
>              in (l,range w) : m 
> posix _ _ = []

> range :: Word -> Maybe (Int,Int)
> range [] = Nothing
> range w = let (left,_) = head w
>               (right,_) = last w
>           in Just (left,right)


------------------------------
------------------------------
-- Simple POSIX Submatcher

> class MatchCl l where
>   extM :: l -> [SubMatch]
>   mCh :: (Int,Char) -> l -> l
>   dontCare :: l
>   collapse :: l -> l -> l
>   combine :: l -> l -> l

> containsEps :: Re l -> Bool
> containsEps (Choice _ rs) = or $ map containsEps rs
> containsEps (Pair _ r1 r2) = (containsEps r1)
>                                  && (containsEps r2)
> containsEps Star{} = True
> containsEps Ch{} = False
> containsEps Eps{} = True
> containsEps Phi{} = False
> containsEps (Range _ _ r) = containsEps r

> mkE :: Re l -> Re l
> mkE (Choice l rs) = Choice l $ map mkE rs
> mkE (Pair l r1 r2) = Pair l (mkE r1) (mkE r2)
> mkE (Star l r) = Star l $ mkE r
> mkE Ch{} = Phi
> mkE (e@Eps{}) = e
> mkE Phi = Phi
> mkE (Range l rng r) = Range l rng $ mkE r



> extract :: MatchCl l => Re l -> [(SubMatch)]
> extract (Choice m rs) = case (filter containsEps rs) of 
>                          (r:_) -> extM m ++ extract r
>                          _ -> []
> extract (r@(Pair m r1 r2)) 
>   | containsEps r = extM m ++ extract r1 ++ extract r2
>   | otherwise       = []
> extract (Star m r) = extM m 
> extract (Eps m) = extM m
> extract (Range m _ r) = extM m ++ extract r
> extract _ = []


> mDeriv :: MatchCl l => (Int,Char) -> Re l -> Re l
> mDeriv c (Choice l rs) =
>     Choice (mCh c l) $ map (mDeriv c) rs
> mDeriv c (Pair l r1 r2) 
>    | containsEps r1  = Choice dontCare [Pair (mCh c l) (mDeriv c r1) r2, Pair (mCh c l) (mkE r1) (mDeriv c r2)]
>    | otherwise         = Pair (mCh c l) (mDeriv c r1) r2
> mDeriv c (t@(Star l r)) = Pair (mCh c l) (mDeriv c r) (Star dontCare r) 
> mDeriv c@(_,ch) (Ch l ch') 
>    | ch == ch' = Eps (mCh c l)
>    | otherwise = Phi
> mDeriv _ Eps{} = Phi
> mDeriv _ Phi = Phi
> mDeriv c (Range l (i,j) r)
>   | i > 1           = Pair dontCare (mDeriv c r) (Range l (i-1,j-1) r)
>   | i == 1 && j > 1 = Choice dontCare [mDeriv c r,
>                               Pair dontCare (mDeriv c r)
>                                             (Range l (1,j-1) r)]

> dontCare_ = -1

> instance MatchCl SubMatch where
>    extM (x,m) = [(x,m)]
>    mCh (i,c) (x,Nothing) = (x,Just (i,i))
>    mCh (i,c) (x,Just (l,r)) = (x,Just (l,r+1))
>    dontCare = (dontCare_,Nothing) 
>    collapse = error "not in use"
>    combine = error "not in use"

> instance Var SubMatch where
>   v i = (i,Nothing)


> select :: [SubMatch] -> [SubMatch]
> select xs = map last $
>             groupBy (\(i,_)-> \(j,_) -> i == j) $
>             sort $
>             filter (\(l,_)-> l /= dontCare_) xs


> subMatches' :: String -> Re SubMatch -> [SubMatch]
> subMatches' s r = subMatches (zip [1..] s) r

> subMatches :: Word -> Re SubMatch -> [SubMatch]
> subMatches w r = select $ extract $ 
>                   foldl (\r-> \c-> mDeriv c r) r w



------------------------------
------------------------------
-- POSIX Submatcher with Simplifications

> type Match = ([SubMatch], [SubMatch])

> instance MatchCl Match where
>   extM (m1,m2) = m1 +++ m2   
>   mCh (i,c) (actives, inactives) = 
>       (map (\(x,m) -> case m of
>                       Nothing -> (x, Just (i,i))
>                       Just (l,r) -> (x, Just (l,r+1)))
>            actives,
>       inactives)
>   collapse (a1,i1) (a2,i2) = (a1,i1+++i2+++a2)
>   combine (a1,i1) (a2,i2) = (a1++a2,i1+++i2)
>   dontCare = ([],[]) 

> notNth xs = filter (\(_,x) -> case x of
>                              Nothing -> False
>                              _ -> True) xs

> -- Keep last match only
> (+++) xs ys = map last $
>               groupBy (\(i,_)-> \(j,_) -> i == j) $
>               sort $ xs ++ ys

> instance Eq (Re l) where
>     (==) (Choice _ rs1) (Choice _ rs2) = rs1 == rs2
>     (==) (Pair _ r1 r2) (Pair _ r1' r2') = r1 == r1' && r2 == r2'
>     (==) (Star _ r1) (Star _ r2) = r1 == r2
>     (==) (Ch _ c1) (Ch _ c2) = c1 == c2
>     (==) Eps{} Eps{} = True
>     (==) Phi Phi = True
>     (==) (Range _ rng1 r1) (Range _ rng2 r2) = rng1 == rng2 && r1 == r2
>     (==) _ _ = False


> isPhi :: Re l -> Bool
> isPhi (Choice _ rs) = and $ map isPhi rs
> isPhi (Pair _ r1 r2) = (isPhi r1) || (isPhi r2)
> isPhi Star{} = False
> isPhi Ch{} = False
> isPhi Eps{} = False
> isPhi Phi{} = True
> isPhi (Range _ _ r) = isPhi r


> shift :: MatchCl l => l -> Re l -> Re l
> shift l1 (Choice l2 rs) = Choice (combine l1 l2) rs
> shift l1 (Pair l2 r1 r2) = Pair (combine l1 l2) r1 r2
> shift l1 (Star l2 r) = Star (combine l1 l2) r
> shift l1 (Ch l2 c) = Ch (combine l1 l2) c
> shift l1 (Eps l2) = Eps $ combine l1 l2 
> shift l1 Phi = Phi
> shift l1 (Range l rng r) = Range (combine l l1) rng r


> isChoice :: Re l -> Bool
> isChoice Choice{} = True
> isChoice _ = False

> simpFix :: MatchCl l => Re l -> Re l
> simpFix p = let q = simp p
>             in if q == p
>                then q
>                else simpFix q


> simp :: MatchCl l => Re l -> Re l
> simp (Pair l1 (Eps l2) r) 
>   | isPhi r   = Phi
>   | otherwise = shift (collapse l1 l2) r
> simp (Pair l r1 r2)
>   | isPhi r1 || isPhi r2 = Phi
>   | otherwise            = Pair l (simp r1) (simp r2)
> simp (Choice l []) = Eps l
> simp (Choice l [r]) = shift l r
> simp (Choice l rs)
>  | any isChoice rs =  
>     Choice l $ 
>        foldl (\rs-> \r-> case r of
>                Choice l rs2 -> rs ++ (map (shift l) rs2)
>                _            -> rs ++ [r])
>              [] rs
>  | otherwise = Choice l $ nub $ filter (not.isPhi) $ map simp rs
> simp (Star l1 (Eps l2)) = Eps $ collapse l1 l2 
> simp (Star l1 (Star l2 r)) = Star (combine l1 l2) r 
> simp (Star l r)
>  | isPhi r   = Eps l
>  | otherwise = Star l $ simp r 
> simp (Range _ _ Phi) = Phi
> simp (Range l1 _ (Eps l2)) = Eps (combine l1 l2) 
> simp (rng@(Range l (i,j) r))
>   | i == 1 && j == 1 = shift l $ r
>   | otherwise = Range l (i,j) $ simp r
> simp x = x 


> instance Var Match where
>   v i = ([(i,Nothing)],[])

> subMatchesS' s r = subMatchesS (zip [1..] s) r

> subMatchesS :: Word -> Re Match -> [SubMatch]
> subMatchesS w r = 
>       (\e -> e +++ []) $ extract $ 
>       foldl (\r-> \c-> simpFix $ mDeriv c (simpFix r)) r w



> conv :: Var l => Re Int -> Re l
> conv (Choice l rs) = Choice (v l) $ map conv rs
> conv (Pair l r1 r2) = Pair (v l) (conv r1) (conv r2)
> conv (Star l r) = Star (v l) $ conv r
> conv (Ch l c) = Ch (v l) c
> conv (Eps l) = Eps $ v l
> conv Phi = Phi
> conv (Range l rng r) = Range (v l) rng $ conv r


------------------------------
------------------------------
-- Explicit POSIX DFA Submatch Construction


> data MatchM = MatchM
>                  (IM.IntMap (Maybe (Int,Int)))
>                  (IM.IntMap (Maybe (Int,Int)))


> instance MatchCl MatchM where
>    mCh (pos,_) (MatchM m1 m2) =
>        MatchM (IM.map (\y -> case y of
>                                     Nothing -> Just (pos,pos)
>                                     (Just (l,r)) -> Just (l,r+1))
>                       m1)
>                 m2
>    extM (MatchM m1 m2) =  
>             (map (\x-> (x,fromJust $ IM.lookup x m1)) $
>                      map fst $ IM.toList m1)
>         +++ (map (\x-> (x,fromJust $ IM.lookup x m2)) $
>                      map fst $ IM.toList m2)

>    collapse (MatchM a1 i1)
>             (MatchM a2 i2) =
>              MatchM a1 
>                     (IM.unionWith unionF i1 $
>                      IM.unionWith unionF i2  a2)
>              where
>              unionF x y = if x < y then y else x
>    combine (MatchM a1 i1)
>             (MatchM a2 i2) =
>              MatchM (IM.union a1 a2)
>                     (IM.unionWith unionF i1 i2)
>              where
>              unionF x y = if x < y then y else x
>    dontCare = MatchM IM.empty IM.empty

> instance Var MatchM where
>    v i = MatchM (IM.insert i Nothing $ IM.empty)
>                 IM.empty


> mDeriV :: MatchCl l => Char -> Re l 
>                     -> (Re l, Int -> Re MatchM -> Re MatchM)
> mDeriV c r = (mDeriv (1,c) r, \p-> \r-> mDeriv (p,c) r)

> simP :: MatchCl l => Re l -> (Re l, Re MatchM -> Re MatchM)
> simP r = (simp r, \r -> simp r)


> simPFix :: MatchCl l => Re l -> (Re l, Re MatchM -> Re MatchM)
> simPFix r = let go (rf@(r,f)) = let (r',g) = simP r
>                                 in if r == r' then rf
>                                    else go (r', g . f)
>             in go (r,id)

> next :: MatchCl l =>
>         Char -> Re l -> 
>         (Re l, Char, Re l, Int -> Re MatchM -> Re MatchM)
> next c r = let (r',f) = mDeriV c r
>                (r'',g) = simPFix r'
>             in (r,c,r'',\p -> \m-> g $ f p m)


> buildDFA :: MatchCl l => Re l 
>          -> M.Map (Int,Char) 
>                   (Int, Int -> Re MatchM -> Re MatchM)
> buildDFA r =
>   let go (rs,tble) curr_rs =
>         let new_ts = 
>               map (\(c,r) -> next c r)
>                   [(c,r) | c <- sigma r, 
>                            r <- curr_rs]
>             new = nub [ r | (_,_,r,_) <- new_ts, 
>                             not (elem r rs)]
>             as = rs ++ new
>             map_as = zip as [1..]
>             new_tble = 
>                foldl (\tble-> \(r,c,r'',f) ->
>                       let s1 = fromJust $ 
>                                lookup r map_as
>                           s2 = fromJust $ 
>                                lookup r'' map_as
>                       in M.insert (s1,c) (s2,f) tble)
>                      tble new_ts
>         in if length new == 0
>            then new_tble
>            else go (as, new_tble) new
>   in go ([r], M.empty) [r]


> subMatchesDFA :: String -> Re MatchM -> [SubMatch]
> subMatchesDFA w r = 
>    let dfa = buildDFA r
>        patM s m pos [] = m
>        patM s m pos (c:w') =
>           let (s',f) = fromJust $ M.lookup (s,c) dfa
>               m' = f  pos m
>               pos' = pos + 1
>           in patM s' m' pos' w'  
>    in select $ extract $ patM 1 r 1 w


------------------------------
------------------------------
-- QuickCheck


> distinct :: Re Int -> Re Int
> distinct r = 
>    let fresh = do i <- get
>                   put (i+1)
>                   return i
>        go (Choice _ rs) = do
>               i <- fresh
>               rs' <- mapM go rs
>               return $ Choice i rs'
>        go (Pair _ r1 r2) = do
>               i <- fresh
>               r1' <- go r1
>               r2' <- go r2
>               return $ Pair i r1' r2'
>        go (Star _ r) = do
>               i <- fresh
>               r' <- go r
>               return $ Star i r'
>        go (Ch _ c) = do
>               i <- fresh
>               return $ Ch i c
>        go (Eps _) = do i <- fresh
>                        return $ Eps i
>        go Phi = return Phi
>        go (Range _ rng r) = do
>               i <- fresh
>               r' <- go r
>               return $ Range i rng r'

>    in fst $ runState (go r) 1

> newtype MyChar = MyChar Char deriving Show
> toChar (MyChar c) = c

> instance Arbitrary MyChar where
>  arbitrary = elements [MyChar 'A', MyChar 'B']

> newtype MyString = MyString String deriving Show
> toStr (MyString s) = s



> comb :: String -> [String]
> comb cs = [""] ++ 
>            [c : xs | xs <- comb cs, c <- cs]


> instance Arbitrary MyString where
>   arbitrary = elements $
>               map MyString $
>               take 15 $ comb "AB"

> -- | Avoid cases such as Choice [Choice [...],...]
> arbitraryNoChoice :: Gen (Re Int)
> arbitraryNoChoice =
>   suchThat arbitrary (not.isChoice)

> instance Arbitrary (Re Int) where
>   arbitrary = oneof [elements [Phi, Eps 1],
>                      do c <- (arbitrary :: Gen MyChar)
>                         return $ Ch 1 $ toChar c,
>                      do r <- (arbitrary :: Gen (Re Int))
>                         return $ Star 1 r,
>                      do r1 <- arbitraryNoChoice 
>                         r2 <- arbitraryNoChoice 
>                         return $ Choice 1 [r1,r2],
>                      do r1 <- arbitrary :: Gen (Re Int)
>                         r2 <- arbitrary :: Gen (Re Int)
>                         return $ Pair 1 r1 r2]

> -- | Simple 'random' permute
> randPermute [] = []
> randPermute (x:xs) = xs ++ [x]

> -- | Reorder some Choice elements
> reorder i (Choice l rs) = 
>      Choice l $ randPermute $ map (reorder (i-1)) rs
> reorder i (Pair l r1 r2) = 
>      Pair l (reorder i r1) (reorder i r2)
> reorder i (Star l r) = Star l $ reorder i r
> reorder _ x = x

> -- | Reordering only starts if below a non-Choice pattern
> reorderTop i (Choice l rs) = Choice l $ map (reorderTop i) rs
> reorderTop i r = reorder i r

> -- | Yields a tripe (w,r1,r2) where r1 and r2 are equivalent
> -- modulo reordering of non-top-level alternatives
> instance Arbitrary (MyString, Re Int, Re Int) where
>       arbitrary = do w <- arbitrary :: Gen MyString
>                      r <- arbitrary :: Gen (Re Int)
>                      let r' = reorderTop (1 + (size r `div` 4)) r
>                      return (w,r,r')

> instance Arbitrary (Re Match) where
>      arbitrary = do r' <- arbitrary :: Gen (Re Int)
>                     let r = conv $ distinct r' :: Re Match
>                     s' <- arbitrary :: Gen (MyString)
>                     let w = zip [1..] $ toStr s'
>                     return $
>                        foldl (\r-> \c-> mDeriv c r) r w

> rmNoth = filter (\(_,m) -> case m of
>                 Nothing -> False
>                 _       -> True)


> -- | instance Arbitrary (MyString, Re Int, Re Int)
> -- so we have more control how alternatives are reordered
> propReorderAlternatives :: (MyString, Re Int, Re Int) -> Bool
> propReorderAlternatives (w', r1,r2) =
>    let w = toStr w'
>    in (sort $ posix' w r1) == (sort $ posix' w r2)


> -- | Simplifications retain POSIX submatches
> propSimp :: Re Match -> Bool
> propSimp r = (rmNoth $ extract r) == (rmNoth $ extract $ simp r)



> -- | Number of normalized derivatives is finite (exponential)
> propFiniteDeriv :: Re Int -> Bool
> propFiniteDeriv r = (toInteger $ length $ allDerivs r) <= 2 ^ (size r) + 1


> propSizeDeriv :: Re Int -> Bool
> propSizeDeriv r =
>    (maximum $ map size $ allDerivs r) <= 2 ^ (size r)

> allDerivs :: Re Int -> [Re Match]
> allDerivs r =
>   let go rs = 
>        if (nub $ rs ++ next) == rs
>        then rs else go $ nub $ rs ++ next
>        where
>        next = nub $ rs ++ [ simpFix $ mDeriv (1,c) r | 
>                             c <- sigma r, r <- rs ]
>   in go [conv r :: Re Match]              





> -- | Simple POSIX Submatcher is correct
> propSubMatches :: (MyString, Re Int) -> Bool
> propSubMatches (w',r') =
>     let r = distinct r' 
>         w = toStr w'
>     in all (\xm -> elem xm $ subMatches' w $ conv r) $
>        rmNoth $ posix' w r

> -- | POSIX Submatcher with normalization is correct
> propSubMatchesS :: (MyString, Re Int) -> Bool
> propSubMatchesS (w',r') =
>     let r = distinct r' 
>         w = toStr w'
>     in all (\xm -> elem xm $ subMatchesS' w $ conv r) $
>        rmNoth $ posix' w r

> -- | Explicit DFA POSIX Submatcher is correct
> propMatchDFA :: (MyString, Re Int) -> Bool
> propMatchDFA (w',r') =
>     let r = distinct r' 
>         w = toStr w'
>     in all (\xm -> elem xm $ subMatchesDFA w $ conv r) $
>        rmNoth $ posix' w r

> propSubMatchLength :: (MyString, Re Int) -> Bool
> propSubMatchLength (w',r') =
>     let r = conv $ distinct r'
>         w = zip [1..] $ toStr w'
>     in maxLen (fromInteger $ size r) $
>        foldl (\r-> \c-> simpFix $ mDeriv c (simpFix r)) r w

> maxP :: Int -> Match -> Bool
> maxP n (a,i) = length a <= n && length i <= n

> maxLen :: Int -> Re Match -> Bool
> maxLen n (Choice l rs) = maxP n l && (and $ map (maxLen n) rs)
> maxLen n (Pair l r1 r2) = maxP n l && maxLen n r1 && maxLen n r2
> maxLen n (Star l r) = maxP n l && maxLen n r
> maxLen n (Ch l _) = maxP n l
> maxLen n (Eps l) = maxP n l
> maxLen n Phi = True



```