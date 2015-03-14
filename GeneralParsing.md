
```





{-# LANGUAGE GADTs #-}



import Data.List
import System.IO.Unsafe

logger _ = ()
-- logger io = unsafePerformIO io

-- regular expression patterns
data RE where
  Phi :: RE                      -- empty language
  Eps :: RE                    -- empty word
  L :: Char -> RE                -- single letter
  Choice :: RE  -> RE  -> RE     -- r1 + r2
  Seq :: RE  -> RE  -> RE        -- (r1,r2)
  Star :: RE  -> RE              -- r*
 deriving (Show, Eq)


-- universal representation of regex patterns
-- [[r]] = u
data U where
  Nil :: U
  Empty :: U
  Letter :: Char -> U
  LeftU :: U -> U
  RightU :: U -> U
  Pair :: (U,U) -> U
  List :: [U] -> U
  deriving Show


resToRE :: [RE] -> RE
resToRE (r:res) = foldl Choice r res
resToRE [] = Phi


sigma :: RE -> [Char]
sigma (L l) = [l]
sigma (Seq r1 r2) = nub ((sigma r1) ++ (sigma r2))
sigma (Choice r1 r2) = nub ((sigma r1) ++ (sigma r2))
sigma (Star r) = sigma r
sigma Phi = []
sigma Eps = []

nullable :: RE -> Bool
nullable Phi = False
nullable Eps = True
nullable (Choice r1 r2) = (nullable r1) || (nullable r2)
nullable (Seq r1 r2) = (nullable r1) && (nullable r2)
nullable (Star r) = True
nullable (L _) = False

isPhi :: RE -> Bool
isPhi Phi = True
isPhi Eps = False
isPhi (Choice r1 r2) = (isPhi r1) && (isPhi r2)
isPhi (Seq r1 r2) = (isPhi r1) || (isPhi r2)
isPhi Star{} = False
isPhi L{} = False


deriv :: RE -> Char -> RE
deriv Phi l = Phi
deriv Eps l = Phi
deriv (L l') l
    | l == l'   = Eps
    | otherwise = Phi
deriv (Choice r1 r2) l = Choice (deriv r1 l) (deriv r2 l)
deriv (Seq r1 r2) l
    | nullable r1 = Choice (Seq (deriv r1 l) r2) (deriv r2 l)
    | otherwise = Seq (deriv r1 l) r2
deriv (Star r) l = Seq (deriv r l) (Star r)


mkEmptyU :: RE -> U
mkEmptyU Phi            = Nil
mkEmptyU Eps            = Empty
mkEmptyU (L l)          = error "mkEmptyU, letter impossible"
mkEmptyU (Choice r1 r2) 
  | nullable r1         = LeftU $ mkEmptyU r1
  | nullable r2         = RightU $ mkEmptyU r2
  | otherwise          = error "mkEmptyU, choice non-empty"
mkEmptyU (Seq r1 r2)   = Pair (mkEmptyU r1, mkEmptyU r2)
mkEmptyU (Star r)      = List []

nullableU :: U -> Bool
nullableU Nil = False
nullableU Empty = True
nullableU (LeftU u1)  = nullableU u1
nullableU (RightU u2) = nullableU u2
nullableU (Pair (u1,u2)) = (nullableU u1) && (nullableU u2)
nullableU (List []) = True
nullableU (List _) = False
nullableU Letter{} = False

-- injD r r\l l yields
-- [[r\l] -> [[r]]
-- r\l derivatives of r wrt l
injD :: RE -> RE -> Char -> U -> U
injD (Star r) (Seq rd _) l (Pair (u, List us)) =  -- _ must be equal to r
    List $ (injD r rd l u) : us

injD (Seq r1 r2) (Choice (Seq rd1 _) _) l (LeftU u) = -- first _ = r2, second _ = r2\l
             let Pair (u', u'') = u
             in Pair (injD r1 rd1 l u', u'')
injD (Seq r1 r2) (Choice _ rd2) l (RightU u) = 
             Pair (mkEmptyU r1, injD r2 rd2 l u)
injD (Seq r1 r2) (Seq rd1 _) l (Pair (u',u'')) =   -- _ = r2
     Pair (injD r1 rd1 l u', u'')

injD (Choice r1 r2) (Choice rd1 rd2) l (LeftU u) =
      LeftU $ injD r1 rd1 l u 
injD (Choice r1 r2) (Choice rd1 rd2) l (RightU u) =
      RightU $ injD r2 rd2 l u 

injD (L l') Eps l Empty
  | l == l'   = Letter l
  | otherwise = error "impossible" 
injD r1 r2 l u = 
  error $ show r1 ++ "\n" ++ show r2 ++ "\n" ++ show l ++ "\n" ++ show u


-- simp r1 yields (r2, u_r2 -> u_r1)
-- where r2 is the simplified version and u_r2 -> u_r1
-- turns r2 parse tree into a parse tree of r1
simp :: RE -> (RE, U -> U)
simp (Seq Eps r) 
  | isPhi r   = (Phi, undefined)
  | otherwise = (r, \u -> Pair (Empty,u))
simp (Seq r1 r2)
  | isPhi r1 || isPhi r2 = (Phi, undefined)
  | otherwise = 
        let (r1',f1) = simp r1
            (r2',f2) = simp r2
        in (Seq r1' r2', \(Pair (u1',u2')) -> Pair (f1 u1', f2 u2'))

-- enforce right associativity of +
simp (Choice (Choice r1 r2) r3) =
    (Choice r1 (Choice r2 r3), 
     \u -> case u of
            (LeftU u1)           -> LeftU $ LeftU u1
            (RightU (LeftU u2))  -> LeftU $ RightU u2
            (RightU (RightU u3)) -> RightU u3)  
simp (Choice r1 r2) 
  | r1 == r2  = (r1, \u -> LeftU u)
  | isPhi r1  = (r2, \u -> RightU u)
  | isPhi r2  = (r1, \u -> LeftU u)
  | otherwise = let (r1',f1) = simp r1
                    (r2',f2) = simp r2
                in (Choice r1' r2', 
                    \u -> case u of
                            (LeftU u1')  -> LeftU $ f1 u1'
                            (RightU u2') -> RightU $ f2 u2') 

simp (Star Eps) = (Eps, \u -> List [u])
simp (Star (Star r)) = (Star r, \u -> List [u])
simp (Star r)
  | isPhi r   =  (Eps, \u -> List [u])
  | otherwise = let (r',f) = simp r
                in (Star r', \(List us) -> List $ map f us) 

simp r = (r, id)

simpFix r = let (r',f) = simp r
            in if r == r'
               then (r,id)
               else let (r'',g) = simpFix r'
                    in (r'', f . g)

simpFix2 r = fst $ simpFix r


 


data DFA = DFA { start :: RE,
                 final :: [RE],
                 states :: [RE],
                 transitions :: [(RE,Char,RE,U->U)] }

instance Show DFA where
   show dfa = show (start dfa) ++ "\n" ++
              show (final dfa) ++ "\n" ++
              unlines (map (\(r1,l,r2,_) -> show (r1,l,r2)) (transitions dfa))



buildDFA r =
  let mkTransition r l =
         let r' = deriv r l
             (r'',f) = simpFix r'
         in (r, l, r'', (\u -> injD r r' l u) . f)

      go (rs,dfa) curr_rs = 
        let new_ts = map (\(l,r) -> mkTransition r l)
                         [ (l,r) | r <- curr_rs, 
                                   l <- sigma r ]

            new_rs = nub [ r | (_,_,r,_) <- new_ts,
                               not (elem r rs) ]

            new_dfa = DFA { start = start dfa,
                            final = (final dfa) ++ filter nullable new_rs,
                            states = (states dfa) ++ new_rs,
                            transitions = (transitions dfa) ++ new_ts }

            out = putStrLn $ show rs
        in logger out `seq` (
           if length new_rs == 0
           then new_dfa
           else go (rs ++ new_rs, new_dfa) new_rs )

  in let dfa = DFA { start = r,
                     final = if nullable r then [r] else [],
                     states = [r],
                     transitions = [] }
     in go ([r],dfa) [r]


allDerivs r = states $ buildDFA r


posix2 (r,m) [] 
   | nullable r = m $ mkEmptyU r
   | otherwise = error "no match"
posix2 (r,m) (l:ls) = 
    let r' = deriv r l
    in posix2 (r', m . (injD r r' l)) ls

posix r w = posix2 (r,id) w


posixF r []
  | nullable r = mkEmptyU r
posixF r (l:ls) =
     injD r (deriv r l) l $ posixF (deriv r l) ls

posixSimp r w =
   let go r m [] 
        | nullable r = m $ mkEmptyU r
        | otherwise  = error "no match"
       go r m (l:ls) = let r' = deriv r l
                           (r'', f) = simp r'              
                       in go r'' (m . ((injD r r' l) . f)) ls
   in go r id w

posixDFA r w =
  let dfa = buildDFA r

      step r l = case (filter (\(r',l',r'',f) -> r' == r && l' == l)
                              (transitions dfa)) of
                  ((_,_,r'',f):_) -> (r'',f)
                  [] -> error "impossible, missing transition"

      go r m []
        | nullable r = m $ mkEmptyU r
        | otherwise  = error "no match"
      go r m (l:ls) = let (r',f) = step r l
                      in go r' (m . f) ls
  in go r id w


-- (1) Reverse the input
-- (2) Run the DFA backwards
-- Yields by construction the POSIX match
posixBwdDFA r w =
  let dfa = buildDFA r
      go rus [] = case (filter (\(r',_) -> r == r) rus) of
                    ((_,u):_) -> u
                    [] -> error "no match"
      go rus (l:ls) = go 
                         -- CHECK:
                         -- nub not required because we use a DFA
                         [ (r1, f u) | (r,u) <- rus,
                                       (r1,l',r2,f) <- transitions dfa, 
                                       l == l',
                                       r2 == r ]                    
                         ls
                      

  in go [ (r, mkEmptyU r) | r <- final dfa ] (reverse w)



-- a few examples


r0 = Star $ L 'A'

-- (A+B+AB)*
r1 = Star $ Choice (L 'A') (Choice (L 'B') (Seq (L 'A') (L 'B')))


-- (AB+A+B)*
r2 = Star $ Choice (Seq (L 'A') (L 'B')) (Choice (L 'A') (L 'B'))


-- (A+B)
r3 = Choice (L 'A') (L 'B')

-- (A+B)*
r4 = Star $ Choice (L 'A') (L 'B') 

-- (A,B)
r5 = Seq (L 'A') (L 'B')

-- (eps (A + AB)) (B + eps)
r6 = Seq (Seq Eps (Choice (L 'A') (Seq (L 'A') (L 'B'))))
         (Choice (L 'B') Eps)



```