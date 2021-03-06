> -- | A translation schema from the external syntax (ERE) to our interal syntax (xhaskell style pattern)
> module Text.Regex.Deriv.Translate 
>     ( translate, translatePosix ) where

> import Control.Monad.State -- needed for the translation scheme
> import Data.Char (chr)
> import qualified Data.IntMap as IM

> import Text.Regex.Deriv.ExtPattern
> import Text.Regex.Deriv.IntPattern
> import Text.Regex.Deriv.RE
> import Text.Regex.Deriv.Common


> -- | A state monad in which we can assign number to groups and non-groups.
> data TState = TState { ngi :: NGI   -- ^ negative group index
>                      , gi :: GI     -- ^ (positive) group index
>                      , anchorStart :: Bool
>                      , anchorEnd :: Bool
>                      , posix :: Bool -- ^ if posix, add binders to non-groups 
>                      , posix_binder :: IM.IntMap () -- ^ keep tracks of posix binder
>                      } -- the state for trasslation
>             deriving Show


> -- variables 0,-1,-2 are reserved for pre, main and post!
> initTState = TState { ngi = -3, gi = 1, anchorStart = False, anchorEnd = False, posix = False, posix_binder = IM.empty } 


> initTStatePosix = TState { ngi = -3, gi = 1, anchorStart = False, anchorEnd = False, posix = True, posix_binder = IM.empty } 


> type NGI = Int -- the non group index

> type GI = Int -- the group index

getters and putters

> getNGI :: State TState NGI
> getNGI = do { st <- get
>             ; return $ ngi st
>             }

> getIncNGI :: State TState NGI -- get then increase
> getIncNGI = do { st <- get
>                ; let i = ngi st
>                ; put st{ngi=(i-1)} 
>                ; return i
>                }

> getGI :: State TState GI
> getGI = do { st <- get
>            ; return $ gi st
>            }

> getIncGI :: State TState GI -- get then increase 
> getIncGI = do { st <- get
>               ; let i = gi st
>               ; put st{gi=(i+1)}
>               ; return i
>               }

> getAnchorStart :: State TState Bool
> getAnchorStart = do { st <- get
>                     ; return (anchorStart st)
>                     }

> setAnchorStart :: State TState ()
> setAnchorStart = do { st <- get
>                     ; put st{anchorStart=True}
>                     }

> getAnchorEnd :: State TState Bool
> getAnchorEnd  = do { st <- get
>                    ; return (anchorEnd st)
>                    }

> setAnchorEnd :: State TState ()
> setAnchorEnd = do { st <- get
>                   ; put st{anchorEnd=True}
>                   }

> isPosix :: State TState Bool
> isPosix = do { st <- get
>              ; return (posix st)
>              }


> isPosixBinder :: GI -> State TState Bool
> isPosixBinder gi =  do { st <- get
>                        ; return (IM.member gi (posix_binder st)) 
>                        }                           

> addPosixBinder :: Int -> State TState ()
> addPosixBinder i = do { st <- get
>                       ; let bs = posix_binder st
>                             bs' = IM.insert i () bs
>                       ; put st{posix_binder=bs'}
>                       }

> -- | Translating external pattern to internal pattern
> translate :: EPat -> Pat
> translate epat = case runState (trans epat) initTState of
>                  (pat, state) ->
>                    let hasAnchorS = anchorStart state
>                        hasAnchorE = anchorEnd state
>                    in case (hasAnchorS, hasAnchorE) of
>                       (True, True)   -> PVar mainBinder [] pat 
>                       (True, False)  -> PPair (PVar mainBinder [] pat) (PVar subBinder [] (PE [(Star Any NotGreedy)]))
>                       (False, True)  -> PPair (PVar preBinder [] (PE [(Star Any NotGreedy)])) (PVar mainBinder [] pat)
>                       -- (False, False) -> PPair (PPair (PVar preBinder [] (PE (Star Any NotGreedy))) (PVar mainBinder [] pat)) (PVar subBinder [] (PE (Star Any NotGreedy)))
>                       -- (False, False) -> PPair (PVar preBinder_ [] (PPair (PVar preBinder [] (PE (Star Any NotGreedy))) (PVar mainBinder [] pat))) (PVar subBinder [] (PE (Star Any NotGreedy)))
>                       (False, False) -> (PPair (PVar preBinder [] (PE [(Star Any NotGreedy)])) (PVar preBinder_ [] (PPair (PVar mainBinder [] pat) (PVar subBinder [] (PE [(Star Any NotGreedy)])))))


> -- |  for posix 
> translatePosix :: EPat -> (Pat,IM.IntMap ())
> translatePosix epat = case runState (trans epat) initTStatePosix of
>                  (pat, state) ->
>                    let hasAnchorS = anchorStart state
>                        hasAnchorE = anchorEnd state
>                        posixBnd   = posix_binder state
>                    in case (hasAnchorS, hasAnchorE) of
>                       (True, True)   -> (PVar mainBinder [] pat, posixBnd)
>                       (True, False)  -> (PPair (PVar mainBinder [] pat) (PVar subBinder [] (PE [(Star Any NotGreedy)])), posixBnd)
>                       (False, True)  -> (PPair (PVar preBinder [] (PE [(Star Any NotGreedy)])) (PVar mainBinder [] pat), posixBnd)
>                       -- (False, False) -> PPair (PPair (PVar preBinder [] (PE (Star Any NotGreedy))) (PVar mainBinder [] pat)) (PVar subBinder [] (PE (Star Any NotGreedy)))
>                       -- (False, False) -> PPair (PVar preBinder_ [] (PPair (PVar preBinder [] (PE (Star Any NotGreedy))) (PVar mainBinder [] pat))) (PVar subBinder [] (PE (Star Any NotGreedy)))
>                       (False, False) -> ((PPair (PVar preBinder [] (PE [(Star Any NotGreedy)])) (PVar preBinder_ [] (PPair (PVar mainBinder [] pat) (PVar subBinder [] (PE [(Star Any NotGreedy)]))))), posixBnd)



> {-| 'trans' The top level translation scheme e ~> p
>     There are two sub rules.
>     e ~>_p p
>     and
>     e ~>_r r
>     which are fired depending on whether e has Group pattern (...) (i.e. pattern variable)
> -}
> trans :: EPat -> State TState Pat
> trans epat = 
>     do { is_posix <- isPosix -- if it is posix, we need to aggresively "tag" every sub expression with a binder
>        ; if is_posix  && isStructural epat
>          then do 
>            { gi <- getIncGI
>            ; ipat <- trans' epat
>            ; if isPVar ipat
>              then do 
>              { return ipat
>              }
>              else do 
>              { addPosixBinder gi
>              ; return (PVar gi [] ipat)
>              }
>            }
>          else trans' epat
>        }
>     where isStructural :: EPat -> Bool -- ^ indicate whether it is a complex structure which we need to add extra binding for POSIX tracking
>           isStructural (EOr _)     = True
>           isStructural (EConcat _) = True                                                                      
>           isStructural (EOpt _ _)  = True
>           isStructural (EPlus _ _) = True
>           isStructural (EStar _ _) = True
>           isStructural _           = False                                                                      
>           isPVar :: Pat -> Bool
>           isPVar (PVar _ _ _) = True
>           isPVar _ = False

> trans' :: EPat -> State TState Pat
> trans' epat 
>      | hasGroup epat = p_trans epat
>      | otherwise     = do 
>                 { r <- r_trans epat
>                 ; return (PE [r])
>                 }

> {-
> trans :: EPat -> State TState Pat
> trans epat | hasGroup epat = p_trans epat
>            | otherwise     = 
>                do 
>                { is_posix <- isPosix 
>                ; if is_posix 
>                  then do 
>                       { gi <- getIncGI
>                       ; r <- r_trans epat
>                       ; addPosixBinder gi
>                       ; return (PVar gi [] (PE r))
>                       }
>                  else do 
>                    { r <- r_trans epat
>                    ; return (PE r)
>                    }
>                }
> -}

> {-| 'adhoc_simp' make some simplification for p' on the fly for nested x :: p'
>     removing immediate nested PVar created for posix
> -}
> adhocSimp :: Pat -> State TState Pat 
> adhocSimp q@(PChoice [ (PVar x _ p) ] g) = do 
>   { b <- isPosixBinder x
>   ; if b 
>     then return p 
>     else return q }
> adhocSimp q = return q

> {-| 'p_trans' implementes the rule 'e ~>_p p'
> convention:
> a,b are non group indices.
> x,y,z are group indices
> -}
> p_trans :: EPat -> State TState Pat
> p_trans epat = 
>     case epat of
>       -- () ~>_p ()
>     { EEmpty ->
>       do { return ( PE [Empty] )
>          }
>       {-
>         e ~> p
>         -----------------
>         ( e ) ~>_p x :: p
>       -}
>     ; EGroup e ->
>       do { i <- getIncGI
>          ; -- p <- trans e
>          ; p <- trans' e -- no need to go through trans which possible tag p with a posix var
>          ; p' <- adhocSimp p
>          ; return ( PVar i [] p')
>          }
>       {-
>         e ~> p
>         -----------------
>         (? e ) ~>_p p 
>        -}
>     ; EGroupNonMarking e -> 
>         trans' e
>     ; EOr es -> 
>         {-
>           e1 ~> p1  e2 ~> p2
>           -------------------
>             e1|e2 ~>_p p1|p2
>          -}
>       do { ps <- mapM trans es
>          ; case ps of
>            { (p':ps') -> 
>               return (PChoice ps Greedy )
>            ; [] -> error "an empty choice enountered." -- todo : capture the error in the monad state
>            }
>          }
>     ; EConcat es ->
>         {- 
>            e1 ~> p1  e2 ~> p2
>            ---------------------
>              (e1,e2) ~>_p (p1,p2)
>          -} 
>         do { ps <- mapM trans es
>            ; case reverse ps of  -- to make sure it is right assoc
>              { (p':ps') -> 
>                    return (foldl (\xs x -> PPair x xs) p' ps')
>              ; [] -> error "an empty sequence enountered." -- todo : capture the error in the moand state
>              }
>            }
>     ; EOpt e b ->
>       {- 
>          todo : not sure whether this makes sense
>            e ~> p
>          -------------------
>            e? ~>_p p|()
>        -}
>       do { p <- trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>          ; return (PChoice [p,PE [Empty]] g)
>          }
>     ; EPlus e b ->
>       {- 
>            e ~> p
>          -------------------
>            p+ ~>_p (p,p*)
>        -}
>       do { p <- trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>          ; return (PPair p (PStar p g))
>          }
>     ; EStar e b -> 
>       {- 
>            e ~> p
>          -------------------
>            e*~>_p p*
>        -}
>       do { p <- trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>          ; return (PStar p g)
>          }
>     ; EBound e low (Just high) b ->
>         {- we could have relax this rule to e ~> p
>             e ~>_r r  
>             r1 = take l (repeat r)
>             r2 = take (h-l) (repeat r?)
>             r' = (r1,r2)
>           -------------------------------------
>             e{l,h} ~> a :: r' 
>          -}
>       do { r <- r_trans e
>          ; i <- getIncNGI
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>                r1s = take low $ repeat r
>                r1 = case r1s of 
>                     { [] -> Empty
>                     ; (r':rs') -> foldl (\ rs r -> Seq rs r) r' rs'
>                     }
>                r2s = take (high - low) $ repeat (Choice [r,Empty] g)
>                r2 = case r2s of
>                     { [] -> Empty
>                     ; (r':rs') -> foldl (\ rs r -> Seq rs r) r' rs'
>                     }
>                r3 = case (r1,r2) of 
>                       (Empty, Empty) -> Empty
>                       (Empty, _    ) -> r2
>                       (_    , Empty) -> r1
>                       (_    , _    ) -> Seq r1 r2
>                p = PVar i [] (PE [r3])
>          ; return p
>          }
>     ; EBound e low Nothing b ->
>         {-
>             e ~>_r r  
>             r1 = take l (repeat r)
>             r' = (r1,r*)
>           -------------------------------------
>             e{l,} ~> a :: r' 
>          -}
>       do { r <- r_trans e
>          ; i <- getIncNGI
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>                r1s = take low $ repeat r
>                r1 = case r1s of 
>                     { [] -> Empty
>                     ; (r':rs') -> foldl (\ rs r -> Seq rs r) r' rs'
>                     }
>                r2 = Seq r1 (Star r g)
>                p = PVar i [] (PE [r2])
>          ; return p
>          }
>     ; ECarat -> 
>       -- currently we anchor the entire expression 
>       -- regardless of where ^ appears, we turn the subsequent
>       -- ECarat into literal,
>       do { f <- getAnchorStart
>          ; if f 
>            then do { i <- getIncNGI -- not the first carat
>                    ; let r = L '^'
>                          p = PVar i [] (PE [r])
>                    ; return p
>                    }
>            else do { setAnchorStart -- the first carat
>                    ; i <- getIncNGI
>                    ; let r = Empty
>                          p = PVar i [] (PE [r])
>                    ; return p
>                    }
>          }
>     ; EDollar -> 
>           -- similar to carat, except that we will not treat
>           -- the subsequent EDollar as literal.
>       do { f <- getAnchorEnd
>          ; if f
>            then return ()
>            else setAnchorEnd
>          ; i <- getIncNGI
>          ; let r = Empty
>                p = PVar i [] (PE [r])
>          ; return p
>          }
>     ; EDot -> 
>         --  . ~> a :: \Sigma 
>         -- we might not need this rule
>       do { i <- getIncNGI
>          ; let r = Any
>                p = PVar i [] (PE [r])
>          ; return p
>         }
>     ; EAny cs ->
>         -- [ abc ] ~> a :: 'a'|'b'|'c' 
>         -- we might not need this rule
>       do { i <- getIncNGI
>          ; let r = char_list_to_re cs
>                -- r = Any
>                p = PVar i [] (PE [r])
>          ; return p
>          }
>     ; ENoneOf cs ->
>         -- [^ abc] ~> a :: \Sigma - 'a'|'b'|'c' 
>         -- we might not need this rule
>       do { i <- getIncNGI
>          ; let -- r = char_list_to_re (filter (\c -> not (c `elem` cs )) sigma)
>                r = Not cs
>                p = PVar i [] (PE [r])
>          ; return p
>          }
>     ; EEscape c ->
>         -- \\c ~> a :: L c 
>         -- we might not need this rule
>       do { i <- getIncNGI 
>          ; let p = PVar i [] (PE [L c])
>          ; return p 
>          }
>     ; EChar c ->
>         -- c ~> a :: L c
>         -- we might not need this rule
>       do { i <- getIncNGI
>          ; let p = PVar i [] (PE [L c])
>          ; return p
>          }
>     }


> char_list_to_re (c:cs) = Choice (map L (c:cs)) Greedy
> char_list_to_re [] = error "char_list_to_re expects non-empty list"

> alphas = char_list_to_re (['a'..'z'] ++ ['A'..'Z'])

> digits = char_list_to_re ['0'..'9']

> sigma = map chr [0 .. 255]

> anychar = char_list_to_re sigma


e ~>_r r


> r_trans :: EPat -> State TState RE
> r_trans e = 
>     case e of 
>     { EEmpty -> 
>       {-
>         () ~>_r ()
>        -}
>       return Empty
>     ; EGroup e ->
>       {- we might not need this rule
>          e ~> r
>         ----------
>         (e) ~> r
>        -}
>       r_trans e
>     ; EGroupNonMarking e ->
>       {- we might not need this rule
>          e ~> r
>         ----------
>         (?e) ~> r
>        -}
>       r_trans e
>     ; EOr es ->
>       {-
>         e1 ~>_r r1 e2 ~>_r r2
>       -------------------
>         e1|e2 ~>_r r1|r2
>        -}
>       do { rs <- mapM r_trans es
>          ; case rs of
>            { [] -> return Phi
>            ; (r:rs) -> return (Choice (r:rs) Greedy)
>            }
>          }
>     ; EConcat es ->
>       {-
>         e1 ~>_r r1  e2 ~>_r r2
>        ----------------------
>         (e1,e2) ~>_r (r1,r2)
>        -}
>       do { rs <- mapM r_trans es
>          ; case rs of
>            { [] -> return Empty
>            ; (r:rs) -> return (foldl (\ xs x -> Seq xs x) r rs)
>            }
>          }
>     ; EOpt e b -> 
>       {-
>           e ~>_r r
>         -----------
>           e? ~>_r r?
>        -}
>       do { r <- r_trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>          ; return (Choice [r,Empty] g)
>          }
>     ; EPlus e b -> 
>       {-
>         e ~>_r r
>         ---------------
>           e+ ~>_r (r,r*)
>        -}
>       do { r <- r_trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>          ; return (Seq r (Star r g))
>          }
>     ; EStar e b -> 
>       {-
>         e ~>_r r
>         ----------------
>           e* ~>_r r*
>        -}
>       do { r <- r_trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>          ; return (Star r g)
>          }
>     ; EBound e low (Just high) b ->
>       {-
>         e ~>_r r
>         r1 = take l (repeat r)
>         r2 = take (h-l) (repeat r?)
>         r' = (r1,r2)         
>         -----------------
>           e{l:h} => r'
>        -}
>       do { r <- r_trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>                r1s = take low $ repeat r
>                r1 = case r1s of
>                     { [] -> Empty
>                     ; (r':rs') -> foldl (\ rs r -> Seq rs r) r' rs'
>                     }
>                r2s = take (high - low) $ repeat (Choice [r,Empty] g)
>                r2 = case r2s of
>                     { [] -> Empty
>                     ; (r':rs') -> foldl (\ rs r -> Seq rs r) r' rs'
>                     }
>                r3 = case (r1,r2) of
>                       (Empty, Empty) -> Empty
>                       (Empty, _    ) -> r2
>                       (_    , Empty) -> r1
>                       (_    , _    ) -> Seq r1 r2
>          ; return r3
>          }
>     ; EBound e low Nothing b -> 
>         {-
>             e ~>_r r  
>             r1 = take l (repeat r)
>             r' = (r1,r*)
>           -------------------------------------
>             e{l,} => r' 
>          -}
>       do { r <- r_trans e
>          ; let g | b = Greedy
>                  | otherwise = NotGreedy
>                r1s = take low $ repeat r
>                r1 = case r1s of 
>                     { [] -> Empty
>                     ; (r':rs') -> foldl (\ rs r -> Seq rs r) r' rs'
>                     }
>                r2 = Seq r1 (Star r g)
>          ; return r2
>          }
>     ; ECarat -> 
>       -- currently we anchor the entire expression 
>       -- regardless of where ^ appears, we turn the subsequent
>       -- ECarat into literal,
>       do { f <- getAnchorStart
>          ; if f 
>            then return (L '^') -- not the first carat
>            else do { setAnchorStart -- the first carat
>                    ; return Empty
>                    }
>          }
>     ; EDollar -> 
>           -- similar to carat, except that we will not treat
>           -- the subsequent EDollar as literal.
>       do { f <- getAnchorEnd
>          ; if f
>            then return ()
>            else setAnchorEnd
>          ; return Empty
>          }
>     ; EDot -> 
>         --  . ~>_r \Sigma
>         -- return anychar
>         return Any
>     ; EAny cs ->
>         --  [ abc ] ~>_r 'a'|'b'|'c'
>         return (char_list_to_re cs)
>     ; ENoneOf cs ->
>         --  [^ abc] ~>_r \Sigma - 'a'|'b'|'c'
>         -- return $ char_list_to_re (filter (\c -> not (c `elem` cs )) sigma)
>         return (Not cs)
>     ; EEscape c ->
>         --  \\c ~>_r c
>         return $ L c
>     ; EChar c ->
>         --  c ~>_r c
>         return $ L c
>     }
>           

