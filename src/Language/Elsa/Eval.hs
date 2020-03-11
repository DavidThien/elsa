{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf#-}

module Language.Elsa.Eval (elsa, elsaOn, evalNO, evalNOLimited, isTrnsEq,
                           isTrnsEqFuel, newAId, isNormEq, isNormEqLimited,
                           isNormEqRunsOutOfFuel, evalCBN, bSubst, alphaNormal,
                           canon, DBExpr(..), evalDeBruijn, evalDeBruijnNO, dbSubst) where

import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Control.Monad.State
import qualified Data.Maybe           as Mb -- (isJust, maybeToList)
import           Language.Elsa.Types
import           Language.Elsa.Utils  (traceShow, qPushes, qInit, qPop, fromEither)
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
elsa :: Elsa a -> [Result a]
--------------------------------------------------------------------------------
elsa = elsaOn (const True)

--------------------------------------------------------------------------------
elsaOn :: (Id -> Bool) -> Elsa a -> [Result a]
--------------------------------------------------------------------------------
elsaOn cond p =
  case mkEnv (defns p) of
    Left err -> [err]
    Right g  -> [result g e | e <- evals p, check e ]
  where
    check = cond . bindId . evName


result :: Env a -> Eval a -> Result a
result g e = fromEither (eval g e)

mkEnv :: [Defn a] -> CheckM a (Env a)
mkEnv = foldM expand M.empty

expand :: Env a -> Defn a -> CheckM a (Env a)
expand g (Defn b e) = case zs of
                        (x,l) : _ -> Left  (Unbound b x l)
                        []        -> Right (M.insert (bindId b) e' g)
  where
    e'              = subst e g
    zs              = M.toList (freeVars' e')

--------------------------------------------------------------------------------
type CheckM a b = Either (Result a) b
type Env a      = M.HashMap Id (Expr a)
type DBEnv      = M.HashMap String DBExpr
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
eval :: Env a -> Eval a -> CheckM a (Result a)
--------------------------------------------------------------------------------
eval g (Eval n e steps) = go e steps
  where
    go e []
      | isNormal g e    = return (OK n)
      | otherwise       = Left (errPartial n e)
    go e (s:steps)      = step g n e s >>= (`go` steps)

step :: Env a -> Bind a -> Expr a -> Step a -> CheckM a (Expr a)
step g n e (Step k e')
  | isEq k g e e' = return e'
  | otherwise     = Left (errInvalid n e k e')

isEq :: Eqn a -> Env a -> Expr a -> Expr a -> Bool
isEq (AlphEq _) = isAlphEq
isEq (BetaEq _) = isBetaEq
isEq (UnBeta _) = isUnBeta
isEq (DefnEq _) = isDefnEq
isEq (TrnsEq _) = isTrnsEq
isEq (UnTrEq _) = isUnTrEq
isEq (NormEq _) = isNormEq


--------------------------------------------------------------------------------
-- | Transitive Reachability
--------------------------------------------------------------------------------
isTrnsEq :: Env a -> Expr a -> Expr a -> Bool
isTrnsEq g e1 e2 = Mb.isJust (findTrans (isEquiv g e2) (canon g e1))

isTrnsEqFuel :: Env a -> Expr a -> Expr a -> Int -> Bool
isTrnsEqFuel g e1 e2 f =
  let test = (findTransFuel (isEquiv g e2) (canon g e1) f)
      ans = Mb.isJust test
   in ans

isUnTrEq :: Env a -> Expr a -> Expr a -> Bool
isUnTrEq g e1 e2 = isTrnsEq g e2 e1

findTrans :: (Expr a -> Bool) -> Expr a -> Maybe (Expr a)
findTrans p e = go S.empty (qInit e)
  where
    go seen q = do
      (e, q') <- qPop q
      if S.member e seen
        then go seen q'
        else if p e
             then return e
             else go (S.insert e seen) (qPushes q (betas e))

findTransFuel :: (Expr a -> Bool) -> Expr a -> Int -> Maybe (Expr a)
findTransFuel p e f = go S.empty (qInit e) f
  where
    go seen q f =
      if f > 0
         then do
           (e, q') <- qPop q
           if S.member e seen
              then go seen q' (f - 1)
              else if p e
                   then return e
                   else let
                        betasAns = (betas e)
                        insertAns = (S.insert e seen)
                        in
                     go insertAns (qPushes q betasAns) (f - 1)
         else
           Nothing


--------------------------------------------------------------------------------
-- | Definition Equivalence
--------------------------------------------------------------------------------
isDefnEq :: Env a -> Expr a -> Expr a -> Bool
isDefnEq g e1 e2 = subst e1 g == subst e2 g

--------------------------------------------------------------------------------
-- | Alpha Equivalence
--------------------------------------------------------------------------------
isAlphEq :: Env a -> Expr a -> Expr a -> Bool
isAlphEq _ e1 e2 = alphaNormal e1 == alphaNormal e2

alphaNormal :: Expr a -> Expr a
alphaNormal = alphaShift 0

alphaShift :: Int -> Expr a -> Expr a
alphaShift n e = evalState (normalize M.empty e) n

type AlphaM a = State Int a

normalize :: M.HashMap Id Id -> Expr a -> AlphaM (Expr a)
normalize g (EVar x z) =
  return (EVar (rename g x) z)

normalize g (EApp e1 e2 z) = do
  e1' <- normalize g e1
  e2' <- normalize g e2
  return (EApp e1' e2' z)

normalize g (ELam (Bind x z1) e z2) = do
  y     <- fresh
  let g' = M.insert x y g
  e'    <- normalize g' e
  return (ELam (Bind y z1) e' z2)

rename :: M.HashMap Id Id -> Id -> Id
rename g x = M.lookupDefault x x g

fresh :: AlphaM Id
fresh = do
  n <- get
  put (n + 1)
  return (newAId n)

newAId :: Int -> Id
newAId n = aId ++ show n

_isAId :: Id -> Maybe Int
_isAId x
  | L.isPrefixOf aId x = Just . read . drop 2 $ x
  | otherwise          = Nothing

aId :: String
aId = "$x"

--------------------------------------------------------------------------------
-- | Beta Reduction
--------------------------------------------------------------------------------
isBetaEq :: Env a -> Expr a -> Expr a -> Bool
isBetaEq _ e1 e2 = or [ e1' == e2  | e1' <- betas e1 ]

isUnBeta :: Env a -> Expr a -> Expr a -> Bool
isUnBeta g e1 e2 = isBetaEq g e2 e1

isNormal :: Env a -> Expr a -> Bool
isNormal g = null . betas . (`subst` g)

-- | `betas e` returns the list [e1,...en] of terms obtainable via a single-step
--   beta reduction from `e`.
betas :: Expr a -> [Expr a]
betas (EVar _ _)     = []
betas (ELam b e z)   = [ ELam b e' z | e' <- betas e ]
betas (EApp e1 e2 z) = [ EApp e1' e2 z | e1' <- betas e1 ]
                    ++ [ EApp e1 e2' z | e2' <- betas e2 ]
                    ++ Mb.maybeToList (beta e1 e2)

beta :: Expr a -> Expr a -> Maybe (Expr a)
beta (ELam (Bind x _) e _) e' = substCA e x e'
beta _                    _   = Nothing

substCA :: Expr a -> Id -> Expr a -> Maybe (Expr a)
substCA e x e'           = go [] e
  where
    zs                   = freeVars e'
    bnd  bs zs           = or [ b `isIn` zs | b <- bs ]
    go bs e@(EVar y _)
      | y /= x           = Just e            -- different var, no subst
      | bnd  bs zs       = Nothing           -- same var, but free-var-captured
      | otherwise        = Just e'           -- same var, but no capture
    go bs (EApp e1 e2 l) = do e1' <- go bs e1
                              e2' <- go bs e2
                              Just (EApp e1' e2' l)
    go bs e@(ELam b e1  l)
      | x == bindId b    = Just e            -- subst-var has been rebound
      | otherwise        = do e1' <- go (b:bs) e1
                              Just (ELam b e1' l)

isIn :: Bind a -> S.HashSet Id -> Bool
isIn = S.member . bindId

--------------------------------------------------------------------------------
-- | Evaluation to Normal Form
--   http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf
--------------------------------------------------------------------------------
isNormEq :: Env a -> Expr a -> Expr a -> Bool
isNormEq g e1 e2 = isEquiv g e1' e2
  where
    e1'          = traceShow ("evalNO" ++ show e1) $ evalNO (traceShow "CANON" $ canon g e1)

isDBNormEq :: DBEnv -> DBExpr -> DBExpr -> Bool
isDBNormEq g e1 e2 = evalDeBruijnNO (dbSubstName e1 g) == dbSubstName e2 g

isNormEqLimited :: Env a -> Expr a -> Expr a -> Int -> Bool
isNormEqLimited g e1 e2 fuel =
  case e1' of
    Just e1' -> isEquiv g e1' e2
    Nothing  -> False
  where
    e1'          = traceShow ("evalNO" ++ show e1) $ evalNOLimited (traceShow "CANON" $ canon g e1) fuel

isNormEqRunsOutOfFuel :: Env a -> Expr a -> Expr a -> Int -> Bool
isNormEqRunsOutOfFuel g e1 e2 fuel =
  case e1' of
    Just e1' -> False
    Nothing  -> True
  where
    e1'          = traceShow ("evalNO" ++ show e1) $ evalNOLimited (traceShow "CANON" $ canon g e1) fuel

evalNOLimited :: Expr a -> Int -> Maybe (Expr a)
evalNOLimited e@(EVar {})    _ = Just e
evalNOLimited (ELam b e l)   i = if i > 0
                             then case evalNOLimited e (i - 1) of
                               Just e' -> Just $ ELam b e' l
                               Nothing -> Nothing
                             else
                              Nothing
evalNOLimited (EApp e1 e2 l) i = if i > 0
                             then case evalCBNLimited e1 (i - 1) of
                               Just (ELam b e1' _) ->
                                 case (bSubstLimited e1' (bindId b) e2 (i - 1)) of
                                   Just e1'' -> evalNOLimited (alphaNormal e1'') (i - 1)
                                   Nothing -> Nothing
                               Just e1'            ->
                                 case (evalNOLimited e1' (i - 1)) of
                                   Just e1'' ->
                                     case (evalNOLimited e2 (i - 1)) of
                                       Just e2' -> Just $ EApp e1'' e2' l
                                       Nothing  -> Nothing
                                   Nothing   -> Nothing
                               Nothing -> Nothing
                             else
                               Nothing

data DBExpr
  = DBLam DBExpr
  | DBApp DBExpr DBExpr
  | DBVar Int
  | DBName String
  deriving Eq

instance Show DBExpr where
  show (DBVar idx) = show idx
  show (DBApp e1 e2   ) = printf "(%s %s)" (show e1) (show e2)
  show (DBLam e       ) = printf "(λ.%s)" (show e)
  show (DBName s      ) = s

liftDB :: DBExpr -> Int -> Int -> DBExpr
liftDB (DBLam e1) k level = DBLam $ liftDB e1 k (level + 1)
liftDB (DBApp e1 e2) k level = DBApp (liftDB e1 k level) (liftDB e2 k level)
liftDB (DBVar i) k level = if i < level then (DBVar i) else (DBVar $ i + k)
liftDB e@(DBName _) _ _= e

-- | Substitute for one application (replace top-level binding)
dbSubst :: DBExpr -> DBExpr -> Int -> DBExpr
dbSubst (DBLam e1) e2 k = DBLam $ dbSubst e1 e2 (k + 1)
dbSubst (DBApp e1 e2) e3 k = DBApp (dbSubst e1 e3 k) (dbSubst e2 e3 k)
dbSubst (DBVar i) e k = if
                        | i == k -> (liftDB e i 0)
                        | i > k -> (DBVar (i - 1))
                        | otherwise -> (DBVar i)
dbSubst e@(DBName _) _ _= e

evalDeBruijnNO :: DBExpr -> DBExpr
evalDeBruijnNO e@(DBVar {}) = e
evalDeBruijnNO (DBLam e) = DBLam $ evalDeBruijnNO e
evalDeBruijnNO (DBApp e1 e2) = case evalDeBruijn e1 of
                          DBLam e1' -> evalDeBruijnNO (dbSubst e1' e2 0)
                          e1'       -> DBApp (evalDeBruijnNO e1') (evalDeBruijnNO e2)
evalDeBurijnNO e@(DBName _) = e

-- | normal-order reduction
evalNO :: Expr a -> Expr a
evalNO e@(EVar {})    = e
evalNO (ELam b e l)   = ELam b (evalNO e) l
evalNO (EApp e1 e2 l) = case evalCBN e1 of
                          ELam b e1' _ -> evalNO (alphaNormal (bSubst e1' (bindId b) e2))
                          e1'          -> EApp (evalNO e1') (evalNO e2) l

evalCBNLimited :: Expr a -> Int -> Maybe (Expr a)
evalCBNLimited e@(EVar {}) _ = Just e
evalCBNLimited e@(ELam {}) _ = Just e
evalCBNLimited (EApp e1 e2 l) i = if i > 0
                              then case evalCBNLimited e1 (i - 1) of
                                Just (ELam b e1' _) ->
                                  case (bSubstLimited e1' (bindId b) e2 (i - 1)) of
                                    Just e1'' -> evalCBNLimited (alphaNormal e1'') (i - 1)
                                    Nothing -> Nothing
                                Just e1'            -> Just (EApp e1' e2 l)
                                Nothing -> Nothing
                              else Nothing

evalDeBruijn :: DBExpr -> DBExpr
evalDeBruijn e@(DBVar {}) = e
evalDeBruijn e@(DBLam {}) = e
evalDeBruijn (DBApp e1 e2) = case evalDeBruijn e1 of
                          DBLam e1' -> evalDeBruijn (dbSubst e1' e2 0)
                          e1'          -> DBApp e1' e2
evalDeBruijn e@(DBName _) = e

-- | call-by-name reduction
evalCBN :: Expr a -> Expr a
evalCBN e@(EVar {}) = e
evalCBN e@(ELam {}) = e
evalCBN (EApp e1 e2 l) = case evalCBN e1 of
                          ELam b e1' _ -> evalCBN (alphaNormal (bSubst e1' (bindId b) e2))
                          e1'          -> EApp e1' e2 l

bSubstLimited :: Expr a -> Id -> Expr a -> Int -> Maybe (Expr a)
bSubstLimited e x e' i = if i > 0
                            then substLimited e (M.singleton x e'') (i - 1)
                            else Nothing
  where
    e''                = e' -- alphaShift n e'

-- | Force alpha-renaming to ensure capture avoiding subst
bSubst :: Expr a -> Id -> Expr a -> Expr a
bSubst e x e' = subst e (M.singleton x e'')
  where
    e''       = e' -- alphaShift n e'

--------------------------------------------------------------------------------
-- | General Helpers
--------------------------------------------------------------------------------
freeVars :: Expr a -> S.HashSet Id
freeVars = S.fromList . M.keys . freeVars'

freeVars' :: Expr a -> M.HashMap Id a
freeVars' (EVar x l)    = M.singleton x l
freeVars' (ELam b e _)  = M.delete (bindId b)    (freeVars' e)
freeVars' (EApp e e' _) = M.union  (freeVars' e) (freeVars' e')

substLimited :: Expr a -> Env a -> Int -> Maybe (Expr a)
substLimited e@(EVar v _)   su _ = Just (M.lookupDefault e v su)
substLimited (EApp e1 e2 z) su i =
  if i > 0
     then case (substLimited e1 su (i - 1)) of
            Just e1' ->
              case (substLimited e2 su (i - 1)) of
                Just e2' -> Just (EApp e1' e2' z)
                Nothing  -> Nothing
            Nothing  -> Nothing
     else
       Nothing
substLimited (ELam b e z)   su i =
  if i > 0
     then case (substLimited e su' (i - 1)) of
            Just e' -> Just (ELam b e' z)
            Nothing -> Nothing
     else
       Nothing
  where
    su'                 = M.delete (bindId b) su

subst :: Expr a -> Env a -> Expr a
subst e@(EVar v _)   su = M.lookupDefault e v su
subst (EApp e1 e2 z) su = EApp (subst e1 su) (subst e2 su) z
subst (ELam b e z)   su = ELam b (subst e su') z
  where
    su'                 = M.delete (bindId b) su

dbSubstName :: DBExpr -> DBEnv -> DBExpr
dbSubstName e@(DBVar _)   su = e
dbSubstName (DBApp e1 e2) su = DBApp (dbSubstName e1 su) (dbSubstName e2 su)
dbSubstName (DBLam e)     su = DBLam (dbSubstName e su)
dbSubstName e@(DBName s)  su = M.lookupDefault e s su

canon :: Env a -> Expr a -> Expr  a
canon g = alphaNormal . (`subst` g)

isEquiv :: Env a -> Expr a -> Expr a -> Bool
isEquiv g e1 e2 = isAlphEq g (subst e1 g) (subst e2 g)
--------------------------------------------------------------------------------
-- | Error Cases
--------------------------------------------------------------------------------

errInvalid :: Bind a -> Expr a -> Eqn a -> Expr a -> Result a
errInvalid b _ eqn _ = Invalid b (tag eqn)

errPartial :: Bind a -> Expr a -> Result a
errPartial b e = Partial b (tag e)
