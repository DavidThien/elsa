{-# Language MultiWayIf #-}

module Language.Elsa.LocallyNameless where

import qualified Data.HashMap.Strict           as M
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
-- | Grammar
--------------------------------------------------------------------------------

data Expr
  = ELam Expr
  | EApp Expr Expr
  | EBVar Int
  | EFVar String
  deriving Eq

instance Show Expr where
  show (EFVar s   ) = s
  show (EBVar idx ) = show idx
  show (EApp e1 e2) = printf "(%s %s)" (show e1) (show e2)
  show (ELam e    ) = printf "(λ.%s)" (show e)

type Env = M.HashMap String Expr

--------------------------------------------------------------------------------
-- | Evaluation
--------------------------------------------------------------------------------

isNormEq :: Env -> Expr -> Expr -> Bool
isNormEq g e1 e2 = evalNO (subst e1 g) == subst e2 g

evalCBN :: Expr -> Expr
evalCBN e@EFVar{}    = e
evalCBN e@EBVar{}    = e
evalCBN e@ELam{}     = e
evalCBN (EApp e1 e2) = case evalCBN e1 of
  ELam e1' -> evalCBN (open e1' e2 0)
  e1'      -> EApp e1' e2

evalNO :: Expr -> Expr
evalNO e@EBVar{}    = e
evalNO e@EFVar{}    = e
evalNO (ELam e    ) = ELam $ evalNO e
evalNO (EApp e1 e2) = case evalCBN e1 of
  ELam e1' -> evalNO (open e1' e2 0)
  e1'      -> EApp (evalNO e1') (evalNO e2)

--------------------------------------------------------------------------------
-- | Substitution
--------------------------------------------------------------------------------

lift :: Expr -> Int -> Int -> Expr
lift (EBVar i)    k nBinders = if i < nBinders then EBVar i else EBVar $ i + k
lift e@EFVar{}    _ _        = e
lift (ELam e1   ) k nBinders = ELam $ lift e1 k (nBinders + 1)
lift (EApp e1 e2) k nBinders = EApp (lift e1 k nBinders) (lift e2 k nBinders)

open :: Expr -> Expr -> Int -> Expr
open (ELam e1   ) e2 k = ELam $ open e1 e2 (k + 1)
open (EApp e1 e2) e3 k = EApp (open e1 e3 k) (open e2 e3 k)
open (EBVar i   ) e  k = if
  | i == k    -> (lift e i 0)
  | i > k     -> (EBVar (i - 1))
  | otherwise -> (EBVar i)
open e@EFVar{} _ _ = e

subst :: Expr -> Env -> Expr
subst e@EBVar{}      _  = e
subst e@(EFVar s   ) su = M.lookupDefault e s su
subst (  EApp e1 e2) su = EApp (subst e1 su) (subst e2 su)
subst (  ELam e    ) su = ELam (subst e su)
