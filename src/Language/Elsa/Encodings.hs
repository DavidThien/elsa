module Language.Elsa.Encodings where

import           Language.Elsa.Types

churchTrue = ELam (Bind "a" 0) (ELam (Bind "b" 0) (EVar "a" 0) 0) 0 :: Expr Int
churchFalse = ELam (Bind "a" 0) (ELam (Bind "b" 0) (EVar "b" 0) 0) 0 :: Expr Int

churchNot = ELam (Bind "p" 0) (EApp (EApp (EVar "p" 0) churchFalse 0) churchTrue 0) 0 :: Expr Int
churchAnd = ELam (Bind "p" 0) (ELam (Bind "q" 0) (EApp (EApp (EVar "p" 0) (EVar "q" 0) 0) (EVar "p" 0) 0) 0) 0 :: Expr Int
churchOr = ELam (Bind "p" 0) (ELam (Bind "q" 0) (EApp (EApp (EVar "p" 0) (EVar "p" 0) 0) (EVar "q" 0) 0) 0) 0 :: Expr Int
