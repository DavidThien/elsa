{-# LANGUAGE FlexibleInstances #-}

module Language.Elsa.Synthesis where

import qualified Data.HashMap.Strict           as M
import qualified Data.Stream.Infinite          as S
import           Language.Elsa.Enumeration
import qualified Language.Elsa.LocallyNameless as LN
-- import qualified Language.Elsa.Types           as N
-- import qualified Language.Elsa.Eval            as N

---------------------------------------------------------------------------------
-- | Synthesis
---------------------------------------------------------------------------------

type Spec e = [(e, e)]
type Env e = M.HashMap String e

class Synthesizable e where
  fromHExpr :: Int -> HExpr -> [e]
  checkEq :: Env e -> e -> e -> Bool

synthesize :: Synthesizable e => Env e -> Spec e -> Int -> Int -> [e]
synthesize env spec max vars =
  let terms = S.take max $ S.concat $ fmap (fromHExpr 0 . prependLambdas vars) bottomUpStream
  in  filter (testSpec env spec) terms

testSpec :: Synthesizable e => Env e -> Spec e -> e -> Bool
testSpec env spec expr = foldr folder True spec
 where
  env' = M.insert "test" expr env
  folder (i, o) b = b && checkEq env' i o

instance Synthesizable LN.Expr where
  fromHExpr n (HLam expr) = [ LN.ELam x | x <- fromHExpr (n + 1) expr ]
  fromHExpr n (HApp expr1 expr2) =
    [ LN.EApp x1 x2 | x1 <- fromHExpr n expr1, x2 <- fromHExpr n expr2 ]
  fromHExpr n HHole = [ LN.EBVar x | x <- [0 .. n - 1] ]

  -- checkEq env e1 e2 = LN.isNormEqUpTo 10 env e1 e2 == LN.NormEqTrue
  checkEq = LN.isNormEq

-- This is not working because N.isNormEq doesn't do alpha renaming while evaluating
-- instance Synthesizable (N.Expr ()) where
--   fromHExpr n (HLam expr) =
--     [ N.ELam (N.Bind ("x" ++ show n) ()) x () | x <- fromHExpr (n + 1) expr ]
--   fromHExpr n (HApp expr1 expr2) =
--     [ N.EApp x1 x2 () | x1 <- fromHExpr n expr1, x2 <- fromHExpr n expr2 ]
--   fromHExpr n HHole = [ N.EVar ("x" ++ show x) () | x <- [0 .. n - 1] ]

--   checkEq = N.isNormEq

prependLambdas :: Int -> HExpr -> HExpr
prependLambdas 0 expr = expr
prependLambdas n expr = prependLambdas (n - 1) (HLam expr)
