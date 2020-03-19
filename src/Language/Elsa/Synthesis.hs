{-# LANGUAGE FlexibleInstances #-}

module Language.Elsa.Synthesis where

import qualified Data.HashMap.Strict           as M
import           Text.Printf                    ( printf )
import qualified Data.Stream.Infinite          as S
import           Language.Elsa.Enumeration
import qualified Language.Elsa.LocallyNameless as LN
-- import qualified Language.Elsa.Types           as N
-- import qualified Language.Elsa.Eval            as N
import           Control.Monad.Logic

---------------------------------------------------------------------------------
-- | Synthesis
---------------------------------------------------------------------------------

-- type Spec e = [(e, e)]
type Env e = M.HashMap String e

class Synthesizable e where
  fromHExpr :: Int -> HExpr -> [e]
  checkEq :: Bool -> Env e -> e -> e -> Bool

exprStream :: Synthesizable e => Int -> S.Stream e
exprStream vars = S.concat $ fmap (fromHExpr 0 . prependLambdas vars) bottomUpStream

synthesize env spec max = filter (testSpec env spec . pure) exprs
 where
  vars  = snd . head $ specTargets spec
  exprs = S.take max $ exprStream vars

coSynthesize env spec max = filter (testSpec env spec) tuples
 where
  vars    = map snd (specTargets spec)
  streams = map exprStream vars :: [S.Stream LN.Expr]
  tuples  = observeMany max $ fairTuples $ map (msum . fmap return) streams

testSpec :: Synthesizable e => Env e -> Spec e -> [e] -> Bool
testSpec env spec exprs = foldr folder True examples
 where
  examples    = specExamples spec
  targetNames = map fst (specTargets spec)
  env'        = foldr (uncurry M.insert) env (zip targetNames exprs)
  folder (i, o) b = b && checkEq (specEtaEq spec) env' i o

instance Synthesizable LN.Expr where
  fromHExpr n (HLam expr) = [ LN.ELam x | x <- fromHExpr (n + 1) expr ]
  fromHExpr n (HApp expr1 expr2) =
    [ LN.EApp x1 x2 | x1 <- fromHExpr n expr1, x2 <- fromHExpr n expr2 ]
  fromHExpr n HHole = [ LN.EBVar x | x <- [0 .. n - 1] ]

  checkEq True  env e1 e2 = LN.isEtaBetaEq 10 env e1 e2 == Just True
  checkEq False env e1 e2 = LN.evaluatesTo 10 env e1 e2 == Just True

prependLambdas :: Int -> HExpr -> HExpr
prependLambdas 0 expr = expr
prependLambdas n expr = prependLambdas (n - 1) (HLam expr)

---------------------------------------------------------------------------------
-- | Spec
---------------------------------------------------------------------------------

data Spec e = Spec { specTargets :: [(String, Int)], specExamples :: [(e, e)], specEtaEq :: Bool }

instance Show e => Show (Spec e) where
  show (Spec targets examples _) = printf "Spec {\n  targets  = %s,\n  examples = %s\n}"
                                          (show targets)
                                          (fmtExamples examples)
   where
    fmtExample (i, o) = printf "             , (%s, %s)\n" (show i) (show o)
    fmtExamples []       = "[]"
    fmtExamples [(i, o)] = printf "[ (%s, %s) ]" (show i) (show o)
    fmtExamples ((i, o) : xs) =
      printf "[ (%s, %s)\n" (show i) (show o)
        ++ foldr (\ex acc -> fmtExample ex ++ acc) "" xs
        ++ "             ]"

