module Language.Elsa.Synthesis where

import qualified Data.HashMap.Strict           as M
import qualified Data.Stream.Infinite          as S
import qualified Language.Elsa.Types           as N
import qualified Language.Elsa.Eval            as N
import           Language.Elsa.Encodings
import           Language.Elsa.Enumeration
import qualified Language.Elsa.LocallyNameless as LN

-- N  := named
-- LN := locally nameless

type NExpr = N.Expr ()

hexprToExpr :: HExpr -> Int -> Maybe NExpr
hexprToExpr (HLam expr) n = case hexprToExpr expr (n + 1) of
  Just x  -> Just $ N.ELam (N.Bind (show n) ()) x ()
  Nothing -> Nothing
hexprToExpr (HApp expr1 expr2) n = case hexprToExpr expr1 n of
  Just x1 -> case hexprToExpr expr2 n of
    Just x2 -> Just $ N.EApp x1 x2 ()
    Nothing -> Nothing
  Nothing -> Nothing
hexprToExpr (HDeBruijn i) n = Just $ N.EVar (show $ (n - i) - 1) ()
hexprToExpr HHole         _ = Nothing

aexprToExpr :: Int -> AExpr -> [NExpr]
aexprToExpr n (ALam expr) =
  [ N.ELam (N.Bind ("x" ++ show n) ()) x () | x <- aexprToExpr (n + 1) expr ]
aexprToExpr n (AApp expr1 expr2) =
  [ N.EApp x1 x2 () | x1 <- aexprToExpr n expr1, x2 <- aexprToExpr n expr2 ]
aexprToExpr n AHole = [ N.EVar ("x" ++ show x) () | x <- [0 .. n - 1] ]

aexprToLNExpr :: Int -> AExpr -> [LN.Expr]
aexprToLNExpr n (ALam expr) = [ LN.ELam x | x <- aexprToLNExpr (n + 1) expr ]
aexprToLNExpr n (AApp expr1 expr2) =
  [ LN.EApp x1 x2 | x1 <- aexprToLNExpr n expr1, x2 <- aexprToLNExpr n expr2 ]
aexprToLNExpr n AHole = [ LN.EBVar x | x <- [0 .. n - 1] ]

prependLambdas 0 expr = expr
prependLambdas n expr = prependLambdas (n - 1) (ALam expr)

---------------------------------------------------------------------------------
-- | Synthesis
---------------------------------------------------------------------------------

-- | Takes a list of input/output expressions where the input expression
-- | has the free variable 'test' where the synthesized test expressions will
-- | be inserted, and a number of iterations to run for. The synthesizer will
-- | check to see if each input expression is transitively equal to the
-- | output, and will return the first expression it finds that satisfies
-- | all the input/output examples.
-- TODO: infer the number of variables for a generated function
synthesizeEncoding :: [(NExpr, NExpr)] -> Int -> Int -> [NExpr]
synthesizeEncoding examples max vars =
  let terms = S.take max $ fmap N.alphaNormal $ S.concat $ fmap
        (aexprToExpr 0 . prependLambdas vars)
        bottomUpStream
  in  filter (testExamples examples) terms

testExamples :: [(NExpr, NExpr )] -> NExpr -> Bool
testExamples examples expr =
  let env = M.singleton "test" expr
      foldFunction (i, o) b = b && N.isNormEq env i o
  in  foldr foldFunction True examples
