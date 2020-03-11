module Language.Elsa.Synthesis where

import qualified Data.HashMap.Strict           as M
import qualified Data.Stream.Infinite          as S
import qualified Data.Sequence                 as Seq
import           Text.Printf                    ( printf )
import           Language.Elsa.Types
import           Language.Elsa.Eval
import           Language.Elsa.Encodings

---------------------------------------------------------------------------------
-- | Top-down enumeration
---------------------------------------------------------------------------------

-- | Lambda expressions with a hole. We use De Bruijn indices because it makes it
-- | easier to keep track of the binders in scope when enumerating closed terms.
data HExpr
  = HLam HExpr
  | HApp HExpr HExpr
  | HDeBruijn Int
  | HHole
  deriving Eq

instance Show HExpr where
  show (HDeBruijn idx) = show idx
  show (HApp e1 e2   ) = printf "(%s %s)" (show e1) (show e2)
  show (HLam e       ) = printf "(λ.%s)" (show e)
  show HHole           = "□"

hexprToExpr :: HExpr -> Int -> Maybe (Expr Int)
hexprToExpr (HLam expr) n = case hexprToExpr expr (n + 1) of
  Just x  -> Just $ ELam (Bind (show n) 0) x 0
  Nothing -> Nothing
hexprToExpr (HApp expr1 expr2) n = case hexprToExpr expr1 n of
  Just x1 -> case hexprToExpr expr2 n of
    Just x2 -> Just $ EApp x1 x2 0
    Nothing -> Nothing
  Nothing -> Nothing
hexprToExpr (HDeBruijn i) n = Just $ EVar (show $ (n - i) - 1) 0
hexprToExpr HHole         _ = Nothing

-- | Evaluates an HExpr to its Expr normal form
hexprNO :: HExpr -> Maybe (Expr Int)
hexprNO e = evalNO <$> hexprToExpr e 0

-- | Enumerates closed term in normal form. It basically enumerates terms in the
-- | following grammar always picking indices in scope:
-- | E := λ.E | idx E | idx
topDown numIt _ accum | numIt <= 0 = accum
topDown numIt queue accum          = case queue of
  p Seq.:<| queue ->
    let unrolled = unrollWith expansion p
        ground   = unrollWith deBruijnsUpTo p
    in  topDown (numIt - 1) (foldl (Seq.|>) queue unrolled) (accum ++ ground)
  Seq.Empty -> accum

qHole = Seq.fromList [HHole]

unrollWith :: (Int -> [HExpr]) -> HExpr -> [HExpr]
unrollWith expand = unroll 0
 where
  unroll nBinders HHole          = expand nBinders
  unroll nBinders (HLam e      ) = [ HLam e' | e' <- unroll (nBinders + 1) e ]
  unroll nBinders (HApp e1 e2  ) = [ HApp e1 e2 | e2 <- unroll nBinders e2 ]
  unroll _        (HDeBruijn id) = [HDeBruijn id]

deBruijnsUpTo :: Int -> [HExpr]
deBruijnsUpTo maxIdx = [ HDeBruijn idx | idx <- [1 .. maxIdx] ]

expansion :: Int -> [HExpr]
expansion maxIdx = HLam HHole : [ HApp idx HHole | idx <- deBruijnsUpTo maxIdx ]

---------------------------------------------------------------------------------
-- | Bottom-up enumeration
---------------------------------------------------------------------------------

data AExpr
  = ALam AExpr
  | AApp AExpr AExpr
  | AHole
  deriving Eq

instance Show AExpr where
  show (AApp e1 e2) = printf "(%s %s)" (show e1) (show e2)
  show (ALam e    ) = printf "(λ.%s)" (show e)
  show AHole        = "□"

data Exprs = Exprs { lambdaExprs :: [AExpr], appExprs :: [AExpr] }

allExprs :: Exprs -> [AExpr]
allExprs (Exprs lambdas apps) = lambdas ++ apps

(++.) :: Exprs -> Exprs -> Exprs
(Exprs lambdas1 apps1) ++. (Exprs lambdas2 apps2) =
  Exprs (lambdas1 ++ lambdas2) (apps1 ++ apps2)

grow exprs_2 exprs_1 = Exprs nextLambdas nextApps
 where
  nextLambdas = [ ALam e | e <- allExprs exprs_1 ]
  nextApps =
    [ AApp e1 e2 | e1 <- appExprs exprs_2, e2 <- allExprs exprs_1 ]
      ++ [ AApp e1 e2 | e1 <- appExprs exprs_1, e2 <- allExprs exprs_1 ]
      ++ [ AApp e1 e2 | e1 <- appExprs exprs_1, e2 <- allExprs exprs_2 ]

exprStream = S.prepend [AHole] $ S.concat $ S.unfold unfolder init
 where
  init = (Exprs [] [], Exprs [] [AHole])
  unfolder (exprs_2, exprs_1) =
    let exprs = grow exprs_2 exprs_1 in (allExprs exprs, (exprs_2 ++. exprs_1, exprs))

aexprToExpr :: Int -> AExpr -> [Expr ()]
aexprToExpr n (ALam expr) = [ ELam (Bind ("x" ++ show n) ()) x () | x <- aexprToExpr (n + 1) expr ]
aexprToExpr n (AApp expr1 expr2) =
  [ EApp x1 x2 () | x1 <- aexprToExpr n expr1, x2 <- aexprToExpr n expr2 ]
aexprToExpr n AHole = [ EVar ("x" ++ show x) () | x <- [0 .. n - 1] ]

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
synthesizeEncoding :: [(Expr (), Expr ())] -> Int -> Int -> [Expr ()]
synthesizeEncoding examples max vars =
  let terms = S.take max $ fmap alphaNormal $ S.concat $ fmap (aexprToExpr 0 . prependLambdas vars) exprStream
  in  filter (testExamples examples) terms

synthesisTestOutOfFuel :: [(Expr (), Expr ())] -> Int -> Int -> [Expr ()]
synthesisTestOutOfFuel examples max vars =
  let terms = S.take max $ fmap alphaNormal $ S.concat $ fmap (aexprToExpr 0 . prependLambdas vars) exprStream
  in  filter (collectLongExamples examples) terms

testExamples :: [(Expr (), Expr ())] -> Expr () -> Bool
testExamples examples expr =
  let env = M.singleton "test" expr
      foldFunction (i, o) b = b && isNormEq env i o
  in  foldr foldFunction True examples

collectLongExamples :: [(Expr (), Expr ())] -> Expr () -> Bool
collectLongExamples examples expr =
  let env = M.singleton "test" expr
      foldFunction (i, o) b = b || isNormEqRunsOutOfFuel env (alphaNormal i) (alphaNormal o) 20
  in  foldr foldFunction False examples
