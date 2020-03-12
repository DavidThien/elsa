module Language.Elsa.Enumeration where

-- import qualified Data.Sequence                 as Seq
import qualified Data.Stream.Infinite          as S
import           Text.Printf                    ( printf )
import           Control.Monad.Logic

-- | Expressions with a hole. They don't have variables because we first enumerate
-- | expressions with holes instead of variables and then we fill in the holes with
-- | all possible variables in scope.
data HExpr
  = HLam HExpr
  | HApp HExpr HExpr
  | HHole
  deriving Eq

instance Show HExpr where
  show (HApp e1 e2) = printf "(%s %s)" (show e1) (show e2)
  show (HLam e    ) = printf "(λ.%s)" (show e)
  show HHole        = "□"

data Exprs = Exprs { lambdaExprs :: [HExpr], appExprs :: [HExpr] }

allExprs :: Exprs -> [HExpr]
allExprs (Exprs lambdas apps) = lambdas ++ apps

---------------------------------------------------------------------------------
-- | Bottom-up enumeration
---------------------------------------------------------------------------------

(++.) :: Exprs -> Exprs -> Exprs
(Exprs lambdas1 apps1) ++. (Exprs lambdas2 apps2) = Exprs (lambdas1 ++ lambdas2) (apps1 ++ apps2)

grow exprs_2 exprs_1 = Exprs nextLambdas nextApps
 where
  nextLambdas = [ HLam e | e <- allExprs exprs_1 ]
  nextApps =
    [ HApp e1 e2 | e1 <- appExprs exprs_2, e2 <- allExprs exprs_1 ]
      ++ [ HApp e1 e2 | e1 <- appExprs exprs_1, e2 <- allExprs exprs_1 ]
      ++ [ HApp e1 e2 | e1 <- appExprs exprs_1, e2 <- allExprs exprs_2 ]

bottomUpStream :: S.Stream HExpr
bottomUpStream = S.prepend [HHole] $ S.concat $ S.unfold unfolder init
 where
  init = (Exprs [] [], Exprs [] [HHole])
  unfolder (exprs_2, exprs_1) =
    let exprs = grow exprs_2 exprs_1 in (allExprs exprs, (exprs_2 ++. exprs_1, exprs))


-- ---------------------------------------------------------------------------------
-- -- | Top-down enumeration
-- ---------------------------------------------------------------------------------

-- topDown numIt _ accum | numIt <= 0 = accum
-- topDown numIt queue accum          = case queue of
--   p Seq.:<| queue ->
--     let unrolled = unroll p
--     in  topDown (numIt - 1) (foldl (Seq.|>) queue unrolled) (accum ++ unrolled)
--   Seq.Empty -> accum

-- qHole = Seq.fromList [HHole]

-- unroll :: HExpr -> [HExpr]
-- unroll HHole          = [HLam HHole, HApp HHole HHole]
-- unroll (HLam e      ) = [ HLam e' | e' <- unroll e ]
-- unroll (HApp e1 e2  ) = [ HApp e1 e2 | e2 <- unroll e2 ]

