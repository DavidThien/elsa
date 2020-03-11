module Language.Elsa.Enumeration
  ( bottomUpStream
  , AExpr(..)
  , HExpr(..)
  )
where

import qualified Data.Sequence                 as Seq
import qualified Data.Stream.Infinite          as S
import           Text.Printf                    ( printf )

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
(Exprs lambdas1 apps1) ++. (Exprs lambdas2 apps2) = Exprs (lambdas1 ++ lambdas2) (apps1 ++ apps2)

grow exprs_2 exprs_1 = Exprs nextLambdas nextApps
 where
  nextLambdas = [ ALam e | e <- allExprs exprs_1 ]
  nextApps =
    [ AApp e1 e2 | e1 <- appExprs exprs_2, e2 <- allExprs exprs_1 ]
      ++ [ AApp e1 e2 | e1 <- appExprs exprs_1, e2 <- allExprs exprs_1 ]
      ++ [ AApp e1 e2 | e1 <- appExprs exprs_1, e2 <- allExprs exprs_2 ]

bottomUpStream = S.prepend [AHole] $ S.concat $ S.unfold unfolder init
 where
  init = (Exprs [] [], Exprs [] [AHole])
  unfolder (exprs_2, exprs_1) =
    let exprs = grow exprs_2 exprs_1 in (allExprs exprs, (exprs_2 ++. exprs_1, exprs))
