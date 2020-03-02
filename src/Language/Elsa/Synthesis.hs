module Language.Elsa.Synthesis where

import qualified Data.HashMap.Strict           as M
import           Data.Maybe
import           Data.Sequence                 as Seq
import           Text.Printf                    ( printf )
import           Language.Elsa.Types
import           Language.Elsa.Eval
import           Language.Elsa.Encodings

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
hexprToExpr (HLam expr) n = do
  case (hexprToExpr expr (n + 1)) of
    Just x -> Just $ ELam (Bind (show n) 0) x 0
    Nothing -> Nothing
hexprToExpr (HApp expr1 expr2) n = do
  case (hexprToExpr expr1 (n + 1)) of
    Just x1 -> do
      case (hexprToExpr expr2 (n + 1)) of
        Just x2 -> Just $ EApp x1 x2 0
        Nothing -> Nothing
    Nothing -> Nothing
hexprToExpr (HDeBruijn i) _ = Just $ EVar (show i) 0
hexprToExpr HHole _ = Nothing

-- | Evaluates an HExpr to its Expr normal form
hexprNO :: HExpr -> Maybe (Expr Int)
hexprNO e = evalNO <$> hexprToExpr e 1

-- | Enumerates closed term in normal form. It basically enumerates terms in the
-- | following grammar always picking indices in scope:
-- | E := λ.E | idx E | idx
topDown numIt _ accum | numIt <= 0 = accum
topDown numIt queue accum          = case queue of
  p :<| queue ->
    let unrolled = unrollWith expansion p
        ground   = unrollWith deBruijnsUpTo p
    in  topDown (numIt - 1) (foldl (|>) queue unrolled) (accum ++ ground)
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

bottomUp 0 indices = indices
bottomUp level indices =
  [ HLam e | e <- bottomUp (level - 1) nextIndices ]
    ++ [ HApp e1 e2 | e1 <- Prelude.reverse indices, e2 <- bottomUp (level - 1) indices ]
  where nextIndices = (HDeBruijn $ Prelude.length indices + 1) : indices

testExamples :: [(Expr Int, Expr Int)] -> Expr Int -> Bool
testExamples examples expr =
  let env = M.singleton "test" expr
      foldFunction = (\(i, o) b -> b && (isTrnsEq env i o))
  in
      foldr foldFunction True examples

-- | Takes a list of input/output expressions where the input expression
-- | has the free variable 'test' where the synthesized test expressions will
-- | be inserted, and a number of iterations to run for. The synthesizer will
-- | check to see if each input expression is transitively equal to the
-- | output, and will return the first expression it finds that satifisfies
-- | all the input/output examples.
synthesizeEncoding :: [(Expr Int, Expr Int)] -> Int -> Maybe (Expr Int)
synthesizeEncoding examples max = synthesizeEncodingHelper examples 0 max

synthesizeEncodingHelper :: [(Expr Int, Expr Int)] -> Int -> Int -> Maybe (Expr Int)
synthesizeEncodingHelper examples n max =
  let hexprs = bottomUp n []
      exprs = catMaybes $ map (\x -> hexprToExpr x 1) hexprs
      satisfyExamples = map (testExamples examples) exprs
  in
      case elemIndexL True (fromList satisfyExamples) of
        Just i  -> Just (exprs !! i)
        Nothing -> if n >= max
                     then Nothing
                     else synthesizeEncodingHelper examples (n + 1) max
