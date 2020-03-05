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

-- data Bindings = Bindings { binders :: Int, maxVar :: Int }

-- data Exprs = Exprs { lambdas :: [Expr Bindings], apps :: [Expr Bindings] }

-- append (Exprs lambdas1 apps1) (Exprs lambdas2 apps2) = Exprs (lambdas1 ++ lambdas2) (apps1 ++ apps2)

-- newBind :: Int -> Bind Bindings
-- newBind n = Bind ("x" ++ show n) (Bindings 0 0)

-- newLambda :: Expr Bindings -> Maybe (Expr Bindings)
-- newLambda e = if binders > max
--   then Nothing
--   else Just $ ELam (newBind binders) e (Bindings (binders + 1) max)
--   where Bindings binders max = exprData e

-- newApp :: Expr Bindings -> Expr Bindings -> Expr Bindings
-- newApp e1 e2 = EApp e1 e2 $ Bindings (max binders1 binders2) (max maxVar1 maxVar2)
--  where
--   Bindings binders1 maxVar1 = exprData e1
--   Bindings binders2 maxVar2 = exprData e2

-- grow :: Exprs -> Exprs -> Exprs
-- grow exprs exprs' = Exprs nextLambdaExprs nextAppExprs
--  where
--   nextLambdaExprs = [ lam | e <- lambdas exprs' ++ apps exprs', lam <- maybeToList $ newLambda e ]
--   nextAppExprs =
--     [ newApp e1 e2 | e1 <- apps exprs, e2 <- lambdas exprs' ++ apps exprs' ]
--       ++ [ newApp e1 e2 | e1 <- apps exprs', e2 <- lambdas exprs ++ apps exprs ]
--       ++ [ newApp e1 e2 | e1 <- apps exprs', e2 <- lambdas exprs' ++ apps exprs' ]

-- isClose e = binders > maxVar
--   where Bindings binders maxVar = exprData e

-- exprsStream n = S.concat $ S.unfold unfolder initial
--   where
--     vars = [EVar ("x" ++ show n) $ Bindings 0 n | n <- 00..n]
--     initial = (Exprs [] [], Exprs [] [EVar "x0" $ Bindings 0 0])
--     unfolder (exprs, exprs') = let exprs'' = grow exprs exprs' in (filter isClose $ lambdas exprs'' ++ apps exprs'', (append exprs exprs', exprs''))

-- bottomUp 0 indices = indices
-- bottomUp level indices =
--   [ HLam e | e <- bottomUp (level - 1) nextIndices ]
--     ++ [ HApp e1 e2 | e1 <- nextLevelCurrIndices, e2 <- nextLevelCurrIndices ]
--     ++ indices
--  where
--   nextIndices          = (HDeBruijn $ Prelude.length indices) : indices
--   nextLevelCurrIndices = bottomUp (level - 1) indices

newBind n = Bind ("x" ++ show n) ()

bottomUp 0 _ = []
bottomUp depth nBinders =
  [ ELam (newBind nBinders) e ()
  | e <- bottomUp (depth - 1) (nBinders + 1) ++ bottomUpT (depth - 1) (nBinders + 1)
  ]

bottomUpT 0 _        = []
bottomUpT 1 nBinders = [ EVar ("x" ++ show n) () | n <- [0 .. nBinders - 1] ]
bottomUpT depth nBinders =
  [ EApp e1 e2 ()
  | e1 <- bottomUpT 1 nBinders ++ if depth == 2 then [] else bottomUpT (depth - 1) nBinders
  , e2 <- bottomUpT (depth - 1) nBinders ++ bottomUp (depth - 1) nBinders
  ]

testExamples :: [(Expr (), Expr ())] -> Expr () -> Bool
testExamples examples expr =
  let env = M.singleton "test" expr
      foldFunction (i, o) b = b && isNormEq env i o
  in  foldr foldFunction True examples

-- | Enumerates terms taking advantage of the fact that functions must have
-- | a number of top-level lambdas equal to the number of variables
-- | the function is called on (followed by an application, and then
-- | arbitrary expressions)
enumerateTerms :: Int -> Int -> [Expr ()]
enumerateTerms depth vars = unrollLambdas depth vars 0

unrollLambdas 0 _ _ = []
unrollLambdas depth vars nBinders | vars == nBinders =
  bottomUp depth nBinders ++ bottomUpT depth nBinders
unrollLambdas depth vars nBinders =
  [ ELam (newBind nBinders) e () | e <- unrollLambdas depth vars (nBinders + 1) ]

termStream vars = S.unfold (\depth -> (enumerateTerms depth vars, depth + 1)) 1

-- | Takes a list of input/output expressions where the input expression
-- | has the free variable 'test' where the synthesized test expressions will
-- | be inserted, and a number of iterations to run for. The synthesizer will
-- | check to see if each input expression is transitively equal to the
-- | output, and will return the first expression it finds that satifisfies
-- | all the input/output examples.
-- TODO: infer the number of variables for a generated function
synthesizeEncoding :: [(Expr (), Expr ())] -> Int -> Int -> [Expr ()]
synthesizeEncoding examples max vars =
  let terms = S.take max $ S.concat $ fmap (aexprToExpr 0 . prependLambdas vars) exprStream
  in  filter (testExamples examples) terms

-- -- TODO: infer the number of variables for a generated function
-- synthesizeEncodingHelper :: [(Expr Int, Expr Int)] -> Int -> Int -> Int -> Maybe (Expr Int)
-- synthesizeEncodingHelper examples n max vars =
--   let hexprs          = enumerateTerms n vars
--       -- exprs           = catMaybes $ map (\x -> hexprToExpr x 0) hexprs
--       exprs = hexprs
--       satisfyExamples = map (testExamples examples) exprs
--   in  case Seq.elemIndexL True (Seq.fromList satisfyExamples) of
--         Just i  -> Just (exprs !! i)
--         Nothing -> if n >= max then Nothing else synthesizeEncodingHelper examples (n + 1) max vars

data AExpr
  = ALam AExpr
  | AApp AExpr AExpr
  | AHole
  deriving Eq

instance Show AExpr where
  show (AApp e1 e2) = printf "(%s %s)" (show e1) (show e2)
  show (ALam e    ) = printf "(λ.%s)" (show e)
  show AHole        = "□"

data Exprs = Exprs { exprsLambdas :: [AExpr], exprsApps :: [AExpr] }

allExprs (Exprs lambdas apps) = lambdas ++ apps
appendExprs (Exprs lambdas1 apps1) (Exprs lambdas2 apps2) =
  Exprs (lambdas1 ++ lambdas2) (apps1 ++ apps2)

grow exprs_2 exprs_1 = Exprs nextLambdas nextApps
 where
  nextLambdas = [ ALam e | e <- allExprs exprs_1 ]
  nextApps =
    [ AApp e1 e2 | e1 <- exprsApps exprs_2, e2 <- allExprs exprs_1 ]
      ++ [ AApp e1 e2 | e1 <- exprsApps exprs_1, e2 <- allExprs exprs_1 ]
      ++ [ AApp e1 e2 | e1 <- exprsApps exprs_1, e2 <- allExprs exprs_2 ]

exprStream = S.prepend [AHole] $ S.concat $ S.unfold unfolder init
 where
  init = (Exprs [] [], Exprs [] [AHole])
  unfolder (exprs_2, exprs_1) =
    let exprs = grow exprs_2 exprs_1 in (allExprs exprs, (appendExprs exprs_2 exprs_1, exprs))

aexprToExpr :: Int -> AExpr -> [Expr ()]
aexprToExpr n (ALam expr) = [ ELam (Bind ("x" ++ show n) ()) x () | x <- aexprToExpr (n + 1) expr ]
aexprToExpr n (AApp expr1 expr2) =
  [ EApp x1 x2 () | x1 <- aexprToExpr n expr1, x2 <- aexprToExpr n expr2 ]
aexprToExpr n AHole = [ EVar ("x" ++ show x) () | x <- [0 .. n - 1] ]

prependLambdas 0 expr = expr
prependLambdas n expr = prependLambdas (n - 1) (ALam expr)
