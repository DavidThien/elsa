{-# LANGUAGE OverloadedStrings #-}
module Language.Elsa.Encodings where

import           Language.Elsa.Types
import           Text.Megaparsec hiding (parse)
import qualified Language.Elsa.Parser          as Parser

churchTrue = parse "\\a b -> a"
churchFalse = parse "\\a b -> b"

churchNot = lam "p" (app (app (var "p") churchFalse) churchTrue)
churchAnd = parse "\\p q -> p q p"
churchOr = parse "\\p q -> p p q"

testsAnd =
  [ (app (app (var "test") churchTrue) (var "x"), var "x")
  , (app (app (var "test") churchFalse) (var "x"), churchFalse)
  ]

zero = parse "\f x -> x"
one = parse "\f x -> f x"
two = parse "\f x -> f f x"

testsNum = [(app (var "test") zero , one), (app (var "test" ) one, two)]

------------------------------------------------------------------------------

lam x e = ELam (Bind x ()) e ()
app e1 e2 = EApp e1 e2 ()
var x = EVar x ()

unwrapRight :: Either a b -> b
unwrapRight (Right b) = b
unwrapRight _ = error "unwrapRight: Left"

unitExpr :: Expr a -> Expr ()
unitExpr (EVar x _) = EVar x ()
unitExpr (ELam (Bind x _) e _) = ELam (Bind x ()) (unitExpr e) ()
unitExpr (EApp e1 e2 _ ) = EApp (unitExpr e1) (unitExpr e2) ()

parse t =  unitExpr $ unwrapRight $ runParser Parser.expr "FILE" t

-- testsAnd =
--   [ (EApp (EApp (EVar "test" ()) churchTrue ()) churchTrue () , churchTrue)
--   , (EApp (EApp (EVar "test" ()) churchFalse ()) churchFalse (), churchFalse)
--   , (EApp (EApp (EVar "test" ()) churchTrue ()) churchFalse (), churchFalse)
--   , (EApp (EApp (EVar "test" ()) churchFalse ()) churchTrue (), churchFalse)
--   ]
