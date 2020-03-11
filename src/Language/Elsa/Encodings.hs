{-# LANGUAGE OverloadedStrings #-}

module Language.Elsa.Encodings where

import           GHC.Exts                       ( IsString(..) )
import qualified Language.Elsa.LocallyNameless as LN
import qualified Data.HashMap.Strict           as M


---------------------------------------------------------------------------------
-- | Booleans
---------------------------------------------------------------------------------

booleans :: M.HashMap String LN.Expr
booleans = M.fromList
  [ ("true" , "λ.λ.1")
  , ("false", "λ.λ.0")
  , ("not"  , "λ.0 false true")
  , ("and"  , "λ.λ.1 0 1")
  , ("or"   , "λ.λ.1 1 0")
  ]

andSpec :: [(LN.Expr, LN.Expr)]
andSpec = [("test true x", "x"), ("test false x", "false")]

---------------------------------------------------------------------------------
-- | Numerals
---------------------------------------------------------------------------------

numerals :: M.HashMap String LN.Expr
numerals = M.fromList
  [ ("zero" , "λ.λ.0")
  , ("one"  , "λ.λ.1 0")
  , ("two"  , "λ.λ.1 (1 0)")
  , ("three", "λ.λ.1 (1 (1 0))")
  , ("succ" , "λ.λ.λ.1 (2 1 0)")
  , ("plus" , "λ.λ.λ.λ.3 1 (2 1 0)")
  ]

succSpec :: [(LN.Expr, LN.Expr)]
succSpec = [("test zero", "one"), ("test one", "two"), ("test two", "three")]

plusSpec :: [(LN.Expr, LN.Expr)]
plusSpec =
  [ ("test zero one", "one")
  , ("test one zero", "one")
  , ("test one two" , "three")
  , ("test two one" , "three")
  ]

instance IsString LN.Expr where
  fromString = unwrapRight . LN.parse

unwrapRight :: Either a b -> b
unwrapRight (Right b) = b
unwrapRight _         = error "unwrapRight: Left"

-- churchTrue = parse "\\a b -> a"
-- churchFalse = parse "\\a b -> b"

-- churchNot = lam "p" (app2 "p" churchFalse churchTrue)
-- churchAnd = parse "\\p q -> p q p"
-- churchOr = parse "\\p q -> p p q"

-- testsAnd = [(app2 "test" churchTrue "x", "x"), (app2 "test" churchFalse "x", churchFalse)]

-- zero = parse "\\f x -> x"
-- one = parse "\\f x -> f x"
-- two = parse "\\f x -> f (f x)"
-- add1 = parse "\\n f x -> f (n f x)"
-- plus = parse "\\m n f x -> m f (n f x)"

-- testsNum = [(app "test" zero, one), (app "test" one, two)]

-- ------------------------------------------------------------------------------

-- instance IsString (Expr ()) where
--   fromString = parse

-- lam x e = ELam (Bind x ()) e ()
-- app f e = EApp f e ()
-- app2 f e1 e2 = app (app f e1) e2
-- var x = EVar x ()

-- unitExpr :: Expr a -> Expr ()
-- unitExpr (EVar x _            ) = EVar x ()
-- unitExpr (ELam (Bind x _) e  _) = ELam (Bind x ()) (unitExpr e) ()
-- unitExpr (EApp e1         e2 _) = EApp (unitExpr e1) (unitExpr e2) ()

-- parse t = unitExpr $ unwrapRight $ runParser Parser.expr "FILE" t

-- -- testsAnd =
-- --   [ (EApp (EApp (EVar "test" ()) churchTrue ()) churchTrue () , churchTrue)
-- --   , (EApp (EApp (EVar "test" ()) churchFalse ()) churchFalse (), churchFalse)
-- --   , (EApp (EApp (EVar "test" ()) churchTrue ()) churchFalse (), churchFalse)
-- --   , (EApp (EApp (EVar "test" ()) churchFalse ()) churchTrue (), churchFalse)
-- --   ]
