{-# LANGUAGE OverloadedStrings #-}

module Language.Elsa.Encodings where

import           GHC.Exts                       ( IsString(..) )
import qualified Language.Elsa.LocallyNameless as LN
import qualified Data.HashMap.Strict           as M
import           Language.Elsa.Synthesis        ( Spec(..) )


---------------------------------------------------------------------------------
-- | Booleans
---------------------------------------------------------------------------------

booleans :: M.HashMap String LN.Expr
booleans = M.fromList
  [ ("true" , "λ.λ.1")
  , ("false", "λ.λ.0")
  , ("not1" , "λ.λ.λ.2 0 1")
  , ("not2" , "λ.0 false true")
  , ("and"  , "λ.λ.1 0 1")
  , ("or"   , "λ.λ.1 1 0")
  , ("ite"  , "λ.λ.λ.2 1 0")
  ]

andSpec :: Spec LN.Expr
andSpec = Spec [("?and", 2)] [("?and true x", "x"), ("?and false x", "false")] False
-- andSpec = Spec
--   [("?and", 2)]
--   [ ("?and true  true" , "true")
--   , ("?and true  false", "false")
--   , ("?and false true" , "false")
--   , ("?and false false", "false")
--   ]

orSpec :: Spec LN.Expr
orSpec = Spec [("?or", 2)] [("?or true x", "true"), ("?or false x", "x")] False

iteSpec :: Spec LN.Expr
iteSpec = Spec [("?ite", 3)] [("?ite true x y", "x"), ("?ite false x y", "y")] False

notSpec :: Spec LN.Expr
notSpec = Spec [("?not", 1)] [("?not true", "false"), ("?not false", "true")] False

trueFalseSpec :: Spec LN.Expr
trueFalseSpec =
  Spec [("?true", 1), ("?false", 1)] [("ite ?true x y", "x"), ("ite ?false x y", "y")] False

boolSpec :: Spec LN.Expr
boolSpec = Spec [("?true", 1), ("?false", 1), ("?ite", 3)]
                [("?ite ?true x y", "x"), ("?ite ?false x y", "y")]
                False

---------------------------------------------------------------------------------
-- | Numerals
---------------------------------------------------------------------------------

numerals :: M.HashMap String LN.Expr
numerals = M.fromList
  [ ("zero" , "λ.λ.0")
  , ("one"  , "λ.λ.1 0")
  , ("two"  , "λ.λ.1 (1 0)")
  , ("three", "λ.λ.1 (1 (1 0))")
  , ("four" , "λ.λ.1 (1 (1 (1 0)))")
  , ("succ" , "λ.λ.λ.1 (2 1 0)")
  , ("plus" , "λ.λ.λ.λ.3 1 (2 1 0)")
  , ("exp"  , "λ.λ.0 1")
  , ("pred" , "λ.λ.λ.2 (λ.λ.0 (1 3)) (λ.1) (λ.0)")
  ]

succSpec :: Spec LN.Expr
succSpec =
  Spec [("?succ", 3)] [("?succ zero", "one"), ("?succ one", "two"), ("?succ two", "three")] False

plusSpec :: Spec LN.Expr
plusSpec = Spec [("?plus", 2)] [("?plus zero x", "x"), ("?plus (succ x) y", "succ (?plus x y)")] True
-- plusSpec = Spec
--   [("?plus", 2)]
--   [ ("?plus zero one", "one")
--   , ("?plus one zero", "one")
--   , ("?plus one two" , "three")
--   , ("?plus two one" , "three")
--   ]
--   False

expSpec :: Spec LN.Expr
expSpec = Spec [("?exp", 2)]
               [("?exp two zero", "one"), ("?exp two one", "two"), ("?exp two two", "four")]
               True

---------------------------------------------------------------------------------
-- | Pairs
---------------------------------------------------------------------------------

pairs :: M.HashMap String LN.Expr
pairs = M.fromList [("pair", "λ.λ.λ.0 2 1"), ("first", "λ.0 (λ.λ.1)"), ("second", "λ.0 (λ.λ.0)")]

firstSpec :: Spec LN.Expr
firstSpec = Spec [("?first", 1)] [("?first (pair x y)", "x")] False

secondSpec :: Spec LN.Expr
secondSpec = Spec [("?second", 1)] [("?second (pair x y)", "y")] False

pairSpec :: Spec LN.Expr
pairSpec = Spec [("?pair", 2), ("?first", 1), ("?second", 1)]
                [("?first (?pair x y)", "x"), ("?second (?pair x y)", "y")]
                False


pairSpec' :: Spec LN.Expr
pairSpec' = Spec [("?pair", 2), ("?first", 1)] [("?first (?pair x y)", "x")] False


---------------------------------------------------------------------------------
-- | Lists
---------------------------------------------------------------------------------

lists :: M.HashMap String LN.Expr
lists = M.fromList
  [ ("nil" , "λ.λ.0")
  , ("cons", "λ.λ.λ.λ.1 3 (2 1 0)")
  , ("head", "λ.0 (λ.λ.1) (λ.λ.0)")
  , ("tail", "λ.λ.λ.2 (λ.λ.λ.0 2 (1 4)) (λ.1) (λ.λ.0)")
  ]

headSpec :: Spec LN.Expr
headSpec = Spec [("?head", 1)] [("cons (?head (cons x xs)) (tail (cons x xs))", "cons x xs"), ("?head nil", "nil")] False


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
