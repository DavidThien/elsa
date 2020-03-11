{-# Language MultiWayIf #-}

module Language.Elsa.LocallyNameless
  ( isNormEq
  , Env
  , Expr(..)
  , parse
  )
where

import qualified Data.HashMap.Strict           as M
import           Text.Printf                    ( printf )
import           Text.Megaparsec         hiding ( parse )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Char
import qualified Data.List                     as L
import           Control.Monad                  ( void )

--------------------------------------------------------------------------------
-- | Grammar
--------------------------------------------------------------------------------

data Expr
  = ELam Expr
  | EApp Expr Expr
  | EBVar Int
  | EFVar String
  deriving Eq

instance Show Expr where
  show (EFVar s   ) = s
  show (EBVar idx ) = show idx
  show (EApp e1 e2) = printf "(%s %s)" (show e1) (show e2)
  show (ELam e    ) = printf "(λ.%s)" (show e)

type Env = M.HashMap String Expr

--------------------------------------------------------------------------------
-- | Evaluation
--------------------------------------------------------------------------------

isNormEq :: Env -> Expr -> Expr -> Bool
isNormEq g e1 e2 = evalNO (subst e1 g) == subst e2 g

evalCBN :: Expr -> Expr
evalCBN e@EFVar{}    = e
evalCBN e@EBVar{}    = e
evalCBN e@ELam{}     = e
evalCBN (EApp e1 e2) = case evalCBN e1 of
  ELam e1' -> evalCBN (open e1' e2 0)
  e1'      -> EApp e1' e2

evalNO :: Expr -> Expr
evalNO e@EBVar{}    = e
evalNO e@EFVar{}    = e
evalNO (ELam e    ) = ELam $ evalNO e
evalNO (EApp e1 e2) = case evalCBN e1 of
  ELam e1' -> evalNO (open e1' e2 0)
  e1'      -> EApp (evalNO e1') (evalNO e2)

--------------------------------------------------------------------------------
-- | Substitution
--------------------------------------------------------------------------------

lift :: Expr -> Int -> Int -> Expr
lift (EBVar i)    k nBinders = if i < nBinders then EBVar i else EBVar $ i + k
lift e@EFVar{}    _ _        = e
lift (ELam e1   ) k nBinders = ELam $ lift e1 k (nBinders + 1)
lift (EApp e1 e2) k nBinders = EApp (lift e1 k nBinders) (lift e2 k nBinders)

open :: Expr -> Expr -> Int -> Expr
open (ELam e1   ) e2 k = ELam $ open e1 e2 (k + 1)
open (EApp e1 e2) e3 k = EApp (open e1 e3 k) (open e2 e3 k)
open (EBVar i   ) e  k = if
  | i == k    -> (lift e i 0)
  | i > k     -> (EBVar (i - 1))
  | otherwise -> (EBVar i)
open e@EFVar{} _ _ = e

subst :: Expr -> Env -> Expr
subst e@EBVar{}      _  = e
subst e@(EFVar s   ) su = M.lookupDefault e s su
subst (  EApp e1 e2) su = EApp (subst e1 su) (subst e2 su)
subst (  ELam e    ) su = ELam (subst e su)

--------------------------------------------------------------------------------
-- | Parsing
--------------------------------------------------------------------------------
type Parser = Parsec () String

parse = runParser (whole expr) ""

whole :: Parser a -> Parser a
whole p = sc *> p <* eof

expr :: Parser Expr
expr = try lamExpr <|> try appExpr <|> try bVar <|> try fVar <|> parenExpr

lamExpr :: Parser Expr
lamExpr = do
  lam <* dot
  ELam <$> expr

bVar :: Parser Expr
bVar = EBVar <$> L.decimal

fVar :: Parser Expr
fVar = EFVar <$> identifier

appExpr :: Parser Expr
appExpr = L.foldl' EApp <$> funExpr <* sc <*> sepBy1 funExpr sc

funExpr :: Parser Expr
funExpr = try fVar <|> try bVar <|> parenExpr

parenExpr :: Parser Expr
parenExpr = parens expr

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lam :: Parser String
lam = symbol "\\" <|> symbol "λ"

dot :: Parser String
dot = symbol "."

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf ['_', '#', '\'']

identifier :: Parser String
identifier = (:) <$> letterChar <*> many identChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")