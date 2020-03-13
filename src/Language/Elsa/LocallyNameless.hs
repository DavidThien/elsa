{-# Language MultiWayIf #-}

module Language.Elsa.LocallyNameless where

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
data NormEq = IsNormEq | IsNotNormEq | NormEqUnk deriving Eq

isNormEq :: Env -> Expr -> Expr -> Bool
isNormEq g e1 e2 = evalNO (subst e1 g) == subst e2 g

isNormEqLimit :: Int -> Env -> Expr -> Expr -> NormEq
isNormEqLimit n g e1 e2 = case evalNOLimit n (subst e1 g) of
  Just (e1', _) -> if e1' == subst e2 g then IsNormEq else IsNotNormEq
  Nothing       -> NormEqUnk

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

-- | Tries to evaluate (by call-by-name) an expression taking at most `n`
-- | beta reductions.
evalCBNLimit :: Int -> Expr -> Maybe (Expr, Int)
evalCBNLimit n _ | n < 0    = Nothing
evalCBNLimit n e@EFVar{}    = Just (e, n)
evalCBNLimit n e@EBVar{}    = Just (e, n)
evalCBNLimit n e@ELam{}     = Just (e, n)
evalCBNLimit n (EApp e1 e2) = case evalCBNLimit n e1 of
  Just (ELam e1, n) -> do
    (e, n) <- evalCBNLimit (n - 1) (open e1 e2 0)
    return (e, n)
  Just (e1, n) -> Just (EApp e1 e2, n)
  Nothing      -> Nothing

-- | Tries to evaluate an expression to normal form taking at most `n`
-- | beta reductions.
evalNOLimit :: Int -> Expr -> Maybe (Expr, Int)
evalNOLimit n _ | n < 0 = Nothing
evalNOLimit n e@EBVar{} = Just (e, n)
evalNOLimit n e@EFVar{} = Just (e, n)
evalNOLimit n (ELam e)  = do
  (e', n) <- evalNOLimit n e
  return (ELam e', n)
evalNOLimit n (EApp e1 e2) = case evalCBNLimit n e1 of
  Just (ELam e1, n) -> do
    (e, n) <- evalNOLimit (n - 1) (open e1 e2 0)
    return (e, n)
  Just (e1, n) -> do
    (e1, n) <- evalNOLimit n e1
    (e2, n) <- evalNOLimit n e2
    return (EApp e1 e2, n)
  Nothing -> Nothing

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
identChar = alphaNumChar <|> oneOf ['_', '\'']

identifier :: Parser String
identifier = (:) <$> (letterChar <|> oneOf ['?']) <*> many identChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
