module Language.Elsa.Encodings where

import           Language.Elsa.Types

churchTrue = ELam (Bind "a" ()) (ELam (Bind "b" ()) (EVar "a" ()) ()) ()
churchFalse = ELam (Bind "a" ()) (ELam (Bind "b" ()) (EVar "b" ()) ()) ()

churchNot = ELam (Bind "p" ()) (EApp (EApp (EVar "p" ()) churchFalse ()) churchTrue ()) ()
churchAnd = ELam
  (Bind "p" ())
  (ELam (Bind "q" ()) (EApp (EApp (EVar "p" ()) (EVar "q" ()) ()) (EVar "p" ()) ()) ())
  ()
churchOr = ELam
  (Bind "p" ())
  (ELam (Bind "q" ()) (EApp (EApp (EVar "p" ()) (EVar "p" ()) ()) (EVar "q" ()) ()) ())
  ()

testsAnd =
  [ (EApp (EApp (EVar "test" ()) churchTrue ()) (EVar "x" ()) () , EVar "x" ())
  , (EApp (EApp (EVar "test" ()) churchFalse ()) (EVar "x" ()) (), churchFalse)
  ]

zero = ELam (Bind "f" ()) (ELam (Bind "x" ()) (EVar "x" ()) ()) ()
one = ELam (Bind "f" ()) (ELam (Bind "x" ()) (EApp (EVar "f" ()) (EVar "x" ()) ()) ()) ()
two = ELam (Bind "f" ()) (ELam (Bind "x" ()) (EApp (EVar "f" ()) (EApp (EVar "f" ()) (EVar "x" ()) ()) ()) ()) ()

testsNum = [
  (EApp (EVar "test" ()) zero (), one),
  (EApp (EVar "test" ()) one (), two)
           ]

-- testsAnd =
--   [ (EApp (EApp (EVar "test" ()) churchTrue ()) churchTrue () , churchTrue)
--   , (EApp (EApp (EVar "test" ()) churchFalse ()) churchFalse (), churchFalse)
--   , (EApp (EApp (EVar "test" ()) churchTrue ()) churchFalse (), churchFalse)
--   , (EApp (EApp (EVar "test" ()) churchFalse ()) churchTrue (), churchFalse)
--   ]
