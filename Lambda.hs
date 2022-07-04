module Lambda where

type Var = String

data Lam = Var :=> Exp
  deriving (Eq, Show)

data Exp
  = Ref Var -- x
  | Abs Lam -- \x.e
  | App Exp Exp -- e e
  deriving (Eq, Show)
