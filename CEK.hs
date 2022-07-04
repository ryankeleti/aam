module CEK where

{-
 - Taken from <https://matt.might.net/articles/cek-machines>
 - and <https://arxiv.org/abs/1007.4446v2>.
 -}

type Var = String

data Lambda = Var :=> Exp
  deriving (Show)

data Exp
  = Ref Var -- x
  | Lam Lambda -- \x.e
  | App Exp Exp -- e e
  deriving (Show)

-- Lambda calculus program is an expression.
type Program = Exp

-- State space.
type State = (Exp, Env, Kont)

-- Values.
data Val = Clos Lambda Env

-- Environment.
type Env = Var -> Val

-- Continuations.
-- Represent evaluation contexts inside-out.
-- Mt: mt, represents [] (empty continuation)
-- Ar: ar(e', ρ, κ), represents E[([]e)]
--     where ρ closes e' to represent e and κ represents E
-- Fn: fn(v', ρ, κ), represents E[(v[])]
--     where ρ closes v' to represent v and κ represents E
data Kont
  = Mt
  | Ar Exp Env Kont
  | Fn Lambda Env Kont

-- step: deterministic transition function
-- isFinal: predicate indicating whether a state has no successor
-- state0: initial state
terminal :: (State -> State) -> (State -> Bool) -> State -> State
terminal step isFinal state0
  | isFinal state0 = state0
  | otherwise = terminal step isFinal (step state0)

-- Maps a program into a state.
inject :: Program -> State
inject e = (e, env0, Mt)
  where
    env0 :: Env
    env0 x = error $ "no binding for " ++ x

(==>) :: a -> b -> (a, b)
(==>) x y = (x, y)

(//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
(//) f [(x, y)] x' = if x == x' then y else f x'
(//) _ _ _ = error "invalid substitution"

-- Move execution forward one step.
step :: State -> State
-- Lookup a reference in the environment.
step (Ref x, env, k) = (Lam lam, env', k)
  where
    Clos lam env' = env x

-- Evaluate a function before the application.
step (App f e, env, k) = (f, env, Ar e env k)
-- Evaluate the argument term after evaluating the function.
step (Lam lam, env, Ar e env' k) = (e, env', Fn lam env k)
-- Perform the application after evaluating the function and argument.
step (Lam lam, env, Fn (x :=> e) env' k) = (e, env' // [x ==> Clos lam env], k)
step _ = error "invalid step"

-- Recognize final state.
isFinal :: State -> Bool
isFinal (Lam _, env, Mt) = True
isFinal _ = False

-- Evaluate a program.
evaluate :: Program -> State
evaluate pr = terminal step isFinal (inject pr)
