module CESK where

import Data.Map (Map)
import qualified Data.Map as Map
import Lambda

type Addr = Int

data Val = Clos Lam Env

type Storable = Val

type Env = Map Var Addr

type Store = Map Addr Storable

data Kont
  = Mt
  | Ar Exp Env Kont
  | Fn Lam Env Kont

type State = (Exp, Env, Store, Kont)

inject :: Exp -> State
inject e = (e, Map.empty, Map.empty, Mt)

step :: State -> State
step (Ref x, env, store, k) = (Abs lam, env', store, k)
  where
    Just (Clos lam env') = do
      addr <- Map.lookup x env
      Map.lookup addr store
step (App f e, env, store, k) = (f, env, store, Ar e env k)
step (Abs lam, env, store, Ar e env' k) = (e, env', store, Fn lam env k)
step (Abs lam, env, store, Fn (x :=> e) env' k) = (e, env'', store', k)
  where
    addr = Map.size store
    env'' = Map.insert x addr env'
    store' = Map.insert addr (Clos lam env) store
step _ = error "invalid step"

terminal :: (State -> State) -> (State -> Bool) -> State -> State
terminal step isFinal state0
  | isFinal state0 = state0
  | otherwise = terminal step isFinal (step state0)

isFinal :: State -> Bool
isFinal (Abs _, _, _, Mt) = True
isFinal _ = False

evaluate :: Exp -> State
evaluate e = terminal step isFinal (inject e)
