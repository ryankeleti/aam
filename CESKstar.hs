module CESKstar where

import Data.Map (Map)
import qualified Data.Map as Map
import Lambda

type Addr = Int

data Storable
  = SClos Lam Env
  | SKont Kont

type Env = Map Var Addr

type Store = Map Addr Storable

data Kont
  = Mt
  | Ar Exp Env Addr
  | Fn Lam Env Addr

type State = (Exp, Env, Store, Addr)

inject :: Exp -> State
inject e = (e, Map.empty, Map.fromList [(0, SKont Mt)], 0)

step :: State -> State
step (Ref x, env, store, a) = (Abs lam, env', store, a)
  where
    Just (SClos lam env') = do
      addr <- Map.lookup x env
      Map.lookup addr store
step (App f e, env, store, a) = (f, env, store', b)
  where
    b = Map.size store
    store' = Map.insert b (SKont (Ar e env a)) store
step (Abs lam, env, store, a) =
  case Map.lookup a store of
    Just (SKont (Ar e env' c)) -> (e, env', store', b)
      where
        b = Map.size store
        store' = Map.insert b (SKont (Fn lam env c)) store
    Just (SKont (Fn (x :=> e) env' c)) -> (e, env'', store', c)
      where
        b = Map.size store
        env'' = Map.insert x b env'
        store' = Map.insert b (SClos lam env) store
    _ -> error "invalid store lookup"

terminal :: (State -> State) -> (State -> Bool) -> State -> State
terminal step isFinal state0
  | isFinal state0 = state0
  | otherwise = terminal step isFinal (step state0)

isFinal :: State -> Bool
isFinal (Abs _, _, _, 0) = True
isFinal _ = False

evaluate :: Exp -> State
evaluate e = terminal step isFinal (inject e)
