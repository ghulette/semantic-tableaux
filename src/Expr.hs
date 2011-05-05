module Expr where
  
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Env = Map String Bool

data Expr = Atom String
          | Neg Expr
          | And Expr Expr
          | Or  Expr Expr
          | Xor Expr Expr
          | Eqv Expr Expr
          | Imp Expr Expr
          deriving (Eq,Ord)

instance Show Expr where
  show (Atom x) = x
  show (Neg p) = "~" ++ (show p)
  show (And p q) = "(" ++ (show p) ++ " and " ++ (show q) ++ ")"
  show (Or p q)  = "(" ++ (show p) ++ " or " ++ (show q) ++ ")"
  show (Xor p q) = "(" ++ (show p) ++ " xor " ++ (show q) ++ ")"
  show (Eqv p q) = "(" ++ (show p) ++ " <-> " ++ (show q) ++ ")"
  show (Imp p q) = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"

xor :: Bool -> Bool -> Bool
xor = (/=)

eqv :: Bool -> Bool -> Bool
eqv = (==)

imp :: Bool -> Bool -> Bool
imp p q = (not p) || q

eval :: Env -> Expr -> Bool
eval env (Atom x) = 
  case Map.lookup x env of
    Just v -> v
    Nothing -> error $ "Prop " ++ x ++ " is undefined"
eval env (Neg p) = not (eval env p)
eval env (p `And` q) = (eval env p)  &&   (eval env q)
eval env (p `Or` q)  = (eval env p)  ||   (eval env q)
eval env (p `Xor` q) = (eval env p) `xor` (eval env q)
eval env (p `Eqv` q) = (eval env p) `eqv` (eval env q)
eval env (p `Imp` q) = (eval env p) `imp` (eval env q)

atoms :: Expr -> Set String
atoms (Atom x) = Set.singleton x
atoms (Neg p) = atoms p
atoms (p `And` q) = (atoms p) `Set.union` (atoms q)
atoms (p `Or`  q) = (atoms p) `Set.union` (atoms q)
atoms (p `Xor` q) = (atoms p) `Set.union` (atoms q)
atoms (p `Eqv` q) = (atoms p) `Set.union` (atoms q)
atoms (p `Imp` q) = (atoms p) `Set.union` (atoms q)
