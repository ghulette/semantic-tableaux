import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Control.Monad (guard)

type Id = String
type Env = Map String Bool

xor :: Bool -> Bool -> Bool
xor = (/=)

eqv :: Bool -> Bool -> Bool
eqv = (==)

imp :: Bool -> Bool -> Bool
imp p q = (not p) || q

data Expr = Atom Id
          | Neg Expr
          | And Expr Expr
          | Or  Expr Expr
          | Xor Expr Expr
          | Eqv Expr Expr
          | Imp Expr Expr
          deriving (Eq,Ord)

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

atoms :: Expr -> Set Id
atoms (Atom x) = Set.singleton x
atoms (Neg p) = atoms p
atoms (p `And` q) = (atoms p) `Set.union` (atoms q)
atoms (p `Or`  q) = (atoms p) `Set.union` (atoms q)
atoms (p `Xor` q) = (atoms p) `Set.union` (atoms q)
atoms (p `Eqv` q) = (atoms p) `Set.union` (atoms q)
atoms (p `Imp` q) = (atoms p) `Set.union` (atoms q)

type Tableau = Tree (Set Expr,Bool)

isLiteral :: Expr -> Bool
isLiteral (Atom x) = True
isLiteral (Neg (Atom x)) = True
isLiteral _ = False

isAllLiterals :: Set Expr -> Bool
isAllLiterals u = Set.null (Set.filter (not . isLiteral) u)

complement :: Expr -> Expr
complement (Neg x) = x
complement x = Neg x

hasComplements :: Set Expr -> Bool
hasComplements u | Set.size u <= 1 = False
hasComplements u = 
  if Set.member (complement x) u' then True else hasComplements u'
  where x  = Set.findMin u
        u' = Set.deleteMin u

closed :: Set Expr -> Maybe Bool
closed u = do
  guard (isAllLiterals u)
  return (hasComplements u)

buildTableu :: Set Expr -> Tableau
buildTableu u = 
  case closed u of
    Just b -> Tree.Node (u,b) []
    Nothing -> undefined



main :: IO ()
main = do
  putStrLn "ok"
