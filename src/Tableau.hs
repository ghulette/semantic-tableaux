module Tableau (Tableau,draw,tableau,satisfiable,isClosed) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Control.Monad (guard)
import Expr

type Tableau = Tree (Set Expr,Bool)

drawTabNode :: (Set Expr,Bool) -> String
drawTabNode (u,b) = show (Set.toList u) ++ (if b then " Unsat" else " Sat")

draw :: Tableau -> String
draw = Tree.drawTree . fmap drawTabNode

isLiteral :: Expr -> Bool
isLiteral (Atom _) = True
isLiteral (Neg (Atom _)) = True
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

isClosed :: Tableau -> Bool
isClosed (Tree.Node (_,b) _) = b

alpha :: [Expr] -> [Set Expr]
alpha xs = [Set.fromList xs]

beta :: [Expr] -> [Set Expr]
beta = map Set.singleton

branch :: Expr -> [Set Expr]
branch (Neg (Neg p))     = alpha [p]
branch (p `And` q)       = alpha [p,q]
branch (Neg (p `Or` q))  = alpha [Neg p,Neg q]
branch (Neg (p `Imp` q)) = alpha [p,Neg q]
branch (p `Eqv` q)       = alpha [p `Imp` q,q `Imp` p]
branch (Neg (p `Xor` q)) = alpha [p `Imp` q,q `Imp` p]
branch (Neg (p `And` q)) = beta  [Neg p,Neg q]
branch (p `Or` q)        = beta  [p,q]
branch (p `Imp` q)       = beta  [Neg p,q]
branch (Neg (p `Eqv` q)) = beta  [Neg (p `Imp` q),Neg (q `Imp` p)]
branch (p `Xor` q)       = beta  [Neg (p `Imp` q),Neg (q `Imp` p)]
branch _                 = undefined

build :: Set Expr -> Tableau
build u = case closed u of
  Just b -> Tree.Node (u,b) []
  Nothing -> Tree.Node (u,b) ts
    where (_,w) = Set.partition isLiteral u
          x = Set.findMin w
          u' = Set.delete x u
          ts = map (build . Set.union u') (branch x)
          b = and (map isClosed ts)

tableau :: Expr -> Tableau
tableau = build . Set.singleton

satisfiable :: Tableau -> Bool
satisfiable = not . isClosed

-- valid :: Expr -> Bool
-- valid e = isClosed $ tableau (Neg e)
