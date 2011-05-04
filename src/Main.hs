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
          deriving (Eq,Ord,Show)

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

tableauIsClosed :: Tableau -> Bool
tableauIsClosed (Tree.Node (_,b) _) = b

alpha :: Expr -> [Expr]
alpha (Neg (Neg p))     = [p]
alpha (p `And` q)       = [p,q]
alpha (Neg (p `Or` q))  = [Neg p,Neg q]
alpha (Neg (p `Imp` q)) = [p,Neg q]
alpha (p `Eqv` q)       = [p `Imp` q,q `Imp` p]
alpha (Neg (p `Xor` q)) = [p `Imp` q,q `Imp` p]
alpha _                 = []

beta :: Expr -> [Expr]
beta (Neg (p `And` q))  = [Neg p,Neg q]
beta (p `Or` q)         = [p,q]
beta (p `Imp` q)        = [Neg p,q]
beta (Neg (p `Eqv` q))  = [Neg (p `Imp` q),Neg (q `Imp` p)]
beta (p `Xor` q)        = [Neg (p `Imp` q),Neg (q `Imp` p)]
beta _                  = []

buildTableau :: Set Expr -> Tableau
buildTableau u = 
  case closed u of
    Just b -> Tree.Node (u,b) []
    Nothing -> 
      let (_,w) = Set.partition isLiteral u in
      let x = Set.findMin w in
      case alpha x of
        [] -> 
          case beta x of
            [b1,b2] -> 
              let 
                u1 = (Set.delete x u) `Set.union` (Set.singleton b1)
                u2 = (Set.delete x u) `Set.union` (Set.singleton b2)
                t1 = buildTableau u1
                t2 = buildTableau u2
                b = and (map tableauIsClosed [t1,t2])
              in
                Tree.Node (u,b) [t1,t2]
            _ -> error $ "Could not reduce " ++ (show x)
        a -> 
          let 
            u' = (Set.delete x u) `Set.union` (Set.fromList a)
            t = buildTableau u'
            b = tableauIsClosed t
          in
            Tree.Node(u,b) [t]

tableau :: Expr -> Tableau
tableau = buildTableau . Set.singleton

main :: IO ()
main = do
  let e = (Atom "p" `Or` Atom "q") `And` (Neg (Atom "p") `And` Neg (Atom "q"))
  let t = tableau e
  putStrLn $ Tree.drawTree $ fmap show t

