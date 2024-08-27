module TypedSession.State.Constraint (SubMap, Constraint (..), constrToSubMap) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Data.List as L

{- | Constraint
 1 ~ 2 = Constraint 1 2
 2 ~ 3 = Constraint 2 3
-}
data Constraint = Constraint Int Int
  deriving (Show, Eq, Ord)

type SubMap = IntMap Int

toTuple :: Constraint -> (Int, Int)
toTuple (Constraint b s) = (b, s)

{- | bigSwapConstr
>>> bigSwapConstr (Constraint 1 2)
Constraint 2 1
-}
bigSwapConstr :: Constraint -> Constraint
bigSwapConstr (Constraint a b)
  | a >= b = Constraint a b
  | otherwise = Constraint b a

subFun :: [Constraint] -> [Constraint]
subFun [] = []
subFun (Constraint b s : ys) = Constraint b s : subFun (replace b s ys)

replace :: Int -> Int -> [Constraint] -> [Constraint]
replace _b _s [] = []
replace b s ((Constraint b' s') : ys) =
  if b == s'
    then Constraint b' s : replace b s ys
    else Constraint b' s' : replace b s ys

{- | findNewConstraint
>>> findNewConstraint [Constraint 3 1, Constraint 4 2, Constraint 4 3]
[[Constraint 3 2]]
-}
findNewConstraint :: [Constraint] -> [[Constraint]]
findNewConstraint ls =
  let grouped = L.groupBy (\(Constraint a _) (Constraint c _) -> a == c) ls
      genCons s val = case val of
        [] -> s
        [_] -> s
        xs ->
          case L.sort $ map (snd . toTuple) xs of
            [] -> error "np"
            minVal : txs' -> map (`Constraint` minVal) txs' : s
   in foldl' genCons [] grouped

{- | stepConstraint
>>> stepConstraint [Constraint 3 1, Constraint 4 2, Constraint 4 3]
[Constraint 3 1,Constraint 4 2,Constraint 4 1]
-}
stepConstraint :: [Constraint] -> [Constraint]
stepConstraint = L.nub . subFun . L.sort . L.nub . map bigSwapConstr

{- | constraintLoop
>>> constraintLoop [Constraint 3 1, Constraint 4 2, Constraint 4 3]
[Constraint 2 1,Constraint 3 1,Constraint 4 1]

-------------------------------------------------

>>> stepConstraint [Constraint 3 1, Constraint 4 2, Constraint 4 3]
[Constraint 3 1,Constraint 4 2,Constraint 4 1]

>>> findNewConstraint [Constraint 3 1,Constraint 4 2,Constraint 4 1]
[[Constraint 2 1]]

>>> stepConstraint ([Constraint 3 1,Constraint 4 2,Constraint 4 1] <> [Constraint 2 1])
[Constraint 2 1,Constraint 3 1,Constraint 4 1]
-}
constraintLoop :: [Constraint] -> [Constraint]
constraintLoop ls =
  let ls' = stepConstraint ls
   in case findNewConstraint ls' of
        [] -> ls'
        xs -> constraintLoop (concat xs <> ls')

{- | constrToSubMap
>>> constrToSubMap [Constraint 3 1, Constraint 4 2, Constraint 4 3]
fromList [(2,1),(3,1),(4,1)]
-}
constrToSubMap :: [Constraint] -> SubMap
constrToSubMap ls = I.fromList $ map toTuple $ constraintLoop ls
