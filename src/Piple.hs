{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Piple where

import Constraint
import Control.Algebra (Has)
import Control.Carrier.Fresh.Strict (runFresh)
import Control.Effect.Fresh (Fresh, fresh)
import Control.Monad (forM)
import Data.Functor.Identity (Identity (..))
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Render
import Type

addIndex :: (Has Fresh sig m) => R role' () -> m (R role' Int)
addIndex = \case
  Terminal _ -> Terminal <$> fresh
  Arrow _ a b st :> r -> do
    i <- fresh
    r' <- addIndex r
    pure (Arrow i a b st :> r')
  Branch _ bvs -> do
    anni <- fresh
    bvs' <- forM bvs $ \(BranchVal st r) -> do
      r' <- addIndex r
      pure (BranchVal st r')
    pure (Branch anni bvs')

addSt :: Int -> R role' Int -> R role' [Int]
addSt roleNum = fmap (\i -> [i * roleNum + rv | rv <- [0 .. roleNum - 1]])

baseConstraint :: Int -> (role' -> Int) -> Arrow role' Int -> Int -> [Constraint]
baseConstraint roleNum roleF (Arrow i a b _st) j =
  let a' = roleF a
      b' = roleF b
      a'' = i * roleNum + a'
      b'' = i * roleNum + b'
      expls = L.delete b' $ L.delete a' [0 .. roleNum - 1]
   in Constraint a'' b''
        : [Constraint (i * roleNum + v) (j * roleNum + v) | v <- expls]

rToConstraints :: Int -> (role' -> Int) -> R role' Int -> [Constraint]
rToConstraints roleNum roleF = \case
  Terminal j -> [Constraint (j * roleNum + v) (-1) | v <- [0 .. roleNum - 1]]
  Branch j vs ->
    let ccs =
          concat
            [ [ Constraint (j * roleNum + ri) (bv * roleNum + ri)
              | ri <- [0 .. roleNum - 1]
              ]
            | bv <- map firstArrowAnn vs
            ]
     in ccs
          <> concatMap
            ( \(BranchVal _ r) -> rToConstraints roleNum roleF r
            )
            vs
  arr@Arrow{} :> r -> baseConstraint roleNum roleF arr (rToAnn r) <> rToConstraints roleNum roleF r

rAllInt :: R role' [Int] -> SubMap
rAllInt r =
  let v = L.delete (-1) $ L.nub $ concat $ rAnnToList r
   in IntMap.fromList $ (-1, -1) : zip v [0 ..]

type SPSet = Set Int

collectAllNoDec' :: R role' [Int] -> [Int]
collectAllNoDec' = \case
  Terminal _ann -> []
  (Arrow _ann _ _ _ :> r) -> collectAllNoDec' r
  Branch ann vs ->
    ann <> concatMap (\(BranchVal _ r) -> collectAllNoDec' r) vs

collectAllNoDec :: R role' [Int] -> SPSet
collectAllNoDec r = Set.fromList $ collectAllNoDec' r

data St
  = St Int
  | StBrSend String Int
  | StBrRecv Int
  | TerminalSt

instance Show St where
  show = \case
    TerminalSt -> "End"
    St i -> "S" ++ show i
    StBrSend st i -> "S" ++ show i ++ " " ++ st
    StBrRecv i -> "S" ++ show i ++ " s"

genRst :: String -> SPSet -> (role' -> Int) -> R role' [Int] -> R role' [St]
genRst branchSt sps roleF = \case
  Terminal ls -> Terminal (map (const TerminalSt) ls)
  (Arrow ann send recv st :> r) ->
    let lll = flip map (zip [0 ..] ann) $ \(i, v) ->
          if
            | v == (-1) -> TerminalSt
            | Set.member v sps ->
                if
                  | i == roleF send -> StBrSend branchSt v
                  | i == roleF recv -> StBrRecv v
                  | otherwise -> StBrRecv v
            | otherwise -> St v
     in Arrow lll send recv st :> genRst branchSt sps roleF r
  Branch ann vs ->
    Branch
      (map StBrRecv ann)
      (map (\(BranchVal st r) -> BranchVal st (genRst st sps roleF r)) vs)

pipleR :: (Show role') => [role'] -> (role' -> Int) -> R role' () -> String
pipleR roles roleF rr =
  let roleNum = length roles
      rInt = snd $ runIdentity $ runFresh 0 $ addIndex rr
      rcs = constrToSubMap $ rToConstraints roleNum roleF rInt
      vv = fmap (\i -> fromMaybe i $ IntMap.lookup i rcs) <$> addSt roleNum rInt
      subMap1 = rAllInt vv
      v1' = fmap (fmap (\i -> fromMaybe i $ IntMap.lookup i subMap1)) vv
      allsps = collectAllNoDec v1'
      v2' = genRst "" allsps roleF v1'
   in render roles roleF (\is -> [CenterFill ((i + 1) * width) ' ' (show v) | (i, v) <- zip [0 ..] is]) v2'

-----------------------------------------------------------------------

data PingPong = Client | Server | ClientBackup
  deriving (Show)

pingPongToInt :: PingPong -> Int
pingPongToInt = \case
  Client -> 0
  Server -> 1
  ClientBackup -> 2

v1 :: R PingPong ()
v1 =
  branch
    [ branchVal "True" $
        (Client --> Server) "Ping"
          :> (Client <-- Server) "Pong"
          :> (Client --> ClientBackup) "Add"
          :> terminal
    , branchVal "False" $
        (Client --> Server) "Stop"
          :> (Client --> ClientBackup) "AStop"
          :> terminal
    ]

rv1 :: String
rv1 = pipleR [Client, Server, ClientBackup] pingPongToInt v1

{-
>>> error rv1
-----------------Client--------------Server-----------ClientBackup--------------
                  S0 s                S0 s                S1 s
    ----------------------------------True----------------------------------
                S0 True               S0 s                S1 s
        Ping       |        --->       |
                   S2                  S2                 S1 s
        Pong       |        <---       |
                S1 True               End                 S1 s
        Add        |                  --->                 |
                  End                 End                 End
                                    Terminal

    ---------------------------------False----------------------------------
                S0 False              S0 s                S1 s
        Stop       |        --->       |
                S1 False              End                 S1 s
       AStop       |                  --->                 |
                  End                 End                 End
                                    Terminal

-}

-----------------------------------------------------------------------
data BookRole
  = Buyer
  | Seller
  | Buyer2
  deriving (Show)

bookRoleToInt :: BookRole -> Int
bookRoleToInt = \case
  Buyer -> 0
  Seller -> 1
  Buyer2 -> 2

p :: R BookRole ()
p =
  (Buyer --> Seller) "Title"
    :> branch
      [ branchVal "BookNotFound" $
          (Buyer <-- Seller) "BookNotFoun"
            :> (Buyer --> Buyer2) "SellerNotFoundBook"
            :> terminal
      , branchVal "BookFound" $
          (Buyer <-- Seller) "Price"
            :> (Buyer --> Buyer2) "PriceToBuyer2"
            :> (Buyer <-- Buyer2) "HalfPrice"
            :> branch
              [ branchVal "EnoughBudget" $
                  ( (Buyer --> Seller) "Afford"
                      :> (Buyer <-- Seller) "Data"
                      :> (Buyer --> Buyer2) "Success"
                      :> terminal
                  )
              , branchVal "NotEnoughBudget" $
                  ( (Buyer --> Seller) "NotBuy"
                      :> (Buyer --> Buyer2) "Failed"
                      :> terminal
                  )
              ]
      ]

v2 :: [Char]
v2 =
  pipleR
    [Buyer, Seller, Buyer2]
    bookRoleToInt
    p

{-

>>> error v2
-----------------Buyer---------------Seller--------------Buyer2-----------------
                   S0                  S0                 S1 s
       Title       |        --->       |
                  S2 s                S2 s                S1 s
    ------------------------------BookNotFound------------------------------
                  S2 s          S2 BookNotFound           S1 s
    BookNotFoun    |        <---       |
            S1 BookNotFound           End                 S1 s
  SellerNotFoundB  |                  --->                 |
                  End                 End                 End
                                    Terminal

    -------------------------------BookFound--------------------------------
                  S2 s            S2 BookFound            S1 s
       Price       |        <---       |
              S1 BookFound            S3 s                S1 s
   PriceToBuyer2   |                  --->                 |
                   S4                 S3 s                 S4
     HalfPrice     |                  <---                 |
                  S3 s                S3 s                S5 s
        --------------------------EnoughBudget--------------------------
            S3 EnoughBudget           S3 s                S5 s
       Afford      |        --->       |
                   S6                  S6                 S5 s
        Data       |        <---       |
            S5 EnoughBudget           End                 S5 s
      Success      |                  --->                 |
                  End                 End                 End
                                    Terminal

        ------------------------NotEnoughBudget-------------------------
           S3 NotEnoughBudget         S3 s                S5 s
       NotBuy      |        --->       |
           S5 NotEnoughBudget         End                 S5 s
       Failed      |                  --->                 |
                  End                 End                 End
                                    Terminal

-}
