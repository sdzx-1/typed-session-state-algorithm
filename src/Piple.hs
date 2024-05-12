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
    StBrSend st i -> "(S" ++ show i ++ " " ++ st <> ")"
    StBrRecv i -> "(S" ++ show i ++ " s)"

showNoParen :: St -> [Char]
showNoParen = \case
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

pipleR' :: (Show role') => [role'] -> (role' -> Int) -> R role' () -> R role' [St]
pipleR' roles roleF rr =
  let roleNum = length roles
      rInt = snd $ runIdentity $ runFresh 0 $ addIndex rr
      rcs = constrToSubMap $ rToConstraints roleNum roleF rInt
      vv = fmap (\i -> fromMaybe i $ IntMap.lookup i rcs) <$> addSt roleNum rInt
      subMap1 = rAllInt vv
      v1' = fmap (fmap (\i -> fromMaybe i $ IntMap.lookup i subMap1)) vv
      allsps = collectAllNoDec v1'
      v2' = genRst "" allsps roleF v1'
   in v2'

-- >>> parenSt "n"
-- "(n)"
parenSt :: String -> String
parenSt st = "(" <> st <> ")"

-- >>> parenSt' "n"
-- "'(n)"
parenSt' :: String -> String
parenSt' st = "\'(" <> st <> ")"

genMsgStr :: (Show role') => String -> String -> (role' -> Int) -> R role' [St] -> String
genMsgStr roleName stName roleF = \case
  Terminal _ -> ""
  (Arrow ann send recv st :> r) ->
    let ann1 = rToAnn r
        vst =
          "  "
            <> st
            <> " :: Msg"
            <> " "
            <> roleName
            <> " "
            <> stName
            <> " "
            <> show (ann !! roleF send)
            <> " "
            <> parenSt' (show send <> ", " <> showNoParen (ann1 !! roleF send))
            <> " "
            <> parenSt' (show recv <> ", " <> showNoParen (ann1 !! roleF recv))
            <> "\n"
     in vst <> genMsgStr roleName stName roleF r
  Branch _ vs -> concatMap (\(BranchVal _ r) -> genMsgStr roleName stName roleF r) vs

pipleR1 :: (Show role') => String -> String -> [role'] -> (role' -> Int) -> R role' () -> String
pipleR1 roleName stName roles roleF rr =
  "data Msg "
    <> roleName
    <> " "
    <> stName
    <> " from send recv where\n"
    <> genMsgStr roleName stName roleF (pipleR' roles roleF rr)

pipleR :: (Show role') => [role'] -> (role' -> Int) -> R role' () -> String
pipleR roles roleF rr =
  render
    roles
    roleF
    (\is -> [CenterFill ((i + 1) * width) ' ' (show v) | (i, v) <- zip [0 ..] is])
    (pipleR' roles roleF rr)

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

rv1' :: String
rv1' = pipleR1 "PingPongRole" "PingPong" [Client, Server, ClientBackup] pingPongToInt v1

{-
>>> error rv1'
>>> error rv1
data Msg PingPongRole PingPong from send recv
  Ping:: Msg PingPongRole PingPong (S0 True) '(Client, S2) '(Server, S2)
  Pong:: Msg PingPongRole PingPong S2 '(Server, End) '(Client, (S1 True))
  Add:: Msg PingPongRole PingPong (S1 True) '(Client, End) '(ClientBackup, End)
  Stop:: Msg PingPongRole PingPong (S0 False) '(Client, (S1 False)) '(Server, End)
  AStop:: Msg PingPongRole PingPong (S1 False) '(Client, End) '(ClientBackup, End)
-----------------Client--------------Server-----------ClientBackup--------------
                 (S0 s)              (S0 s)              (S1 s)
    ----------------------------------True----------------------------------
               (S0 True)             (S0 s)              (S1 s)
        Ping       |        --->       |
                   S2                  S2                (S1 s)
        Pong       |        <---       |
               (S1 True)              End                (S1 s)
        Add        |                  --->                 |
                  End                 End                 End
                                    Terminal

    ---------------------------------False----------------------------------
               (S0 False)            (S0 s)              (S1 s)
        Stop       |        --->       |
               (S1 False)             End                (S1 s)
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

v2' :: [Char]
v2' =
  pipleR1
    "Role"
    "BookSt"
    [Buyer, Seller, Buyer2]
    bookRoleToInt
    p

{-

>>> error v2'
data Msg Role BookSt from send recv where
  Title :: Msg Role BookSt S0 '(Buyer, S2 s) '(Seller, S2 s)
  BookNotFoun :: Msg Role BookSt (S2 BookNotFound) '(Seller, End) '(Buyer, S1 BookNotFound)
  SellerNotFoundBook :: Msg Role BookSt (S1 BookNotFound) '(Buyer, End) '(Buyer2, End)
  Price :: Msg Role BookSt (S2 BookFound) '(Seller, S3 s) '(Buyer, S1 BookFound)
  PriceToBuyer2 :: Msg Role BookSt (S1 BookFound) '(Buyer, S4) '(Buyer2, S4)
  HalfPrice :: Msg Role BookSt S4 '(Buyer2, S5 s) '(Buyer, S3 s)
  Afford :: Msg Role BookSt (S3 EnoughBudget) '(Buyer, S6) '(Seller, S6)
  Data :: Msg Role BookSt S6 '(Seller, End) '(Buyer, S5 EnoughBudget)
  Success :: Msg Role BookSt (S5 EnoughBudget) '(Buyer, End) '(Buyer2, End)
  NotBuy :: Msg Role BookSt (S3 NotEnoughBudget) '(Buyer, S5 NotEnoughBudget) '(Seller, End)
  Failed :: Msg Role BookSt (S5 NotEnoughBudget) '(Buyer, End) '(Buyer2, End)
-}
