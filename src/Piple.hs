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

addIndex :: (Has Fresh sig m) => R role' bSt () -> m (R role' bSt Int)
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

addSt :: Int -> R role' bSt Int -> R role' bSt [Int]
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

rToConstraints :: Int -> (role' -> Int) -> R role' bSt Int -> [Constraint]
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

rAllInt :: R role' bSt [Int] -> SubMap
rAllInt r =
  let v = L.delete (-1) $ L.nub $ concat $ rAnnToList r
   in IntMap.fromList $ (-1, -1) : zip v [0 ..]

type SPSet = Set Int

collectAllNoDec' :: R role' bSt [Int] -> [Int]
collectAllNoDec' = \case
  Terminal _ann -> []
  (Arrow _ann _ _ _ :> r) -> collectAllNoDec' r
  Branch ann vs ->
    ann <> concatMap (\(BranchVal _ r) -> collectAllNoDec' r) vs

collectAllNoDec :: R role' bSt [Int] -> SPSet
collectAllNoDec r = Set.fromList $ collectAllNoDec' r

bStTobSts :: [bSt] -> R role' bSt [Int] -> R role' [bSt] [Int]
bStTobSts bst = \case
  Terminal ls -> Terminal ls
  (arr :> r) -> arr :> bStTobSts bst r
  Branch ann vs ->
    Branch
      ann
      ( map
          ( \(BranchVal v r) ->
              let v' = v : bst
               in BranchVal v' (bStTobSts v' r)
          )
          vs
      )

data St bSt
  = St Int
  | StBrSend [bSt] Int
  | StBrRecv Int
  | TerminalSt

instance (Show bSt) => Show (St bSt) where
  show = \case
    TerminalSt -> "End"
    St i -> "S" ++ show i
    StBrSend st i -> "(S" ++ show i ++ " " ++ show st <> ")"
    StBrRecv i -> "(S" ++ show i ++ " s)"

showNoParen :: (Show bSt) => St bSt -> [Char]
showNoParen = \case
  TerminalSt -> "End"
  St i -> "S" ++ show i
  StBrSend st i -> "S" ++ show i ++ " " ++ show st
  StBrRecv i -> "S" ++ show i ++ " s"

genRst :: [bSt] -> SPSet -> (role' -> Int) -> R role' [bSt] [Int] -> R role' [bSt] [St bSt]
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

pipleR' :: (Show role') => [role'] -> (role' -> Int) -> R role' bSt () -> R role' [bSt] [St bSt]
pipleR' roles roleF rr =
  let roleNum = length roles
      rInt = snd $ runIdentity $ runFresh 0 $ addIndex rr
      rcs = constrToSubMap $ rToConstraints roleNum roleF rInt
      vv = fmap (\i -> fromMaybe i $ IntMap.lookup i rcs) <$> addSt roleNum rInt
      subMap1 = rAllInt vv
      v1' = fmap (fmap (\i -> fromMaybe i $ IntMap.lookup i subMap1)) vv
      allsps = collectAllNoDec v1'
      v1'' = bStTobSts [] v1'
      v3' = genRst [] allsps roleF v1''
   in v3'

-- >>> parenSt "n"
-- "(n)"
parenSt :: String -> String
parenSt st = "(" <> st <> ")"

-- >>> parenSt' "n"
-- "'(n)"
parenSt' :: String -> String
parenSt' st = "\'(" <> st <> ")"

splSt :: String -> (String, String)
splSt st = case words st of
  [] -> error "np"
  x : xs -> (x, concatMap (<> " -> ") xs)

genMsgStr :: (Show role', Show bSt) => String -> String -> (role' -> Int) -> R role' [bSt] [St bSt] -> String
genMsgStr roleName stName roleF = \case
  Terminal _ -> ""
  (Arrow ann send recv st :> r) ->
    let ann1 = rToAnn r
        (sth, stt) = splSt st
        vst =
          "  "
            <> sth
            <> " :: "
            <> stt
            <> "Msg"
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

pipleR1 :: (Show role', Show bSt) => String -> String -> [role'] -> (role' -> Int) -> R role' bSt () -> String
pipleR1 roleName stName roles roleF rr =
  "data Msg "
    <> roleName
    <> " "
    <> stName
    <> " from send recv where\n"
    <> genMsgStr roleName stName roleF (pipleR' roles roleF rr)

pipleR :: (Show role', Show bSt) => [role'] -> (role' -> Int) -> R role' bSt () -> String
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

data PPSt = PTrue | PFalse
  deriving (Show)

v1 :: R PingPong PPSt ()
v1 =
  branch
    [ branchVal PTrue $
        (Client --> Server) "Ping"
          :> (Client <-- Server) "Pong"
          :> (Client --> ClientBackup) "Add"
          :> terminal
    , branchVal PFalse $
        (Client --> Server) "Stop"
          :> (Client --> ClientBackup) "AStop"
          :> terminal
    ]

rv1 :: String
rv1 = pipleR [Client, Server, ClientBackup] pingPongToInt v1

rv1' :: String
rv1' = pipleR1 "PingPongRole" "PingPong" [Client, Server, ClientBackup] pingPongToInt v1

{-
>>> error rv1
---------------------------Client------------------------Server---------------------ClientBackup------------------------
                           (S0 s)                        (S0 s)                        (S1 s)
    ----------------------------------------------------[PTrue]-----------------------------------------------------
                        (S0 [PTrue])                     (S0 s)                        (S1 s)
             Ping            |            ----->           |
                             S2                            S2                          (S1 s)
             Pong            |            <-----           |
                        (S1 [PTrue])                      End                          (S1 s)
             Add             |                           ----->                          |
                            End                           End                           End
                                                        Terminal

    ----------------------------------------------------[PFalse]----------------------------------------------------
                       (S0 [PFalse])                     (S0 s)                        (S1 s)
             Stop            |            ----->           |
                       (S1 [PFalse])                      End                          (S1 s)
            AStop            |                           ----->                          |
                            End                           End                           End
                                                        Terminal

-}

-- -----------------------------------------------------------------------
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

data BookBranchSt
  = NotFound
  | Found
  | One
  | Two
  | Support
  | NotSupport
  | Enough
  | NotEnough
  deriving (Show)

p :: R BookRole BookBranchSt ()
p =
  (Buyer --> Seller) "Title String"
    :> branch
      [ branchVal NotFound $
          (Buyer <-- Seller) "NoBook"
            :> (Buyer --> Buyer2) "SellerNoBook"
            :> terminal
      , branchVal Found $
          (Buyer <-- Seller) "Price Int"
            :> branch
              [ branchVal One $
                  (Buyer --> Buyer2) "OneAfford"
                    :> (Buyer --> Seller) "OneAccept"
                    :> (Buyer <-- Seller) "OneDate Date"
                    :> (Buyer --> Buyer2) "OneSuccess Date"
                    :> terminal
              , branchVal Two $
                  (Buyer --> Buyer2) "PriceToBuyer2 Int"
                    :> branch
                      [ branchVal NotSupport $
                          (Buyer <-- Buyer2) "NotSupport"
                            :> (Buyer --> Seller) "TwoNotBuy"
                            :> terminal
                      , branchVal Support $
                          (Buyer <-- Buyer2) "SupportVal Int"
                            :> branch
                              [ branchVal Enough $
                                  ( (Buyer --> Seller) "TwoAccept"
                                      :> (Buyer <-- Seller) "TwoDate Date"
                                      :> (Buyer --> Buyer2) "TwoSuccess Date"
                                      :> terminal
                                  )
                              , branchVal NotEnough $
                                  ( (Buyer --> Seller) "TwoNotBuy1"
                                      :> (Buyer --> Buyer2) "TwoFailed"
                                      :> terminal
                                  )
                              ]
                      ]
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
  Title :: String -> Msg Role BookSt S0 '(Buyer, S2 s) '(Seller, S2 s)
  NoBook :: Msg Role BookSt (S2 [NotFound]) '(Seller, End) '(Buyer, S1 [NotFound])
  SellerNoBook :: Msg Role BookSt (S1 [NotFound]) '(Buyer, End) '(Buyer2, End)
  Price :: Int -> Msg Role BookSt (S2 [Found]) '(Seller, S3 s) '(Buyer, S1 s)
  OneAfford :: Msg Role BookSt (S1 [One,Found]) '(Buyer, S3 [One,Found]) '(Buyer2, S4)
  OneAccept :: Msg Role BookSt (S3 [One,Found]) '(Buyer, S5) '(Seller, S5)
  OneDate :: Date -> Msg Role BookSt S5 '(Seller, End) '(Buyer, S4)
  OneSuccess :: Date -> Msg Role BookSt S4 '(Buyer, End) '(Buyer2, End)
  PriceToBuyer2 :: Int -> Msg Role BookSt (S1 [Two,Found]) '(Buyer, S6 s) '(Buyer2, S6 s)
  NotSupport :: Msg Role BookSt (S6 [NotSupport,Two,Found]) '(Buyer2, End) '(Buyer, S3 [NotSupport,Two,Found])
  TwoNotBuy :: Msg Role BookSt (S3 [NotSupport,Two,Found]) '(Buyer, End) '(Seller, End)
  SupportVal :: Int -> Msg Role BookSt (S6 [Support,Two,Found]) '(Buyer2, S7 s) '(Buyer, S3 s)
  TwoAccept :: Msg Role BookSt (S3 [Enough,Support,Two,Found]) '(Buyer, S8) '(Seller, S8)
  TwoDate :: Date -> Msg Role BookSt S8 '(Seller, End) '(Buyer, S7 [Enough,Support,Two,Found])
  TwoSuccess :: Date -> Msg Role BookSt (S7 [Enough,Support,Two,Found]) '(Buyer, End) '(Buyer2, End)
  TwoNotBuy1 :: Msg Role BookSt (S3 [NotEnough,Support,Two,Found]) '(Buyer, S7 [NotEnough,Support,Two,Found]) '(Seller, End)
  TwoFailed :: Msg Role BookSt (S7 [NotEnough,Support,Two,Found]) '(Buyer, End) '(Buyer2, End)

---------------------------Buyer-------------------------Seller------------------------Buyer2---------------------------
                             S0                            S0                          (S1 s)
            Title            |            ----->           |
                           (S2 s)                        (S2 s)                        (S1 s)
    ---------------------------------------------------[NotFound]---------------------------------------------------
                           (S2 s)                   (S2 [NotFound])                    (S1 s)
            NoBook           |            <-----           |
                      (S1 [NotFound])                     End                          (S1 s)
         SellerNoBook        |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
    ----------------------------------------------------[Found]-----------------------------------------------------
                           (S2 s)                     (S2 [Found])                     (S1 s)
            Price            |            <-----           |
                           (S1 s)                        (S3 s)                        (S1 s)
        ----------------------------------------------[One,Found]-----------------------------------------------
                      (S1 [One,Found])                   (S3 s)                        (S1 s)
          OneAfford          |                           ----->                          |
                      (S3 [One,Found])                   (S3 s)                          S4
          OneAccept          |            ----->           |
                             S5                            S5                            S4
           OneDate           |            <-----           |
                             S4                           End                            S4
          OneSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
        ----------------------------------------------[Two,Found]-----------------------------------------------
                      (S1 [Two,Found])                   (S3 s)                        (S1 s)
        PriceToBuyer2        |                           ----->                          |
                           (S6 s)                        (S3 s)                        (S6 s)
            -------------------------------------[NotSupport,Two,Found]-------------------------------------
                           (S6 s)                        (S3 s)             (S6 [NotSupport,Two,Found])
          NotSupport         |                           <-----                          |
                (S3 [NotSupport,Two,Found])              (S3 s)                         End
          TwoNotBuy          |            ----->           |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
            --------------------------------------[Support,Two,Found]---------------------------------------
                           (S6 s)                        (S3 s)               (S6 [Support,Two,Found])
          SupportVal         |                           <-----                          |
                           (S3 s)                        (S3 s)                        (S7 s)
                -------------------------------[Enough,Support,Two,Found]-------------------------------
              (S3 [Enough,Support,Two,Found])            (S3 s)                        (S7 s)
          TwoAccept          |            ----->           |
                             S8                            S8                          (S7 s)
           TwoDate           |            <-----           |
              (S7 [Enough,Support,Two,Found])             End                          (S7 s)
          TwoSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
                -----------------------------[NotEnough,Support,Two,Found]------------------------------
             (S3 [NotEnough,Support,Two,Found])          (S3 s)                        (S7 s)
          TwoNotBuy1         |            ----->           |
             (S7 [NotEnough,Support,Two,Found])           End                          (S7 s)
          TwoFailed          |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
-}
