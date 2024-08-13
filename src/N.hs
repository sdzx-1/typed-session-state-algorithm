{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module N where

import qualified Constraint as C
import Control.Algebra (Has, (:+:))
import qualified Control.Applicative as C
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.Error
import Control.Effect.Fresh
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad
import Data.Foldable (Foldable (toList), for_)
import Data.Functor.Identity (Identity (runIdentity))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Constraint, Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Void

data BranchSt eta r bst = BranchSt bst (Protocol eta r bst)
  deriving (Functor)

data MsgOrLabel eta r
  = Msg (XMsg eta) String [String] r r
  | Label (XLabel eta) Int
  deriving (Functor)

infixr 5 :>

data Protocol eta r bst
  = (MsgOrLabel eta r) :> (Protocol eta r bst)
  | Branch r [BranchSt eta r bst]
  | Goto (XGoto eta) Int
  | Terminal (XTerminal eta)
  deriving (Functor)

instance (ForallX Show eta, Show r, Show bst) => Show (Protocol eta r bst) where
  show = ppProtocol

type family XMsg eta
type family XLabel eta
type family XGoto eta
type family XTerminal eta

type ForallX (f :: Type -> Constraint) eta =
  (f (XMsg eta), f (XLabel eta), f (XGoto eta), f (XTerminal eta))

data ProtocolError r bst
  = AtLeastTwoBranches Int [BranchSt Creat r bst]
  | DefLabelMultTimes (MsgOrLabel AddNums r)
  | LabelUndefined (Protocol AddNums r bst)

instance Show (ProtocolError r bst) where
  show = \case
    AtLeastTwoBranches _i _ls -> "At least two branches are required"
    DefLabelMultTimes _ -> "Defining Label multiple times"
    LabelUndefined _ -> "Label Undefined"

------------------------
data Creat

type instance XMsg Creat = ()
type instance XLabel Creat = ()
type instance XGoto Creat = ()
type instance XTerminal Creat = ()

data AddNums

type instance XMsg AddNums = ([Int], [Int])
type instance XLabel AddNums = [Int]
type instance XGoto AddNums = [Int]
type instance XTerminal AddNums = [Int]

addNums'
  :: forall r bst sig m
   . ( Has (Fresh :+: Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     )
  => [Int] -> Protocol Creat r bst -> m ([Int], Protocol AddNums r bst)
addNums' inputNums = \case
  msgOrLabel :> ms -> do
    case msgOrLabel of
      Msg _ a b c d -> do
        i <- fresh
        let tmp = [minBound @r .. maxBound]
            sized = length tmp
            nums = fmap (\x -> i * sized + fromEnum x) tmp
        (is', ms') <- addNums' nums ms
        pure (is', Msg (inputNums, nums) a b c d :> ms')
      Label _ i -> do
        (is', ms') <- addNums' inputNums ms
        pure (is', Label inputNums i :> ms')
  Branch r ls -> do
    let len = length ls
    when (len < 2) (throwError (AtLeastTwoBranches len ls))
    (ins, ls') <- go inputNums ls
    pure (ins, Branch r ls')
  Goto _ i -> pure (inputNums, Goto inputNums i)
  Terminal _ -> pure (inputNums, Terminal inputNums)

go
  :: forall r bst sig m
   . ( Has (Fresh :+: Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     )
  => [Int] -> [BranchSt Creat r bst] -> m ([Int], [BranchSt AddNums r bst])
go inputNums = \case
  [] -> pure (inputNums, [])
  BranchSt bst prot : xs -> do
    (_, prot') <- addNums' inputNums prot
    (ins'', branchs) <- go inputNums xs
    pure (ins'', BranchSt bst prot' : branchs)

addNums
  :: forall r bst
   . (Enum r, Bounded r)
  => Protocol Creat r bst -> Either (ProtocolError r bst) (Protocol AddNums r bst)
addNums protoc =
  fmap snd
    . snd
    . runIdentity
    . runFresh 1
    . runError @(ProtocolError r bst)
    $ (addNums' (fmap fromEnum [minBound @r .. maxBound]) protoc)

----------------------------------

tellSeq :: (Has (Writer (Seq a)) sig m) => [a] -> m ()
tellSeq ls = tell (Seq.fromList ls)

genConstraint'
  :: forall r bst sig m
   . (Has (State (IntMap [Int]) :+: Writer (Seq C.Constraint) :+: Error (ProtocolError r bst)) sig m, Enum r)
  => Protocol AddNums r bst -> m ()
genConstraint' = \case
  msgOrLabel :> prots -> do
    case msgOrLabel of
      Msg (is, os) _ _ from to -> do
        let ifrom = fromEnum from
            ito = fromEnum to
            from' = is !! ifrom --- is
            to' = is !! ito --- is
            deleteIndexFromTo ks = fmap snd $ filter (\(idx, _) -> idx /= ifrom && idx /= ito) $ zip [0 ..] ks
        tellSeq $ C.Constraint from' to' : zipWith C.Constraint (deleteIndexFromTo is) (deleteIndexFromTo os)
      lb@(Label is i) -> do
        gets @(IntMap [Int]) (IntMap.lookup i) >>= \case
          Just _ -> throwError @(ProtocolError r bst) (DefLabelMultTimes lb)
          Nothing -> modify (IntMap.insert i is)
    genConstraint' prots
  Branch _ ls -> for_ ls $ \(BranchSt _ prot) -> genConstraint' prot
  gt@(Goto xs i) -> do
    gets @(IntMap [Int]) (IntMap.lookup i) >>= \case
      Nothing -> throwError (LabelUndefined gt)
      Just ls -> tellSeq $ zipWith C.Constraint xs ls
  Terminal xs -> tellSeq $ zipWith C.Constraint xs (cycle [-1])

genSubMap
  :: forall r bst
   . (Enum r)
  => Protocol AddNums r bst
  -> Either (ProtocolError r bst) C.SubMap
genSubMap protc =
  (\(a, (_, b)) -> (const (C.constrToSubMap $ toList a) <$> b))
    . run
    . runWriter @(Seq C.Constraint)
    . runState @(IntMap [Int]) (IntMap.empty)
    $ runError @(ProtocolError r bst) (genConstraint' protc)

----------------------------------

ppProtocol :: (ForallX Show eta, Show r, Show bst) => Protocol eta r bst -> String
ppProtocol v = case v of
  msgOrLabel :> prot ->
    let r1 = case msgOrLabel of
          Msg xmsg a b c d ->
            "Msg " ++ show xmsg ++ " " ++ a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
          Label xlabel i -> "Label " ++ show xlabel ++ " " ++ show i
     in r1 ++ "\n" ++ ppProtocol prot
  Branch r ls ->
    "Branch "
      ++ show r
      ++ "\n"
      ++ concatMap
        ( \(BranchSt bst prot) ->
            "BranchSt " ++ show bst ++ "\n" ++ ppProtocol prot
        )
        ls
  Goto xv i -> "Goto " ++ show xv ++ " " ++ show i ++ "\n"
  Terminal xv -> "Terminal " ++ show xv ++ "\n"

----------------------------------
data PingPong = Client | Server | Counter
  deriving (Show, Eq, Ord, Enum, Bounded)

v1 :: Protocol Creat PingPong Bool
v1 =
  Label () 0
    :> Branch
      Client
      [ BranchSt True $
          Msg () "Ping" [] Client Server
            :> Msg () "Pong" [] Server Client
            :> Msg () "Add" [] Client Counter
            :> Goto () 0
      , BranchSt False $
          Msg () "Stop" [] Client Server
            :> Msg () "AStop" [] Client Counter
            :> Terminal ()
      ]

-- >>> error "------------------------"
-- >>> error $ show v1
-- >>> error "------------------------"
-- >>> error $ show (addNums v1)
-- >>> error "------------------------"
-- >>> error $ show (addNums v1 >>= genSubMap)
-- ------------------------
-- Label () 0
-- Branch Client
-- BranchSt True
-- Msg () Ping [] Client Server
-- Msg () Pong [] Server Client
-- Msg () Add [] Client Counter
-- Goto () 0
-- BranchSt False
-- Msg () Stop [] Client Server
-- Msg () AStop [] Client Counter
-- Terminal ()
-- ------------------------
-- Right Label [0,1,2] 0
-- Branch Client
-- BranchSt True
-- Msg ([0,1,2],[3,4,5]) Ping [] Client Server
-- Msg ([3,4,5],[6,7,8]) Pong [] Server Client
-- Msg ([6,7,8],[9,10,11]) Add [] Client Counter
-- Goto [9,10,11] 0
-- BranchSt False
-- Msg ([0,1,2],[12,13,14]) Stop [] Client Server
-- Msg ([12,13,14],[15,16,17]) AStop [] Client Counter
-- Terminal [15,16,17]
-- ------------------------
-- Right (fromList [(1,0),(4,3),(5,2),(6,2),(7,0),(8,2),(9,0),(10,0),(11,2),(12,2),(13,-1),(14,2),(15,-1),(16,-1),(17,-1)])
