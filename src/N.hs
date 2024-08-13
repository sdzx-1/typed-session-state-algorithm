{-# LANGUAGE AllowAmbiguousTypes #-}
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

module N where

import qualified Constraint as C
import Control.Algebra ((:+:))
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.Error
import Control.Effect.Writer
import Control.Monad
import Data.Foldable (Foldable (toList), for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Constraint, Type)
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
  | BranchNoMsg
  | BranchFirstMsgMustHaveTheSameReceiver
  | BranchFirstMsgMustHaveTheSameSender

instance Show (ProtocolError r bst) where
  show = \case
    AtLeastTwoBranches _i _ls -> "At least two branches are required"
    DefLabelMultTimes _ -> "Defining Label multiple times"
    LabelUndefined _ -> "Label Undefined"
    BranchNoMsg -> "Branch No Msg"
    BranchFirstMsgMustHaveTheSameReceiver ->
      "The first message of each branch must have the same receiver."
    BranchFirstMsgMustHaveTheSameSender ->
      "The first message of each branch must have the same sender."

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
     , Eq r
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
    -- At least two branches.
    when (len < 2) (throwError (AtLeastTwoBranches len ls))
    -- The first message of each branch must have the same receiver and sender.
    tos <- forM ls $ \(BranchSt _ prot) -> do
      case getFirstMsgInfo prot of
        Nothing -> throwError @(ProtocolError r bst) BranchNoMsg
        Just (from, to) -> do
          when (from /= r) (throwError @(ProtocolError r bst) BranchFirstMsgMustHaveTheSameReceiver)
          pure to
    when (any (/= head tos) tos) (throwError @(ProtocolError r bst) BranchFirstMsgMustHaveTheSameSender)

    -- Each branch sender must send a message to all other receivers to notify the state change.
    (ins, ls') <- go inputNums ls
    pure (ins, Branch r ls')
  Goto _ i -> pure (inputNums, Goto inputNums i)
  Terminal _ -> pure (inputNums, Terminal inputNums)

getFirstMsgInfo :: Protocol eta r bst -> Maybe (r, r)
getFirstMsgInfo = \case
  msgOrLabel :> prots -> case msgOrLabel of
    Msg _ _ _ from to -> Just (from, to)
    _ -> getFirstMsgInfo prots
  _ -> Nothing

go
  :: forall r bst sig m
   . ( Has (Fresh :+: Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Eq r
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
   . (Enum r, Bounded r, Eq r)
  => Protocol Creat r bst -> Either (ProtocolError r bst) (Protocol AddNums r bst)
addNums protoc =
  fmap snd
    . snd
    . run
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

compressSubMap :: C.SubMap -> C.SubMap
compressSubMap sbm' =
  let (minKey, maxKey) = (fst $ IntMap.findMin sbm', fst $ IntMap.findMax sbm')
      list = [minKey .. maxKey]
      (keys, vals) = (list, fmap (\k -> fromMaybe k $ IntMap.lookup k sbm') list)
      minVal = minimum vals
      tmap = IntMap.fromList $ zip (L.nub $ L.sort vals) [minVal, minVal + 1 ..]
      vals' = fmap (\k -> fromJust $ IntMap.lookup k tmap) vals
   in IntMap.fromList $ zip keys vals'

genSubMap
  :: forall r bst
   . (Enum r)
  => Protocol AddNums r bst
  -> Either (ProtocolError r bst) C.SubMap
genSubMap protc =
  (\(a, (_, b)) -> (const (compressSubMap $ C.constrToSubMap $ toList a) <$> b))
    . run
    . runWriter @(Seq C.Constraint)
    . runState @(IntMap [Int]) (IntMap.empty)
    $ runError @(ProtocolError r bst) (genConstraint' protc)

replaceList :: C.SubMap -> [Int] -> [Int]
replaceList sbm ls = fmap (\k -> fromMaybe k $ IntMap.lookup k sbm) ls

replaceNums :: C.SubMap -> Protocol AddNums r bst -> Protocol AddNums r bst
replaceNums sbm = \case
  msgOrLabel :> prots ->
    let prots' = replaceNums sbm prots
     in case msgOrLabel of
          Msg (a, b) c d e f ->
            Msg (replaceList sbm a, replaceList sbm b) c d e f :> prots'
          Label a i -> Label (replaceList sbm a) i :> prots'
  Branch r ls -> Branch r $ fmap (\(BranchSt v prots) -> BranchSt v (replaceNums sbm prots)) ls
  Goto xv i -> Goto (replaceList sbm xv) i
  Terminal xv -> Terminal (replaceList sbm xv)

piple
  :: (Enum r, Bounded r, Eq r)
  => Protocol Creat r bst
  -> Either (ProtocolError r bst) (Protocol AddNums r bst)
piple prot = do
  prot' <- addNums prot
  sbm <- genSubMap prot'
  pure $ replaceNums sbm prot'

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
