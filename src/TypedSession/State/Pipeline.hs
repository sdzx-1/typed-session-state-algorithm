{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypedSession.State.Pipeline (pipe, pipeWithTracer, genGraph, PipeResult (..)) where

import Control.Algebra ((:+:))
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Fresh.Strict
import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Monad
import Data.Foldable (Foldable (toList), for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified TypedSession.State.Constraint as C
import TypedSession.State.Render
import TypedSession.State.Type
import TypedSession.State.Utils

newtype Index = Index Int deriving (Show, Eq, Ord, Num)

addIdxXTraverse
  :: forall r bst sig m
   . ( Has (State Int :+: State Index) sig m
     , Enum r
     , Bounded r
     , Ord r
     )
  => XTraverse m Creat Idx r bst
addIdxXTraverse =
  ( \_ -> do
      inputIdx <- get @Int
      modify @Int (+ 1)
      Index idx <- get @Index
      modify @Index (+ 1)
      outputInx <- get @Int
      pure (inputIdx, outputInx, idx)
  , const get
  , \(_, _) -> do
      inputIdx <- get @Int
      pure (inputIdx, id)
  , \_ -> do
      put (Index 0)
      modify @Int (+ 1)
  , const get
  , const get
  )

addNumsXTraverse
  :: forall r bst sig m
   . ( Has (Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Ord r
     )
  => XTraverse m Idx AddNums r bst
addNumsXTraverse =
  let mkNums i =
        let sized = fromEnum (maxBound @r) + 1
         in fmap (\x -> i * sized + fromEnum x) (rRange @r)
   in ( \((va, vb, idx), _) -> pure (mkNums va, mkNums vb, idx)
      , \(va, _) -> pure $ mkNums va
      , \(va, _) -> pure (mkNums va, id)
      , \_ -> pure ()
      , \(va, _) -> pure $ mkNums va
      , \va -> pure $ mkNums va
      )

toGenConstrXTraverse :: (Monad m) => XTraverse m AddNums (GenConst r) r bst
toGenConstrXTraverse =
  ( \((a, b, i), (_, _, from, to, _)) -> pure ((a, b), (from, to), i)
  , \(is, (i, _)) -> pure (is, i)
  , \(xv, _) -> pure (xv, id)
  , \_ -> pure ()
  , \(xs, i) -> pure (xs, i)
  , \xv -> pure xv
  )

data CurrSt = Decide | Undecide deriving (Show, Eq, Ord)

getRCurrSt :: forall r sig m. (Has (State (Map r CurrSt)) sig m, Ord r) => r -> m CurrSt
getRCurrSt r =
  gets @(Map r CurrSt) (Map.lookup r) >>= \case
    Nothing -> error internalError
    Just v -> pure v

restoreWrapper1 :: forall r sig m a. (Has (State (Map r CurrSt) :+: State r) sig m) => m a -> m a
restoreWrapper1 m = do
  s1 <- get @(Map r CurrSt)
  s2 <- get @r
  a <- m
  put s1
  put s2
  pure a

checkProtXFold
  :: forall r bst sig m
   . ( Has (State (Map r CurrSt) :+: State r :+: Error (ProtocolError r bst)) sig m
     , Eq r
     , Ord r
     , Enum r
     , Bounded r
     , Show bst
     )
  => XFold m (GenConst r) r bst
checkProtXFold =
  ( \((_, (from, to), idx), (msgName, _, _, _, prot)) -> do
      when (idx == 0) $ do
        r1 <- get @r
        if from == r1
          then pure ()
          else throwError @(ProtocolError r bst) (BranchFirstMsgMustHaveTheSameSender r1 msgName from)
      fromCurrSt <- getRCurrSt from
      when (fromCurrSt == Undecide) (throwError @(ProtocolError r bst) (UndecideStateCanNotSendMsg msgName))
      modify (Map.insert to Decide)
      case prot of
        Terminal _ -> do
          vals <- gets @(Map r CurrSt) Map.elems
          when (any (/= Decide) vals) (throwError @(ProtocolError r bst) (TerminalNeedAllRoleDecide msgName))
        _ -> pure ()
  , \_ -> pure ()
  , \(_, (r1, _, ls)) -> do
      r1CurrSt <- getRCurrSt r1
      when (r1CurrSt == Undecide) (throwError @(ProtocolError r bst) (UndecideStateCanNotStartBranch ls))
      for_ [r | r <- rRange, r /= r1] $ \r -> modify (Map.insert r Undecide)
      when (length ls < 1) (throwError @(ProtocolError r bst) BranchAtLeastOneBranch)
      put r1
      pure (restoreWrapper1 @r)
  , \(_, (r, _, prot)) ->
      if isMsgExistBeforeNextTerm prot
        then pure ()
        else throwError @(ProtocolError r bst) (MsgDoNotExistBeforeNextTerm (show r))
  , \_ -> pure ()
  , \_ -> pure ()
  )

isMsgExistBeforeNextTerm :: Protocol eta r bst -> Bool
isMsgExistBeforeNextTerm = \case
  Msg{} :> _ -> True
  Label{} :> port -> isMsgExistBeforeNextTerm port
  _ -> False

genConstrXFold
  :: forall r bst sig m
   . (Has (State (IntMap [Int]) :+: State [Int] :+: Writer (Seq C.Constraint) :+: Error (ProtocolError r bst)) sig m, Enum r)
  => XFold m (GenConst r) r bst
genConstrXFold =
  ( \(((is, os), (from, to), index), _) -> do
      let ifrom = fromEnum from
          ito = fromEnum to
          from' = is !! ifrom --- is
          to' = is !! ito ------- is
          deleteIndexFromTo ks =
            fmap snd $ filter (\(idx, _) -> idx /= ifrom && idx /= ito) $ zip [0 ..] ks
          deleteIndexFrom ks =
            fmap snd $ filter (\(idx, _) -> idx /= ifrom) $ zip [0 ..] ks

      when (index == 0) $ do
        branchSts <- get @[Int]
        tellSeq $ map (uncurry C.Constraint) $ zip (deleteIndexFrom branchSts) (deleteIndexFrom is)

      tellSeq $
        C.Constraint from' to'
          : zipWith C.Constraint (deleteIndexFromTo is) (deleteIndexFromTo os)
  , \((is, i), lb) ->
      gets (IntMap.lookup @[Int] i) >>= \case
        Just _ -> throwError @(ProtocolError r bst) (DefLabelMultTimes lb)
        Nothing -> modify (IntMap.insert i is)
  , \(is, _) -> do
      put is
      pure (restoreWrapper @[Int])
  , \_ -> pure ()
  , \((xs, i), gt) -> do
      gets (IntMap.lookup i) >>= \case
        Nothing -> throwError @(ProtocolError r bst) (LabelUndefined gt)
        Just ls -> tellSeq $ zipWith C.Constraint xs ls
  , \(xs) -> tellSeq $ zipWith C.Constraint xs (cycle [-1])
  )

replXTraverse :: (Has (State (Set Int)) sig m) => C.SubMap -> XTraverse m (GenConst r) (GenConst r) r bst
replXTraverse sbm =
  ( \(((a, b), (from, to), i), _) -> do
      a' <- replaceList sbm a
      b' <- replaceList sbm b
      pure ((a', b'), (from, to), i)
  , \((xs, i), _) -> do
      xs' <- replaceList sbm xs
      pure (xs', i)
  , \(a, _) -> do
      a' <- replaceList sbm a
      pure (a', id)
  , \_ -> pure ()
  , \((xs, i), _) -> do
      xs' <- replaceList sbm xs
      pure (xs', i)
  , \xs -> replaceList sbm xs
  )

verifyProtXFold
  :: forall r bst sig m
   . (Has (State (IntMap (r, r)) :+: Error (ProtocolError r bst)) sig m, Enum r, Eq r)
  => XFold m (GenConst r) r bst
verifyProtXFold =
  ( \(((is, _), ft@(from, _to), _), _) -> do
      let from' = is !! fromEnum from
      res <- gets @(IntMap (r, r)) (IntMap.lookup from')
      case res of
        Nothing -> modify (IntMap.insert from' ft)
        Just ft1 -> when (ft1 /= ft) (throwError @(ProtocolError r bst) AStateOnlyBeUsedForTheSamePair)
  , \_ -> pure ()
  , \_ -> pure (restoreWrapper @(IntMap (r, r)))
  , \_ -> pure ()
  , \_ -> pure ()
  , \_ -> pure ()
  )

collectBranchDynValXFold :: (Has (State (Set Int)) sig m, Enum r) => XFold m (GenConst r) r bst
collectBranchDynValXFold =
  ( \_ -> pure ()
  , \_ -> pure ()
  , \(ls, (r, _, _)) -> do
      let ls' = map snd $ filter (\(i, _) -> i /= fromEnum r) $ zip [0 ..] ls
      modify (`Set.union` (Set.fromList ls'))
      pure id
  , \_ -> pure ()
  , \_ -> pure ()
  , \_ -> pure ()
  )

genT
  :: forall bst sig m
   . (Has (Reader (Set Int) :+: State bst) sig m)
  => (bst -> Int -> T bst) -> Int -> m (T bst)
genT fun i = do
  dynSet <- ask @(Set Int)
  if i == -1
    then pure (TEnd)
    else
      if Set.member i dynSet
        then do
          bst <- get
          pure (fun bst i)
        else pure $ TNum i

genMsgTXTraverse
  :: forall r bst sig m
   . (Has (Reader (Set Int) :+: State bst) sig m, Enum r, Eq r, Bounded r)
  => XTraverse m (GenConst r) (MsgT r bst) r bst
genMsgTXTraverse =
  ( \(((is, _), (from, to), vi), _) -> do
      is' <- forM (zip rRange is) $
        \(key, i) -> genT @bst (\bst1 i1 -> if key == from then BstList i1 bst1 else TAny i1) i
      pure (is', (from, to), vi)
  , \((ls, idx), _) -> do
      ls' <- mapM (genT (const TAny)) ls
      pure (ls', idx)
  , \(ls, (r, _, _)) -> do
      ls' <- mapM (\(idx, v) -> genT (if idx == fromEnum r then const TNum else (const TAny)) v) (zip [0 ..] ls)
      pure (ls', restoreWrapper @bst)
  , \(_, (bst, _, _)) -> put bst
  , \((is, i), _) -> do
      is' <- mapM (genT @bst (const TAny)) is
      pure (is', i)
  , \ls -> pure $ fmap (const TEnd) ls
  )

getFirstXV :: Protocol (MsgT r bst) r bst -> [T bst]
getFirstXV = \case
  Msg (xv, _, _) _ _ _ _ :> _ -> xv
  Label (xv, _) _ :> _ -> xv
  Branch xv _ _ _ -> xv
  Goto (xv, _) _ -> xv
  Terminal xv -> xv

genMsgT1XTraverse :: (Monad m, Enum r) => XTraverse m (MsgT r bst) (MsgT1 r bst) r bst
genMsgT1XTraverse =
  ( \((is, (from, to), i), (_, _, _, _, prot)) -> do
      let os = getFirstXV prot
          from' = fromEnum from
          to' = fromEnum to
      pure ((is !! from', os !! from', os !! to'), (from, to), i)
  , \(a, _) -> pure a
  , \(a, _) -> pure (a, id)
  , \(a, _) -> pure a
  , \(a, _) -> pure a
  , \a -> pure a
  )

collAllMsgBAs :: (Has (Writer [(String, [T bst], [T bst])]) sig m) => XFold m (MsgT r bst) r bst
collAllMsgBAs =
  ( \((is, _, _), (cname, _, _, _, prot)) -> do
      let os = getFirstXV prot
      tell [(cname, is, os)]
  , \_ -> pure ()
  , \_ -> pure id
  , \_ -> pure ()
  , \_ -> pure ()
  , \_ -> pure ()
  )

data PipeResult r bst = PipeResult
  { msgT :: Protocol (MsgT r bst) r bst
  , msgT1 :: Protocol (MsgT1 r bst) r bst
  , dnySet :: Set Int
  , stList :: [Int]
  , branchResultTypeInfo :: [(String, [(bst, [[String]], T bst)])]
  , branchFunList :: [(r, String, T bst)]
  , allMsgBATypes :: [(String, [T bst], [T bst])]
  }

genBranchResultTIXFold
  :: forall r bst sig m
   . ( Has (State String :+: Writer [(r, String, T bst)] :+: (State (Map String [(bst, [[String]], T bst)]))) sig m
     , Enum r
     )
  => XFold m (MsgT1 r bst) r bst
genBranchResultTIXFold =
  ( \_ -> pure ()
  , \_ -> pure ()
  , \(ts, (r, st, _)) -> do
      tell [(r, st, ts !! (fromEnum r))]
      put st
      pure (restoreWrapper @String)
  , \(_, (bst, args, prot)) -> do
      case getNextT prot of
        Nothing -> error internalError
        Just t -> do
          name <- get @String
          modify @(Map String [(bst, [[String]], T bst)]) (Map.insertWith (<>) name [(bst, args, t)])
  , \_ -> pure ()
  , \_ -> pure ()
  )

getNextT :: Protocol (MsgT1 r bst) r bst -> Maybe (T bst)
getNextT = \case
  Msg ((a, _, _), _, _) _ _ _ _ :> _ -> Just a
  Label{} :> prot -> getNextT prot
  _ -> Nothing

pipe'
  :: forall r bst sig m
   . ( Has (Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Eq r
     , Ord r
     , Show bst
     )
  => (Tracer r bst -> m ())
  -> Protocol Creat r bst
  -> m (PipeResult r bst)
pipe' trace prot0 = do
  trace (TracerProtocolCreat prot0)
  idxProt <-
    fmap (snd . snd)
      . runState @Int 0
      . runState @Index (Index 1_000_000)
      $ (xtraverse addIdxXTraverse prot0)
  trace (TracerProtocolIdx idxProt)
  prot1 <- xtraverse addNumsXTraverse idxProt
  trace (TracerProtocolAddNum prot1)
  prot2 <- xtraverse toGenConstrXTraverse prot1
  trace (TracerProtocolGenConst prot2)
  void
    . runState @(Map r CurrSt) (Map.fromList $ zip (rRange @r) (cycle [Decide]))
    . runState @r (error internalError)
    $ xfold checkProtXFold prot2
  (constraintList, _) <-
    runWriter @(Seq C.Constraint)
      . runState @(IntMap [Int]) (IntMap.empty)
      . runState @[Int] (error internalError)
      $ xfold genConstrXFold prot2
  trace (TracerConstraints constraintList)
  let sbm = C.constrToSubMap $ toList constraintList
  (stSet, prot3) <- runState (Set.singleton (-1)) $ xtraverse (replXTraverse sbm) prot2
  trace (TracerSubMapAndStList (sbm, stSet))
  trace (TracerProtocolGenConstN prot3)
  verifyResult <- fst <$> runState @(IntMap (r, r)) (IntMap.empty) (xfold verifyProtXFold prot3)
  trace (TracerVerifyResult verifyResult)
  dnys <- fst <$> runState @((Set Int)) (Set.empty) (xfold collectBranchDynValXFold prot3)
  trace (TracerCollectBranchDynVal dnys)
  prot4 <-
    fmap snd
      . runReader @(Set Int) dnys
      . runState @bst (error internalError)
      $ (xtraverse genMsgTXTraverse prot3)
  trace (TracerProtocolMsgT prot4)
  allMsgBAs <- fst <$> runWriter @[(String, [T bst], [T bst])] (xfold collAllMsgBAs prot4)
  prot5 <- xtraverse genMsgT1XTraverse prot4
  trace (TracerProtocolMsgT1 prot5)
  (bfl, (_, (branchTIMap, _))) <-
    runWriter @[(r, String, T bst)]
      . runState @String (error internalError)
      . runState @(Map String [(bst, [[String]], T bst)]) Map.empty
      $ xfold genBranchResultTIXFold prot5
  trace (TracerBranchResultTI bfl branchTIMap)
  pure (PipeResult prot4 prot5 dnys (Set.toList stSet) (Map.toList branchTIMap) bfl allMsgBAs)

pipe
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r, Show bst)
  => Protocol Creat r bst
  -> Either
      (ProtocolError r bst)
      (PipeResult r bst)
pipe protocol =
  run $ runError @(ProtocolError r bst) $ (pipe' (const (pure ())) protocol)

pipeWithTracer
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r, Show bst)
  => Protocol Creat r bst
  -> ( Seq (Tracer r bst)
     , Either
        (ProtocolError r bst)
        (PipeResult r bst)
     )
pipeWithTracer protocol =
  run
    . runWriter @(Seq (Tracer r bst))
    . runError @(ProtocolError r bst)
    $ (pipe' (\w -> tell @(Seq (Tracer r bst)) (Seq.singleton w)) protocol)

genGraph :: (Enum r, Bounded r, Show bst, Ord r, Show r) => PipeResult r bst -> String
genGraph PipeResult{msgT} = runRender msgT