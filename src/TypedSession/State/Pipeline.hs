{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
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

module TypedSession.State.Pipeline where

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
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import qualified TypedSession.State.Constraint as C
import TypedSession.State.Render
import TypedSession.State.Type
import TypedSession.State.Utils

------------------------

newtype Index = Index Int deriving (Show, Eq, Ord, Num)

addIdxXTraverse
  :: forall r bst sig m
   . ( Has (State Int :+: State Index :+: State (Set Int) :+: Error (ProtocolError r bst)) sig m
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
      modify (Set.insert inputIdx)
      pure (inputIdx, id)
  , \_ -> do
      put (Index 0)
      modify @Int (+ 1)
  , const get
  , const get
  )

reRankXTraverse :: (Monad m) => IntMap Int -> XTraverse m Idx Idx r bst
reRankXTraverse sbm =
  ( \((a, b, idx), _) -> pure (replaceVal sbm a, replaceVal sbm b, idx)
  , \(xs, _) -> pure (replaceVal sbm xs)
  , \(a, _) -> pure (replaceVal sbm a, id)
  , \_ -> pure ()
  , \(xs, _) -> pure (replaceVal sbm xs)
  , \xs -> pure (replaceVal sbm xs)
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
    Nothing -> error "np"
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
   . (Has (State (Map r CurrSt) :+: State r :+: Error (ProtocolError r bst)) sig m, Eq r, Ord r, Enum r, Bounded r)
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
  , \(_, (r1, ls)) -> do
      r1CurrSt <- getRCurrSt r1
      when (r1CurrSt == Undecide) (throwError @(ProtocolError r bst) (UndecideStateCanNotStartBranch ls))
      for_ [r | r <- rRange, r /= r1] $ \r -> modify (Map.insert r Undecide)
      when (length ls < 1) (throwError @(ProtocolError r bst) BranchAtLeastOneBranch)
      put r1
      pure (restoreWrapper1 @r)
  , \_ -> pure ()
  , \_ -> pure ()
  , \_ -> pure ()
  )

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

replXTraverse :: (Monad m) => C.SubMap -> XTraverse m (GenConst r) (GenConst r) r bst
replXTraverse sbm =
  ( \(((a, b), (from, to), i), _) ->
      pure ((replaceList sbm a, replaceList sbm b), (from, to), i)
  , \((xs, i), _) -> pure (replaceList sbm xs, i)
  , \(a, _) -> pure (replaceList sbm a, id)
  , \_ -> pure ()
  , \((xs, i), _) -> pure (replaceList sbm xs, i)
  , \xs -> pure (replaceList sbm xs)
  )

collectBranchDynValXFold :: (Has (State (Set Int)) sig m, Enum r) => XFold m (GenConst r) r bst
collectBranchDynValXFold =
  ( \_ -> pure ()
  , \_ -> pure ()
  , \(ls, (r, _)) -> do
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
  , \(ls, (r, _)) -> do
      ls' <- mapM (\(idx, v) -> genT (if idx == fromEnum r then const TNum else (const TAny)) v) (zip [0 ..] ls)
      pure (ls', restoreWrapper @bst)
  , \(_, (bst, _)) -> put bst
  , \((is, i), _) -> do
      is' <- mapM (genT @bst (const TAny)) is
      pure (is', i)
  , \ls -> pure $ fmap (const TEnd) ls
  )

getFirstXV :: Protocol (MsgT r bst) r bst -> [T bst]
getFirstXV = \case
  Msg (xv, _, _) _ _ _ _ :> _ -> xv
  Label (xv, _) _ :> _ -> xv
  Branch xv _ _ -> xv
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

data PipleResult r bst = PipleResult
  { msgT :: Protocol (MsgT r bst) r bst
  , msgT1 :: Protocol (MsgT1 r bst) r bst
  , dnySet :: Set Int
  , stBound :: (Int, Int)
  }

reRank :: Set Int -> Int -> IntMap Int
reRank branchValSet maxSize =
  let allSet = Set.insert 0 branchValSet
      restList = [i | i <- [0 .. maxSize], i `Set.notMember` allSet]
   in IntMap.fromList $ zip (Set.toList allSet ++ restList) [0 ..]

piple'
  :: forall r bst sig m
   . ( Has (Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Eq r
     , Ord r
     )
  => (Tracer r bst -> m ())
  -> Protocol Creat r bst
  -> m (PipleResult r bst)
piple' trace prot0 = do
  trace (TracerProtocolCreat prot0)
  (brSet, (maxSzie, (_, idxProt))) <-
    runState @(Set Int) Set.empty
      . runState @Int 0
      . runState @Index (Index 100)
      $ (xtraverse addIdxXTraverse prot0)
  trace (TracerProtocolIdx idxProt)
  trace (TracerReRank (reRank brSet maxSzie))
  idxProt1 <- xtraverse (reRankXTraverse (reRank brSet maxSzie)) idxProt
  trace (TracerProtocolIdx idxProt1)
  prot1 <- xtraverse addNumsXTraverse idxProt1
  trace (TracerProtocolAddNum prot1)
  prot2 <- xtraverse toGenConstrXTraverse prot1
  trace (TracerProtocolGenConst prot2)
  void
    . runState @(Map r CurrSt) (Map.fromList $ zip (rRange @r) (cycle [Decide]))
    . runState @r undefined
    $ xfold checkProtXFold prot2
  (constraintList, _) <-
    runWriter @(Seq C.Constraint)
      . runState @(IntMap [Int]) (IntMap.empty)
      . runState @[Int] undefined
      $ xfold genConstrXFold prot2
  trace (TracerConstraints constraintList)
  let (sbm, stBound) = compressSubMap $ C.constrToSubMap $ toList constraintList
  trace (TracerSubMap sbm)
  prot3 <- xtraverse (replXTraverse sbm) prot2
  trace (TracerProtocolGenConstN prot3)
  dnys <- fst <$> runState @((Set Int)) (Set.empty) (xfold collectBranchDynValXFold prot3)
  trace (TracerCollectBranchDynVal dnys)
  prot4 <-
    fmap snd
      . runReader @(Set Int) dnys
      . runState @bst undefined
      $ (xtraverse genMsgTXTraverse prot3)
  trace (TracerProtocolMsgT prot4)
  prot5 <- xtraverse genMsgT1XTraverse prot4
  trace (TracerProtocolMsgT1 prot5)
  pure (PipleResult prot4 prot5 dnys stBound)

piple
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> Either
      (ProtocolError r bst)
      (PipleResult r bst)
piple protocol =
  run $ runError @(ProtocolError r bst) $ (piple' (const (pure ())) protocol)

pipleWithTracer
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> ( Seq (Tracer r bst)
     , Either
        (ProtocolError r bst)
        (PipleResult r bst)
     )
pipleWithTracer protocol =
  run
    . runWriter @(Seq (Tracer r bst))
    . runError @(ProtocolError r bst)
    $ (piple' (\w -> tell @(Seq (Tracer r bst)) (Seq.singleton w)) protocol)

genDocXFold
  :: forall r bst ann sig m
   . ( Has (Writer [Doc ann]) sig m
     , Show r
     , Show bst
     )
  => String -> String -> XFold m (MsgT1 r bst) r bst
genDocXFold rName protName =
  ( \( ((sendStart, sendEnd, recEnd), (from, to), _)
      , (cons, args, _, _, _)
      ) -> do
        tell @[Doc ann]
          [ pretty cons
              <+> "::"
              <+> pretty (L.intercalate "->" args)
              <+> (if null args then emptyDoc else "->")
              <+> "Msg"
              <+> pretty rName
              <+> pretty (protName <> "St")
              <+> parens (pretty $ show sendStart)
              <+> (pretty $ '\'' : show (from, sendEnd))
              <+> (pretty $ '\'' : show (to, recEnd))
          ]
  , \_ -> pure ()
  , \_ -> pure (id)
  , \_ -> pure ()
  , \_ -> pure ()
  , \_ -> pure ()
  )

genDoc :: forall r bst ann. (Show r, Show bst) => String -> String -> Protocol (MsgT1 r bst) r bst -> [Doc ann]
genDoc rName protName prot =
  fst $ run $ runWriter @[Doc ann] (xfold (genDocXFold @r @bst @ann rName protName) prot)

genGraph :: (Enum r, Bounded r, Show bst, Ord r, Show r) => StrFillEnv -> PipleResult r bst -> String
genGraph sfe PipleResult{msgT} = runRender sfe (stMsgT sfe) msgT