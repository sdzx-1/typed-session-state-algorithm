{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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

module TypedSession.State.Piple where

import qualified TypedSession.State.Constraint as C
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
import Data.Foldable (Foldable (toList))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import TypedSession.State.Render
import TypedSession.State.Type
import TypedSession.State.Utils

------------------------
addNumsXTraverse
  :: forall r bst sig m
   . ( Has (Fresh :+: State [Int] :+: Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Ord r
     )
  => XTraverse m Creat AddNums r bst
addNumsXTraverse =
  ( \_ -> do
      i <- fresh
      let tmp = rRange @r
          sized = length tmp
          outNums = fmap (\x -> i * sized + fromEnum x) tmp
      inNums <- get
      put outNums
      pure (inNums, outNums)
  , const get
  , \(_, (r, ls)) -> do
      let len = length ls
      -- At least two branches.
      when (len < 2) (throwError (AtLeastTwoBranches (Branch () r ls)))
      void $ runState @(Maybe r) Nothing $ forM_ ls $ \(BranchSt _ _ prot) -> do
        -- The first message of each branch must have the same receiver and sender.
        case getFirstMsgInfo prot of
          Nothing -> throwError (BranchNoMsg prot)
          Just (from, to) -> do
            when (from /= r) $
              throwError (BranchFirstMsgMustHaveTheSameSender prot)
            get @(Maybe r) >>= \case
              Nothing -> put (Just to)
              Just to' ->
                when (to /= to') $
                  throwError (BranchFirstMsgMustHaveTheSameReceiver prot)
        -- Each branch sender must send (directly or indirectly) a message to all other receivers to notify the state change.
        let receivers = L.nub $ L.sort $ r : (fmap snd $ getAllMsgInfo prot)
        when (receivers /= [minBound .. maxBound]) (throwError (BranchNotNotifyAllOtherReceivers prot))
      -- create output
      inNums <- get
      pure (inNums, restoreWrapper @[Int])
  , const (pure ())
  , const get
  , const get
  )

toGenConstrXTraverse :: (Monad m) => XTraverse m AddNums (GenConst r) r bst
toGenConstrXTraverse =
  ( \(xv, (_, _, from, to, _)) -> pure (xv, (from, to))
  , \(is, (i, _)) -> pure (is, i)
  , \(xv, _) -> pure (xv, id)
  , \_ -> pure ()
  , \(xs, i) -> pure (xs, i)
  , \xv -> pure xv
  )

genConstrXFold
  :: forall r bst sig m
   . (Has (State (IntMap [Int]) :+: Writer (Seq C.Constraint) :+: Error (ProtocolError r bst)) sig m, Enum r)
  => XFold m (GenConst r) r bst
genConstrXFold =
  ( \(((is, os), (from, to)), _) -> do
      let ifrom = fromEnum from
          ito = fromEnum to
          from' = is !! ifrom --- is
          to' = is !! ito ------- is
          deleteIndexFromTo ks =
            fmap snd $ filter (\(idx, _) -> idx /= ifrom && idx /= ito) $ zip [0 ..] ks
      tellSeq $
        C.Constraint from' to'
          : zipWith C.Constraint (deleteIndexFromTo is) (deleteIndexFromTo os)
  , \((is, i), lb) ->
      gets (IntMap.lookup @[Int] i) >>= \case
        Just _ -> throwError @(ProtocolError r bst) (DefLabelMultTimes lb)
        Nothing -> modify (IntMap.insert i is)
  , \_ -> pure id
  , \_ -> pure ()
  , \((xs, i), gt) -> do
      gets (IntMap.lookup i) >>= \case
        Nothing -> throwError @(ProtocolError r bst) (LabelUndefined gt)
        Just ls -> tellSeq $ zipWith C.Constraint xs ls
  , \(xs) -> tellSeq $ zipWith C.Constraint xs (cycle [-1])
  )

replXTraverse :: (Monad m) => C.SubMap -> XTraverse m (GenConst r) (GenConst r) r bst
replXTraverse sbm =
  ( \(((a, b), (from, to)), _) ->
      pure ((replaceList sbm a, replaceList sbm b), (from, to))
  , \((xs, i), _) -> pure (replaceList sbm xs, i)
  , \(a, _) -> pure (replaceList sbm a, id)
  , \_ -> pure ()
  , \((xs, i), _) -> pure (replaceList sbm xs, i)
  , \xs -> pure (replaceList sbm xs)
  )

collectBranchDynValXFold :: (Has (State (Set Int)) sig m) => XFold m (GenConst r) r bst
collectBranchDynValXFold =
  ( \_ -> pure ()
  , \_ -> pure ()
  , \(ls, _) -> do
      modify (`Set.union` (Set.fromList ls))
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
  ( \(((is, _), (from, to)), _) -> do
      is' <- forM (zip rRange is) $
        \(key, i) -> genT @bst (\bst1 i1 -> if key == from then BstList i1 bst1 else TAny i1) i
      pure (is', (from, to))
  , \((ls, idx), _) -> do
      ls' <- mapM (genT (const TAny)) ls
      pure (ls', idx)
  , \(ls, _) -> do
      ls' <- mapM (genT (const TAny)) ls
      pure (ls', restoreWrapper @bst)
  , \(_, (bst, _)) -> put bst
  , \((is, i), _) -> do
      is' <- mapM (genT @bst (const TAny)) is
      pure (is', i)
  , \ls -> pure $ fmap (const TEnd) ls
  )

getFirstXV :: Protocol (MsgT r bst) r bst -> [T bst]
getFirstXV = \case
  Msg (xv, _) _ _ _ _ :> _ -> xv
  Label (xv, _) _ :> _ -> xv
  Branch xv _ _ -> xv
  Goto (xv, _) _ -> xv
  Terminal xv -> xv

genMsgT1XTraverse :: (Monad m, Enum r) => XTraverse m (MsgT r bst) (MsgT1 r bst) r bst
genMsgT1XTraverse =
  ( \((is, (from, to)), (_, _, _, _, prot)) -> do
      let os = getFirstXV prot
          from' = fromEnum from
          to' = fromEnum to
      pure ((is !! from', os !! from', os !! to'), (from, to))
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
  prot1 <-
    fmap (snd . snd)
      . runFresh 1
      . runState @[Int] (fmap fromEnum (rRange @r))
      $ xtraverse addNumsXTraverse prot0
  trace (TracerProtocolAddNum prot1)
  prot2 <- xtraverse toGenConstrXTraverse prot1
  trace (TracerProtocolGenConst prot2)
  (constraintList, _) <-
    runWriter @(Seq C.Constraint)
      . runState @(IntMap [Int]) (IntMap.empty)
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
  ( \( ((sendStart, sendEnd, recEnd), (from, to))
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