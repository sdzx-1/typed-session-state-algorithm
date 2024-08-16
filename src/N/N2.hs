{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module N.N2 where

import qualified Constraint as C
import Control.Algebra ((:+:))
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.Error
import Control.Effect.Writer
import Control.Monad
import Data.Foldable (Foldable (toList))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import N.Type
import N.Utils

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
      let tmp = [minBound @r .. maxBound]
          sized = length tmp
          outNums = fmap (\x -> i * sized + fromEnum x) tmp
      inNums <- get
      put outNums
      pure (inNums, outNums)
  , const get
  , \(_, v) -> case v of
      -- check Branch
      Branch is r ls -> do
        let len = length ls
        -- At least two branches.
        when (len < 2) (throwError (AtLeastTwoBranches (Branch is r ls)))
        void $ runState @(Maybe r) Nothing $ forM_ ls $ \(BranchSt _ prot) -> do
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
      _ -> error "np"
  , const get
  , const get
  )

toGenConstrXTraverse :: (Monad m) => XTraverse m AddNums (GenConst r) r bst
toGenConstrXTraverse =
  ( \case
      (_, Msg xv _ _ from to :> _) -> pure (xv, (from, to))
      _ -> error "np"
  , \case
      (_, Label is i :> _) -> pure (is, i)
      _ -> error "np"
  , \(xv, _) -> pure (xv, id)
  , \case
      (_, Goto xs i) -> pure (xs, i)
      _ -> error "np"
  , \(xv, _) -> pure xv
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
  , \((xs, i), gt) -> do
      gets (IntMap.lookup i) >>= \case
        Nothing -> throwError (LabelUndefined gt)
        Just ls -> tellSeq $ zipWith C.Constraint xs ls
  , \(xs, _) -> tellSeq $ zipWith C.Constraint xs (cycle [-1])
  )

replXTraverse :: (Monad m) => C.SubMap -> XTraverse m (GenConst r) (GenConst r) r bst
replXTraverse sbm =
  ( \(((a, b), (from, to)), _) ->
      pure ((replaceList sbm a, replaceList sbm b), (from, to))
  , \((xs, i), _) -> pure (replaceList sbm xs, i)
  , \(a, _) -> pure (replaceList sbm a, id)
  , \((xs, i), _) -> pure (replaceList sbm xs, i)
  , pure . replaceList sbm . fst
  )

data Tracer r bst
  = TraceProtocolCreat (Protocol Creat r bst)
  | TraceProtocolAddNum (Protocol AddNums r bst)
  | TraceProtocolGenConst (Protocol (GenConst r) r bst)
  | TraceConstraints (Seq C.Constraint)
  | TraceSubMap C.SubMap
  | TraceProtocolGenConstN (Protocol (GenConst r) r bst)

traceWrapper :: String -> String -> String
traceWrapper desc st =
  "--------------------"
    ++ desc
    ++ "-----------------\n"
    ++ st
    ++ "\n"

instance (Show r, Show bst) => Show (Tracer r bst) where
  show = \case
    TraceProtocolCreat p -> traceWrapper "Creat" $ show p
    TraceProtocolAddNum p -> traceWrapper "AddNum" $ show p
    TraceProtocolGenConst p -> traceWrapper "GenConst" $ show p
    TraceConstraints p -> traceWrapper "Constrains" $ show p
    TraceSubMap p -> traceWrapper "SubMap" $ show p
    TraceProtocolGenConstN p -> traceWrapper "GenConstN" $ show p

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
  -> m (Protocol (GenConst r) r bst)
piple' trace prot = do
  trace (TraceProtocolCreat prot)
  prot' <-
    fmap (snd . snd)
      . runFresh 1
      . runState @[Int] (fmap fromEnum [minBound @r .. maxBound])
      $ xtraverse addNumsXTraverse prot
  trace (TraceProtocolAddNum prot')
  prot'' <- xtraverse toGenConstrXTraverse prot'
  trace (TraceProtocolGenConst prot'')
  (constraintList, _) <-
    runWriter @(Seq C.Constraint)
      . runState @(IntMap [Int]) (IntMap.empty)
      $ xfold genConstrXFold prot''
  trace (TraceConstraints constraintList)
  let sbm = compressSubMap $ C.constrToSubMap $ toList constraintList
  trace (TraceSubMap sbm)
  prot''' <- xtraverse (replXTraverse sbm) prot''
  trace (TraceProtocolGenConstN prot''')
  pure prot'''

piple
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> Either (ProtocolError r bst) (Protocol (GenConst r) r bst)
piple protocol =
  run $ runError @(ProtocolError r bst) $ (piple' (const (pure ())) protocol)

pipleWithTracer
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> (Seq (Tracer r bst), Either (ProtocolError r bst) (Protocol (GenConst r) r bst))
pipleWithTracer protocol =
  run
    . runWriter @(Seq (Tracer r bst))
    . runError @(ProtocolError r bst)
    $ (piple' (\w -> tell @(Seq (Tracer r bst)) (Seq.singleton w)) protocol)
