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

module N.N2 where

import qualified Constraint as C
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
import N.Type
import N.Utils
import Render

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
      is' <- forM (zip [minBound @r .. maxBound] is) $
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
  -> m (Protocol (MsgT r bst) r bst)
piple' trace prot0 = do
  trace (TraceProtocolCreat prot0)
  prot1 <-
    fmap (snd . snd)
      . runFresh 1
      . runState @[Int] (fmap fromEnum [minBound @r .. maxBound])
      $ xtraverse addNumsXTraverse prot0
  trace (TraceProtocolAddNum prot1)
  prot2 <- xtraverse toGenConstrXTraverse prot1
  trace (TraceProtocolGenConst prot2)
  (constraintList, _) <-
    runWriter @(Seq C.Constraint)
      . runState @(IntMap [Int]) (IntMap.empty)
      $ xfold genConstrXFold prot2
  trace (TraceConstraints constraintList)
  let sbm = compressSubMap $ C.constrToSubMap $ toList constraintList
  trace (TraceSubMap sbm)
  prot3 <- xtraverse (replXTraverse sbm) prot2
  trace (TraceProtocolGenConstN prot3)
  dnys <- fst <$> runState @((Set Int)) (Set.empty) (xfold collectBranchDynValXFold prot3)
  trace (TraceCollectBranchDynVal dnys)
  prot4 <-
    fmap snd
      . runReader @(Set Int) dnys
      . runState @bst undefined
      $ (xtraverse genMsgTXTraverse prot3)
  trace (TraceProtocolMsgT prot4)
  pure prot4

piple
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> Either (ProtocolError r bst) (Protocol (MsgT r bst) r bst)
piple protocol =
  run $ runError @(ProtocolError r bst) $ (piple' (const (pure ())) protocol)

pipleWithTracer
  :: forall r bst
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> (Seq (Tracer r bst), Either (ProtocolError r bst) (Protocol (MsgT r bst) r bst))
pipleWithTracer protocol =
  run
    . runWriter @(Seq (Tracer r bst))
    . runError @(ProtocolError r bst)
    $ (piple' (\w -> tell @(Seq (Tracer r bst)) (Seq.singleton w)) protocol)

type XStringFill eta r bst =
  ( XMsg eta -> [StringFill]
  , XLabel eta -> [StringFill]
  , XBranch eta -> [StringFill]
  , XBranchSt eta -> [StringFill]
  , XGoto eta -> [StringFill]
  , XTerminal eta -> [StringFill]
  )

renderXFold
  :: forall r eta bst sig m
   . ( Has (Writer [[StringFill]] :+: State Int) sig m
     , ForallX Show eta
     , Enum r
     , Bounded r
     , Show r
     , Show bst
     )
  => XStringFill eta r bst -> XFold m eta r bst
renderXFold (xmsg, xlabel, xbranch, _xbranchst, xgoto, xterminal) =
  ( \(xv, (con, _, _, _, _)) -> do
      indentVal <- get @Int
      let va = [LeftAlign (indentVal * 2 + 3) ' ' (reSt con)]
      tell [va ++ xmsg xv]
  , \(xv, i) -> tell [[LeftAlign 1 ' ' ("LABEL " ++ show i)] ++ xlabel xv]
  , \(xv, (r, _)) -> do
      indentVal <- get @Int
      modify @Int (+ 1)
      tell [[LeftAlign (indentVal * 2 + 3) ' ' ("[Branch] " ++ show r)] ++ xbranch xv]
      pure (restoreWrapper @Int)
  , \(_, (bst, _)) -> do
      indentVal <- get @Int
      tell [[LeftAlign (indentVal * 2 + 3) ' ' ("* BranchSt " ++ show bst)]]
  , \(xv, i) -> do
      indentVal <- get @Int
      tell [[LeftAlign (indentVal * 2 + 3) ' ' ("^ Goto " ++ show i)] ++ xgoto xv]
  , \xv -> do
      indentVal <- get @Int
      tell [[LeftAlign (indentVal * 2 + 3) ' ' "~ Terminal"] ++ xterminal xv]
  )

getSF
  :: forall r eta bst
   . (ForallX Show eta, Show bst, Enum r, Bounded r, Show r)
  => XStringFill eta r bst -> Protocol eta r bst -> String
getSF xst prot =
  unlines
    . fmap runCenterFills
    . fst
    . run
    . runWriter @[[StringFill]]
    . runState @Int 0
    $ do
      let header =
            [CenterFill ((fromEnum r + 1) * width + leftWidth) '-' (show r) | r <- [minBound @r .. maxBound]]
      tell [header]
      (xfold (renderXFold xst) prot)

data Tracer r bst
  = TraceProtocolCreat (Protocol Creat r bst)
  | TraceProtocolAddNum (Protocol AddNums r bst)
  | TraceProtocolGenConst (Protocol (GenConst r) r bst)
  | TraceConstraints (Seq C.Constraint)
  | TraceSubMap C.SubMap
  | TraceProtocolGenConstN (Protocol (GenConst r) r bst)
  | TraceCollectBranchDynVal (Set Int)
  | TraceProtocolMsgT (Protocol (MsgT r bst) r bst)

traceWrapper :: String -> String -> String
traceWrapper desc st =
  "--------------------"
    ++ desc
    ++ "-----------------\n"
    ++ st
    ++ "\n"

stCreat :: XStringFill Creat r bst
stCreat =
  ( \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  )

stAddNums :: forall r bst. (Enum r, Bounded r) => XStringFill AddNums r bst
stAddNums =
  ( \(xs, ys) ->
      let zs = zip xs ys
          rg = fmap ((+ leftWidth) . (width *) . (+ 1) . fromEnum) [minBound @r .. maxBound]
          res = zip rg zs
       in [CenterFill i ' ' $ show v | (i, v) <- res]
  , \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  )

foo :: (Ord a) => a -> a -> [Char] -> a -> [Char]
foo from to str i =
  if
    | i == from ->
        if from > to
          then "<-" ++ str
          else str ++ "->"
    | i == to ->
        if from > to
          then str ++ "<-"
          else "->" ++ str
    | otherwise -> str

stGenConst :: forall r bst. (Enum r, Bounded r, Eq r, Ord r) => XStringFill (GenConst r) r bst
stGenConst =
  let
    too :: [Int] -> [StringFill]
    too xs = [CenterFill ps ' ' (show v) | (v, ps) <- zip xs $ fmap ((+ leftWidth) . (width *) . (+ 1) . fromEnum) [minBound @r .. maxBound]]
   in
    ( \((xs, ys), (from, to)) ->
        let zs = zip xs ys
            is = [minBound @r .. maxBound]
            rg = fmap ((+ leftWidth) . (width *) . (+ 1) . fromEnum) is
            res = zip rg zs
         in [CenterFill ps ' ' $ foo from to (show v) i | (i, (ps, v)) <- zip is res]
    , \(xs, _) -> too xs
    , \xs -> too xs
    , \_ -> []
    , \_ -> []
    , \_ -> []
    )

stMsgT :: forall r bst. (Show bst, Ord r, Enum r, Bounded r) => XStringFill (MsgT r bst) r bst
stMsgT =
  let
    rtops = ((+ leftWidth) . (width *) . (+ 1) . fromEnum)
    too xs = [CenterFill ps ' ' (show v) | (v, ps) <- zip xs $ fmap rtops [minBound @r .. maxBound]]
   in
    ( \(ls, (from, to)) -> [CenterFill ps ' ' $ foo from to (show v) i | (i, (v, ps)) <- zip [minBound @r .. maxBound] $ zip ls $ fmap rtops [minBound @r .. maxBound]]
    , \(xs, _) -> too xs
    , \xs -> too xs
    , \_ -> []
    , \(xs, _) -> too xs
    , \xs -> too xs
    )

instance (Show r, Show bst, Enum r, Bounded r, Eq r, Ord r) => Show (Tracer r bst) where
  show = \case
    TraceProtocolCreat p -> traceWrapper "Creat" $ getSF stCreat p
    TraceProtocolAddNum p -> traceWrapper "AddNum" $ getSF (stAddNums @r) p
    TraceProtocolGenConst p -> traceWrapper "GenConst" $ getSF (stGenConst @r) p
    TraceConstraints p -> traceWrapper "Constrains" $ show p
    TraceSubMap p -> traceWrapper "SubMap" $ show p
    TraceProtocolGenConstN p -> traceWrapper "GenConstN" $ getSF (stGenConst @r) p
    TraceCollectBranchDynVal dvs -> traceWrapper "CollectBranchDynVal" $ show dvs
    TraceProtocolMsgT p -> traceWrapper "MsgT" $ getSF (stMsgT @r) p
