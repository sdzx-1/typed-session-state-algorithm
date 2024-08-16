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

type XStringFill eta r =
  ( XMsg eta -> [StringFill]
  , XLabel eta -> [StringFill]
  , XBranch eta -> [StringFill]
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
     )
  => XStringFill eta r -> XFold m eta r bst
renderXFold (xmsg, xlabel, xbranch, _xgoto, _xterminal) =
  ( \(_, prot) -> case prot of
      Msg xv con _args _from _to :> _ -> do
        indentVal <- get @Int
        let va = [LeftAlign (indentVal * 2 + 3) ' ' (reSt con)]
        tell [va ++ xmsg xv]
      _ -> error "np"
  , \(_, prot) -> case prot of
      Label xv i :> _ -> do
        tell [[LeftAlign 1 ' ' ("LABEL " ++ show i)] ++ xlabel xv]
      _ -> pure ()
  , \(_, prot) -> case prot of
      Branch xv r _ls -> do
        indentVal <- get @Int
        modify @Int (+ 1)
        tell [[LeftAlign (indentVal * 2 + 3) ' ' ("[Branch] " ++ show r)] ++ xbranch xv]
        pure (restoreWrapper @Int)
      _ -> error "np"
  , \(_, prot) -> case prot of
      Goto _ i ->
        tell [[LeftAlign 1 ' ' ("Goto " ++ show i)]]
      _ -> error "np"
  , \(_, prot) -> case prot of
      _ ->
        tell [[LeftAlign 1 ' ' "Terminal"]]
  )

getSF
  :: forall r eta bst
   . (ForallX Show eta, Enum r, Bounded r, Show r)
  => XStringFill eta r -> Protocol eta r bst -> String
getSF xst prot =
  unlines
    . fmap runCenterFills
    . fst
    . run
    . runWriter @[[StringFill]]
    . runState @Int 0
    $ do
      let header =
            [CenterFill ((fromEnum r + 1) * width) '-' (show r) | r <- [minBound @r .. maxBound]]
      tell [header]
      (xfold (renderXFold xst) prot)

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

stCreat :: XStringFill Creat r
stCreat =
  ( \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  )

stAddNums :: forall r. (Enum r, Bounded r) => XStringFill AddNums r
stAddNums =
  ( \(xs, ys) ->
      let zs = zip xs ys
          rg = fmap ((width *) . (+ 1) . fromEnum) [minBound @r .. maxBound]
          res = zip rg zs
       in [CenterFill i ' ' $ show v | (i, v) <- res]
  , \_ -> []
  , \_ -> []
  , \_ -> []
  , \_ -> []
  )

too :: forall r. (Enum r, Bounded r) => [Int] -> [StringFill]
too xs = [CenterFill ps ' ' (show v) | (v, ps) <- zip xs $ fmap ((width *) . (+ 1) . fromEnum) [minBound @r .. maxBound]]

stGenConst :: forall r. (Enum r, Bounded r, Eq r, Ord r) => XStringFill (GenConst r) r
stGenConst =
  ( \((xs, ys), (from, to)) ->
      let zs = zip xs ys
          is = [minBound @r .. maxBound]
          rg = fmap ((width *) . (+ 1) . fromEnum) is
          res = zip rg zs
          foo str i =
            if
              | i == from ->
                  if from > to
                    then "<-" ++ str
                    else str ++ "->"
              | i == to ->
                  if from > to
                    then str ++ "<-"
                    else "->" ++ str
              | otherwise -> ""
       in [CenterFill ps ' ' $ foo (show v) i | (i, (ps, v)) <- zip is res]
  , \(xs, _) -> too @r xs
  , \xs -> too @r xs
  , \_ -> []
  , \_ -> []
  )

instance (Show r, Show bst, Enum r, Bounded r, Eq r, Ord r) => Show (Tracer r bst) where
  show = \case
    TraceProtocolCreat p -> traceWrapper "Creat" $ getSF stCreat p
    TraceProtocolAddNum p -> traceWrapper "AddNum" $ getSF (stAddNums @r) p
    TraceProtocolGenConst p -> traceWrapper "GenConst" $ getSF (stGenConst @r) p
    TraceConstraints p -> traceWrapper "Constrains" $ show p
    TraceSubMap p -> traceWrapper "SubMap" $ show p
    TraceProtocolGenConstN p -> traceWrapper "GenConstN" $ getSF (stGenConst @r) p

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
