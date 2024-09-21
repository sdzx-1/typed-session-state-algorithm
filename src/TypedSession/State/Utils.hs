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

module TypedSession.State.Utils where

import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Control.Effect.Writer
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import qualified TypedSession.State.Constraint as C
import TypedSession.State.Type
import Prelude hiding (traverse)

------------------------

restoreWrapper
  :: forall s sig m a
   . (Has (State s) sig m) => m a -> m a
restoreWrapper m = do
  st <- get @s
  a <- m
  put st
  pure a

getFirstMsgInfo :: Protocol eta r bst -> Maybe (r, r)
getFirstMsgInfo = \case
  msgOrLabel :> prots -> case msgOrLabel of
    Msg _ _ _ from to -> Just (from, to)
    _ -> getFirstMsgInfo prots
  _ -> Nothing

getAllMsgInfo :: Protocol eta r bst -> [(r, r)]
getAllMsgInfo = \case
  msgOrLabel :> prots -> case msgOrLabel of
    Msg _ _ _ from to -> (from, to) : getAllMsgInfo prots
    _ -> getAllMsgInfo prots
  Branch _ _ _ ls -> concatMap (\(BranchSt _ _ _ prots) -> getAllMsgInfo prots) ls
  Goto _ _ -> []
  Terminal _ -> []

tellSeq :: (Has (Writer (Seq a)) sig m) => [a] -> m ()
tellSeq ls = tell (Seq.fromList ls)

replaceList :: (Has (State (Set Int)) sig m) => C.SubMap -> [Int] -> m [Int]
replaceList sbm ls = do
  for ls $ \k -> do
    let newVal = fromMaybe k $ IntMap.lookup k sbm
    modify (Set.insert newVal)
    pure newVal

replaceVal :: IntMap Int -> Int -> Int
replaceVal sbm k = fromMaybe (error internalError) $ IntMap.lookup k sbm

rRange :: forall r. (Enum r, Bounded r) => [r]
rRange = [minBound @r .. maxBound]