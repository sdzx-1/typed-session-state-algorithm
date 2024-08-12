{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module N where

import Control.Algebra (Has)
import Control.Effect.Fresh
import Data.Void

data BranchSt eta r bst = BranchSt bst (M eta r bst)
  deriving (Functor)

data MsgOrLabel eta r
  = Msg (XMsg eta) String [String] r r
  | Label (XLabel eta) Int
  deriving (Functor)

data M eta r bst
  = (MsgOrLabel eta r) :> (M eta r bst)
  | Branch (XBranch eta) r [BranchSt eta r bst]
  | Goto (XBranch eta) Int
  | Terminal (XBranch eta)
  deriving (Functor)

type family XMsg eta
type family XLabel eta
type family XBranch eta
type family XGoto eta
type family XTerminal eta

data Creat

type instance XMsg Creat = ()
type instance XLabel Creat = ()
type instance XBranch Creat = ()
type instance XGoto Creat = ()
type instance XTerminal Creat = ()

data AddNums

type instance XMsg AddNums = ([Int], [Int])
type instance XLabel AddNums = [Int]
type instance XBranch AddNums = [Int]
type instance XGoto AddNums = [Int]
type instance XTerminal AddNums = [Int]

addNums
  :: forall r bst sig m
   . ( Has Fresh sig m
     , Enum r
     , Bounded r
     )
  => [Int] -> M Creat r bst -> m ([Int], M AddNums r bst)
addNums inputNums = \case
  msgOrLabel :> ms -> do
    case msgOrLabel of
      Msg _ a b c d -> do
        i <- fresh
        let tmp = [minBound @r .. maxBound]
            sized = length tmp
            nums = fmap (\x -> i * sized + fromEnum x) tmp
        (is', ms') <- addNums nums ms
        pure (is', Msg (inputNums, nums) a b c d :> ms')
      Label _ i -> do
        (is', ms') <- addNums inputNums ms
        pure (is', Label inputNums i :> ms')
  Branch _ r ls -> undefined
  Goto _ i -> undefined
  Terminal _ -> undefined
