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

import qualified Constraint as Constraint
import Control.Algebra (Has, (:+:))
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Fresh.Strict
import Control.Effect.Error
import Control.Effect.Fresh
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad
import Data.Functor.Identity (Identity (runIdentity))
import Data.IntMap (IntMap)
import Data.Kind (Constraint, Type)
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

data ProtocolError = AtLeastTwoBranches String
  deriving (Show)

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
   . ( Has (Fresh :+: Error ProtocolError) sig m
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
    (ins, ls') <- go inputNums ls
    pure (ins, Branch r ls')
  Goto _ i -> pure (inputNums, Goto inputNums i)
  Terminal _ -> pure (inputNums, Terminal inputNums)

go
  :: forall r bst sig m
   . ( Has (Fresh :+: Error ProtocolError) sig m
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
  => Protocol Creat r bst -> Either ProtocolError (Protocol AddNums r bst)
addNums protoc =
  fmap snd
    . snd
    . runIdentity
    . runFresh 1
    . runError @ProtocolError
    $ (addNums' (fmap fromEnum [minBound @r .. maxBound]) protoc)

----------------------------------

generateConstraint
  :: (Has (State (IntMap Int) :+: Writer [Int]) sig m)
  => Protocol AddNums r bst -> m ()
generateConstraint = undefined

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

-- >>> error $ show v1
-- >>> error "------------------------"
-- >>> error $ show (addNums v1)
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
