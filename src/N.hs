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

module N where

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
import Data.Foldable (Foldable (toList), for_)
import Data.Functor.Identity (Identity (runIdentity))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Constraint, Type)
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Prelude hiding (traverse)

data BranchSt eta r bst = BranchSt bst (Protocol eta r bst)
  deriving (Functor)

instance
  ( Pretty (Protocol eta r bst)
  , Show bst
  )
  => Show (BranchSt eta r bst)
  where
  show = renderString . layoutPretty defaultLayoutOptions . pretty

data MsgOrLabel eta r
  = Msg (XMsg eta) String [String] r r
  | Label (XLabel eta) Int
  deriving (Functor)

instance
  ( Pretty (XMsg eta)
  , Pretty (XLabel eta)
  , Show r
  )
  => Show (MsgOrLabel eta r)
  where
  show = renderString . layoutPretty defaultLayoutOptions . pretty

infixr 5 :>

data Protocol eta r bst
  = (MsgOrLabel eta r) :> (Protocol eta r bst)
  | Branch (XBranch eta) r [BranchSt eta r bst]
  | Goto (XGoto eta) Int
  | Terminal (XTerminal eta)
  deriving (Functor)

instance
  ( ForallX Pretty eta
  , Show r
  , Show bst
  )
  => Show (Protocol eta r bst)
  where
  show = renderString . layoutPretty defaultLayoutOptions . pretty

type family XMsg eta
type family XLabel eta
type family XBranch eta
type family XGoto eta
type family XTerminal eta

type ForallX (f :: Type -> Constraint) eta =
  (f (XMsg eta), f (XLabel eta), f (XBranch eta), f (XGoto eta), f (XTerminal eta))

type XTraverse m eta gama r bst =
  ( XMsg eta -> m (XMsg gama)
  , XLabel eta -> m (XLabel gama)
  , XBranch eta -> m (XBranch gama)
  , XGoto eta -> m (XGoto gama)
  , XTerminal eta -> m (XTerminal gama)
  )

traverse
  :: (Monad m)
  => XTraverse m eta gama r bst
  -> Protocol eta r bst
  -> m (Protocol gama r bst)
traverse xt@(xmsg, xlabel, xbranch, xgoto, xterminal) = \case
  msgOrLabel :> prots -> do
    res <- case msgOrLabel of
      Msg xv a b c d -> do
        xv' <- xmsg xv
        pure (Msg xv' a b c d)
      Label xv i -> do
        xv' <- xlabel xv
        pure (Label xv' i)
    prots' <- traverse xt prots
    pure (res :> prots')
  Branch xv r ls -> do
    xv' <- xbranch xv
    ls' <- forM ls $ \(BranchSt bst prot) -> do
      prot' <- traverse xt prot
      pure (BranchSt bst prot')
    pure (Branch xv' r ls')
  Goto xv i -> do
    xv' <- xgoto xv
    pure (Goto xv' i)
  Terminal xv -> do
    xv' <- xterminal xv
    pure (Terminal xv')

data ProtocolError r bst
  = AtLeastTwoBranches (Protocol Creat r bst)
  | DefLabelMultTimes (MsgOrLabel AddNums r)
  | LabelUndefined (Protocol AddNums r bst)
  | BranchNoMsg (Protocol Creat r bst)
  | BranchFirstMsgMustHaveTheSameReceiver (Protocol Creat r bst)
  | BranchFirstMsgMustHaveTheSameSender (Protocol Creat r bst)
  | BranchNotNotifyAllOtherReceivers (Protocol Creat r bst)

instance (Show r, Show bst) => Show (ProtocolError r bst) where
  show = \case
    AtLeastTwoBranches prot -> "At least two branches are required\n" <> show prot
    DefLabelMultTimes msgOrLabel -> "Defining Label multiple times\n" <> show msgOrLabel
    LabelUndefined prot -> "Label Undefined\n" <> show prot
    BranchNoMsg prot -> "Branch No Msg\n" <> show prot
    BranchFirstMsgMustHaveTheSameReceiver port ->
      "The first message of each branch must have the same receiver.\n" <> show port
    BranchFirstMsgMustHaveTheSameSender prot ->
      "The first message of each branch must have the same sender.\n" <> show prot
    BranchNotNotifyAllOtherReceivers prot ->
      "Each branch sender must send (directly or indirectly) a message to all other receivers to notify the state change.\n" <> show prot

------------------------
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

-- Null | [r] | s
data T bst
  = Null Int
  | TList Int [bst]
  | TAny Int
  | End

instance (Show bst) => Show (T bst) where
  show = \case
    Null i -> show i
    TList i ls -> show i ++ " " ++ show ls
    TAny i -> show i ++ " s"
    End -> "End"

-- from, to1, to2:  (Int, Null | [r] | s)
-- (from, (r, to1), (r, to2))
data Zst r bst = Zst (T bst) (r, T bst) (r, T bst)

instance (Show r, Show bst) => Show (Zst r bst) where
  show (Zst from (sender, to1) (recver, to2)) =
    "(" ++ show from ++ " -> " ++ show to1 ++ " , " ++ show to2 ++ ") " ++ show sender ++ " -> " ++ show recver

instance (Show bst, Show r) => Pretty (Zst r bst) where
  pretty (Zst from (sender, to1) (recver, to2)) =
    pretty (show from)
      <+> "->"
      <+> parens (pretty (show to1) <+> "," <+> pretty (show to2))
      <+> pretty (show sender)
      <+> "->"
      <+> pretty (show recver)

data Yst r bst

type instance XMsg (Yst r bst) = Zst r bst
type instance XLabel (Yst r bst) = ()
type instance XBranch (Yst r bst) = ()
type instance XGoto (Yst r bst) = ()
type instance XTerminal (Yst r bst) = ()

------------------------

addNums'
  :: forall r bst sig m
   . ( Has (Fresh :+: Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Eq r
     , Ord r
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

    (ins, ls') <- go inputNums ls
    pure (ins, Branch inputNums r ls')
  Goto _ i -> pure (inputNums, Goto inputNums i)
  Terminal _ -> pure (inputNums, Terminal inputNums)

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
  Branch _ _ ls -> concatMap (\(BranchSt _ prots) -> getAllMsgInfo prots) ls
  Goto _ _ -> []
  Terminal _ -> []

go
  :: forall r bst sig m
   . ( Has (Fresh :+: Error (ProtocolError r bst)) sig m
     , Enum r
     , Bounded r
     , Eq r
     , Ord r
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
   . (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst -> Either (ProtocolError r bst) (Protocol AddNums r bst)
addNums protoc =
  fmap snd
    . snd
    . run
    . runFresh 1
    . runError @(ProtocolError r bst)
    $ (addNums' (fmap fromEnum [minBound @r .. maxBound]) protoc)

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
  Branch _ _ ls -> for_ ls $ \(BranchSt _ prot) -> genConstraint' prot
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

replXTraverse :: C.SubMap -> XTraverse Identity AddNums AddNums r bst
replXTraverse sbm =
  ( \(a, b) -> pure $ (replaceList sbm a, replaceList sbm b)
  , pure . replaceList sbm
  , pure . replaceList sbm
  , pure . replaceList sbm
  , pure . replaceList sbm
  )

replaceNums :: C.SubMap -> Protocol AddNums r bst -> Protocol AddNums r bst
replaceNums sbm prot = runIdentity $ traverse (replXTraverse sbm) prot

piple
  :: (Enum r, Bounded r, Eq r, Ord r)
  => Protocol Creat r bst
  -> Either (ProtocolError r bst) (Protocol (Yst r bst) r bst)
piple prot = do
  prot' <- addNums prot
  sbm <- genSubMap prot'
  pure $ genZst $ replaceNums sbm prot'

instance
  ( Pretty (XMsg eta)
  , Pretty (XLabel eta)
  , Show r
  )
  => Pretty (MsgOrLabel eta r)
  where
  pretty = \case
    Msg xv cst args from to ->
      hsep ["Msg", angles (pretty xv), pretty cst, pretty args, pretty (show from), pretty (show to)]
    Label xv i -> hsep ["Label", pretty xv, pretty i]

instance
  ( Pretty (Protocol eta r bst)
  , Show bst
  )
  => Pretty (BranchSt eta r bst)
  where
  pretty (BranchSt bst prot) = "* BranchSt" <+> pretty (show bst) <> line <> (pretty prot)

instance
  ( ForallX Pretty eta
  , Show r
  , Show bst
  )
  => Pretty (Protocol eta r bst)
  where
  pretty = \case
    msgOrLabel :> prots -> pretty msgOrLabel <> line <> pretty prots
    Branch is r ls -> nest 2 $ "[Branch]" <+> pretty is <+> pretty (show r) <> line <> vsep (fmap pretty ls)
    Goto xv i -> "Goto" <+> pretty xv <+> pretty (show i)
    Terminal xv -> "Terminal" <+> pretty xv

------------------------------------------------
genZst'
  :: forall r bst sig m
   . (Has (State [bst] :+: State (Set Int) :+: Reader (Set Int)) sig m, Enum r)
  => Protocol AddNums r bst -> m (Protocol (Yst r bst) r bst)
genZst' = \case
  msgOrLabel :> prots -> do
    mol' <- case msgOrLabel of
      Msg (is, os) cont args from to -> do
        let fromSt = is !! fromEnum from
            sendToSt = os !! fromEnum from
            recvToSt = os !! fromEnum to
        bsts <- get @([bst])
        unSet <- ask @(Set Int)
        lbs <- get @(Set Int)
        let fun i =
              if i == -1
                then End
                else
                  if i `elem` unSet
                    then
                      if isMsg prots
                        then
                          -- if (i `Set.member` lbs)
                          --   then (TAny i)
                          --   else TList i bsts
                          TList i bsts
                        else TAny i
                    else Null i
            zst =
              Zst
                (if fromSt `elem` unSet then (TList fromSt bsts) else Null fromSt)
                (from, fun sendToSt)
                (to, fun recvToSt)
        pure (Msg zst cont args from to)
      Label is i -> do
        modify (Set.union (Set.fromList is))
        pure (Label @(Yst r bst) () i)
    prots' <- genZst' prots
    pure (mol' :> prots')
  Branch _ r ls -> do
    bstBackup <- get
    prots' <- forM ls $ \(BranchSt st prot) -> do
      put (st : bstBackup)
      rs <- genZst' prot
      pure (BranchSt st rs)
    pure (Branch () r prots')
  Goto _ i -> pure (Goto () i)
  Terminal _ -> pure (Terminal ())

isMsg :: Protocol AddNums r bst -> Bool
isMsg (Msg _ _ _ _ _ :> _) = True
isMsg _ = False

collectBranchSt :: Protocol AddNums r bst -> Set Int
collectBranchSt = \case
  _ :> prots -> collectBranchSt prots
  Branch is _ ls ->
    foldl'
      Set.union
      (Set.fromList is)
      (fmap (\(BranchSt _ prot) -> collectBranchSt prot) ls)
  _ -> Set.empty

genZst
  :: forall r bst
   . (Enum r)
  => Protocol AddNums r bst -> Protocol (Yst r bst) r bst
genZst protocol =
  snd
    . snd
    . run
    . runState @[bst] []
    . runState @(Set Int) Set.empty
    . runReader @(Set Int) (collectBranchSt protocol)
    $ genZst' protocol
