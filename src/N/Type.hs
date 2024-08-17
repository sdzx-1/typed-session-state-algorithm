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

module N.Type where

import Control.Monad
import Data.Kind (Constraint, Type)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Prelude hiding (traverse)

type family XMsg eta
type family XLabel eta
type family XBranch eta
type family XBranchSt eta
type family XGoto eta
type family XTerminal eta

-- | ForallX
type ForallX (f :: Type -> Constraint) eta =
  (f (XMsg eta), f (XLabel eta), f (XBranch eta), f (XBranchSt eta), f (XGoto eta), f (XTerminal eta))

-- | BranchSt
data BranchSt eta r bst = BranchSt (XBranchSt eta) bst (Protocol eta r bst)
  deriving (Functor)

-- | MsgOrLabel
data MsgOrLabel eta r
  = Msg (XMsg eta) String [String] r r
  | Label (XLabel eta) Int
  deriving (Functor)

infixr 5 :>

-- | Protocol
data Protocol eta r bst
  = (MsgOrLabel eta r) :> (Protocol eta r bst)
  | Branch (XBranch eta) r [BranchSt eta r bst]
  | Goto (XGoto eta) Int
  | Terminal (XTerminal eta)
  deriving (Functor)

-- | XTraverse
type XTraverse m eta gama r bst =
  ( (XMsg eta, (String, [String], r, r, Protocol eta r bst)) -> m (XMsg gama)
  , (XLabel eta, (Int, Protocol eta r bst)) -> m (XLabel gama)
  , (XBranch eta, (r, [BranchSt eta r bst]))
    -> m
        ( XBranch gama
        , m (Protocol gama r bst) -> m (Protocol gama r bst)
        )
  , (XBranchSt eta, (bst, Protocol eta r bst)) -> m (XBranchSt gama)
  , (XGoto eta, Int) -> m (XGoto gama)
  , (XTerminal eta) -> m (XTerminal gama)
  )

-- | xtraverse
xtraverse
  :: (Monad m)
  => XTraverse m eta gama r bst
  -> Protocol eta r bst
  -> m (Protocol gama r bst)
xtraverse xt@(xmsg, xlabel, xbranch, xbranchSt, xgoto, xterminal) prot = case prot of
  msgOrLabel :> prots -> do
    res <- case msgOrLabel of
      Msg xv a b c d -> do
        xv' <- xmsg (xv, (a, b, c, d, prots))
        pure (Msg xv' a b c d)
      Label xv i -> do
        xv' <- xlabel (xv, (i, prots))
        pure (Label xv' i)
    prots' <- xtraverse xt prots
    pure (res :> prots')
  Branch xv r ls -> do
    (xv', wrapper) <- xbranch (xv, (r, ls))
    ls' <- forM ls $ \(BranchSt xbst bst prot1) -> do
      xbst' <- xbranchSt (xbst, (bst, prot1))
      prot' <- wrapper $ xtraverse xt prot1
      pure (BranchSt xbst' bst prot')
    pure (Branch xv' r ls')
  Goto xv i -> do
    xv' <- xgoto (xv, i)
    pure (Goto xv' i)
  Terminal xv -> do
    xv' <- xterminal xv
    pure (Terminal xv')

-- | XFold
type XFold m eta r bst =
  ( (XMsg eta, (String, [String], r, r, Protocol eta r bst)) -> m ()
  , (XLabel eta, Int) -> m ()
  , (XBranch eta, (r, [BranchSt eta r bst])) -> m (m () -> m ())
  , (XBranchSt eta, (bst, Protocol eta r bst)) -> m ()
  , (XGoto eta, Int) -> m ()
  , (XTerminal eta) -> m ()
  )

-- | xfold
xfold :: (Monad m) => XFold m eta r bst -> Protocol eta r bst -> m ()
xfold xt@(xmsg, xlabel, xbranch, xbranchst, xgoto, xterminal) prot = case prot of
  msgOrLabel :> prots -> do
    case msgOrLabel of
      Msg xv a b c d -> xmsg (xv, (a, b, c, d, prots))
      Label xv i -> xlabel (xv, i)
    xfold xt prots
  Branch xv r ls -> do
    wrapper <- xbranch (xv, (r, ls))
    forM_ ls $ \(BranchSt xbst bst prot1) -> do
      xbranchst (xbst, (bst, prot1))
      wrapper $ xfold xt prot1
  Goto xv i -> xgoto (xv, i)
  Terminal xv -> xterminal xv

-- | ProtocolError
data ProtocolError r bst
  = AtLeastTwoBranches (Protocol Creat r bst)
  | DefLabelMultTimes Int
  | LabelUndefined Int
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
type instance XBranchSt Creat = ()
type instance XGoto Creat = ()
type instance XTerminal Creat = ()

data AddNums

type instance XMsg AddNums = ([Int], [Int])
type instance XLabel AddNums = [Int]
type instance XBranch AddNums = [Int]
type instance XBranchSt AddNums = ()
type instance XGoto AddNums = [Int]
type instance XTerminal AddNums = [Int]

data GenConst r

type instance XMsg (GenConst r) = (([Int], [Int]), (r, r))
type instance XLabel (GenConst r) = ([Int], Int)
type instance XBranch (GenConst r) = [Int]
type instance XBranchSt (GenConst r) = ()
type instance XGoto (GenConst r) = ([Int], Int)
type instance XTerminal (GenConst r) = [Int]

data T bst
  = TNum Int
  | BstList Int bst
  | TAny Int
  | TEnd
instance (Show bst) => Show (T bst) where
  show = \case
    TNum i -> "S" ++ show i
    BstList i bst -> "S" ++ show i ++ " " ++ show bst
    TAny i -> "S" ++ show i ++ " s"
    TEnd -> "End"

data MsgT r bst

type instance XMsg (MsgT r bst) = ([T bst], (r, r))
type instance XLabel (MsgT r bst) = ([T bst], Int)
type instance XBranch (MsgT r bst) = [T bst]
type instance XBranchSt (MsgT r bst) = ()
type instance XGoto (MsgT r bst) = ([T bst], Int)
type instance XTerminal (MsgT r bst) = [T bst]

data MsgT1 r bst

type instance XMsg (MsgT1 r bst) = ((T bst, T bst, T bst), (r, r))
type instance XLabel (MsgT1 r bst) = ([T bst], Int)
type instance XBranch (MsgT1 r bst) = [T bst]
type instance XBranchSt (MsgT1 r bst) = ()
type instance XGoto (MsgT1 r bst) = ([T bst], Int)
type instance XTerminal (MsgT1 r bst) = [T bst]

------------------------

instance (Pretty (Protocol eta r bst), Show (XBranchSt eta), Show bst) => Pretty (BranchSt eta r bst) where
  pretty (BranchSt xbst bst prot) = "* BranchSt" <+> pretty (show xbst) <+> pretty (show bst) <> line <> (pretty prot)

instance (Show (XMsg eta), Show (XLabel eta), Show r) => Pretty (MsgOrLabel eta r) where
  pretty = \case
    Msg xv cst args from to ->
      hsep ["Msg", angles (pretty $ show xv), pretty cst, pretty args, pretty (show from), pretty (show to)]
    Label xv i -> hsep ["Label", pretty $ show xv, pretty i]

instance (ForallX Show eta, Show r, Show bst) => Pretty (Protocol eta r bst) where
  pretty = \case
    msgOrLabel :> prots -> pretty msgOrLabel <> line <> pretty prots
    Branch is r ls -> nest 2 $ "[Branch]" <+> pretty (show is) <+> pretty (show r) <> line <> vsep (fmap pretty ls)
    Goto xv i -> "Goto" <+> pretty (show xv) <+> pretty i
    Terminal xv -> "Terminal" <+> pretty (show xv)

-----------------------------
instance (Pretty (Protocol eta r bst), Show (XBranchSt eta), Show bst) => Show (BranchSt eta r bst) where
  show = renderString . layoutPretty defaultLayoutOptions . pretty

instance (Show (XMsg eta), Show (XLabel eta), Show r) => Show (MsgOrLabel eta r) where
  show = renderString . layoutPretty defaultLayoutOptions . pretty

instance (ForallX Show eta, Show r, Show bst) => Show (Protocol eta r bst) where
  show = renderString . layoutPretty defaultLayoutOptions . pretty