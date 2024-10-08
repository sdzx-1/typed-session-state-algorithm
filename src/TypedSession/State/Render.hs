{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module TypedSession.State.Render (runRender) where

import Control.Algebra ((:+:))
import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad (when)
import Data.Foldable (for_)
import qualified Data.List as L
import Data.Semigroup (Max (..))
import Data.Traversable (for)
import TypedSession.State.Type
import TypedSession.State.Utils

data RenderProt

type instance XMsg RenderProt = (String, [String])
type instance XLabel RenderProt = (String, [String])
type instance XBranch RenderProt = (String, [String])
type instance XBranchSt RenderProt = String
type instance XGoto RenderProt = (String, [String])
type instance XTerminal RenderProt = (String, [String])

parensWarapper :: String -> String
parensWarapper st = "(" <> st <> ")"

newtype LV = LV Int
  deriving (Show, Eq, Ord, Num, Bounded)

newtype RV = RV Int
  deriving (Show, Eq, Ord, Num, Bounded)

mkLeftStr :: (Has (State Int :+: Writer (Max LV)) sig m) => String -> m String
mkLeftStr str = do
  indent <- get @Int
  let str' = replicate (indent * 2 + 2) ' ' <> str
  tell (Max $ LV $ length str')
  pure str'

render1XTraverse
  :: forall r bst sig m
   . ( Has (State Int :+: Writer (Max LV) :+: Writer (Max RV)) sig m
     , Show bst
     , Enum r
     , Bounded r
     , Eq r
     , Ord r
     , Show r
     )
  => XTraverse m (MsgT r bst) RenderProt r bst
render1XTraverse =
  ( \((ts, (from, to), idx), (constr, args, _, _, _)) -> do
      when (idx == 0) (modify @Int (+ 1))
      nst <- mkLeftStr (constr <> " [" <> L.intercalate "," (fmap (L.intercalate " ") args) <> "]")
      ts' <- for (zip (rRange @r) ts) $ \(r, t) -> do
        let sht = parensWarapper $ show t
            sht' =
              if
                | r == from -> "Send " <> sht
                | r == to -> "Recv " <> sht
                | otherwise -> "     " <> sht
        tell $ Max $ RV (length sht')
        pure sht'
      pure (nst, ts')
  , \((ts, i), _) -> pure ("Label " <> show i, mkStrs ts)
  , \(ts, (r, st, _)) -> do
      nst <- mkLeftStr $ "[Branch " <> show r <> " " <> st <> "]"
      pure ((nst, mkStrs ts), restoreWrapper @Int)
  , \(_, (bst, args, _)) -> do
      nst <- mkLeftStr $ "* BranchSt_" <> show bst <> " [" <> L.intercalate "," (fmap (L.intercalate " ") args) <> "]"
      pure nst
  , \((ts, i), _) -> do
      nst <- mkLeftStr $ "Goto " <> show i
      pure (nst, mkStrs ts)
  , \ts -> do
      nst <- mkLeftStr "Terminal"
      pure (nst, mkStrs ts)
  )

mkStrs :: (Show bst) => [T bst] -> [String]
mkStrs ts = map (("     " <>) . parensWarapper . show) ts

fillStr :: Char -> Int -> String -> String
fillStr c i st =
  let len = length st
   in case compare len i of
        EQ -> st
        LT -> st <> replicate (i - len) c
        GT -> st

mkLine
  :: forall r sig m
   . ( Has (Reader (LV, RV) :+: Writer [String]) sig m
     , Enum r
     , Bounded r
     )
  => (String, [String]) -> m ()
mkLine (ls, rs) = do
  (LV maxLv, RV maxRv) <- ask
  let
    leftMaxPos = maxLv + 3
    rightMaxPos = maxRv + 2
  tell [fillStr ' ' leftMaxPos ls <> concatMap (fillStr ' ' rightMaxPos) rs]

render2XFold
  :: forall r bst sig m
   . ( Has (Reader (LV, RV) :+: Writer [String]) sig m
     , Enum r
     , Bounded r
     )
  => XFold m RenderProt r bst
render2XFold =
  ( \(vs, _) -> mkLine @r vs
  , \(vs, _) -> mkLine @r vs
  , \(vs, _) -> do
      mkLine @r vs
      pure id
  , \(ls, _) -> mkLine @r (ls, [])
  , \(vs, _) -> mkLine @r vs
  , \vs -> mkLine @r vs
  )

runRender1
  :: forall r bst
   . (Enum r, Bounded r, Ord r, Show bst, Show r)
  => Protocol (MsgT r bst) r bst
  -> (Max LV, (Max RV, (Int, Protocol RenderProt r bst)))
runRender1 prot =
  run
    . runWriter @(Max LV)
    . runWriter @(Max RV)
    . runState @Int 0
    $ do
      let rg = rRange @r
      for_ rg $ \r -> tell (Max (RV (length (show r))))
      xtraverse render1XTraverse prot

runRender :: forall r bst. (Enum r, Bounded r, Ord r, Show bst, Show r) => Protocol (MsgT r bst) r bst -> String
runRender prot =
  let (Max lv@(LV maxLv), (Max rv@(RV maxRv), (_, prot1))) = runRender1 prot
      header = replicate (maxLv + 3) '-' <> concatMap (fillStr '-' (maxRv + 2)) [show r | r <- rRange @r]
   in unlines
        . fst
        . run
        . runReader (lv, rv)
        . runWriter @[String]
        $ do
          tell [header]
          xfold render2XFold prot1
