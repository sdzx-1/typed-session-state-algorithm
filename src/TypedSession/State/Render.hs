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
import qualified Data.List as L
import Data.Semigroup (Max (..))
import Data.Traversable (for)
import TypedSession.State.Type
import TypedSession.State.Utils

data RenderProt

type instance XMsg RenderProt = (String, [String])
type instance XLabel RenderProt = (String, [String])
type instance XBranch RenderProt = (String, [String])
type instance XBranchSt RenderProt = ()
type instance XGoto RenderProt = (String, [String])
type instance XTerminal RenderProt = (String, [String])

parensWarapper :: String -> String
parensWarapper st = "{" <> st <> "}"

newtype LV = LV Int
  deriving (Show, Eq, Ord, Num, Bounded)

newtype RV = RV Int
  deriving (Show, Eq, Ord, Num, Bounded)

mkLeftStr :: (Has (State Int :+: Writer (Max LV)) sig m) => String -> m String
mkLeftStr str = do
  indent <- get @Int
  let str' = replicate (indent * 2 + 3) ' ' <> str
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
      nst <- mkLeftStr (constr <> " [" <> L.intercalate "," args <> "]")
      ts' <- for (zip (rRange @r) ts) $ \(r, t) -> do
        indent <- get @Int
        let sht =
              if
                | idx == 0 && r == from -> replicate ((indent) * 2 + 2) ' ' <> (parensWarapper $ show t)
                | otherwise -> show t
            sht' =
              if
                | r == from -> sht <> " ->"
                | r == to -> sht <> " <-"
                | otherwise -> sht
        tell $ Max $ RV (length sht')
        pure sht'
      when (idx == 0) (modify @Int (+ 1))
      pure (nst, ts')
  , \((ts, i), _) -> pure ("Label " <> show i, map show ts)
  , \(ts, (r, _)) -> do
      nst <- mkLeftStr $ "[Branch " <> show r <> "]"
      indent <- get @Int
      let ts' =
            [ if r1 == r then replicate (indent * 2 + 2) ' ' <> show t else show t
            | (r1, t) <- zip (rRange @r) ts
            ]
      pure ((nst, ts'), restoreWrapper @Int)
  , \_ -> pure ()
  , \((ts, i), _) -> do
      nst <- mkLeftStr $ "Goto " <> show i
      pure (nst, map show ts)
  , \ts -> do
      nst <- mkLeftStr "Terminal"
      pure (nst, map show ts)
  )

fillStr :: Char -> Int -> String -> String
fillStr c i st =
  let len = length st
   in case compare len i of
        EQ -> st
        LT -> st <> replicate (i - len) c
        GT -> error "np"

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
  , \_ -> pure ()
  , \(vs, _) -> mkLine @r vs
  , \vs -> mkLine @r vs
  )

runRender1
  :: (Enum r, Bounded r, Ord r, Show bst, Show r)
  => Protocol (MsgT r bst) r bst
  -> (Max LV, (Max RV, (Int, Protocol RenderProt r bst)))
runRender1 prot =
  run
    . runWriter @(Max LV)
    . runWriter @(Max RV)
    . runState @Int 0
    $ xtraverse render1XTraverse prot

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
