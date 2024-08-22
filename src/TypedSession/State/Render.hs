{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module TypedSession.State.Render where

import Control.Algebra ((:+:))
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad (when)
import Data.IntMap (IntMap)
import qualified Data.List as L
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified TypedSession.State.Constraint as C
import TypedSession.State.Type
import TypedSession.State.Utils

data StringFill
  = CenterFill Int Char String
  | LeftAlign Int Char String
  deriving (Show)

runCenterFill :: String -> StringFill -> String
runCenterFill startSt (CenterFill centerPoint c st) =
  let stlen = length st
      hlen = stlen `div` 2
      rplen = (centerPoint - length startSt) - if odd stlen then hlen + 1 else hlen
      rpSt = replicate rplen c
   in startSt ++ rpSt ++ st
runCenterFill startSt v@(LeftAlign leftAlignPoint c st) =
  if leftAlignPoint < length startSt
    then error $ "np: " ++ startSt ++ " " ++ show v
    else
      let repLen = leftAlignPoint - length startSt - 1
          fillSt = replicate repLen c
       in startSt ++ fillSt ++ st

getPoint :: StringFill -> Int
getPoint = \case
  CenterFill cp _ _ -> cp
  LeftAlign lp _ _ -> lp

runCenterFills :: [StringFill] -> String
runCenterFills ls =
  let ls' = L.sortOn getPoint ls
   in foldl' runCenterFill "" ls'

-- width :: Int
-- width = 30

-- leftWidth :: Int
-- leftWidth = 20

data StrFillEnv = StrFillEnv
  { width :: Int
  , leftWidth :: Int
  }
  deriving (Show)

defaultStrFilEnv :: StrFillEnv
defaultStrFilEnv = StrFillEnv 30 20

reSt :: StrFillEnv -> String -> String
reSt StrFillEnv{width} st =
  let st' = words st
   in case st' of
        [] -> error "np"
        (x : _) ->
          let lv = width - 6
           in if length x >= lv
                then take (lv - 2) x <> ".."
                else x

--------------------------------------------
type XStringFill eta r bst =
  ( XMsg eta -> ([StringFill], Bool)
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
  => StrFillEnv -> XStringFill eta r bst -> XFold m eta r bst
renderXFold sfe (xmsg, xlabel, xbranch, _xbranchst, xgoto, xterminal) =
  ( \(xv, (con, _, _, _, _)) -> do
      indentVal <- get @Int
      let va = [LeftAlign (indentVal * 2 + 3) ' ' (reSt sfe con)]
          (xv', isFirst) = xmsg xv
      when isFirst (modify @Int (+ 1))
      tell [va ++ xv']
  , \(xv, i) -> tell [[LeftAlign 1 ' ' ("LABEL " ++ show i)] ++ xlabel xv]
  , \(xv, (r, _)) -> do
      indentVal <- get @Int
      modify @Int (+ 1)
      tell [[LeftAlign (indentVal * 2 + 3) ' ' ("[Branch " ++ show r ++ "]")] ++ xbranch xv]
      pure (restoreWrapper @Int)
  , \(_, (_, _)) -> pure ()
  , \(xv, i) -> do
      indentVal <- get @Int
      tell [[LeftAlign (indentVal * 2 + 3) ' ' ("Goto " ++ show i)] ++ xgoto xv]
  , \xv -> do
      indentVal <- get @Int
      tell [[LeftAlign (indentVal * 2 + 3) ' ' "Terminal"] ++ xterminal xv]
  )

runRender
  :: forall r eta bst
   . (ForallX Show eta, Show bst, Enum r, Bounded r, Show r)
  => StrFillEnv -> XStringFill eta r bst -> Protocol eta r bst -> String
runRender sfe@(StrFillEnv{width, leftWidth}) xst prot =
  unlines
    . fmap runCenterFills
    . fst
    . run
    . runWriter @[[StringFill]]
    . runState @Int 0
    $ do
      let header =
            [CenterFill ((fromEnum r + 1) * width + leftWidth) '-' (show r) | r <- rRange @r]
      tell [header]
      (xfold (renderXFold sfe xst) prot)

data Tracer r bst
  = TracerProtocolCreat (Protocol Creat r bst)
  | TracerProtocolIdx (Protocol Idx r bst)
  | TracerReRank (IntMap Int)
  | TracerProtocolAddNum (Protocol AddNums r bst)
  | TracerProtocolGenConst (Protocol (GenConst r) r bst)
  | TracerConstraints (Seq C.Constraint)
  | TracerSubMap C.SubMap
  | TracerProtocolGenConstN (Protocol (GenConst r) r bst)
  | TracerCollectBranchDynVal (Set Int)
  | TracerProtocolMsgT (Protocol (MsgT r bst) r bst)
  | TracerProtocolMsgT1 (Protocol (MsgT1 r bst) r bst)

traceWrapper :: String -> String -> String
traceWrapper desc st =
  "--------------------"
    ++ desc
    ++ "-----------------\n"
    ++ st
    ++ "\n"

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

rtops :: (Enum r) => StrFillEnv -> r -> Int
rtops StrFillEnv{width, leftWidth} = ((+ leftWidth) . (width *) . (+ 1) . fromEnum)

rRange :: forall r. (Enum r, Bounded r) => [r]
rRange = [minBound @r .. maxBound]

too :: forall r a. (Show a, Enum r, Bounded r) => StrFillEnv -> [a] -> [StringFill]
too sfe xs = [CenterFill ps ' ' (show v) | (v, ps) <- zip xs $ fmap (rtops sfe) (rRange @r)]

stMsgT :: forall r bst. (Show bst, Ord r, Enum r, Bounded r) => StrFillEnv -> XStringFill (MsgT r bst) r bst
stMsgT sfe =
  let
   in ( \(ls, (from, to), idx) ->
          ( [ CenterFill ps ' ' $ foo from to ((if (i, idx) == (from, 0) then parensWarapper else id) $ show v) i
            | (i, (ps, v)) <- zip (rRange @r) $ zip (fmap (rtops sfe) (rRange @r)) ls
            ]
          , idx == 0
          )
      , \(xs, _) -> too @r sfe xs
      , \xs -> too @r sfe xs
      , \_ -> []
      , \(xs, _) -> too @r sfe xs
      , \xs -> too @r sfe xs
      )

parensWarapper :: String -> String
parensWarapper st = "{" <> st <> "}"

instance (Show r, Show bst, Enum r, Bounded r, Eq r, Ord r) => Show (Tracer r bst) where
  show = \case
    TracerProtocolCreat p -> traceWrapper "Creat" $ show p
    TracerProtocolIdx p -> traceWrapper "Idx" $ show p
    TracerReRank p -> traceWrapper "ReRank" $ show p
    TracerProtocolAddNum p -> traceWrapper "AddNum" $ show p
    TracerProtocolGenConst p -> traceWrapper "GenConst" $ show p
    TracerConstraints p -> traceWrapper "Constrains" $ show p
    TracerSubMap p -> traceWrapper "SubMap" $ show p
    TracerProtocolGenConstN p -> traceWrapper "GenConstN" $ show p
    TracerCollectBranchDynVal dvs -> traceWrapper "CollectBranchDynVal" $ show dvs
    TracerProtocolMsgT p -> traceWrapper "MsgT" $ show p
    TracerProtocolMsgT1 p -> traceWrapper "MsgT1" $ show p