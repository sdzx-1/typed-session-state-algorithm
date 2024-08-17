{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module N.Render where

import qualified Constraint as C
import Control.Algebra (Has, (:+:))
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.State
import Control.Effect.Writer
import qualified Data.List as L
import Data.Sequence (Seq)
import Data.Set (Set)
import N.Type
import N.Utils

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

width :: Int
width = 30

leftWidth :: Int
leftWidth = 20

reSt :: String -> String
reSt st =
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
  = TracerProtocolCreat (Protocol Creat r bst)
  | TracerProtocolAddNum (Protocol AddNums r bst)
  | TracerProtocolGenConst (Protocol (GenConst r) r bst)
  | TracerConstraints (Seq C.Constraint)
  | TracerSubMap C.SubMap
  | TracerProtocolGenConstN (Protocol (GenConst r) r bst)
  | TracerCollectBranchDynVal (Set Int)
  | TracerProtocolMsgT (Protocol (MsgT r bst) r bst)

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
      [ CenterFill i ' ' $ show v
      | (i, v) <- zip (fmap rtops (rRange @r)) (zip xs ys)
      ]
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
  ( \((xs, ys), (from, to)) ->
      [ CenterFill ps ' ' $ foo from to (show v) i
      | (i, (ps, v)) <- zip (rRange @r) (zip (fmap rtops (rRange @r)) (zip xs ys))
      ]
  , \(xs, _) -> too @r xs
  , \xs -> too @r xs
  , \_ -> []
  , \_ -> []
  , \_ -> []
  )

rtops :: (Enum r) => r -> Int
rtops = ((+ leftWidth) . (width *) . (+ 1) . fromEnum)

rRange :: forall r. (Enum r, Bounded r) => [r]
rRange = [minBound @r .. maxBound]

too :: forall r a. (Show a, Enum r, Bounded r) => [a] -> [StringFill]
too xs = [CenterFill ps ' ' (show v) | (v, ps) <- zip xs $ fmap rtops (rRange @r)]

stMsgT :: forall r bst. (Show bst, Ord r, Enum r, Bounded r) => XStringFill (MsgT r bst) r bst
stMsgT =
  let
   in ( \(ls, (from, to)) ->
          [ CenterFill ps ' ' $ foo from to (show v) i
          | (i, (ps, v)) <- zip (rRange @r) $ zip (fmap rtops (rRange @r)) ls
          ]
      , \(xs, _) -> too @r xs
      , \xs -> too @r xs
      , \_ -> []
      , \(xs, _) -> too @r xs
      , \xs -> too @r xs
      )

instance (Show r, Show bst, Enum r, Bounded r, Eq r, Ord r) => Show (Tracer r bst) where
  show = \case
    TracerProtocolCreat p -> traceWrapper "Creat" $ getSF stCreat p
    TracerProtocolAddNum p -> traceWrapper "AddNum" $ getSF (stAddNums @r) p
    TracerProtocolGenConst p -> traceWrapper "GenConst" $ getSF (stGenConst @r) p
    TracerConstraints p -> traceWrapper "Constrains" $ show p
    TracerSubMap p -> traceWrapper "SubMap" $ show p
    TracerProtocolGenConstN p -> traceWrapper "GenConstN" $ getSF (stGenConst @r) p
    TracerCollectBranchDynVal dvs -> traceWrapper "CollectBranchDynVal" $ show dvs
    TracerProtocolMsgT p -> traceWrapper "MsgT" $ getSF (stMsgT @r) p