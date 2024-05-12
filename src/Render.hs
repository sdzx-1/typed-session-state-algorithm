module Render where

import Data.Foldable (foldl')
import qualified Data.List as L
import Type

data StringFill
  = CenterFill Int Char String
  deriving (Show)

runCenterFill :: String -> StringFill -> String
runCenterFill startSt (CenterFill centerPoint c st) =
  let stlen = length st
      hlen = stlen `div` 2
      rplen = (centerPoint - length startSt) - if odd stlen then hlen + 1 else hlen
      rpSt = replicate rplen c
   in startSt ++ rpSt ++ st

runCenterFills :: [StringFill] -> String
runCenterFills ls =
  let ls' = L.sortOn (\(CenterFill cp _ _) -> cp) ls
   in foldl' runCenterFill "" ls'

width :: Int
width = 20

render'
  :: (Show ann, Show role')
  => Int
  -> (role' -> Int)
  -> Int
  -> (ann -> [StringFill])
  -> R role' ann
  -> [[StringFill]]
render' roleNum roleF nestVal renderAnn rr =
  case rr of
    Terminal ann ->
      let maxSize = (roleNum + 1) * width
       in [ renderAnn ann
          ,
            [ CenterFill (maxSize `div` 2) ' ' "Terminal"
            ]
          , [CenterFill maxSize ' ' " "]
          ]
    (Arrow ann a b st) :> r ->
      let a' = (roleF a + 1) * width
          b' = (roleF b + 1) * width
          va =
            [ CenterFill (width `div` 2) ' ' (take 15 st)
            , CenterFill a' ' ' "|"
            , CenterFill b' ' ' "|"
            , CenterFill ((a' + b') `div` 2) ' ' (if a' > b' then "<---" else "--->")
            ]
       in renderAnn ann : va : render' roleNum roleF nestVal renderAnn r
    Branch ann vs ->
      renderAnn ann
        : concatMap
          ( \(BranchVal st r) ->
              let maxSize = (roleNum + 1) * width
               in [ CenterFill (nestVal * 4 + 1) ' ' "-"
                  , CenterFill (maxSize `div` 2) '-' st
                  , CenterFill (maxSize - nestVal * 4) '-' "-"
                  ]
                    : render' roleNum roleF (nestVal + 1) renderAnn r
          )
          vs

render
  :: (Show ann, Show role')
  => [role']
  -> (role' -> Int)
  -> (ann -> [StringFill])
  -> R role' ann
  -> String
render roles roleF renderAnn rr =
  let roleNum = length roles
      maxSize = (roleNum + 1) * width
      header =
        CenterFill maxSize '-' "-"
          : [CenterFill ((roleF r + 1) * width) '-' (show r) | r <- roles]
   in unlines $
        map runCenterFills $
          header : render' roleNum roleF 1 renderAnn rr
