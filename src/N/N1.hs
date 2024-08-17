{-# LANGUAGE PatternSynonyms #-}

module N.N1 where

import N.N2
import N.Type (Creat, Protocol)
import qualified N.Type as N
import Prettyprinter

pattern Msg :: String -> [String] -> r -> r -> N.MsgOrLabel Creat r
pattern Msg a b c d = N.Msg () a b c d

pattern Label :: Int -> N.MsgOrLabel Creat r
pattern Label i = N.Label () i

pattern BranchSt :: bst -> Protocol Creat r bst -> N.BranchSt Creat r bst
pattern BranchSt a b = N.BranchSt () a b

infixr 5 :>

pattern (:>) :: N.MsgOrLabel Creat r -> Protocol Creat r bst -> Protocol Creat r bst
pattern (:>) a b = a N.:> b

pattern Branch :: r -> [N.BranchSt Creat r bst] -> Protocol Creat r bst
pattern Branch a b = N.Branch () a b

pattern Goto :: Int -> Protocol Creat r bst
pattern Goto i = N.Goto () i

pattern Terminal :: Protocol Creat r bst
pattern Terminal = N.Terminal ()

----------------------------------

-- data PingPong = Client | Server | Counter
--   deriving (Show, Eq, Ord, Enum, Bounded)

-- v1 :: Protocol Creat PingPong Bool
-- v1 =
--   Label 0
--     :> Branch
--       Client
--       [ BranchSt True $
--           Msg "Ping" [] Client Server
--             :> Msg "Pong" [] Server Client
--             :> Msg "Add" [] Client Counter
--             :> Goto 0
--       , BranchSt False $
--           Msg "Stop" [] Client Server
--             :> Msg "AStop" [] Client Counter
--             :> Terminal
--       ]


-- data BookRole
--   = Buyer
--   | Seller
--   | Buyer2
--   deriving (Show, Eq, Ord, Enum, Bounded)

-- data BookBranchSt
--   = NotFound
--   | Found
--   | One
--   | Two
--   | Support
--   | NotSupport
--   | Enough
--   | NotEnough
--   deriving (Show)

-- v2 :: Protocol Creat BookRole BookBranchSt
-- v2 =
--   Label 0
--     :> Msg "Title" [] Buyer Seller
--     :> Branch
--       Seller
--       [ BranchSt NotFound $
--           Msg "NoBook" [] Seller Buyer
--             :> Msg "SellerNoBook" [] Buyer Buyer2
--             :> Goto 0
--       , BranchSt Found $
--           Msg "Price" [] Seller Buyer
--             :> Branch
--               Buyer
--               [ BranchSt One $
--                   Msg "OneAfford" [] Buyer Buyer2
--                     :> Msg "OneAccept" [] Buyer Seller
--                     :> Msg "OneDate" [] Seller Buyer
--                     :> Msg "OneSuccess" [] Buyer Buyer2
--                     :> Goto 0
--               , BranchSt Two $
--                   Msg "PriceToBuyer2" [] Buyer Buyer2
--                     :> Branch
--                       Buyer2
--                       [ BranchSt NotSupport $
--                           Msg "NotSupport" [] Buyer2 Buyer
--                             :> Msg "TwoNotBuy" [] Buyer Seller
--                             :> Goto 0
--                       , BranchSt Support $
--                           Msg "SupportVal" [] Buyer2 Buyer
--                             :> Branch
--                               Buyer
--                               [ BranchSt Enough $
--                                   Msg "TwoAccept" [] Buyer Seller
--                                     :> Msg "TwoDate" [] Seller Buyer
--                                     :> Msg "TwoSuccess" [] Buyer Buyer2
--                                     :> Goto 0
--                               , BranchSt NotEnough $
--                                   Msg "TwoNotBuy1" [] Buyer Seller
--                                     :> Msg "TwoFailed" [] Buyer Buyer2
--                                     :> Terminal
--                               ]
--                       ]
--               ]
--       ]
