{-# LANGUAGE PatternSynonyms #-}

module N1 where

import N (Creat, Protocol)
import qualified N

pattern Msg :: String -> [String] -> r -> r -> N.MsgOrLabel Creat r
pattern Msg a b c d = N.Msg () a b c d

pattern Label :: Int -> N.MsgOrLabel Creat r
pattern Label i = N.Label () i

pattern BranchSt :: bst -> Protocol Creat r bst -> N.BranchSt Creat r bst
pattern BranchSt a b = N.BranchSt a b

infixr 5 :>

pattern (:>) :: N.MsgOrLabel Creat r -> Protocol Creat r bst -> Protocol Creat r bst
pattern (:>) a b = a N.:> b

pattern Branch :: r -> [N.BranchSt Creat r bst] -> Protocol Creat r bst
pattern Branch a b = N.Branch a b

pattern Goto :: Int -> Protocol Creat r bst
pattern Goto i = N.Goto () i

pattern Terminal :: Protocol Creat r bst
pattern Terminal = N.Terminal ()

----------------------------------

data PingPong = Client | Server | Counter
  deriving (Show, Eq, Ord, Enum, Bounded)

v1 :: Protocol Creat PingPong Bool
v1 =
  Label 0
    :> Branch
      Client
      [ BranchSt True $
          Msg "Ping" [] Client Server
            :> Msg "Pong" [] Server Client
            :> Msg "Add" [] Client Counter
            :> Goto 0
      , BranchSt False $
          Msg "Stop" [] Client Server
            :> Msg "AStop" [] Client Counter
            :> Terminal
      ]

-- >>> error "------------------------"
-- >>> error $ show (N.piple v1)
-- ------------------------
-- Right Label [0,0,1] 0
-- Branch Client
-- BranchSt True
-- Msg ([0,0,1],[2,2,1]) Ping [] Client Server
-- Msg ([2,2,1],[1,0,1]) Pong [] Server Client
-- Msg ([1,0,1],[0,0,1]) Add [] Client Counter
-- Goto [0,0,1] 0
-- BranchSt False
-- Msg ([0,0,1],[1,-1,1]) Stop [] Client Server
-- Msg ([1,-1,1],[-1,-1,-1]) AStop [] Client Counter
-- Terminal [-1,-1,-1]

data BookRole
  = Buyer
  | Seller
  | Buyer2
  deriving (Show, Eq, Ord, Enum, Bounded)

data BookBranchSt
  = NotFound
  | Found
  | One
  | Two
  | Support
  | NotSupport
  | Enough
  | NotEnough
  deriving (Show)

v2 :: Protocol Creat BookRole BookBranchSt
v2 =
  Label 0
    :> Msg "Title" [] Buyer Seller
    :> Branch
      Seller
      [ BranchSt NotFound $
          Msg "NoBook" [] Seller Buyer
            :> Msg "SellerNoBook" [] Buyer Buyer2
            :> Goto 0
      , BranchSt Found $
          Msg "Price" [] Seller Buyer
            :> Branch
              Buyer
              [ BranchSt One $
                  Msg "OneAccept" [] Buyer Seller
                    :> Msg "OneDate" [] Seller Buyer
                    :> Msg "OneSuccess" [] Buyer Buyer2
                    :> Goto 0
              , BranchSt Two $
                  Msg "PriceToBuyer2" [] Buyer Buyer2
                    :> Branch
                      Buyer2
                      [ BranchSt NotSupport $
                          Msg "NotSupport" [] Buyer2 Buyer
                            :> Msg "TwoNotBuy" [] Buyer Seller
                            :> Goto 0
                      , BranchSt Support $
                          Msg "SupportVal" [] Buyer2 Buyer
                            :> Branch
                              Buyer
                              [ BranchSt Enough $
                                  Msg "TwoAccept" [] Buyer Seller
                                    :> Msg "TwoDate" [] Seller Buyer
                                    :> Msg "TwoSuccess" [] Buyer Buyer2
                                    :> Goto 0
                              , BranchSt NotEnough $
                                  Msg "TwoNotBuy1" [] Buyer Seller
                                    :> Msg "TwoFailed" [] Buyer Buyer2
                                    :> Terminal
                              ]
                      ]
              ]
      ]

-- >>> N.piple v2
-- Right Label [0,0,1] 0
-- Msg ([0,0,1],[2,2,1]) Title [] Buyer Seller
-- Branch Seller
-- BranchSt NotFound
-- Msg ([2,2,1],[1,0,1]) NoBook [] Seller Buyer
-- Msg ([1,0,1],[0,0,1]) SellerNoBook [] Buyer Buyer2
-- Goto [0,0,1] 0
-- BranchSt Found
-- Msg ([2,2,1],[1,1,1]) Price [] Seller Buyer
-- Branch Buyer
-- BranchSt One
-- Msg ([1,1,1],[3,3,1]) OneAccept [] Buyer Seller
-- Msg ([3,3,1],[1,0,1]) OneDate [] Seller Buyer
-- Msg ([1,0,1],[0,0,1]) OneSuccess [] Buyer Buyer2
-- Goto [0,0,1] 0
-- BranchSt Two
-- Msg ([1,1,1],[4,1,4]) PriceToBuyer2 [] Buyer Buyer2
-- Branch Buyer2
-- BranchSt NotSupport
-- Msg ([4,1,4],[1,1,1]) NotSupport [] Buyer2 Buyer
-- Msg ([1,1,1],[0,0,1]) TwoNotBuy [] Buyer Seller
-- Goto [0,0,1] 0
-- BranchSt Support
-- Msg ([4,1,4],[1,1,5]) SupportVal [] Buyer2 Buyer
-- Branch Buyer
-- BranchSt Enough
-- Msg ([1,1,5],[6,6,5]) TwoAccept [] Buyer Seller
-- Msg ([6,6,5],[5,0,5]) TwoDate [] Seller Buyer
-- Msg ([5,0,5],[0,0,1]) TwoSuccess [] Buyer Buyer2
-- Goto [0,0,1] 0
-- BranchSt NotEnough
-- Msg ([1,1,5],[5,-1,5]) TwoNotBuy1 [] Buyer Seller
-- Msg ([5,-1,5],[-1,-1,-1]) TwoFailed [] Buyer Buyer2
-- Terminal [-1,-1,-1]
