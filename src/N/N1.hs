{-# LANGUAGE PatternSynonyms #-}

module N.N1 where

import N.N2
import N.Type (Creat, Protocol)
import qualified N.Type as N

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
pattern Branch a b = N.Branch () a b

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

-- k = show $ piple v1

-- >>> error $ show (piple v1)
-- No instance for `Pretty PingPong' arising from a use of `show'
-- In the second argument of `($)', namely `show (piple v1)'
-- In the expression: error $ show (piple v1)
-- In an equation for `it_ay6F': it_ay6F = error $ show (piple v1)

-- Right Label () 0
-- [Branch] () Client
--   * BranchSt True
--   Msg <0 [True] -> (2 , 2) Client -> Server> Ping [] Client Server
--   Msg <2 -> (0 [True] , 1 [True]) Server -> Client> Pong [] Server Client
--   Msg <1 [True] -> (0 s , 1 s) Client -> Counter> Add [] Client Counter
--   Goto () 0
--   * BranchSt False
--   Msg <0 [False] -> (1 [False] , End) Client -> Server> Stop [] Client Server
--   Msg <1 [False] -> (End , End) Client -> Counter> AStop [] Client Counter
--   Terminal ()

-- Right Label [0, 0, 1] 0
-- [Branch] [0, 0, 1] Client
--   * BranchSt True
--   Msg ([0, 0, 1], [2, 2, 1]) Ping [] Client Server
--   Msg ([2, 2, 1], [1, 0, 1]) Pong [] Server Client
--   Msg ([1, 0, 1], [0, 0, 1]) Add [] Client Counter
--   Goto [0, 0, 1] 0
--   * BranchSt False
--   Msg ([0, 0, 1], [1, -1, 1]) Stop [] Client Server
--   Msg ([1, -1, 1], [-1, -1, -1]) AStop [] Client Counter
--   Terminal [-1, -1, -1]

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
                  Msg "OneAfford" [] Buyer Buyer2
                    :> Msg "OneAccept" [] Buyer Seller
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

-- >>> error $ show (N.piple v2)
-- Right Label () 0
-- Msg <0 -> (2 s , 2 s) Buyer -> Seller> Title [] Buyer Seller
-- [Branch] () Seller
--   * BranchSt NotFound
--   Msg <2 [NotFound] -> (0 , 1 s) Seller -> Buyer> NoBook [] Seller Buyer
--   Msg <1 [NotFound] -> (0 , 1 s) Buyer -> Buyer2> SellerNoBook [] Buyer Buyer2
--   Goto () 0
--   * BranchSt Found
--   Msg <2 [Found] -> (3 s , 1 s) Seller -> Buyer> Price [] Seller Buyer
--   [Branch] () Buyer
--     * BranchSt One
--     Msg <1 [One,Found] -> (3 [One,Found] , 4) Buyer -> Buyer2> OneAfford [  ] Buyer Buyer2
--     Msg <3 [One,Found] -> (5 , 5) Buyer -> Seller> OneAccept [] Buyer Seller
--     Msg <5 -> (0 , 4) Seller -> Buyer> OneDate [] Seller Buyer
--     Msg <4 -> (0 , 1 s) Buyer -> Buyer2> OneSuccess [] Buyer Buyer2
--     Goto () 0
--     * BranchSt Two
--     Msg <1 [Two,Found] -> (6 s , 6 s) Buyer -> Buyer2> PriceToBuyer2 [  ] Buyer Buyer2
--     [Branch] () Buyer2
--       * BranchSt NotSupport
--       Msg <6 [NotSupport,Two,Found] -> (1 s , 3 [NotSupport,Two,Found]) Buyer2 -> Buyer> NotSupport [  ] Buyer2 Buyer
--       Msg <3 [NotSupport,Two,Found] -> (0 , 0) Buyer -> Seller> TwoNotBuy [  ] Buyer Seller
--       Goto () 0
--       * BranchSt Support
--       Msg <6 [Support,Two,Found] -> (7 s , 3 s) Buyer2 -> Buyer> SupportVal [  ] Buyer2 Buyer
--       [Branch] () Buyer
--         * BranchSt Enough
--         Msg <3 [Enough,Support,Two,Found] -> (8 , 8) Buyer -> Seller> TwoAccept [  ] Buyer Seller
--         Msg <8 -> (0 , 7 [Enough,Support,Two,Found]) Seller -> Buyer> TwoDate [  ] Seller Buyer
--         Msg <7 [Enough,Support,Two,Found] -> (0 , 1 s) Buyer -> Buyer2> TwoSuccess [  ] Buyer Buyer2
--         Goto () 0
--         * BranchSt NotEnough
--         Msg <3 [NotEnough,Support,Two,Found] -> (7 [NotEnough,Support,Two,Found] , End) Buyer -> Seller> TwoNotBuy1 [  ] Buyer Seller
--         Msg <7 [NotEnough,Support,Two,Found] -> (End , End) Buyer -> Buyer2> TwoFailed [  ] Buyer Buyer2
--         Terminal ()

-- Right Label () 0
-- Msg <0 -> (2 s , 2 s) Buyer -> Seller> Title [] Buyer Seller
-- [Branch] () Seller
--   * BranchSt NotFound
--   Msg <2 [NotFound] -> (0 , 1 s) Seller -> Buyer> NoBook [] Seller Buyer
--   Msg <1 [NotFound] -> (0 , 1 s) Buyer -> Buyer2> SellerNoBook [] Buyer Buyer2
--   Goto () 0
--   * BranchSt Found
--   Msg <2 [Found] -> (3 s , 1 s) Seller -> Buyer> Price [] Seller Buyer
--   [Branch] () Buyer
--     * BranchSt One
--     Msg <1 [One,Found] -> (3 s , 4) Buyer -> Buyer2> OneAfford [] Buyer Buyer2
--     Msg <3 [One,Found] -> (5 , 5) Buyer -> Seller> OneAccept [] Buyer Seller
--     Msg <5 -> (0 , 4) Seller -> Buyer> OneDate [] Seller Buyer
--     Msg <4 -> (0 , 1 s) Buyer -> Buyer2> OneSuccess [] Buyer Buyer2
--     Goto () 0
--     * BranchSt Two
--     Msg <1 [Two,Found] -> (6 s , 6 s) Buyer -> Buyer2> PriceToBuyer2 [  ] Buyer Buyer2
--     [Branch] () Buyer2
--       * BranchSt NotSupport
--       Msg <6 [NotSupport,Two,Found] -> (1 s , 3 s) Buyer2 -> Buyer> NotSupport [  ] Buyer2 Buyer
--       Msg <3 [NotSupport,Two,Found] -> (0 , 0) Buyer -> Seller> TwoNotBuy [  ] Buyer Seller
--       Goto () 0
--       * BranchSt Support
--       Msg <6 [Support,Two,Found] -> (7 s , 3 s) Buyer2 -> Buyer> SupportVal [  ] Buyer2 Buyer
--       [Branch] () Buyer
--         * BranchSt Enough
--         Msg <3 [Enough,Support,Two,Found] -> (8 , 8) Buyer -> Seller> TwoAccept [  ] Buyer Seller
--         Msg <8 -> (0 , 7 s) Seller -> Buyer> TwoDate [] Seller Buyer
--         Msg <7 [Enough,Support,Two,Found] -> (0 , 1 s) Buyer -> Buyer2> TwoSuccess [  ] Buyer Buyer2
--         Goto () 0
--         * BranchSt NotEnough
--         Msg <3 [NotEnough,Support,Two,Found] -> (7 s , End) Buyer -> Seller> TwoNotBuy1 [  ] Buyer Seller
--         Msg <7 [NotEnough,Support,Two,Found] -> (End , End) Buyer -> Buyer2> TwoFailed [  ] Buyer Buyer2
--         Terminal ()

-- Right Label [0, 0, 1] 0
-- Msg ([0, 0, 1], [2, 2, 1]) Title [] Buyer Seller
-- [Branch] [2, 2, 1] Seller
--   * BranchSt NotFound
--   Msg ([2, 2, 1], [1, 0, 1]) NoBook [] Seller Buyer
--   Msg ([1, 0, 1], [0, 0, 1]) SellerNoBook [] Buyer Buyer2
--   Goto [0, 0, 1] 0
--   * BranchSt Found
--   Msg ([2, 2, 1], [1, 3, 1]) Price [] Seller Buyer
--   [Branch] [1, 3, 1] Buyer
--     * BranchSt One
--     Msg ([1, 3, 1], [3, 3, 4]) OneAfford [] Buyer Buyer2
--     Msg ([3, 3, 4], [5, 5, 4]) OneAccept [] Buyer Seller
--     Msg ([5, 5, 4], [4, 0, 4]) OneDate [] Seller Buyer
--     Msg ([4, 0, 4], [0, 0, 1]) OneSuccess [] Buyer Buyer2
--     Goto [0, 0, 1] 0
--     * BranchSt Two
--     Msg ([1, 3, 1], [6, 3, 6]) PriceToBuyer2 [] Buyer Buyer2
--     [Branch] [6, 3, 6] Buyer2
--       * BranchSt NotSupport
--       Msg ([6, 3, 6], [3, 3, 1]) NotSupport [] Buyer2 Buyer
--       Msg ([3, 3, 1], [0, 0, 1]) TwoNotBuy [] Buyer Seller
--       Goto [0, 0, 1] 0
--       * BranchSt Support
--       Msg ([6, 3, 6], [3, 3, 7]) SupportVal [] Buyer2 Buyer
--       [Branch] [3, 3, 7] Buyer
--         * BranchSt Enough
--         Msg ([3, 3, 7], [8, 8, 7]) TwoAccept [] Buyer Seller
--         Msg ([8, 8, 7], [7, 0, 7]) TwoDate [] Seller Buyer
--         Msg ([7, 0, 7], [0, 0, 1]) TwoSuccess [] Buyer Buyer2
--         Goto [0, 0, 1] 0
--         * BranchSt NotEnough
--         Msg ([3, 3, 7], [7, -1, 7]) TwoNotBuy1 [] Buyer Seller
--         Msg ([7, -1, 7], [-1, -1, -1]) TwoFailed [] Buyer Buyer2
--         Terminal [-1, -1, -1]
