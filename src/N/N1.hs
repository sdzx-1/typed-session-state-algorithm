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

instance Pretty PingPong where
  pretty = pretty . show

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

-- >>> error $ show (pipleWithTracer v1)
-- (fromList [--------------------Creat-----------------
-- Label () 0
-- [Branch] () Client
--   * BranchSt True
--   Msg <()> Ping [] Client Server
--   Msg <()> Pong [] Server Client
--   Msg <()> Add [] Client Counter
--   Goto () 0
--   * BranchSt False
--   Msg <()> Stop [] Client Server
--   Msg <()> AStop [] Client Counter
--   Terminal ()
-- ,--------------------AddNum-----------------
-- Label [0, 1, 2] 0
-- [Branch] [0, 1, 2] Client
--   * BranchSt True
--   Msg <([0, 1, 2], [3, 4, 5])> Ping [] Client Server
--   Msg <([3, 4, 5], [6, 7, 8])> Pong [] Server Client
--   Msg <([6, 7, 8], [9, 10, 11])> Add [] Client Counter
--   Goto [9, 10, 11] 0
--   * BranchSt False
--   Msg <([0, 1, 2], [12, 13, 14])> Stop [] Client Server
--   Msg <([12, 13, 14], [15, 16, 17])> AStop [] Client Counter
--   Terminal [15, 16, 17]
-- ,--------------------GenConst-----------------
-- Label ([0, 1, 2], 0) 0
-- [Branch] [0, 1, 2] Client
--   * BranchSt True
--   Msg <(([0, 1, 2], [3, 4, 5]), (Client, Server))> Ping [] Client Server
--   Msg <(([3, 4, 5], [6, 7, 8]), (Server, Client))> Pong [] Server Client
--   Msg <(([6, 7, 8], [9, 10, 11]), (Client, Counter))> Add [] Client Counter
--   Goto ([9, 10, 11], 0) 0
--   * BranchSt False
--   Msg <(([0, 1, 2], [12, 13, 14]), (Client, Server))> Stop [] Client Server
--   Msg <( ([12, 13, 14], [15, 16, 17])
--   , (Client, Counter) )> AStop [] Client Counter
--   Terminal [15, 16, 17]
-- ,--------------------Constrains-----------------
-- fromList [Constraint 0 1,Constraint 2 5,Constraint 4 3,Constraint 5 8,Constraint 6 8,Constraint 7 10,Constraint 9 0,Constraint 10 1,Constraint 11 2,Constraint 0 1,Constraint 2 14,Constraint 12 14,Constraint 13 16,Constraint 15 (-1),Constraint 16 (-1),Constraint 17 (-1)]
-- ,--------------------SubMap-----------------
-- fromList [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,0),(8,1),(9,0),(10,0),(11,1),(12,1),(13,-1),(14,1),(15,-1),(16,-1),(17,-1)]
-- ,--------------------GenConstN-----------------
-- Label ([0, 0, 1], 0) 0
-- [Branch] [0, 0, 1] Client
--   * BranchSt True
--   Msg <(([0, 0, 1], [2, 2, 1]), (Client, Server))> Ping [] Client Server
--   Msg <(([2, 2, 1], [1, 0, 1]), (Server, Client))> Pong [] Server Client
--   Msg <(([1, 0, 1], [0, 0, 1]), (Client, Counter))> Add [] Client Counter
--   Goto ([0, 0, 1], 0) 0
--   * BranchSt False
--   Msg <(([0, 0, 1], [1, -1, 1]), (Client, Server))> Stop [] Client Server
--   Msg <(([1, -1, 1], [-1, -1, -1]), (Client, Counter))> AStop [] Client Counter
--   Terminal [-1, -1, -1]
-- ],Right Label ([0, 0, 1], 0) 0
-- [Branch] [0, 0, 1] Client
--   * BranchSt True
--   Msg <(([0, 0, 1], [2, 2, 1]), (Client, Server))> Ping [] Client Server
--   Msg <(([2, 2, 1], [1, 0, 1]), (Server, Client))> Pong [] Server Client
--   Msg <(([1, 0, 1], [0, 0, 1]), (Client, Counter))> Add [] Client Counter
--   Goto ([0, 0, 1], 0) 0
--   * BranchSt False
--   Msg <(([0, 0, 1], [1, -1, 1]), (Client, Server))> Stop [] Client Server
--   Msg <(([1, -1, 1], [-1, -1, -1]), (Client, Counter))> AStop [] Client Counter
--   Terminal [-1, -1, -1])

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

instance Pretty BookRole where
  pretty = pretty . show

-- >>> error $ show (pipleWithTracer v2)
-- (fromList [--------------------Creat-----------------
-- Label () 0
-- Msg <()> Title [] Buyer Seller
-- [Branch] () Seller
--   * BranchSt NotFound
--   Msg <()> NoBook [] Seller Buyer
--   Msg <()> SellerNoBook [] Buyer Buyer2
--   Goto () 0
--   * BranchSt Found
--   Msg <()> Price [] Seller Buyer
--   [Branch] () Buyer
--     * BranchSt One
--     Msg <()> OneAfford [] Buyer Buyer2
--     Msg <()> OneAccept [] Buyer Seller
--     Msg <()> OneDate [] Seller Buyer
--     Msg <()> OneSuccess [] Buyer Buyer2
--     Goto () 0
--     * BranchSt Two
--     Msg <()> PriceToBuyer2 [] Buyer Buyer2
--     [Branch] () Buyer2
--       * BranchSt NotSupport
--       Msg <()> NotSupport [] Buyer2 Buyer
--       Msg <()> TwoNotBuy [] Buyer Seller
--       Goto () 0
--       * BranchSt Support
--       Msg <()> SupportVal [] Buyer2 Buyer
--       [Branch] () Buyer
--         * BranchSt Enough
--         Msg <()> TwoAccept [] Buyer Seller
--         Msg <()> TwoDate [] Seller Buyer
--         Msg <()> TwoSuccess [] Buyer Buyer2
--         Goto () 0
--         * BranchSt NotEnough
--         Msg <()> TwoNotBuy1 [] Buyer Seller
--         Msg <()> TwoFailed [] Buyer Buyer2
--         Terminal ()
-- ,--------------------AddNum-----------------
-- Label [0, 1, 2] 0
-- Msg <([0, 1, 2], [3, 4, 5])> Title [] Buyer Seller
-- [Branch] [3, 4, 5] Seller
--   * BranchSt NotFound
--   Msg <([3, 4, 5], [6, 7, 8])> NoBook [] Seller Buyer
--   Msg <([6, 7, 8], [9, 10, 11])> SellerNoBook [] Buyer Buyer2
--   Goto [9, 10, 11] 0
--   * BranchSt Found
--   Msg <([3, 4, 5], [12, 13, 14])> Price [] Seller Buyer
--   [Branch] [12, 13, 14] Buyer
--     * BranchSt One
--     Msg <([12, 13, 14], [15, 16, 17])> OneAfford [] Buyer Buyer2
--     Msg <([15, 16, 17], [18, 19, 20])> OneAccept [] Buyer Seller
--     Msg <([18, 19, 20], [21, 22, 23])> OneDate [] Seller Buyer
--     Msg <([21, 22, 23], [24, 25, 26])> OneSuccess [] Buyer Buyer2
--     Goto [24, 25, 26] 0
--     * BranchSt Two
--     Msg <([12, 13, 14], [27, 28, 29])> PriceToBuyer2 [] Buyer Buyer2
--     [Branch] [27, 28, 29] Buyer2
--       * BranchSt NotSupport
--       Msg <([27, 28, 29], [30, 31, 32])> NotSupport [] Buyer2 Buyer
--       Msg <([30, 31, 32], [33, 34, 35])> TwoNotBuy [] Buyer Seller
--       Goto [33, 34, 35] 0
--       * BranchSt Support
--       Msg <([27, 28, 29], [36, 37, 38])> SupportVal [] Buyer2 Buyer
--       [Branch] [36, 37, 38] Buyer
--         * BranchSt Enough
--         Msg <([36, 37, 38], [39, 40, 41])> TwoAccept [] Buyer Seller
--         Msg <([39, 40, 41], [42, 43, 44])> TwoDate [] Seller Buyer
--         Msg <([42, 43, 44], [45, 46, 47])> TwoSuccess [] Buyer Buyer2
--         Goto [45, 46, 47] 0
--         * BranchSt NotEnough
--         Msg <([36, 37, 38], [48, 49, 50])> TwoNotBuy1 [] Buyer Seller
--         Msg <([48, 49, 50], [51, 52, 53])> TwoFailed [] Buyer Buyer2
--         Terminal [51, 52, 53]
-- ,--------------------GenConst-----------------
-- Label ([0, 1, 2], 0) 0
-- Msg <(([0, 1, 2], [3, 4, 5]), (Buyer, Seller))> Title [] Buyer Seller
-- [Branch] [3, 4, 5] Seller
--   * BranchSt NotFound
--   Msg <(([3, 4, 5], [6, 7, 8]), (Seller, Buyer))> NoBook [] Seller Buyer
--   Msg <(([6, 7, 8], [9, 10, 11]), (Buyer, Buyer2))> SellerNoBook [] Buyer Buyer2
--   Goto ([9, 10, 11], 0) 0
--   * BranchSt Found
--   Msg <(([3, 4, 5], [12, 13, 14]), (Seller, Buyer))> Price [] Seller Buyer
--   [Branch] [12, 13, 14] Buyer
--     * BranchSt One
--     Msg <( ([12, 13, 14], [15, 16, 17])
--     , (Buyer, Buyer2) )> OneAfford [] Buyer Buyer2
--     Msg <( ([15, 16, 17], [18, 19, 20])
--     , (Buyer, Seller) )> OneAccept [] Buyer Seller
--     Msg <( ([18, 19, 20], [21, 22, 23])
--     , (Seller, Buyer) )> OneDate [] Seller Buyer
--     Msg <( ([21, 22, 23], [24, 25, 26])
--     , (Buyer, Buyer2) )> OneSuccess [] Buyer Buyer2
--     Goto ([24, 25, 26], 0) 0
--     * BranchSt Two
--     Msg <( ([12, 13, 14], [27, 28, 29])
--     , (Buyer, Buyer2) )> PriceToBuyer2 [] Buyer Buyer2
--     [Branch] [27, 28, 29] Buyer2
--       * BranchSt NotSupport
--       Msg <( ([27, 28, 29], [30, 31, 32])
--       , (Buyer2, Buyer) )> NotSupport [] Buyer2 Buyer
--       Msg <( ([30, 31, 32], [33, 34, 35])
--       , (Buyer, Seller) )> TwoNotBuy [] Buyer Seller
--       Goto ([33, 34, 35], 0) 0
--       * BranchSt Support
--       Msg <( ([27, 28, 29], [36, 37, 38])
--       , (Buyer2, Buyer) )> SupportVal [] Buyer2 Buyer
--       [Branch] [36, 37, 38] Buyer
--         * BranchSt Enough
--         Msg <( ([36, 37, 38], [39, 40, 41])
--         , (Buyer, Seller) )> TwoAccept [] Buyer Seller
--         Msg <( ([39, 40, 41], [42, 43, 44])
--         , (Seller, Buyer) )> TwoDate [] Seller Buyer
--         Msg <( ([42, 43, 44], [45, 46, 47])
--         , (Buyer, Buyer2) )> TwoSuccess [] Buyer Buyer2
--         Goto ([45, 46, 47], 0) 0
--         * BranchSt NotEnough
--         Msg <( ([36, 37, 38], [48, 49, 50])
--         , (Buyer, Seller) )> TwoNotBuy1 [] Buyer Seller
--         Msg <( ([48, 49, 50], [51, 52, 53])
--         , (Buyer, Buyer2) )> TwoFailed [] Buyer Buyer2
--         Terminal [51, 52, 53]
-- ,--------------------Constrains-----------------
-- fromList [Constraint 0 1,Constraint 2 5,Constraint 4 3,Constraint 5 8,Constraint 6 8,Constraint 7 10,Constraint 9 0,Constraint 10 1,Constraint 11 2,Constraint 4 3,Constraint 5 14,Constraint 12 14,Constraint 13 16,Constraint 15 16,Constraint 17 20,Constraint 19 18,Constraint 20 23,Constraint 21 23,Constraint 22 25,Constraint 24 0,Constraint 25 1,Constraint 26 2,Constraint 12 14,Constraint 13 28,Constraint 29 27,Constraint 28 31,Constraint 30 31,Constraint 32 35,Constraint 33 0,Constraint 34 1,Constraint 35 2,Constraint 29 27,Constraint 28 37,Constraint 36 37,Constraint 38 41,Constraint 40 39,Constraint 41 44,Constraint 42 44,Constraint 43 46,Constraint 45 0,Constraint 46 1,Constraint 47 2,Constraint 36 37,Constraint 38 50,Constraint 48 50,Constraint 49 52,Constraint 51 (-1),Constraint 52 (-1),Constraint 53 (-1)]
-- ,--------------------SubMap-----------------
-- fromList [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,0),(8,1),(9,0),(10,0),(11,1),(12,1),(13,3),(14,1),(15,3),(16,3),(17,4),(18,5),(19,5),(20,4),(21,4),(22,0),(23,4),(24,0),(25,0),(26,1),(27,6),(28,3),(29,6),(30,3),(31,3),(32,1),(33,0),(34,0),(35,1),(36,3),(37,3),(38,7),(39,8),(40,8),(41,7),(42,7),(43,0),(44,7),(45,0),(46,0),(47,1),(48,7),(49,-1),(50,7),(51,-1),(52,-1),(53,-1)]
-- ,--------------------GenConstN-----------------
-- Label ([0, 0, 1], 0) 0
-- Msg <(([0, 0, 1], [2, 2, 1]), (Buyer, Seller))> Title [] Buyer Seller
-- [Branch] [2, 2, 1] Seller
--   * BranchSt NotFound
--   Msg <(([2, 2, 1], [1, 0, 1]), (Seller, Buyer))> NoBook [] Seller Buyer
--   Msg <(([1, 0, 1], [0, 0, 1]), (Buyer, Buyer2))> SellerNoBook [] Buyer Buyer2
--   Goto ([0, 0, 1], 0) 0
--   * BranchSt Found
--   Msg <(([2, 2, 1], [1, 3, 1]), (Seller, Buyer))> Price [] Seller Buyer
--   [Branch] [1, 3, 1] Buyer
--     * BranchSt One
--     Msg <(([1, 3, 1], [3, 3, 4]), (Buyer, Buyer2))> OneAfford [] Buyer Buyer2
--     Msg <(([3, 3, 4], [5, 5, 4]), (Buyer, Seller))> OneAccept [] Buyer Seller
--     Msg <(([5, 5, 4], [4, 0, 4]), (Seller, Buyer))> OneDate [] Seller Buyer
--     Msg <(([4, 0, 4], [0, 0, 1]), (Buyer, Buyer2))> OneSuccess [] Buyer Buyer2
--     Goto ([0, 0, 1], 0) 0
--     * BranchSt Two
--     Msg <( ([1, 3, 1], [6, 3, 6])
--     , (Buyer, Buyer2) )> PriceToBuyer2 [] Buyer Buyer2
--     [Branch] [6, 3, 6] Buyer2
--       * BranchSt NotSupport
--       Msg <(([6, 3, 6], [3, 3, 1]), (Buyer2, Buyer))> NotSupport [] Buyer2 Buyer
--       Msg <(([3, 3, 1], [0, 0, 1]), (Buyer, Seller))> TwoNotBuy [] Buyer Seller
--       Goto ([0, 0, 1], 0) 0
--       * BranchSt Support
--       Msg <(([6, 3, 6], [3, 3, 7]), (Buyer2, Buyer))> SupportVal [] Buyer2 Buyer
--       [Branch] [3, 3, 7] Buyer
--         * BranchSt Enough
--         Msg <( ([3, 3, 7], [8, 8, 7])
--         , (Buyer, Seller) )> TwoAccept [] Buyer Seller
--         Msg <(([8, 8, 7], [7, 0, 7]), (Seller, Buyer))> TwoDate [] Seller Buyer
--         Msg <( ([7, 0, 7], [0, 0, 1])
--         , (Buyer, Buyer2) )> TwoSuccess [] Buyer Buyer2
--         Goto ([0, 0, 1], 0) 0
--         * BranchSt NotEnough
--         Msg <( ([3, 3, 7], [7, -1, 7])
--         , (Buyer, Seller) )> TwoNotBuy1 [] Buyer Seller
--         Msg <( ([7, -1, 7], [-1, -1, -1])
--         , (Buyer, Buyer2) )> TwoFailed [] Buyer Buyer2
--         Terminal [-1, -1, -1]
-- ],Right Label ([0, 0, 1], 0) 0
-- Msg <(([0, 0, 1], [2, 2, 1]), (Buyer, Seller))> Title [] Buyer Seller
-- [Branch] [2, 2, 1] Seller
--   * BranchSt NotFound
--   Msg <(([2, 2, 1], [1, 0, 1]), (Seller, Buyer))> NoBook [] Seller Buyer
--   Msg <(([1, 0, 1], [0, 0, 1]), (Buyer, Buyer2))> SellerNoBook [] Buyer Buyer2
--   Goto ([0, 0, 1], 0) 0
--   * BranchSt Found
--   Msg <(([2, 2, 1], [1, 3, 1]), (Seller, Buyer))> Price [] Seller Buyer
--   [Branch] [1, 3, 1] Buyer
--     * BranchSt One
--     Msg <(([1, 3, 1], [3, 3, 4]), (Buyer, Buyer2))> OneAfford [] Buyer Buyer2
--     Msg <(([3, 3, 4], [5, 5, 4]), (Buyer, Seller))> OneAccept [] Buyer Seller
--     Msg <(([5, 5, 4], [4, 0, 4]), (Seller, Buyer))> OneDate [] Seller Buyer
--     Msg <(([4, 0, 4], [0, 0, 1]), (Buyer, Buyer2))> OneSuccess [] Buyer Buyer2
--     Goto ([0, 0, 1], 0) 0
--     * BranchSt Two
--     Msg <( ([1, 3, 1], [6, 3, 6])
--     , (Buyer, Buyer2) )> PriceToBuyer2 [] Buyer Buyer2
--     [Branch] [6, 3, 6] Buyer2
--       * BranchSt NotSupport
--       Msg <(([6, 3, 6], [3, 3, 1]), (Buyer2, Buyer))> NotSupport [] Buyer2 Buyer
--       Msg <(([3, 3, 1], [0, 0, 1]), (Buyer, Seller))> TwoNotBuy [] Buyer Seller
--       Goto ([0, 0, 1], 0) 0
--       * BranchSt Support
--       Msg <(([6, 3, 6], [3, 3, 7]), (Buyer2, Buyer))> SupportVal [] Buyer2 Buyer
--       [Branch] [3, 3, 7] Buyer
--         * BranchSt Enough
--         Msg <( ([3, 3, 7], [8, 8, 7])
--         , (Buyer, Seller) )> TwoAccept [] Buyer Seller
--         Msg <(([8, 8, 7], [7, 0, 7]), (Seller, Buyer))> TwoDate [] Seller Buyer
--         Msg <( ([7, 0, 7], [0, 0, 1])
--         , (Buyer, Buyer2) )> TwoSuccess [] Buyer Buyer2
--         Goto ([0, 0, 1], 0) 0
--         * BranchSt NotEnough
--         Msg <( ([3, 3, 7], [7, -1, 7])
--         , (Buyer, Seller) )> TwoNotBuy1 [] Buyer Seller
--         Msg <( ([7, -1, 7], [-1, -1, -1])
--         , (Buyer, Buyer2) )> TwoFailed [] Buyer Buyer2
--         Terminal [-1, -1, -1])
