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

{-
>>> error $ show (pipleWithTracer v1)
(fromList [--------------------Creat-----------------
---------------------------Client------------------------Server-----------------------Counter
LABEL 0
  [Branch] Client
    ▶️️BranchStTrue
    Ping
    Pong
    Add
    Goto 0
    ▶️️BranchStFalse
    Stop
    AStop
    Terminal
,--------------------AddNum-----------------
---------------------------Client------------------------Server-----------------------Counter
LABEL 0
  [Branch] Client
    ▶️️BranchStTrue
    Ping                   (0,3)                         (1,4)                         (2,5)
    Pong                   (3,6)                         (4,7)                         (5,8)
    Add                    (6,9)                         (7,10)                        (8,11)
    Goto 0
    ▶️️BranchStFalse
    Stop                   (0,12)                        (1,13)                        (2,14)
    AStop                 (12,15)                       (13,16)                       (14,17)
    Terminal
,--------------------GenConst-----------------
---------------------------Client------------------------Server-----------------------Counter
LABEL 0                      0                             1                             2
  [Branch] Client            0                             1                             2
    ▶️️BranchStTrue
    Ping                  (0,3)->                       ->(1,4)                           
    Pong                  (3,6)<-                       <-(4,7)                           
    Add                   (6,9)->                                                     ->(8,11)
    Goto 0
    ▶️️BranchStFalse
    Stop                  (0,12)->                      ->(1,13)                          
    AStop                (12,15)->                                                   ->(14,17)
    Terminal
,--------------------Constrains-----------------
fromList [Constraint 0 1,Constraint 2 5,Constraint 4 3,Constraint 5 8,Constraint 6 8,Constraint 7 10,Constraint 9 0,Constraint 10 1,Constraint 11 2,Constraint 0 1,Constraint 2 14,Constraint 12 14,Constraint 13 16,Constraint 15 (-1),Constraint 16 (-1),Constraint 17 (-1)]
,--------------------SubMap-----------------
fromList [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,0),(8,1),(9,0),(10,0),(11,1),(12,1),(13,-1),(14,1),(15,-1),(16,-1),(17,-1)]
,--------------------GenConstN-----------------
---------------------------Client------------------------Server-----------------------Counter
LABEL 0                      0                             0                             1
  [Branch] Client            0                             0                             1
    ▶️️BranchStTrue
    Ping                  (0,2)->                       ->(0,2)                           
    Pong                  (2,1)<-                       <-(2,0)                           
    Add                   (1,0)->                                                     ->(1,1)
    Goto 0
    ▶️️BranchStFalse
    Stop                  (0,1)->                       ->(0,-1)                          
    AStop                 (1,-1)->                                                    ->(1,-1)
    Terminal
],Right Label ([0,0,1],0) 0
[Branch] [0,0,1] Client
  * BranchSt () True
  Msg <(([0,0,1],[2,2,1]),(Client,Server))> Ping [] Client Server
  Msg <(([2,2,1],[1,0,1]),(Server,Client))> Pong [] Server Client
  Msg <(([1,0,1],[0,0,1]),(Client,Counter))> Add [] Client Counter
  Goto ([0,0,1],0) 0
  * BranchSt () False
  Msg <(([0,0,1],[1,-1,1]),(Client,Server))> Stop [] Client Server
  Msg <(([1,-1,1],[-1,-1,-1]),(Client,Counter))> AStop [] Client Counter
  Terminal [-1,-1,-1])

-}




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
              [ BranchSt Two $
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
              , BranchSt One $
                  Msg "OneAfford" [] Buyer Buyer2
                    :> Msg "OneAccept" [] Buyer Seller
                    :> Msg "OneDate" [] Seller Buyer
                    :> Msg "OneSuccess" [] Buyer Buyer2
                    :> Goto 0
              ]
      ]

{-
>>> error $ show (pipleWithTracer v2)
(fromList [--------------------Creat-----------------
---------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0
  Title
  [Branch] Seller
    ▶️️BranchStNotFound
    NoBook
    SellerNoBook
    Goto 0
    ▶️️BranchStFound
    Price
    [Branch] Buyer
      ▶️️BranchStTwo
      PriceToBuyer2
      [Branch] Buyer2
        ▶️️BranchStNotSupport
        NotSupport
        TwoNotBuy
        Goto 0
        ▶️️BranchStSupport
        SupportVal
        [Branch] Buyer
          ▶️️BranchStEnough
          TwoAccept
          TwoDate
          TwoSuccess
          Goto 0
          ▶️️BranchStNotEnough
          TwoNotBuy1
          TwoFailed
          Terminal
      ▶️️BranchStOne
      OneAfford
      OneAccept
      OneDate
      OneSuccess
      Goto 0
,--------------------AddNum-----------------
---------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0
  Title                    (0,3)                         (1,4)                         (2,5)
  [Branch] Seller
    ▶️️BranchStNotFound
    NoBook                 (3,6)                         (4,7)                         (5,8)
    SellerNoBook           (6,9)                         (7,10)                        (8,11)
    Goto 0
    ▶️️BranchStFound
    Price                  (3,12)                        (4,13)                        (5,14)
    [Branch] Buyer
      ▶️️BranchStTwo
      PriceToBuyer2       (12,15)                       (13,16)                       (14,17)
      [Branch] Buyer2
        ▶️️BranchStNotSupport
        NotSupport        (15,18)                       (16,19)                       (17,20)
        TwoNotBuy         (18,21)                       (19,22)                       (20,23)
        Goto 0
        ▶️️BranchStSupport
        SupportVal        (15,24)                       (16,25)                       (17,26)
        [Branch] Buyer
          ▶️️BranchStEnough
          TwoAccept       (24,27)                       (25,28)                       (26,29)
          TwoDate         (27,30)                       (28,31)                       (29,32)
          TwoSuccess      (30,33)                       (31,34)                       (32,35)
          Goto 0
          ▶️️BranchStNotEnough
          TwoNotBuy1      (24,36)                       (25,37)                       (26,38)
          TwoFailed       (36,39)                       (37,40)                       (38,41)
          Terminal
      ▶️️BranchStOne
      OneAfford           (12,42)                       (13,43)                       (14,44)
      OneAccept           (42,45)                       (43,46)                       (44,47)
      OneDate             (45,48)                       (46,49)                       (47,50)
      OneSuccess          (48,51)                       (49,52)                       (50,53)
      Goto 0
,--------------------GenConst-----------------
---------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                      0                             1                             2
  Title                   (0,3)->                       ->(1,4)                           
  [Branch] Seller            3                             4                             5
    ▶️️BranchStNotFound
    NoBook                (3,6)<-                       <-(4,7)                           
    SellerNoBook          (6,9)->                                                     ->(8,11)
    Goto 0
    ▶️️BranchStFound
    Price                 (3,12)<-                      <-(4,13)                          
    [Branch] Buyer           12                            13                            14
      ▶️️BranchStTwo
      PriceToBuyer2      (12,15)->                                                   ->(14,17)
      [Branch] Buyer2        15                            16                            17
        ▶️️BranchStNotSupport
        NotSupport       (15,18)<-                                                   <-(17,20)
        TwoNotBuy        (18,21)->                     ->(19,22)                          
        Goto 0
        ▶️️BranchStSupport
        SupportVal       (15,24)<-                                                   <-(17,26)
        [Branch] Buyer       24                            25                            26
          ▶️️BranchStEnough
          TwoAccept      (24,27)->                     ->(25,28)                          
          TwoDate        (27,30)<-                     <-(28,31)                          
          TwoSuccess     (30,33)->                                                   ->(32,35)
          Goto 0
          ▶️️BranchStNotEnough
          TwoNotBuy1     (24,36)->                     ->(25,37)                          
          TwoFailed      (36,39)->                                                   ->(38,41)
          Terminal
      ▶️️BranchStOne
      OneAfford          (12,42)->                                                   ->(14,44)
      OneAccept          (42,45)->                     ->(43,46)                          
      OneDate            (45,48)<-                     <-(46,49)                          
      OneSuccess         (48,51)->                                                   ->(50,53)
      Goto 0
,--------------------Constrains-----------------
fromList [Constraint 0 1,Constraint 2 5,Constraint 4 3,Constraint 5 8,Constraint 6 8,Constraint 7 10,Constraint 9 0,Constraint 10 1,Constraint 11 2,Constraint 4 3,Constraint 5 14,Constraint 12 14,Constraint 13 16,Constraint 17 15,Constraint 16 19,Constraint 18 19,Constraint 20 23,Constraint 21 0,Constraint 22 1,Constraint 23 2,Constraint 17 15,Constraint 16 25,Constraint 24 25,Constraint 26 29,Constraint 28 27,Constraint 29 32,Constraint 30 32,Constraint 31 34,Constraint 33 0,Constraint 34 1,Constraint 35 2,Constraint 24 25,Constraint 26 38,Constraint 36 38,Constraint 37 40,Constraint 39 (-1),Constraint 40 (-1),Constraint 41 (-1),Constraint 12 14,Constraint 13 43,Constraint 42 43,Constraint 44 47,Constraint 46 45,Constraint 47 50,Constraint 48 50,Constraint 49 52,Constraint 51 0,Constraint 52 1,Constraint 53 2]
,--------------------SubMap-----------------
fromList [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,0),(8,1),(9,0),(10,0),(11,1),(12,1),(13,3),(14,1),(15,4),(16,3),(17,4),(18,3),(19,3),(20,1),(21,0),(22,0),(23,1),(24,3),(25,3),(26,5),(27,6),(28,6),(29,5),(30,5),(31,0),(32,5),(33,0),(34,0),(35,1),(36,5),(37,-1),(38,5),(39,-1),(40,-1),(41,-1),(42,3),(43,3),(44,7),(45,8),(46,8),(47,7),(48,7),(49,0),(50,7),(51,0),(52,0),(53,1)]
,--------------------GenConstN-----------------
---------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                      0                             0                             1
  Title                   (0,2)->                       ->(0,2)                           
  [Branch] Seller            2                             2                             1
    ▶️️BranchStNotFound
    NoBook                (2,1)<-                       <-(2,0)                           
    SellerNoBook          (1,0)->                                                     ->(1,1)
    Goto 0
    ▶️️BranchStFound
    Price                 (2,1)<-                       <-(2,3)                           
    [Branch] Buyer           1                             3                             1
      ▶️️BranchStTwo
      PriceToBuyer2       (1,4)->                                                     ->(1,4)
      [Branch] Buyer2        4                             3                             4
        ▶️️BranchStNotSupport
        NotSupport        (4,3)<-                                                     <-(4,1)
        TwoNotBuy         (3,0)->                       ->(3,0)                           
        Goto 0
        ▶️️BranchStSupport
        SupportVal        (4,3)<-                                                     <-(4,5)
        [Branch] Buyer       3                             3                             5
          ▶️️BranchStEnough
          TwoAccept       (3,6)->                       ->(3,6)                           
          TwoDate         (6,5)<-                       <-(6,0)                           
          TwoSuccess      (5,0)->                                                     ->(5,1)
          Goto 0
          ▶️️BranchStNotEnough
          TwoNotBuy1      (3,5)->                       ->(3,-1)                          
          TwoFailed       (5,-1)->                                                    ->(5,-1)
          Terminal
      ▶️️BranchStOne
      OneAfford           (1,3)->                                                     ->(1,7)
      OneAccept           (3,8)->                       ->(3,8)                           
      OneDate             (8,7)<-                       <-(8,0)                           
      OneSuccess          (7,0)->                                                     ->(7,1)
      Goto 0
],Right Label ([0,0,1],0) 0
Msg <(([0,0,1],[2,2,1]),(Buyer,Seller))> Title [] Buyer Seller
[Branch] [2,2,1] Seller
  * BranchSt () NotFound
  Msg <(([2,2,1],[1,0,1]),(Seller,Buyer))> NoBook [] Seller Buyer
  Msg <(([1,0,1],[0,0,1]),(Buyer,Buyer2))> SellerNoBook [] Buyer Buyer2
  Goto ([0,0,1],0) 0
  * BranchSt () Found
  Msg <(([2,2,1],[1,3,1]),(Seller,Buyer))> Price [] Seller Buyer
  [Branch] [1,3,1] Buyer
    * BranchSt () Two
    Msg <(([1,3,1],[4,3,4]),(Buyer,Buyer2))> PriceToBuyer2 [] Buyer Buyer2
    [Branch] [4,3,4] Buyer2
      * BranchSt () NotSupport
      Msg <(([4,3,4],[3,3,1]),(Buyer2,Buyer))> NotSupport [] Buyer2 Buyer
      Msg <(([3,3,1],[0,0,1]),(Buyer,Seller))> TwoNotBuy [] Buyer Seller
      Goto ([0,0,1],0) 0
      * BranchSt () Support
      Msg <(([4,3,4],[3,3,5]),(Buyer2,Buyer))> SupportVal [] Buyer2 Buyer
      [Branch] [3,3,5] Buyer
        * BranchSt () Enough
        Msg <(([3,3,5],[6,6,5]),(Buyer,Seller))> TwoAccept [] Buyer Seller
        Msg <(([6,6,5],[5,0,5]),(Seller,Buyer))> TwoDate [] Seller Buyer
        Msg <(([5,0,5],[0,0,1]),(Buyer,Buyer2))> TwoSuccess [] Buyer Buyer2
        Goto ([0,0,1],0) 0
        * BranchSt () NotEnough
        Msg <(([3,3,5],[5,-1,5]),(Buyer,Seller))> TwoNotBuy1 [] Buyer Seller
        Msg <(([5,-1,5],[-1,-1,-1]),(Buyer,Buyer2))> TwoFailed [] Buyer Buyer2
        Terminal [-1,-1,-1]
    * BranchSt () One
    Msg <(([1,3,1],[3,3,7]),(Buyer,Buyer2))> OneAfford [] Buyer Buyer2
    Msg <(([3,3,7],[8,8,7]),(Buyer,Seller))> OneAccept [] Buyer Seller
    Msg <(([8,8,7],[7,0,7]),(Seller,Buyer))> OneDate [] Seller Buyer
    Msg <(([7,0,7],[0,0,1]),(Buyer,Buyer2))> OneSuccess [] Buyer Buyer2
    Goto ([0,0,1],0) 0)

-}
