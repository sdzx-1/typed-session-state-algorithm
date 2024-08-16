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
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0
  [Branch] Client
    * BranchSt True
    Ping
    Pong
    Add
    ^ Goto 0
    * BranchSt False
    Stop
    AStop
    ~ Terminal
,--------------------AddNum-----------------
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0
  [Branch] Client
    * BranchSt True
    Ping                                       (0,3)                         (1,4)                         (2,5)
    Pong                                       (3,6)                         (4,7)                         (5,8)
    Add                                        (6,9)                         (7,10)                        (8,11)
    ^ Goto 0
    * BranchSt False
    Stop                                       (0,12)                        (1,13)                        (2,14)
    AStop                                     (12,15)                       (13,16)                       (14,17)
    ~ Terminal
,--------------------GenConst-----------------
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                          0                             1                             2
  [Branch] Client                                0                             1                             2
    * BranchSt True
    Ping                                      (0,3)->                       ->(1,4)                           
    Pong                                      (3,6)<-                       <-(4,7)                           
    Add                                       (6,9)->                                                     ->(8,11)
    ^ Goto 0
    * BranchSt False
    Stop                                      (0,12)->                      ->(1,13)                          
    AStop                                    (12,15)->                                                   ->(14,17)
    ~ Terminal
,--------------------Constrains-----------------
fromList [Constraint 0 1,Constraint 2 5,Constraint 4 3,Constraint 5 8,Constraint 6 8,Constraint 7 10,Constraint 9 0,Constraint 10 1,Constraint 11 2,Constraint 0 1,Constraint 2 14,Constraint 12 14,Constraint 13 16,Constraint 15 (-1),Constraint 16 (-1),Constraint 17 (-1)]
,--------------------SubMap-----------------
fromList [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,0),(8,1),(9,0),(10,0),(11,1),(12,1),(13,-1),(14,1),(15,-1),(16,-1),(17,-1)]
,--------------------GenConstN-----------------
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                          0                             0                             1
  [Branch] Client                                0                             0                             1
    * BranchSt True
    Ping                                      (0,2)->                       ->(0,2)                           
    Pong                                      (2,1)<-                       <-(2,0)                           
    Add                                       (1,0)->                                                     ->(1,1)
    ^ Goto 0
    * BranchSt False
    Stop                                      (0,1)->                       ->(0,-1)                          
    AStop                                     (1,-1)->                                                    ->(1,-1)
    ~ Terminal
,--------------------CollectBranchDynVal-----------------
fromList [0,1]
,--------------------MsgT-----------------
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                         S0 s                          S0 s                          S1 s
  [Branch] Client                               S0 s                          S0 s                          S1 s
    * BranchSt True
    Ping                                    S0 [True ..]                      S0 s                          S1 s
    Pong                                         S2                            S2                           S1 s
    Add                                     S1 [True ..]                      S0 s                          S1 s
    ^ Goto 0                                    S0 s                          S0 s                          S1 s
    * BranchSt False
    Stop                                   S0 [False ..]                      S0 s                          S1 s
    AStop                                  S1 [False ..]                      End                           S1 s
    ~ Terminal                                  End                           End                           End
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

{-
>>> error $ show (pipleWithTracer v2)
(fromList [--------------------Creat-----------------
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0
  Title
  [Branch] Seller
    * BranchSt NotFound
    NoBook
    SellerNoBook
    ^ Goto 0
    * BranchSt Found
    Price
    [Branch] Buyer
      * BranchSt One
      OneAfford
      OneAccept
      OneDate
      OneSuccess
      ^ Goto 0
      * BranchSt Two
      PriceToBuyer2
      [Branch] Buyer2
        * BranchSt NotSupport
        NotSupport
        TwoNotBuy
        ^ Goto 0
        * BranchSt Support
        SupportVal
        [Branch] Buyer
          * BranchSt Enough
          TwoAccept
          TwoDate
          TwoSuccess
          ^ Goto 0
          * BranchSt NotEnough
          TwoNotBuy1
          TwoFailed
          ~ Terminal
,--------------------AddNum-----------------
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0
  Title                                        (0,3)                         (1,4)                         (2,5)
  [Branch] Seller
    * BranchSt NotFound
    NoBook                                     (3,6)                         (4,7)                         (5,8)
    SellerNoBook                               (6,9)                         (7,10)                        (8,11)
    ^ Goto 0
    * BranchSt Found
    Price                                      (3,12)                        (4,13)                        (5,14)
    [Branch] Buyer
      * BranchSt One
      OneAfford                               (12,15)                       (13,16)                       (14,17)
      OneAccept                               (15,18)                       (16,19)                       (17,20)
      OneDate                                 (18,21)                       (19,22)                       (20,23)
      OneSuccess                              (21,24)                       (22,25)                       (23,26)
      ^ Goto 0
      * BranchSt Two
      PriceToBuyer2                           (12,27)                       (13,28)                       (14,29)
      [Branch] Buyer2
        * BranchSt NotSupport
        NotSupport                            (27,30)                       (28,31)                       (29,32)
        TwoNotBuy                             (30,33)                       (31,34)                       (32,35)
        ^ Goto 0
        * BranchSt Support
        SupportVal                            (27,36)                       (28,37)                       (29,38)
        [Branch] Buyer
          * BranchSt Enough
          TwoAccept                           (36,39)                       (37,40)                       (38,41)
          TwoDate                             (39,42)                       (40,43)                       (41,44)
          TwoSuccess                          (42,45)                       (43,46)                       (44,47)
          ^ Goto 0
          * BranchSt NotEnough
          TwoNotBuy1                          (36,48)                       (37,49)                       (38,50)
          TwoFailed                           (48,51)                       (49,52)                       (50,53)
          ~ Terminal
,--------------------GenConst-----------------
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                                          0                             1                             2
  Title                                       (0,3)->                       ->(1,4)                           
  [Branch] Seller                                3                             4                             5
    * BranchSt NotFound
    NoBook                                    (3,6)<-                       <-(4,7)                           
    SellerNoBook                              (6,9)->                                                     ->(8,11)
    ^ Goto 0
    * BranchSt Found
    Price                                     (3,12)<-                      <-(4,13)                          
    [Branch] Buyer                               12                            13                            14
      * BranchSt One
      OneAfford                              (12,15)->                                                   ->(14,17)
      OneAccept                              (15,18)->                     ->(16,19)                          
      OneDate                                (18,21)<-                     <-(19,22)                          
      OneSuccess                             (21,24)->                                                   ->(23,26)
      ^ Goto 0
      * BranchSt Two
      PriceToBuyer2                          (12,27)->                                                   ->(14,29)
      [Branch] Buyer2                            27                            28                            29
        * BranchSt NotSupport
        NotSupport                           (27,30)<-                                                   <-(29,32)
        TwoNotBuy                            (30,33)->                     ->(31,34)                          
        ^ Goto 0
        * BranchSt Support
        SupportVal                           (27,36)<-                                                   <-(29,38)
        [Branch] Buyer                           36                            37                            38
          * BranchSt Enough
          TwoAccept                          (36,39)->                     ->(37,40)                          
          TwoDate                            (39,42)<-                     <-(40,43)                          
          TwoSuccess                         (42,45)->                                                   ->(44,47)
          ^ Goto 0
          * BranchSt NotEnough
          TwoNotBuy1                         (36,48)->                     ->(37,49)                          
          TwoFailed                          (48,51)->                                                   ->(50,53)
          ~ Terminal
,--------------------Constrains-----------------
fromList [Constraint 0 1,Constraint 2 5,Constraint 4 3,Constraint 5 8,Constraint 6 8,Constraint 7 10,Constraint 9 0,Constraint 10 1,Constraint 11 2,Constraint 4 3,Constraint 5 14,Constraint 12 14,Constraint 13 16,Constraint 15 16,Constraint 17 20,Constraint 19 18,Constraint 20 23,Constraint 21 23,Constraint 22 25,Constraint 24 0,Constraint 25 1,Constraint 26 2,Constraint 12 14,Constraint 13 28,Constraint 29 27,Constraint 28 31,Constraint 30 31,Constraint 32 35,Constraint 33 0,Constraint 34 1,Constraint 35 2,Constraint 29 27,Constraint 28 37,Constraint 36 37,Constraint 38 41,Constraint 40 39,Constraint 41 44,Constraint 42 44,Constraint 43 46,Constraint 45 0,Constraint 46 1,Constraint 47 2,Constraint 36 37,Constraint 38 50,Constraint 48 50,Constraint 49 52,Constraint 51 (-1),Constraint 52 (-1),Constraint 53 (-1)]
,--------------------SubMap-----------------
fromList [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,0),(8,1),(9,0),(10,0),(11,1),(12,1),(13,3),(14,1),(15,3),(16,3),(17,4),(18,5),(19,5),(20,4),(21,4),(22,0),(23,4),(24,0),(25,0),(26,1),(27,6),(28,3),(29,6),(30,3),(31,3),(32,1),(33,0),(34,0),(35,1),(36,3),(37,3),(38,7),(39,8),(40,8),(41,7),(42,7),(43,0),(44,7),(45,0),(46,0),(47,1),(48,7),(49,-1),(50,7),(51,-1),(52,-1),(53,-1)]
,--------------------GenConstN-----------------
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                                          0                             0                             1
  Title                                       (0,2)->                       ->(0,2)                           
  [Branch] Seller                                2                             2                             1
    * BranchSt NotFound
    NoBook                                    (2,1)<-                       <-(2,0)                           
    SellerNoBook                              (1,0)->                                                     ->(1,1)
    ^ Goto 0
    * BranchSt Found
    Price                                     (2,1)<-                       <-(2,3)                           
    [Branch] Buyer                               1                             3                             1
      * BranchSt One
      OneAfford                               (1,3)->                                                     ->(1,4)
      OneAccept                               (3,5)->                       ->(3,5)                           
      OneDate                                 (5,4)<-                       <-(5,0)                           
      OneSuccess                              (4,0)->                                                     ->(4,1)
      ^ Goto 0
      * BranchSt Two
      PriceToBuyer2                           (1,6)->                                                     ->(1,6)
      [Branch] Buyer2                            6                             3                             6
        * BranchSt NotSupport
        NotSupport                            (6,3)<-                                                     <-(6,1)
        TwoNotBuy                             (3,0)->                       ->(3,0)                           
        ^ Goto 0
        * BranchSt Support
        SupportVal                            (6,3)<-                                                     <-(6,7)
        [Branch] Buyer                           3                             3                             7
          * BranchSt Enough
          TwoAccept                           (3,8)->                       ->(3,8)                           
          TwoDate                             (8,7)<-                       <-(8,0)                           
          TwoSuccess                          (7,0)->                                                     ->(7,1)
          ^ Goto 0
          * BranchSt NotEnough
          TwoNotBuy1                          (3,7)->                       ->(3,-1)                          
          TwoFailed                           (7,-1)->                                                    ->(7,-1)
          ~ Terminal
,--------------------CollectBranchDynVal-----------------
fromList [1,2,3,6,7]
,--------------------MsgT-----------------
-----------------------------------------------Buyer-------------------------Seller------------------------Buyer2
LABEL 0                                          S0                            S0                           S1 s
  Title                                          S0                            S0                           S1 s
  [Branch] Seller                               S2 s                          S2 s                          S1 s
    * BranchSt NotFound
    NoBook                                      S2 s                    S2 [NotFound ..]                    S1 s
    SellerNoBook                          S1 [NotFound ..]                     S0                           S1 s
    ^ Goto 0                                     S0                            S0                           S1 s
    * BranchSt Found
    Price                                       S2 s                     S2 [Found ..]                      S1 s
    [Branch] Buyer                              S1 s                          S3 s                          S1 s
      * BranchSt One
      OneAfford                             S1 [One ..]                       S3 s                          S1 s
      OneAccept                             S3 [One ..]                       S3 s                           S4
      OneDate                                    S5                            S5                            S4
      OneSuccess                                 S4                            S0                            S4
      ^ Goto 0                                   S0                            S0                           S1 s
      * BranchSt Two
      PriceToBuyer2                         S1 [Two ..]                       S3 s                          S1 s
      [Branch] Buyer2                           S6 s                          S3 s                          S6 s
        * BranchSt NotSupport
        NotSupport                              S6 s                          S3 s                   S6 [NotSupport ..]
        TwoNotBuy                        S3 [NotSupport ..]                   S3 s                          S1 s
        ^ Goto 0                                 S0                            S0                           S1 s
        * BranchSt Support
        SupportVal                              S6 s                          S3 s                    S6 [Support ..]
        [Branch] Buyer                          S3 s                          S3 s                          S7 s
          * BranchSt Enough
          TwoAccept                        S3 [Enough ..]                     S3 s                          S7 s
          TwoDate                                S8                            S8                           S7 s
          TwoSuccess                       S7 [Enough ..]                      S0                           S7 s
          ^ Goto 0                               S0                            S0                           S1 s
          * BranchSt NotEnough
          TwoNotBuy1                     S3 [NotEnough ..]                    S3 s                          S7 s
          TwoFailed                      S7 [NotEnough ..]                    End                           S7 s
          ~ Terminal                            End                           End                           End
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
    * BranchSt () One
    Msg <(([1,3,1],[3,3,4]),(Buyer,Buyer2))> OneAfford [] Buyer Buyer2
    Msg <(([3,3,4],[5,5,4]),(Buyer,Seller))> OneAccept [] Buyer Seller
    Msg <(([5,5,4],[4,0,4]),(Seller,Buyer))> OneDate [] Seller Buyer
    Msg <(([4,0,4],[0,0,1]),(Buyer,Buyer2))> OneSuccess [] Buyer Buyer2
    Goto ([0,0,1],0) 0
    * BranchSt () Two
    Msg <(([1,3,1],[6,3,6]),(Buyer,Buyer2))> PriceToBuyer2 [] Buyer Buyer2
    [Branch] [6,3,6] Buyer2
      * BranchSt () NotSupport
      Msg <(([6,3,6],[3,3,1]),(Buyer2,Buyer))> NotSupport [] Buyer2 Buyer
      Msg <(([3,3,1],[0,0,1]),(Buyer,Seller))> TwoNotBuy [] Buyer Seller
      Goto ([0,0,1],0) 0
      * BranchSt () Support
      Msg <(([6,3,6],[3,3,7]),(Buyer2,Buyer))> SupportVal [] Buyer2 Buyer
      [Branch] [3,3,7] Buyer
        * BranchSt () Enough
        Msg <(([3,3,7],[8,8,7]),(Buyer,Seller))> TwoAccept [] Buyer Seller
        Msg <(([8,8,7],[7,0,7]),(Seller,Buyer))> TwoDate [] Seller Buyer
        Msg <(([7,0,7],[0,0,1]),(Buyer,Buyer2))> TwoSuccess [] Buyer Buyer2
        Goto ([0,0,1],0) 0
        * BranchSt () NotEnough
        Msg <(([3,3,7],[7,-1,7]),(Buyer,Seller))> TwoNotBuy1 [] Buyer Seller
        Msg <(([7,-1,7],[-1,-1,-1]),(Buyer,Buyer2))> TwoFailed [] Buyer Buyer2
        Terminal [-1,-1,-1])

-}
