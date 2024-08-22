{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Text.RawString.QQ (r)
import TypedSession.State.GenDoc (genGraph)
import TypedSession.State.Parser (runProtocolParser)
import TypedSession.State.Pattern
import TypedSession.State.Piple (PipleResult (..), piple, pipleWithTracer)
import TypedSession.State.Render (StrFillEnv (StrFillEnv), defaultStrFilEnv)
import TypedSession.State.Type (Creat, Protocol)

main :: IO ()
main = putStrLn "Test suite not yet implemented."

data PingPongRole = Client | Server | Counter
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data PingPongBranchSt = STrue | SFalse
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

s1 =
  [r|
  
  Label 0
    Branch Client {
      BranchSt STrue
          Msg "AddOne" [] Client Counter
          Msg "Ping" ["Int", "Int", "Int"] Client Server
          Msg "Pong" [] Server Client
          Goto 0
      BranchSt SFalse
          Msg "Stop" [] Client Server
          Msg "CStop" [] Client Counter
          Terminal
    }
|]

r1 = case runProtocolParser @PingPongRole @PingPongBranchSt s1 of
  Left e -> e
  Right a ->
    let (lq, res) = pipleWithTracer a
     in case res of
          Left e -> show e
          Right ppResult -> show lq <> "\n" <> genGraph (StrFillEnv 20 20) ppResult

-- >>> error r1
-- fromList [--------------------Creat-----------------
-- Label () 0
-- [Branch] () Client
--   * BranchSt () STrue
--   Msg <()> AddOne [] Client Counter
--   Msg <()> Ping [Int, Int, Int] Client Server
--   Msg <()> Pong [] Server Client
--   Goto () 0
--   * BranchSt () SFalse
--   Msg <()> Stop [] Client Server
--   Msg <()> CStop [] Client Counter
--   Terminal ()
-- ,--------------------Idx-----------------
-- Label 0 0
-- [Branch] 0 Client
--   * BranchSt () STrue
--   Msg <(1,2)> AddOne [] Client Counter
--   Msg <(2,3)> Ping [Int, Int, Int] Client Server
--   Msg <(3,4)> Pong [] Server Client
--   Goto 4 0
--   * BranchSt () SFalse
--   Msg <(5,6)> Stop [] Client Server
--   Msg <(6,7)> CStop [] Client Counter
--   Terminal 7
-- ,--------------------ReRank-----------------
-- fromList [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
-- ,--------------------Idx-----------------
-- Label 0 0
-- [Branch] 0 Client
--   * BranchSt () STrue
--   Msg <(1,2)> AddOne [] Client Counter
--   Msg <(2,3)> Ping [Int, Int, Int] Client Server
--   Msg <(3,4)> Pong [] Server Client
--   Goto 4 0
--   * BranchSt () SFalse
--   Msg <(5,6)> Stop [] Client Server
--   Msg <(6,7)> CStop [] Client Counter
--   Terminal 7
-- ,--------------------AddNum-----------------
-- Label [0,1,2] 0
-- [Branch] [0,1,2] Client
--   * BranchSt () STrue
--   Msg <([3,4,5],[6,7,8],0)> AddOne [] Client Counter
--   Msg <([6,7,8],[9,10,11],1)> Ping [Int, Int, Int] Client Server
--   Msg <([9,10,11],[12,13,14],2)> Pong [] Server Client
--   Goto [12,13,14] 0
--   * BranchSt () SFalse
--   Msg <([15,16,17],[18,19,20],0)> Stop [] Client Server
--   Msg <([18,19,20],[21,22,23],1)> CStop [] Client Counter
--   Terminal [21,22,23]
-- ,--------------------GenConst-----------------
-- Label ([0,1,2],0) 0
-- [Branch] [0,1,2] Client
--   * BranchSt () STrue
--   Msg <(([3,4,5],[6,7,8]),(Client,Counter),0)> AddOne [] Client Counter
--   Msg <(([6,7,8],[9,10,11]),(Client,Server),1)> Ping [ Int
--                                                      , Int
--                                                      , Int ] Client Server
--   Msg <(([9,10,11],[12,13,14]),(Server,Client),2)> Pong [] Server Client
--   Goto ([12,13,14],0) 0
--   * BranchSt () SFalse
--   Msg <(([15,16,17],[18,19,20]),(Client,Server),0)> Stop [] Client Server
--   Msg <(([18,19,20],[21,22,23]),(Client,Counter),1)> CStop [] Client Counter
--   Terminal [21,22,23]
-- ,--------------------Constrains-----------------
-- fromList [Constraint 1 4,Constraint 2 5,Constraint 3 5,Constraint 4 7,Constraint 6 7,Constraint 8 11,Constraint 10 9,Constraint 11 14,Constraint 12 0,Constraint 13 1,Constraint 14 2,Constraint 1 16,Constraint 2 17,Constraint 15 16,Constraint 17 20,Constraint 18 20,Constraint 19 22,Constraint 21 (-1),Constraint 22 (-1),Constraint 23 (-1)]
-- ,--------------------SubMap-----------------
-- fromList [(3,2),(4,1),(5,2),(6,1),(7,1),(8,2),(9,3),(10,3),(11,2),(12,0),(13,1),(14,2),(15,1),(16,1),(17,2),(18,2),(19,-1),(20,2),(21,-1),(22,-1),(23,-1)]
-- ,--------------------GenConstN-----------------
-- Label ([0,1,2],0) 0
-- [Branch] [0,1,2] Client
--   * BranchSt () STrue
--   Msg <(([2,1,2],[1,1,2]),(Client,Counter),0)> AddOne [] Client Counter
--   Msg <(([1,1,2],[3,3,2]),(Client,Server),1)> Ping [Int, Int, Int] Client Server
--   Msg <(([3,3,2],[0,1,2]),(Server,Client),2)> Pong [] Server Client
--   Goto ([0,1,2],0) 0
--   * BranchSt () SFalse
--   Msg <(([1,1,2],[2,-1,2]),(Client,Server),0)> Stop [] Client Server
--   Msg <(([2,-1,2],[-1,-1,-1]),(Client,Counter),1)> CStop [] Client Counter
--   Terminal [-1,-1,-1]
-- ,--------------------CollectBranchDynVal-----------------
-- fromList [1,2]
-- ,--------------------MsgT-----------------
-- Label ([S0,S1 s,S2 s],0) 0
-- [Branch] [S0,S1 s,S2 s] Client
--   * BranchSt () STrue
--   Msg <([S2 STrue,S1 s,S2 s],(Client,Counter),0)> AddOne [] Client Counter
--   Msg <([S1 STrue,S1 s,S2 s],(Client,Server),1)> Ping [ Int
--                                                       , Int
--                                                       , Int ] Client Server
--   Msg <([S3,S3,S2 s],(Server,Client),2)> Pong [] Server Client
--   Goto ([S0,S1 s,S2 s],0) 0
--   * BranchSt () SFalse
--   Msg <([S1 SFalse,S1 s,S2 s],(Client,Server),0)> Stop [] Client Server
--   Msg <([S2 SFalse,End,S2 s],(Client,Counter),1)> CStop [] Client Counter
--   Terminal [End,End,End]
-- ,--------------------MsgT1-----------------
-- Label ([S0,S1 s,S2 s],0) 0
-- [Branch] [S0,S1 s,S2 s] Client
--   * BranchSt () STrue
--   Msg <((S2 STrue,S1 STrue,S2 s),(Client,Counter),0)> AddOne [] Client Counter
--   Msg <((S1 STrue,S3,S3),(Client,Server),1)> Ping [Int, Int, Int] Client Server
--   Msg <((S3,S1 s,S0),(Server,Client),2)> Pong [] Server Client
--   Goto ([S0,S1 s,S2 s],0) 0
--   * BranchSt () SFalse
--   Msg <((S1 SFalse,S2 SFalse,End),(Client,Server),0)> Stop [] Client Server
--   Msg <((S2 SFalse,End,End),(Client,Counter),1)> CStop [] Client Counter
--   Terminal [End,End,End]
-- ]
-- -------------------------------------Client--------------Server-------------Counter
-- LABEL 0                                S0                 S1 s                S2 s
--   [Branch Client]                      S0                 S1 s                S2 s
--     * BranchSt STrue
--     AddOne                        {S2 STrue}->            S1 s               ->S2 s
--     Ping                           S1 STrue->            ->S1 s               S2 s
--     Pong                              S3<-                <-S3                S2 s
--     Goto 0                             S0                 S1 s                S2 s
--     * BranchSt SFalse
--     Stop                         {S1 SFalse}->           ->S1 s               S2 s
--     CStop                         S2 SFalse->             End                ->S2 s
--     ~ Terminal                        End                 End                 End

data Role
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
  deriving (Show, Eq, Ord, Enum, Bounded)

s2 =
  [r|
  Label 0
    Msg "Title" ["String"] Buyer Seller
    Branch Seller {
      BranchSt Found 
        Msg "Price" ["Int"] Seller Buyer
        Branch Buyer {
          BranchSt Two 
            Msg "PriceToBuyer2" ["Int"] Buyer Buyer2
            Branch Buyer2 {
              BranchSt NotSupport 
                Msg "NotSupport1" [] Buyer2 Buyer
                Msg "TwoNotBuy" [] Buyer Seller
                Goto 0
              BranchSt Support 
                Msg "SupportVal" ["Int"] Buyer2 Buyer
                Branch Buyer {
                  BranchSt Enough 
                    Msg "TwoAccept" [] Buyer Seller
                    Msg "TwoDate" ["Int"] Seller Buyer
                    Msg "TwoSuccess" ["Int"] Buyer Buyer2
                    Goto 0
                  BranchSt NotEnough 
                    Msg "TwoNotBuy1" [] Buyer Seller
                    Msg "TwoFailed" [] Buyer Buyer2
                    Terminal
                  }
              }
          BranchSt One 
            Msg "OneAccept" [] Buyer Seller
            Msg "OneDate" ["Int"] Seller Buyer
            Msg "OneSuccess" ["Int"] Buyer Buyer2
            Goto 0
          }
      BranchSt NotFound 
        Msg "NoBook" [] Seller Buyer
             Msg "SellerNoBook" [] Buyer Buyer2
             Goto 0
      }
|]

r2 = case runProtocolParser @Role @BookBranchSt s2 of
  Left e -> e
  Right a ->
    let (seqList, res) = pipleWithTracer a
     in case res of
          Left e -> show e
          Right ppResult ->
            let st = show seqList
             in st <> "\n" <> genGraph (StrFillEnv 20 20) ppResult

-- >>> error r2
-- fromList [--------------------Creat-----------------
-- Label () 0
-- Msg <()> Title [String] Buyer Seller
-- [Branch] () Seller
--   * BranchSt () Found
--   Msg <()> Price [Int] Seller Buyer
--   [Branch] () Buyer
--     * BranchSt () Two
--     Msg <()> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] () Buyer2
--       * BranchSt () NotSupport
--       Msg <()> NotSupport1 [] Buyer2 Buyer
--       Msg <()> TwoNotBuy [] Buyer Seller
--       Goto () 0
--       * BranchSt () Support
--       Msg <()> SupportVal [Int] Buyer2 Buyer
--       [Branch] () Buyer
--         * BranchSt () Enough
--         Msg <()> TwoAccept [] Buyer Seller
--         Msg <()> TwoDate [Int] Seller Buyer
--         Msg <()> TwoSuccess [Int] Buyer Buyer2
--         Goto () 0
--         * BranchSt () NotEnough
--         Msg <()> TwoNotBuy1 [] Buyer Seller
--         Msg <()> TwoFailed [] Buyer Buyer2
--         Terminal ()
--     * BranchSt () One
--     Msg <()> OneAccept [] Buyer Seller
--     Msg <()> OneDate [Int] Seller Buyer
--     Msg <()> OneSuccess [Int] Buyer Buyer2
--     Goto () 0
--   * BranchSt () NotFound
--   Msg <()> NoBook [] Seller Buyer
--   Msg <()> SellerNoBook [] Buyer Buyer2
--   Goto () 0
-- ,--------------------Idx-----------------
-- Label 0 0
-- Msg <(0,1)> Title [String] Buyer Seller
-- [Branch] 1 Seller
--   * BranchSt () Found
--   Msg <(2,3)> Price [Int] Seller Buyer
--   [Branch] 3 Buyer
--     * BranchSt () Two
--     Msg <(4,5)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] 5 Buyer2
--       * BranchSt () NotSupport
--       Msg <(6,7)> NotSupport1 [] Buyer2 Buyer
--       Msg <(7,8)> TwoNotBuy [] Buyer Seller
--       Goto 8 0
--       * BranchSt () Support
--       Msg <(9,10)> SupportVal [Int] Buyer2 Buyer
--       [Branch] 10 Buyer
--         * BranchSt () Enough
--         Msg <(11,12)> TwoAccept [] Buyer Seller
--         Msg <(12,13)> TwoDate [Int] Seller Buyer
--         Msg <(13,14)> TwoSuccess [Int] Buyer Buyer2
--         Goto 14 0
--         * BranchSt () NotEnough
--         Msg <(15,16)> TwoNotBuy1 [] Buyer Seller
--         Msg <(16,17)> TwoFailed [] Buyer Buyer2
--         Terminal 17
--     * BranchSt () One
--     Msg <(18,19)> OneAccept [] Buyer Seller
--     Msg <(19,20)> OneDate [Int] Seller Buyer
--     Msg <(20,21)> OneSuccess [Int] Buyer Buyer2
--     Goto 21 0
--   * BranchSt () NotFound
--   Msg <(22,23)> NoBook [] Seller Buyer
--   Msg <(23,24)> SellerNoBook [] Buyer Buyer2
--   Goto 24 0
-- ,--------------------ReRank-----------------
-- fromList [(0,0),(1,1),(2,5),(3,2),(4,6),(5,3),(6,7),(7,8),(8,9),(9,10),(10,4),(11,11),(12,12),(13,13),(14,14),(15,15),(16,16),(17,17),(18,18),(19,19),(20,20),(21,21),(22,22),(23,23),(24,24)]
-- ,--------------------Idx-----------------
-- Label 0 0
-- Msg <(0,1)> Title [String] Buyer Seller
-- [Branch] 1 Seller
--   * BranchSt () Found
--   Msg <(5,2)> Price [Int] Seller Buyer
--   [Branch] 2 Buyer
--     * BranchSt () Two
--     Msg <(6,3)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] 3 Buyer2
--       * BranchSt () NotSupport
--       Msg <(7,8)> NotSupport1 [] Buyer2 Buyer
--       Msg <(8,9)> TwoNotBuy [] Buyer Seller
--       Goto 9 0
--       * BranchSt () Support
--       Msg <(10,4)> SupportVal [Int] Buyer2 Buyer
--       [Branch] 4 Buyer
--         * BranchSt () Enough
--         Msg <(11,12)> TwoAccept [] Buyer Seller
--         Msg <(12,13)> TwoDate [Int] Seller Buyer
--         Msg <(13,14)> TwoSuccess [Int] Buyer Buyer2
--         Goto 14 0
--         * BranchSt () NotEnough
--         Msg <(15,16)> TwoNotBuy1 [] Buyer Seller
--         Msg <(16,17)> TwoFailed [] Buyer Buyer2
--         Terminal 17
--     * BranchSt () One
--     Msg <(18,19)> OneAccept [] Buyer Seller
--     Msg <(19,20)> OneDate [Int] Seller Buyer
--     Msg <(20,21)> OneSuccess [Int] Buyer Buyer2
--     Goto 21 0
--   * BranchSt () NotFound
--   Msg <(22,23)> NoBook [] Seller Buyer
--   Msg <(23,24)> SellerNoBook [] Buyer Buyer2
--   Goto 24 0
-- ,--------------------AddNum-----------------
-- Label [0,1,2] 0
-- Msg <([0,1,2],[3,4,5],100)> Title [String] Buyer Seller
-- [Branch] [3,4,5] Seller
--   * BranchSt () Found
--   Msg <([15,16,17],[6,7,8],0)> Price [Int] Seller Buyer
--   [Branch] [6,7,8] Buyer
--     * BranchSt () Two
--     Msg <([18,19,20],[9,10,11],0)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] [9,10,11] Buyer2
--       * BranchSt () NotSupport
--       Msg <([21,22,23],[24,25,26],0)> NotSupport1 [] Buyer2 Buyer
--       Msg <([24,25,26],[27,28,29],1)> TwoNotBuy [] Buyer Seller
--       Goto [27,28,29] 0
--       * BranchSt () Support
--       Msg <([30,31,32],[12,13,14],0)> SupportVal [Int] Buyer2 Buyer
--       [Branch] [12,13,14] Buyer
--         * BranchSt () Enough
--         Msg <([33,34,35],[36,37,38],0)> TwoAccept [] Buyer Seller
--         Msg <([36,37,38],[39,40,41],1)> TwoDate [Int] Seller Buyer
--         Msg <([39,40,41],[42,43,44],2)> TwoSuccess [Int] Buyer Buyer2
--         Goto [42,43,44] 0
--         * BranchSt () NotEnough
--         Msg <([45,46,47],[48,49,50],0)> TwoNotBuy1 [] Buyer Seller
--         Msg <([48,49,50],[51,52,53],1)> TwoFailed [] Buyer Buyer2
--         Terminal [51,52,53]
--     * BranchSt () One
--     Msg <([54,55,56],[57,58,59],0)> OneAccept [] Buyer Seller
--     Msg <([57,58,59],[60,61,62],1)> OneDate [Int] Seller Buyer
--     Msg <([60,61,62],[63,64,65],2)> OneSuccess [Int] Buyer Buyer2
--     Goto [63,64,65] 0
--   * BranchSt () NotFound
--   Msg <([66,67,68],[69,70,71],0)> NoBook [] Seller Buyer
--   Msg <([69,70,71],[72,73,74],1)> SellerNoBook [] Buyer Buyer2
--   Goto [72,73,74] 0
-- ,--------------------GenConst-----------------
-- Label ([0,1,2],0) 0
-- Msg <(([0,1,2],[3,4,5]),(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [3,4,5] Seller
--   * BranchSt () Found
--   Msg <(([15,16,17],[6,7,8]),(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [6,7,8] Buyer
--     * BranchSt () Two
--     Msg <(([18,19,20],[9,10,11]),(Buyer,Buyer2),0)> PriceToBuyer2 [ Int ] Buyer Buyer2
--     [Branch] [9,10,11] Buyer2
--       * BranchSt () NotSupport
--       Msg <(([21,22,23],[24,25,26]),(Buyer2,Buyer),0)> NotSupport1 [  ] Buyer2 Buyer
--       Msg <(([24,25,26],[27,28,29]),(Buyer,Seller),1)> TwoNotBuy [] Buyer Seller
--       Goto ([27,28,29],0) 0
--       * BranchSt () Support
--       Msg <(([30,31,32],[12,13,14]),(Buyer2,Buyer),0)> SupportVal [ Int ] Buyer2 Buyer
--       [Branch] [12,13,14] Buyer
--         * BranchSt () Enough
--         Msg <(([33,34,35],[36,37,38]),(Buyer,Seller),0)> TwoAccept [  ] Buyer Seller
--         Msg <(([36,37,38],[39,40,41]),(Seller,Buyer),1)> TwoDate [ Int ] Seller Buyer
--         Msg <(([39,40,41],[42,43,44]),(Buyer,Buyer2),2)> TwoSuccess [ Int ] Buyer Buyer2
--         Goto ([42,43,44],0) 0
--         * BranchSt () NotEnough
--         Msg <(([45,46,47],[48,49,50]),(Buyer,Seller),0)> TwoNotBuy1 [  ] Buyer Seller
--         Msg <(([48,49,50],[51,52,53]),(Buyer,Buyer2),1)> TwoFailed [  ] Buyer Buyer2
--         Terminal [51,52,53]
--     * BranchSt () One
--     Msg <(([54,55,56],[57,58,59]),(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <(([57,58,59],[60,61,62]),(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <(([60,61,62],[63,64,65]),(Buyer,Buyer2),2)> OneSuccess [ Int ] Buyer Buyer2
--     Goto ([63,64,65],0) 0
--   * BranchSt () NotFound
--   Msg <(([66,67,68],[69,70,71]),(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <(([69,70,71],[72,73,74]),(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([72,73,74],0) 0
-- ,--------------------Constrains-----------------
-- fromList [Constraint 0 1,Constraint 2 5,Constraint 3 15,Constraint 5 17,Constraint 16 15,Constraint 17 8,Constraint 7 19,Constraint 8 20,Constraint 18 20,Constraint 19 10,Constraint 9 21,Constraint 10 22,Constraint 23 21,Constraint 22 25,Constraint 24 25,Constraint 26 29,Constraint 27 0,Constraint 28 1,Constraint 29 2,Constraint 9 30,Constraint 10 31,Constraint 32 30,Constraint 31 13,Constraint 13 34,Constraint 14 35,Constraint 33 34,Constraint 35 38,Constraint 37 36,Constraint 38 41,Constraint 39 41,Constraint 40 43,Constraint 42 0,Constraint 43 1,Constraint 44 2,Constraint 13 46,Constraint 14 47,Constraint 45 46,Constraint 47 50,Constraint 48 50,Constraint 49 52,Constraint 51 (-1),Constraint 52 (-1),Constraint 53 (-1),Constraint 7 55,Constraint 8 56,Constraint 54 55,Constraint 56 59,Constraint 58 57,Constraint 59 62,Constraint 60 62,Constraint 61 64,Constraint 63 0,Constraint 64 1,Constraint 65 2,Constraint 3 66,Constraint 5 68,Constraint 67 66,Constraint 68 71,Constraint 69 71,Constraint 70 73,Constraint 72 0,Constraint 73 1,Constraint 74 2]
-- ,--------------------SubMap-----------------
-- fromList [(1,0),(2,1),(3,2),(4,3),(5,1),(6,4),(7,5),(8,1),(9,6),(10,5),(11,7),(12,8),(13,5),(14,9),(15,2),(16,2),(17,1),(18,1),(19,5),(20,1),(21,6),(22,5),(23,6),(24,5),(25,5),(26,1),(27,0),(28,0),(29,1),(30,6),(31,5),(32,6),(33,5),(34,5),(35,9),(36,10),(37,10),(38,9),(39,9),(40,0),(41,9),(42,0),(43,0),(44,1),(45,5),(46,5),(47,9),(48,9),(49,-1),(50,9),(51,-1),(52,-1),(53,-1),(54,5),(55,5),(56,1),(57,11),(58,11),(59,1),(60,1),(61,0),(62,1),(63,0),(64,0),(65,1),(66,2),(67,2),(68,1),(69,1),(70,0),(71,1),(72,0),(73,0),(74,1)]
-- ,--------------------GenConstN-----------------
-- Label ([0,0,1],0) 0
-- Msg <(([0,0,1],[2,3,1]),(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [2,3,1] Seller
--   * BranchSt () Found
--   Msg <(([2,2,1],[4,5,1]),(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [4,5,1] Buyer
--     * BranchSt () Two
--     Msg <(([1,5,1],[6,5,7]),(Buyer,Buyer2),0)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] [6,5,7] Buyer2
--       * BranchSt () NotSupport
--       Msg <(([6,5,6],[5,5,1]),(Buyer2,Buyer),0)> NotSupport1 [] Buyer2 Buyer
--       Msg <(([5,5,1],[0,0,1]),(Buyer,Seller),1)> TwoNotBuy [] Buyer Seller
--       Goto ([0,0,1],0) 0
--       * BranchSt () Support
--       Msg <(([6,5,6],[8,5,9]),(Buyer2,Buyer),0)> SupportVal [Int] Buyer2 Buyer
--       [Branch] [8,5,9] Buyer
--         * BranchSt () Enough
--         Msg <(([5,5,9],[10,10,9]),(Buyer,Seller),0)> TwoAccept [] Buyer Seller
--         Msg <(([10,10,9],[9,0,9]),(Seller,Buyer),1)> TwoDate [Int] Seller Buyer
--         Msg <(([9,0,9],[0,0,1]),(Buyer,Buyer2),2)> TwoSuccess [Int] Buyer Buyer2
--         Goto ([0,0,1],0) 0
--         * BranchSt () NotEnough
--         Msg <(([5,5,9],[9,-1,9]),(Buyer,Seller),0)> TwoNotBuy1 [] Buyer Seller
--         Msg <(([9,-1,9],[-1,-1,-1]),(Buyer,Buyer2),1)> TwoFailed [] Buyer Buyer2
--         Terminal [-1,-1,-1]
--     * BranchSt () One
--     Msg <(([5,5,1],[11,11,1]),(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <(([11,11,1],[1,0,1]),(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <(([1,0,1],[0,0,1]),(Buyer,Buyer2),2)> OneSuccess [Int] Buyer Buyer2
--     Goto ([0,0,1],0) 0
--   * BranchSt () NotFound
--   Msg <(([2,2,1],[1,0,1]),(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <(([1,0,1],[0,0,1]),(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([0,0,1],0) 0
-- ,--------------------CollectBranchDynVal-----------------
-- fromList [1,2,5,6,9]
-- ,--------------------MsgT-----------------
-- Label ([S0,S0,S1 s],0) 0
-- Msg <([S0,S0,S1 s],(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [S2 s,S3,S1 s] Seller
--   * BranchSt () Found
--   Msg <([S2 s,S2 Found,S1 s],(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [S4,S5 s,S1 s] Buyer
--     * BranchSt () Two
--     Msg <([S1 Two,S5 s,S1 s],(Buyer,Buyer2),0)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] [S6 s,S5 s,S7] Buyer2
--       * BranchSt () NotSupport
--       Msg <([S6 s,S5 s,S6 NotSupport],(Buyer2,Buyer),0)> NotSupport1 [  ] Buyer2 Buyer
--       Msg <([S5 NotSupport,S5 s,S1 s],(Buyer,Seller),1)> TwoNotBuy [  ] Buyer Seller
--       Goto ([S0,S0,S1 s],0) 0
--       * BranchSt () Support
--       Msg <([S6 s,S5 s,S6 Support],(Buyer2,Buyer),0)> SupportVal [ Int ] Buyer2 Buyer
--       [Branch] [S8,S5 s,S9 s] Buyer
--         * BranchSt () Enough
--         Msg <([S5 Enough,S5 s,S9 s],(Buyer,Seller),0)> TwoAccept [] Buyer Seller
--         Msg <([S10,S10,S9 s],(Seller,Buyer),1)> TwoDate [Int] Seller Buyer
--         Msg <([S9 Enough,S0,S9 s],(Buyer,Buyer2),2)> TwoSuccess [ Int ] Buyer Buyer2
--         Goto ([S0,S0,S1 s],0) 0
--         * BranchSt () NotEnough
--         Msg <([S5 NotEnough,S5 s,S9 s],(Buyer,Seller),0)> TwoNotBuy1 [  ] Buyer Seller
--         Msg <([S9 NotEnough,End,S9 s],(Buyer,Buyer2),1)> TwoFailed [  ] Buyer Buyer2
--         Terminal [End,End,End]
--     * BranchSt () One
--     Msg <([S5 One,S5 s,S1 s],(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <([S11,S11,S1 s],(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <([S1 One,S0,S1 s],(Buyer,Buyer2),2)> OneSuccess [Int] Buyer Buyer2
--     Goto ([S0,S0,S1 s],0) 0
--   * BranchSt () NotFound
--   Msg <([S2 s,S2 NotFound,S1 s],(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <([S1 NotFound,S0,S1 s],(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([S0,S0,S1 s],0) 0
-- ,--------------------MsgT1-----------------
-- Label ([S0,S0,S1 s],0) 0
-- Msg <((S0,S2 s,S3),(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [S2 s,S3,S1 s] Seller
--   * BranchSt () Found
--   Msg <((S2 Found,S5 s,S4),(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [S4,S5 s,S1 s] Buyer
--     * BranchSt () Two
--     Msg <((S1 Two,S6 s,S7),(Buyer,Buyer2),0)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] [S6 s,S5 s,S7] Buyer2
--       * BranchSt () NotSupport
--       Msg <((S6 NotSupport,S1 s,S5 NotSupport),(Buyer2,Buyer),0)> NotSupport1 [  ] Buyer2 Buyer
--       Msg <((S5 NotSupport,S0,S0),(Buyer,Seller),1)> TwoNotBuy [] Buyer Seller
--       Goto ([S0,S0,S1 s],0) 0
--       * BranchSt () Support
--       Msg <((S6 Support,S9 s,S8),(Buyer2,Buyer),0)> SupportVal [ Int ] Buyer2 Buyer
--       [Branch] [S8,S5 s,S9 s] Buyer
--         * BranchSt () Enough
--         Msg <((S5 Enough,S10,S10),(Buyer,Seller),0)> TwoAccept [] Buyer Seller
--         Msg <((S10,S0,S9 Enough),(Seller,Buyer),1)> TwoDate [Int] Seller Buyer
--         Msg <((S9 Enough,S0,S1 s),(Buyer,Buyer2),2)> TwoSuccess [ Int ] Buyer Buyer2
--         Goto ([S0,S0,S1 s],0) 0
--         * BranchSt () NotEnough
--         Msg <((S5 NotEnough,S9 NotEnough,End),(Buyer,Seller),0)> TwoNotBuy1 [  ] Buyer Seller
--         Msg <((S9 NotEnough,End,End),(Buyer,Buyer2),1)> TwoFailed [  ] Buyer Buyer2
--         Terminal [End,End,End]
--     * BranchSt () One
--     Msg <((S5 One,S11,S11),(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <((S11,S0,S1 One),(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <((S1 One,S0,S1 s),(Buyer,Buyer2),2)> OneSuccess [Int] Buyer Buyer2
--     Goto ([S0,S0,S1 s],0) 0
--   * BranchSt () NotFound
--   Msg <((S2 NotFound,S0,S1 NotFound),(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <((S1 NotFound,S0,S1 s),(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([S0,S0,S1 s],0) 0
-- ]
-- -------------------------------------Buyer---------------Seller--------------Buyer2
-- LABEL 0                                S0                  S0                 S1 s
--   Title                               S0->                ->S0                S1 s
--   [Branch Seller]                     S2 s                 S3                 S1 s
--     * BranchSt Found
--     Price                            S2 s<-           <-{S2 Found}            S1 s
--     [Branch Buyer]                     S4                 S5 s                S1 s
--       * BranchSt Two
--       PriceToBuyer2                {S1 Two}->             S5 s               ->S1 s
--       [Branch Buyer2]                 S6 s                S5 s                 S7
--         * BranchSt NotSupport
--         NotSupport1                  S6 s<-               S5 s         <-{S6 NotSupport}
--         TwoNotBuy               S5 NotSupport->          ->S5 s               S1 s
--         Goto 0                         S0                  S0                 S1 s
--         * BranchSt Support
--         SupportVal                   S6 s<-               S5 s           <-{S6 Support}
--         [Branch Buyer]                 S8                 S5 s                S9 s
--           * BranchSt Enough
--           TwoAccept              {S5 Enough}->           ->S5 s               S9 s
--           TwoDate                    S10<-               <-S10                S9 s
--           TwoSuccess              S9 Enough->              S0                ->S9 s
--           Goto 0                       S0                  S0                 S1 s
--           * BranchSt NotEnough
--           TwoNotBuy1            {S5 NotEnough}->         ->S5 s               S9 s
--           TwoFailed              S9 NotEnough->           End                ->S9 s
--           ~ Terminal                  End                 End                 End
--       * BranchSt One
--       OneAccept                    {S5 One}->            ->S5 s               S1 s
--       OneDate                        S11<-               <-S11                S1 s
--       OneSuccess                    S1 One->               S0                ->S1 s
--       Goto 0                           S0                  S0                 S1 s
--     * BranchSt NotFound
--     NoBook                           S2 s<-         <-{S2 NotFound}           S1 s
--     SellerNoBook                 S1 NotFound->             S0                ->S1 s
--     Goto 0                             S0                  S0                 S1 s
