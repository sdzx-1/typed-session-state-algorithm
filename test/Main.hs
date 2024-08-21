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

v1 :: Protocol Creat PingPongRole PingPongBranchSt
v1 =
  Label 0
    :> Branch
      Client
      [ BranchSt STrue $
          Msg "Ping" ["Int", "Int", "Int"] Client Server
            :> Msg "Pong" [] Server Client
            :> Msg "AddOne" [] Client Counter
            :> Goto 0
      , BranchSt SFalse $
          Msg "Stop" [] Client Server
            :> Msg "CStop" [] Client Counter
            :> Terminal
      ]

s1 =
  [r|
  
  Label 0
    Branch Client 
      BranchSt STrue
          Msg "AddOne" [] Client Counter
          Msg "Ping" ["Int", "Int", "Int"] Client Server
          Msg "Pong" [] Server Client
          Goto 0
      BranchSt SFalse
          Msg "Stop" [] Client Server
          Msg "CStop" [] Client Counter
          Terminal
|]

r1 = case runProtocolParser @PingPongRole @PingPongBranchSt s1 of
  Left e -> e
  Right a ->
    let res = piple a
     in case res of
          Left e -> show e
          Right ppResult -> genGraph (StrFillEnv 20 20) ppResult

-- >>> error r1
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

v2 :: Protocol Creat Role BookBranchSt
v2 =
  Label 0
    :> Msg "Title" ["String"] Buyer Seller
    :> Branch
      Seller
      [ BranchSt NotFound $
          Msg "NoBook" [] Seller Buyer
            :> Msg "SellerNoBook" [] Buyer Buyer2
            :> Goto 0
      , BranchSt Found $
          Msg "Price" ["Int"] Seller Buyer
            :> Branch
              Buyer
              [ BranchSt One $
                  Msg "OneAfford" [] Buyer Buyer2
                    :> Msg "OneAccept" [] Buyer Seller
                    :> Msg "OneDate" ["Int"] Seller Buyer
                    :> Msg "OneSuccess" ["Int"] Buyer Buyer2
                    :> Goto 0
              , BranchSt Two $
                  Msg "PriceToBuyer2" ["Int"] Buyer Buyer2
                    :> Branch
                      Buyer2
                      [ BranchSt NotSupport $
                          Msg "NotSupport1" [] Buyer2 Buyer
                            :> Msg "TwoNotBuy" [] Buyer Seller
                            :> Goto 0
                      , BranchSt Support $
                          Msg "SupportVal" ["Int"] Buyer2 Buyer
                            :> Branch
                              Buyer
                              [ BranchSt Enough $
                                  Msg "TwoAccept" [] Buyer Seller
                                    :> Msg "TwoDate" ["Int"] Seller Buyer
                                    :> Msg "TwoSuccess" ["Int"] Buyer Buyer2
                                    :> Goto 0
                              , BranchSt NotEnough $
                                  Msg "TwoNotBuy1" [] Buyer Seller
                                    :> Msg "TwoFailed" [] Buyer Buyer2
                                    :> Terminal
                              ]
                      ]
              ]
      ]

s2 =
  [r|
  Label 0
    Msg "Title" ["String"] Buyer Seller
    Branch Seller
        BranchSt NotFound 
          Msg "NoBook" [] Seller Buyer
               Msg "SellerNoBook" [] Buyer Buyer2
               Goto 0
        BranchSt Found 
          Msg "Price" ["Int"] Seller Buyer
          Branch Buyer
            BranchSt One 
              Msg "OneOffecd" [] Buyer Buyer2
              Msg "OneAccept" [] Buyer Seller
              Msg "OneDate" ["Int"] Seller Buyer
              Msg "OneSuccess" ["Int"] Buyer Buyer2
              Goto 0
            BranchSt Two 
              Msg "PriceToBuyer2" ["Int"] Buyer Buyer2
              Branch Buyer2
                BranchSt NotSupport 
                  Msg "NotSupport1" [] Buyer2 Buyer
                  Msg "TwoNotBuy" [] Buyer Seller
                  Goto 0
                BranchSt Support 
                  Msg "SupportVal" ["Int"] Buyer2 Buyer
                  Branch Buyer
                    BranchSt Enough 
                      Msg "TwoAccept" [] Buyer Seller
                      Msg "TwoDate" ["Int"] Seller Buyer
                      Msg "TwoSuccess" ["Int"] Buyer Buyer2
                      Goto 0
                    BranchSt NotEnough 
                      Msg "TwoNotBuy1" [] Buyer Seller
                      Msg "TwoFailed" [] Buyer Buyer2
                      Terminal
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
--   * BranchSt () NotFound
--   Msg <()> NoBook [] Seller Buyer
--   Msg <()> SellerNoBook [] Buyer Buyer2
--   Goto () 0
--   * BranchSt () Found
--   Msg <()> Price [Int] Seller Buyer
--   [Branch] () Buyer
--     * BranchSt () One
--     Msg <()> OneAccept [] Buyer Seller
--     Msg <()> OneDate [Int] Seller Buyer
--     Msg <()> OneSuccess [Int] Buyer Buyer2
--     Goto () 0
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
-- ,--------------------AddNum-----------------
-- Label [0,1,2] 0
-- Msg <([0,1,2],[3,4,5],100)> Title [String] Buyer Seller
-- [Branch] [3,4,5] Seller
--   * BranchSt () NotFound
--   Msg <([6,7,8],[9,10,11],0)> NoBook [] Seller Buyer
--   Msg <([9,10,11],[12,13,14],1)> SellerNoBook [] Buyer Buyer2
--   Goto [12,13,14] 0
--   * BranchSt () Found
--   Msg <([15,16,17],[18,19,20],0)> Price [Int] Seller Buyer
--   [Branch] [18,19,20] Buyer
--     * BranchSt () One
--     Msg <([21,22,23],[24,25,26],0)> OneAccept [] Buyer Seller
--     Msg <([24,25,26],[27,28,29],1)> OneDate [Int] Seller Buyer
--     Msg <([27,28,29],[30,31,32],2)> OneSuccess [Int] Buyer Buyer2
--     Goto [30,31,32] 0
--     * BranchSt () Two
--     Msg <([33,34,35],[36,37,38],0)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] [36,37,38] Buyer2
--       * BranchSt () NotSupport
--       Msg <([39,40,41],[42,43,44],0)> NotSupport1 [] Buyer2 Buyer
--       Msg <([42,43,44],[45,46,47],1)> TwoNotBuy [] Buyer Seller
--       Goto [45,46,47] 0
--       * BranchSt () Support
--       Msg <([48,49,50],[51,52,53],0)> SupportVal [Int] Buyer2 Buyer
--       [Branch] [51,52,53] Buyer
--         * BranchSt () Enough
--         Msg <([54,55,56],[57,58,59],0)> TwoAccept [] Buyer Seller
--         Msg <([57,58,59],[60,61,62],1)> TwoDate [Int] Seller Buyer
--         Msg <([60,61,62],[63,64,65],2)> TwoSuccess [Int] Buyer Buyer2
--         Goto [63,64,65] 0
--         * BranchSt () NotEnough
--         Msg <([66,67,68],[69,70,71],0)> TwoNotBuy1 [] Buyer Seller
--         Msg <([69,70,71],[72,73,74],1)> TwoFailed [] Buyer Buyer2
--         Terminal [72,73,74]
-- ,--------------------CollectBranchVals-----------------
-- fromList [3,4,5,18,19,20,36,37,38,51,52,53]
-- ,--------------------AddNumN-----------------
-- Label [0,1,2] 0
-- Msg <([0,1,2],[3,4,5],100)> Title [String] Buyer Seller
-- [Branch] [3,4,5] Seller
--   * BranchSt () NotFound
--   Msg <([18,19,20],[36,37,38],0)> NoBook [] Seller Buyer
--   Msg <([36,37,38],[51,52,53],1)> SellerNoBook [] Buyer Buyer2
--   Goto [51,52,53] 0
--   * BranchSt () Found
--   Msg <([15,16,17],[6,7,8],0)> Price [Int] Seller Buyer
--   [Branch] [6,7,8] Buyer
--     * BranchSt () One
--     Msg <([21,22,23],[24,25,26],0)> OneAccept [] Buyer Seller
--     Msg <([24,25,26],[27,28,29],1)> OneDate [Int] Seller Buyer
--     Msg <([27,28,29],[30,31,32],2)> OneSuccess [Int] Buyer Buyer2
--     Goto [30,31,32] 0
--     * BranchSt () Two
--     Msg <([33,34,35],[9,10,11],0)> PriceToBuyer2 [Int] Buyer Buyer2
--     [Branch] [9,10,11] Buyer2
--       * BranchSt () NotSupport
--       Msg <([39,40,41],[42,43,44],0)> NotSupport1 [] Buyer2 Buyer
--       Msg <([42,43,44],[45,46,47],1)> TwoNotBuy [] Buyer Seller
--       Goto [45,46,47] 0
--       * BranchSt () Support
--       Msg <([48,49,50],[12,13,14],0)> SupportVal [Int] Buyer2 Buyer
--       [Branch] [12,13,14] Buyer
--         * BranchSt () Enough
--         Msg <([54,55,56],[57,58,59],0)> TwoAccept [] Buyer Seller
--         Msg <([57,58,59],[60,61,62],1)> TwoDate [Int] Seller Buyer
--         Msg <([60,61,62],[63,64,65],2)> TwoSuccess [Int] Buyer Buyer2
--         Goto [63,64,65] 0
--         * BranchSt () NotEnough
--         Msg <([66,67,68],[69,70,71],0)> TwoNotBuy1 [] Buyer Seller
--         Msg <([69,70,71],[72,73,74],1)> TwoFailed [] Buyer Buyer2
--         Terminal [72,73,74]
-- ,--------------------GenConst-----------------
-- Label ([0,1,2],0) 0
-- Msg <(([0,1,2],[3,4,5]),(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [3,4,5] Seller
--   * BranchSt () NotFound
--   Msg <(([18,19,20],[36,37,38]),(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <(([36,37,38],[51,52,53]),(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([51,52,53],0) 0
--   * BranchSt () Found
--   Msg <(([15,16,17],[6,7,8]),(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [6,7,8] Buyer
--     * BranchSt () One
--     Msg <(([21,22,23],[24,25,26]),(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <(([24,25,26],[27,28,29]),(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <(([27,28,29],[30,31,32]),(Buyer,Buyer2),2)> OneSuccess [ Int ] Buyer Buyer2
--     Goto ([30,31,32],0) 0
--     * BranchSt () Two
--     Msg <(([33,34,35],[9,10,11]),(Buyer,Buyer2),0)> PriceToBuyer2 [ Int ] Buyer Buyer2
--     [Branch] [9,10,11] Buyer2
--       * BranchSt () NotSupport
--       Msg <(([39,40,41],[42,43,44]),(Buyer2,Buyer),0)> NotSupport1 [  ] Buyer2 Buyer
--       Msg <(([42,43,44],[45,46,47]),(Buyer,Seller),1)> TwoNotBuy [] Buyer Seller
--       Goto ([45,46,47],0) 0
--       * BranchSt () Support
--       Msg <(([48,49,50],[12,13,14]),(Buyer2,Buyer),0)> SupportVal [ Int ] Buyer2 Buyer
--       [Branch] [12,13,14] Buyer
--         * BranchSt () Enough
--         Msg <(([54,55,56],[57,58,59]),(Buyer,Seller),0)> TwoAccept [  ] Buyer Seller
--         Msg <(([57,58,59],[60,61,62]),(Seller,Buyer),1)> TwoDate [ Int ] Seller Buyer
--         Msg <(([60,61,62],[63,64,65]),(Buyer,Buyer2),2)> TwoSuccess [ Int ] Buyer Buyer2
--         Goto ([63,64,65],0) 0
--         * BranchSt () NotEnough
--         Msg <(([66,67,68],[69,70,71]),(Buyer,Seller),0)> TwoNotBuy1 [  ] Buyer Seller
--         Msg <(([69,70,71],[72,73,74]),(Buyer,Buyer2),1)> TwoFailed [  ] Buyer Buyer2
--         Terminal [72,73,74]
-- ,--------------------Constrains-----------------
-- fromList [Constraint 0 1,Constraint 2 5,Constraint 3 18,Constraint 5 20,Constraint 19 18,Constraint 20 38,Constraint 36 38,Constraint 37 52,Constraint 51 0,Constraint 52 1,Constraint 53 2,Constraint 3 15,Constraint 5 17,Constraint 16 15,Constraint 17 8,Constraint 7 22,Constraint 8 23,Constraint 21 22,Constraint 23 26,Constraint 25 24,Constraint 26 29,Constraint 27 29,Constraint 28 31,Constraint 30 0,Constraint 31 1,Constraint 32 2,Constraint 7 34,Constraint 8 35,Constraint 33 35,Constraint 34 10,Constraint 9 39,Constraint 10 40,Constraint 41 39,Constraint 40 43,Constraint 42 43,Constraint 44 47,Constraint 45 0,Constraint 46 1,Constraint 47 2,Constraint 9 48,Constraint 10 49,Constraint 50 48,Constraint 49 13,Constraint 13 55,Constraint 14 56,Constraint 54 55,Constraint 56 59,Constraint 58 57,Constraint 59 62,Constraint 60 62,Constraint 61 64,Constraint 63 0,Constraint 64 1,Constraint 65 2,Constraint 13 67,Constraint 14 68,Constraint 66 67,Constraint 68 71,Constraint 69 71,Constraint 70 73,Constraint 72 (-1),Constraint 73 (-1),Constraint 74 (-1)]
-- ,--------------------SubMap-----------------
-- fromList [(1,0),(2,1),(3,2),(4,3),(5,1),(6,4),(7,5),(8,1),(9,6),(10,5),(11,7),(12,8),(13,5),(14,9),(15,2),(16,2),(17,1),(18,2),(19,2),(20,1),(21,5),(22,5),(23,1),(24,10),(25,10),(26,1),(27,1),(28,0),(29,1),(30,0),(31,0),(32,1),(33,1),(34,5),(35,1),(36,1),(37,0),(38,1),(39,6),(40,5),(41,6),(42,5),(43,5),(44,1),(45,0),(46,0),(47,1),(48,6),(49,5),(50,6),(51,0),(52,0),(53,1),(54,5),(55,5),(56,9),(57,11),(58,11),(59,9),(60,9),(61,0),(62,9),(63,0),(64,0),(65,1),(66,5),(67,5),(68,9),(69,9),(70,-1),(71,9),(72,-1),(73,-1),(74,-1)]
-- ,--------------------GenConstN-----------------
-- Label ([0,0,1],0) 0
-- Msg <(([0,0,1],[2,3,1]),(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [2,3,1] Seller
--   * BranchSt () NotFound
--   Msg <(([2,2,1],[1,0,1]),(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <(([1,0,1],[0,0,1]),(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([0,0,1],0) 0
--   * BranchSt () Found
--   Msg <(([2,2,1],[4,5,1]),(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [4,5,1] Buyer
--     * BranchSt () One
--     Msg <(([5,5,1],[10,10,1]),(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <(([10,10,1],[1,0,1]),(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <(([1,0,1],[0,0,1]),(Buyer,Buyer2),2)> OneSuccess [Int] Buyer Buyer2
--     Goto ([0,0,1],0) 0
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
--         Msg <(([5,5,9],[11,11,9]),(Buyer,Seller),0)> TwoAccept [] Buyer Seller
--         Msg <(([11,11,9],[9,0,9]),(Seller,Buyer),1)> TwoDate [Int] Seller Buyer
--         Msg <(([9,0,9],[0,0,1]),(Buyer,Buyer2),2)> TwoSuccess [Int] Buyer Buyer2
--         Goto ([0,0,1],0) 0
--         * BranchSt () NotEnough
--         Msg <(([5,5,9],[9,-1,9]),(Buyer,Seller),0)> TwoNotBuy1 [] Buyer Seller
--         Msg <(([9,-1,9],[-1,-1,-1]),(Buyer,Buyer2),1)> TwoFailed [] Buyer Buyer2
--         Terminal [-1,-1,-1]
-- ,--------------------CollectBranchDynVal-----------------
-- fromList [1,2,5,6,9]
-- ,--------------------MsgT-----------------
-- Label ([S0,S0,S1 s],0) 0
-- Msg <([S0,S0,S1 s],(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [S2 s,S3,S1 s] Seller
--   * BranchSt () NotFound
--   Msg <([S2 s,S2 NotFound,S1 s],(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <([S1 NotFound,S0,S1 s],(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([S0,S0,S1 s],0) 0
--   * BranchSt () Found
--   Msg <([S2 s,S2 Found,S1 s],(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [S4,S5 s,S1 s] Buyer
--     * BranchSt () One
--     Msg <([S5 One,S5 s,S1 s],(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <([S10,S10,S1 s],(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <([S1 One,S0,S1 s],(Buyer,Buyer2),2)> OneSuccess [Int] Buyer Buyer2
--     Goto ([S0,S0,S1 s],0) 0
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
--         Msg <([S11,S11,S9 s],(Seller,Buyer),1)> TwoDate [Int] Seller Buyer
--         Msg <([S9 Enough,S0,S9 s],(Buyer,Buyer2),2)> TwoSuccess [ Int ] Buyer Buyer2
--         Goto ([S0,S0,S1 s],0) 0
--         * BranchSt () NotEnough
--         Msg <([S5 NotEnough,S5 s,S9 s],(Buyer,Seller),0)> TwoNotBuy1 [  ] Buyer Seller
--         Msg <([S9 NotEnough,End,S9 s],(Buyer,Buyer2),1)> TwoFailed [  ] Buyer Buyer2
--         Terminal [End,End,End]
-- ,--------------------MsgT1-----------------
-- Label ([S0,S0,S1 s],0) 0
-- Msg <((S0,S2 s,S3),(Buyer,Seller),100)> Title [String] Buyer Seller
-- [Branch] [S2 s,S3,S1 s] Seller
--   * BranchSt () NotFound
--   Msg <((S2 NotFound,S0,S1 NotFound),(Seller,Buyer),0)> NoBook [] Seller Buyer
--   Msg <((S1 NotFound,S0,S1 s),(Buyer,Buyer2),1)> SellerNoBook [] Buyer Buyer2
--   Goto ([S0,S0,S1 s],0) 0
--   * BranchSt () Found
--   Msg <((S2 Found,S5 s,S4),(Seller,Buyer),0)> Price [Int] Seller Buyer
--   [Branch] [S4,S5 s,S1 s] Buyer
--     * BranchSt () One
--     Msg <((S5 One,S10,S10),(Buyer,Seller),0)> OneAccept [] Buyer Seller
--     Msg <((S10,S0,S1 One),(Seller,Buyer),1)> OneDate [Int] Seller Buyer
--     Msg <((S1 One,S0,S1 s),(Buyer,Buyer2),2)> OneSuccess [Int] Buyer Buyer2
--     Goto ([S0,S0,S1 s],0) 0
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
--         Msg <((S5 Enough,S11,S11),(Buyer,Seller),0)> TwoAccept [] Buyer Seller
--         Msg <((S11,S0,S9 Enough),(Seller,Buyer),1)> TwoDate [Int] Seller Buyer
--         Msg <((S9 Enough,S0,S1 s),(Buyer,Buyer2),2)> TwoSuccess [ Int ] Buyer Buyer2
--         Goto ([S0,S0,S1 s],0) 0
--         * BranchSt () NotEnough
--         Msg <((S5 NotEnough,S9 NotEnough,End),(Buyer,Seller),0)> TwoNotBuy1 [  ] Buyer Seller
--         Msg <((S9 NotEnough,End,End),(Buyer,Buyer2),1)> TwoFailed [  ] Buyer Buyer2
--         Terminal [End,End,End]
-- ]
-- -------------------------------------Buyer---------------Seller--------------Buyer2
-- LABEL 0                                S0                  S0                 S1 s
--   Title                               S0->                ->S0                S1 s
--   [Branch Seller]                     S2 s                 S3                 S1 s
--     * BranchSt NotFound
--     NoBook                           S2 s<-         <-{S2 NotFound}           S1 s
--     SellerNoBook                 S1 NotFound->             S0                ->S1 s
--     Goto 0                             S0                  S0                 S1 s
--     * BranchSt Found
--     Price                            S2 s<-           <-{S2 Found}            S1 s
--     [Branch Buyer]                     S4                 S5 s                S1 s
--       * BranchSt One
--       OneAccept                    {S5 One}->            ->S5 s               S1 s
--       OneDate                        S10<-               <-S10                S1 s
--       OneSuccess                    S1 One->               S0                ->S1 s
--       Goto 0                           S0                  S0                 S1 s
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
--           TwoDate                    S11<-               <-S11                S9 s
--           TwoSuccess              S9 Enough->              S0                ->S9 s
--           Goto 0                       S0                  S0                 S1 s
--           * BranchSt NotEnough
--           TwoNotBuy1            {S5 NotEnough}->         ->S5 s               S9 s
--           TwoFailed              S9 NotEnough->           End                ->S9 s
--           ~ Terminal                  End                 End                 End
