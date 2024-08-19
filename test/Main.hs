{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Text.RawString.QQ (r)
import TypedSession.State.Parser (runProtocolParser)
import TypedSession.State.Pattern
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
          Msg "Ping" ["Int", "Int", "Int"] Client Server
          Msg "Pong" [] Server Client
          Msg "AddOne" [] Client Counter
          Goto 0
      BranchSt SFalse
          Msg "Stop" [] Client Server
          Msg "CStop" [] Client Counter
          Terminal
|]

r1 = runProtocolParser @PingPongRole @PingPongBranchSt s1

-- >>> r1
-- Right Label () 0
-- [Branch] () Client
--   * BranchSt () STrue
--   Msg <()> Ping [Int, Int, Int] Client Server
--   Msg <()> Pong [] Server Client
--   Msg <()> AddOne [] Client Counter
--   Goto () 0
--   * BranchSt () SFalse
--   Msg <()> Stop [] Client Server
--   Msg <()> CStop [] Client Counter
--   Terminal ()


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
              Msg "OneAfford" [] Buyer Buyer2
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

r2 = runProtocolParser @Role @BookBranchSt s2

-- >>> r2
-- Right Label () 0
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
--     Msg <()> OneAfford [] Buyer Buyer2
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
