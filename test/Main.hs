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
    let (res) = piple a
     in case res of
          Left e -> show e
          Right ppResult -> genGraph (StrFillEnv 20 20) ppResult

-- >>> error r2
-- -------------------------------------Buyer---------------Seller--------------Buyer2
-- LABEL 0                                S0                  S0                 S1 s
--   Title                               S0->                ->S0                S1 s
-- LABEL 1                               S2 s                 S3                 S1 s
--   [Branch Seller]                     S2 s                 S3                 S1 s
--     * BranchSt NotFound
--     NoBook                           S2 s<-         <-{S2 NotFound}           S1 s
--     SellerNoBook                 S1 NotFound->             S0                ->S1 s
--     Goto 0                             S0                  S0                 S1 s
--     * BranchSt Found
--     Price                            S2 s<-           <-{S2 Found}            S1 s
--     [Branch Buyer]                     S7                 S8 s                S1 s
--       * BranchSt One
--       OneAccept                    {S8 One}->            ->S8 s               S1 s
--       OneDate                        S12<-               <-S12                S1 s
--       OneSuccess                    S1 One->               S0                ->S1 s
--       Goto 0                           S0                  S0                 S1 s
--       * BranchSt Two
--       PriceToBuyer2                {S1 Two}->             S8 s               ->S1 s
--       [Branch Buyer2]                S13 s                S8 s                S14
--         * BranchSt NotSupport
--         NotSupport1                 S13 s<-               S8 s         <-{S13 NotSupport}
--         TwoNotBuy               S8 NotSupport->          ->S8 s               S1 s
--         Goto 0                         S0                  S0                 S1 s
--         * BranchSt Support
--         SupportVal                  S13 s<-               S8 s          <-{S13 Support}
--         [Branch Buyer]                S18                 S8 s               S19 s
--           * BranchSt Enough
--           TwoAccept              {S8 Enough}->           ->S8 s              S19 s
--           TwoDate                    S23<-               <-S23               S19 s
--           TwoSuccess              S19 Enough->             S0               ->S19 s
--           Goto 0                       S0                  S0                 S1 s
--           * BranchSt NotEnough
--           TwoNotBuy1            {S8 NotEnough}->         ->S8 s              S19 s
--           TwoFailed             S19 NotEnough->           End               ->S19 s
--           ~ Terminal                  End                 End                 End
