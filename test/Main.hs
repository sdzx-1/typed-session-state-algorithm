{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Text.RawString.QQ (r)
import TypedSession.State.Parser (runProtocolParser)
import TypedSession.State.Pipeline (PipeResult (..), genGraph, pipeWithTracer)
import TypedSession.State.Render

main :: IO ()
main = putStrLn "Test suite not yet implemented."

data PingPongRole = Client | Server | Counter
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data PingPongBranchSt = Continue | Finish
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

s1 =
  [r|
  
  Label 0
    Branch Client ContinueOrFinish {
      BranchSt Continue
          Msg AddOne [Maybe Int, Int, Either String Int] Client Counter
          Msg Ping [Int, Int, Int] Client Server
          Msg Pong [] Server Client
          Goto 0
      BranchSt Finish
          Msg Stop [] Client Server
          Msg CStop [] Client Counter
          Terminal
    }
|]

r1 = case runProtocolParser @PingPongRole @PingPongBranchSt s1 of
  Left e -> e
  Right a ->
    let (lq, res) = pipeWithTracer a
     in case res of
          Left e -> show e
          Right ppResult -> genGraph ppResult

{-
>>> error r1
---------------------------------------------Client--------------Server--------------Counter-------------
Label 0                                      S0                  S1 s                S2 s                
  [Branch Client ContinueOrFinish]             S0                S1 s                S2 s                
  * BranchSt_Continue                        
  AddOne [Maybe Int,Int,Either String Int]     {S2 Continue} ->  S1 s                S2 s <-             
    Ping [Int,Int,Int]                       S1 Continue ->      S1 s <-             S2 s                
    Pong []                                  S3 <-               S3 ->               S2 s                
    Goto 0                                   S0                  S1 s                S2 s                
  * BranchSt_Finish                          
  Stop []                                      {S1 Finish} ->    S1 s <-             S2 s                
    CStop []                                 S2 Finish ->        End                 S2 s <-             
    Terminal                                 End                 End                 End                 

-}

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
    Msg Title [String] Buyer Seller
    Branch Seller FindBookResult {
      BranchSt Found 
        Msg Price [Int] Seller Buyer
        Branch Buyer OneOrTwo {
          BranchSt Two 
            Msg PriceToBuyer2 [Int] Buyer Buyer2
            Branch Buyer2 SupportOrNotSupport {
              BranchSt NotSupport 
                Msg NotSupport1 [] Buyer2 Buyer
                Msg TwoNotBuy [] Buyer Seller
                Goto 0
              BranchSt Support 
                Msg SupportVal [Int] Buyer2 Buyer
                Branch Buyer EnoughOrNotEnough {
                  BranchSt Enough 
                    Msg TwoAccept [] Buyer Seller
                    Msg TwoDate [Int] Seller Buyer
                    Msg TwoSuccess [Int] Buyer Buyer2
                    Goto 0
                  BranchSt NotEnough 
                    Msg TwoNotBuy1 [] Buyer Seller
                    Msg TwoFailed [] Buyer Buyer2
                    Terminal
                  }
              }
          BranchSt One 
            Msg OneAccept [] Buyer Seller
            Msg OneDate [Int] Seller Buyer
            Msg OneSuccess [Int] Buyer Buyer2
            Goto 0
          }
      BranchSt NotFound 
        Msg NoBook [] Seller Buyer
             Msg SellerNoBook [] Buyer Buyer2
             Goto 0
      }
|]

r2 = case runProtocolParser @Role @BookBranchSt s2 of
  Left e -> e
  Right a ->
    let (seqList, res) = pipeWithTracer a
     in case res of
          Left e -> show e
          Right PipeResult{msgT} ->
            let st = show seqList
             in runRender msgT

{-
>>> error r2
--------------------------------------------Buyer----------------------Seller---------------------Buyer2---------------------
Label 0                                     S0                         S0                         S1 s                       
  Title [String]                            S0 ->                      S0 <-                      S1 s                       
  [Branch Seller FindBookResult]            S2 s                         S3                       S1 s                       
  * BranchSt_Found                          
  Price [Int]                               S2 s <-                      {S2 Found} ->            S1 s                       
    [Branch Buyer OneOrTwo]                     S4                     S5 s                       S1 s                       
    * BranchSt_Two                          
    PriceToBuyer2 [Int]                         {S1 Two} ->            S5 s                       S1 s <-                    
      [Branch Buyer2 SupportOrNotSupport]   S6 s                       S5 s                             S7                   
      * BranchSt_NotSupport                 
      NotSupport1 []                        S6 s <-                    S5 s                             {S6 NotSupport} ->   
        TwoNotBuy []                        S5 NotSupport ->           S5 s <-                    S1 s                       
        Goto 0                              S0                         S0                         S1 s                       
      * BranchSt_Support                    
      SupportVal [Int]                      S6 s <-                    S5 s                             {S6 Support} ->      
        [Branch Buyer EnoughOrNotEnough]            S8                 S5 s                       S9 s                       
        * BranchSt_Enough                   
        TwoAccept []                                {S5 Enough} ->     S5 s <-                    S9 s                       
          TwoDate [Int]                     S10 <-                     S10 ->                     S9 s                       
          TwoSuccess [Int]                  S9 Enough ->               S0                         S9 s <-                    
          Goto 0                            S0                         S0                         S1 s                       
        * BranchSt_NotEnough                
        TwoNotBuy1 []                               {S5 NotEnough} ->  S5 s <-                    S9 s                       
          TwoFailed []                      S9 NotEnough ->            End                        S9 s <-                    
          Terminal                          End                        End                        End                        
    * BranchSt_One                          
    OneAccept []                                {S5 One} ->            S5 s <-                    S1 s                       
      OneDate [Int]                         S11 <-                     S11 ->                     S1 s                       
      OneSuccess [Int]                      S1 One ->                  S0                         S1 s <-                    
      Goto 0                                S0                         S0                         S1 s                       
  * BranchSt_NotFound                       
  NoBook []                                 S2 s <-                      {S2 NotFound} ->         S1 s                       
    SellerNoBook []                         S1 NotFound ->             S0                         S1 s <-                    
    Goto 0                                  S0                         S0                         S1 s                       

-}
