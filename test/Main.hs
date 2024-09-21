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
      BranchSt Continue []
          Msg AddOne [Maybe Int, Int, Either String Int] Client Counter
          Msg Ping [Int, Int, Int] Client Server
          Msg Pong [] Server Client
          Goto 0
      BranchSt Finish []
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
-----------------------------------------------Client--------------Server--------------Counter-------------
Label 0                                             (S0)                (S1 s)              (S2 s)         
  [Branch Client ContinueOrFinish]                  (S0)                (S1 s)              (S2 s)         
  * BranchSt_Continue []                       
    AddOne [Maybe Int,Int,Either String Int]   Send (S2 Continue)       (S1 s)         Recv (S2 s)         
    Ping [Int,Int,Int]                         Send (S1 Continue)  Recv (S1 s)              (S2 s)         
    Pong []                                    Recv (S9)           Send (S9)                (S2 s)         
    Goto 0                                          (S0)                (S1 s)              (S2 s)         
  * BranchSt_Finish []                         
    Stop []                                    Send (S1 Finish)    Recv (S1 s)              (S2 s)         
    CStop []                                   Send (S2 Finish)         (End)          Recv (S2 s)         
    Terminal                                        (End)               (End)               (End)          

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
      BranchSt Found  []
        Msg Price [Int] Seller Buyer
        Branch Buyer OneOrTwo {
          BranchSt Two []
            Msg PriceToBuyer2 [Int] Buyer Buyer2
            Branch Buyer2 SupportOrNotSupport {
              BranchSt NotSupport [] 
                Msg NotSupport1 [] Buyer2 Buyer
                Msg TwoNotBuy [] Buyer Seller
                Goto 0
              BranchSt Support []
                Msg SupportVal [Int] Buyer2 Buyer
                Branch Buyer EnoughOrNotEnough {
                  BranchSt Enough []
                    Msg TwoAccept [] Buyer Seller
                    Msg TwoDate [Int] Seller Buyer
                    Msg TwoSuccess [Int] Buyer Buyer2
                    Goto 0
                  BranchSt NotEnough []
                    Msg TwoNotBuy1 [] Buyer Seller
                    Msg TwoFailed [] Buyer Buyer2
                    Terminal
                  }
              }
          BranchSt One []
            Msg OneAccept [] Buyer Seller
            Msg OneDate [Int] Seller Buyer
            Msg OneSuccess [Int] Buyer Buyer2
            Goto 0
          }
      BranchSt NotFound []
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
--------------------------------------------Buyer------------------Seller-----------------Buyer2-----------------
Label 0                                          (S0)                   (S0)                   (S2 s)            
  Title [String]                            Send (S0)              Recv (S0)                   (S2 s)            
  [Branch Seller FindBookResult]                 (S3 s)                 (S4)                   (S2 s)            
  * BranchSt_Found []                       
    Price [Int]                             Recv (S3 s)            Send (S3 Found)             (S2 s)            
    [Branch Buyer OneOrTwo]                      (S9)                   (S10 s)                (S2 s)            
    * BranchSt_Two []                       
      PriceToBuyer2 [Int]                   Send (S2 Two)               (S10 s)           Recv (S2 s)            
      [Branch Buyer2 SupportOrNotSupport]        (S15 s)                (S10 s)                (S17)             
      * BranchSt_NotSupport []              
        NotSupport1 []                      Recv (S15 s)                (S10 s)           Send (S15 NotSupport)  
        TwoNotBuy []                        Send (S10 NotSupport)  Recv (S10 s)                (S2 s)            
        Goto 0                                   (S0)                   (S0)                   (S2 s)            
      * BranchSt_Support []                 
        SupportVal [Int]                    Recv (S15 s)                (S10 s)           Send (S15 Support)     
        [Branch Buyer EnoughOrNotEnough]         (S30)                  (S10 s)                (S32 s)           
        * BranchSt_Enough []                
          TwoAccept []                      Send (S10 Enough)      Recv (S10 s)                (S32 s)           
          TwoDate [Int]                     Recv (S36)             Send (S36)                  (S32 s)           
          TwoSuccess [Int]                  Send (S32 Enough)           (S0)              Recv (S32 s)           
          Goto 0                                 (S0)                   (S0)                   (S2 s)            
        * BranchSt_NotEnough []             
          TwoNotBuy1 []                     Send (S10 NotEnough)   Recv (S10 s)                (S32 s)           
          TwoFailed []                      Send (S32 NotEnough)        (End)             Recv (S32 s)           
          Terminal                               (End)                  (End)                  (End)             
    * BranchSt_One []                       
      OneAccept []                          Send (S10 One)         Recv (S10 s)                (S2 s)            
      OneDate [Int]                         Recv (S57)             Send (S57)                  (S2 s)            
      OneSuccess [Int]                      Send (S2 One)               (S0)              Recv (S2 s)            
      Goto 0                                     (S0)                   (S0)                   (S2 s)            
  * BranchSt_NotFound []                    
    NoBook []                               Recv (S3 s)            Send (S3 NotFound)          (S2 s)            
    SellerNoBook []                         Send (S2 NotFound)          (S0)              Recv (S2 s)            
    Goto 0                                       (S0)                   (S0)                   (S2 s)            

-}
