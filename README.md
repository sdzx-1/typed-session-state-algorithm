Automatically generate status for typed-communication-protocol.

```haskell
data BookRole
  = Buyer
  | Seller
  | Buyer2
  deriving (Show)

bookRoleToInt :: BookRole -> Int
bookRoleToInt = \case
  Buyer -> 0
  Seller -> 1
  Buyer2 -> 2

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

p :: R BookRole BookBranchSt ()
p =
  (Buyer --> Seller) "Title String"
    :> branch
      [ branchVal NotFound $
          (Buyer <-- Seller) "NoBook"
            :> (Buyer --> Buyer2) "SellerNoBook"
            :> terminal
      , branchVal Found $
          (Buyer <-- Seller) "Price Int"
            :> branch
              [ branchVal One $
                  (Buyer --> Buyer2) "OneAfford"
                    :> (Buyer --> Seller) "OneAccept"
                    :> (Buyer <-- Seller) "OneDate Date"
                    :> (Buyer --> Buyer2) "OneSuccess Date"
                    :> terminal
              , branchVal Two $
                  (Buyer --> Buyer2) "PriceToBuyer2 Int"
                    :> branch
                      [ branchVal NotSupport $
                          (Buyer <-- Buyer2) "NotSupport"
                            :> (Buyer --> Seller) "TwoNotBuy"
                            :> terminal
                      , branchVal Support $
                          (Buyer <-- Buyer2) "SupportVal Int"
                            :> branch
                              [ branchVal Enough $
                                  ( (Buyer --> Seller) "TwoAccept"
                                      :> (Buyer <-- Seller) "TwoDate Date"
                                      :> (Buyer --> Buyer2) "TwoSuccess Date"
                                      :> terminal
                                  )
                              , branchVal NotEnough $
                                  ( (Buyer --> Seller) "TwoNotBuy1"
                                      :> (Buyer --> Buyer2) "TwoFailed"
                                      :> terminal
                                  )
                              ]
                      ]
              ]
      ]

data Msg Role BookSt from send recv where
  Title :: String -> Msg Role BookSt S0 '(Buyer, S2 s) '(Seller, S2 s)
  NoBook :: Msg Role BookSt (S2 [NotFound]) '(Seller, End) '(Buyer, S1 [NotFound])
  SellerNoBook :: Msg Role BookSt (S1 [NotFound]) '(Buyer, End) '(Buyer2, End)
  Price :: Int -> Msg Role BookSt (S2 [Found]) '(Seller, S3 s) '(Buyer, S1 s)
  OneAfford :: Msg Role BookSt (S1 [One,Found]) '(Buyer, S3 [One,Found]) '(Buyer2, S4)
  OneAccept :: Msg Role BookSt (S3 [One,Found]) '(Buyer, S5) '(Seller, S5)
  OneDate :: Date -> Msg Role BookSt S5 '(Seller, End) '(Buyer, S4)
  OneSuccess :: Date -> Msg Role BookSt S4 '(Buyer, End) '(Buyer2, End)
  PriceToBuyer2 :: Int -> Msg Role BookSt (S1 [Two,Found]) '(Buyer, S6 s) '(Buyer2, S6 s)
  NotSupport :: Msg Role BookSt (S6 [NotSupport,Two,Found]) '(Buyer2, End) '(Buyer, S3 [NotSupport,Two,Found])
  TwoNotBuy :: Msg Role BookSt (S3 [NotSupport,Two,Found]) '(Buyer, End) '(Seller, End)
  SupportVal :: Int -> Msg Role BookSt (S6 [Support,Two,Found]) '(Buyer2, S7 s) '(Buyer, S3 s)
  TwoAccept :: Msg Role BookSt (S3 [Enough,Support,Two,Found]) '(Buyer, S8) '(Seller, S8)
  TwoDate :: Date -> Msg Role BookSt S8 '(Seller, End) '(Buyer, S7 [Enough,Support,Two,Found])
  TwoSuccess :: Date -> Msg Role BookSt (S7 [Enough,Support,Two,Found]) '(Buyer, End) '(Buyer2, End)
  TwoNotBuy1 :: Msg Role BookSt (S3 [NotEnough,Support,Two,Found]) '(Buyer, S7 [NotEnough,Support,Two,Found]) '(Seller, End)
  TwoFailed :: Msg Role BookSt (S7 [NotEnough,Support,Two,Found]) '(Buyer, End) '(Buyer2, End)

---------------------------Buyer-------------------------Seller------------------------Buyer2---------------------------
                             S0                            S0                          (S1 s)
            Title            |            ----->           |
                           (S2 s)                        (S2 s)                        (S1 s)
    ---------------------------------------------------[NotFound]---------------------------------------------------
                           (S2 s)                   (S2 [NotFound])                    (S1 s)
            NoBook           |            <-----           |
                      (S1 [NotFound])                     End                          (S1 s)
         SellerNoBook        |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
    ----------------------------------------------------[Found]-----------------------------------------------------
                           (S2 s)                     (S2 [Found])                     (S1 s)
            Price            |            <-----           |
                           (S1 s)                        (S3 s)                        (S1 s)
        ----------------------------------------------[One,Found]-----------------------------------------------
                      (S1 [One,Found])                   (S3 s)                        (S1 s)
          OneAfford          |                           ----->                          |
                      (S3 [One,Found])                   (S3 s)                          S4
          OneAccept          |            ----->           |
                             S5                            S5                            S4
           OneDate           |            <-----           |
                             S4                           End                            S4
          OneSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
        ----------------------------------------------[Two,Found]-----------------------------------------------
                      (S1 [Two,Found])                   (S3 s)                        (S1 s)
        PriceToBuyer2        |                           ----->                          |
                           (S6 s)                        (S3 s)                        (S6 s)
            -------------------------------------[NotSupport,Two,Found]-------------------------------------
                           (S6 s)                        (S3 s)             (S6 [NotSupport,Two,Found])
          NotSupport         |                           <-----                          |
                (S3 [NotSupport,Two,Found])              (S3 s)                         End
          TwoNotBuy          |            ----->           |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
            --------------------------------------[Support,Two,Found]---------------------------------------
                           (S6 s)                        (S3 s)               (S6 [Support,Two,Found])
          SupportVal         |                           <-----                          |
                           (S3 s)                        (S3 s)                        (S7 s)
                -------------------------------[Enough,Support,Two,Found]-------------------------------
              (S3 [Enough,Support,Two,Found])            (S3 s)                        (S7 s)
          TwoAccept          |            ----->           |
                             S8                            S8                          (S7 s)
           TwoDate           |            <-----           |
              (S7 [Enough,Support,Two,Found])             End                          (S7 s)
          TwoSuccess         |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
                -----------------------------[NotEnough,Support,Two,Found]------------------------------
             (S3 [NotEnough,Support,Two,Found])          (S3 s)                        (S7 s)
          TwoNotBuy1         |            ----->           |
             (S7 [NotEnough,Support,Two,Found])           End                          (S7 s)
          TwoFailed          |                           ----->                          |
                            End                           End                           End
                                                        Terminal
                                                                                                                        
```
