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

p :: R BookRole ()
p =
  (Buyer --> Seller) "Title"
    :> branch
      [ branchVal "BookNotFound" $
          (Buyer <-- Seller) "BookNotFoun"
            :> (Buyer --> Buyer2) "SellerNotFoundBook"
            :> terminal
      , branchVal "BookFound" $
          (Buyer <-- Seller) "Price"
            :> (Buyer --> Buyer2) "PriceToBuyer2"
            :> (Buyer <-- Buyer2) "HalfPrice"
            :> branch
              [ branchVal "EnoughBudget" $
                  ( (Buyer --> Seller) "Afford"
                      :> (Buyer <-- Seller) "Data"
                      :> (Buyer --> Buyer2) "Success"
                      :> terminal
                  )
              , branchVal "NotEnoughBudget" $
                  ( (Buyer --> Seller) "NotBuy"
                      :> (Buyer --> Buyer2) "Failed"
                      :> terminal
                  )
              ]
      ]

v2 :: [Char]
v2 =
  pipleR
    [Buyer, Seller, Buyer2]
    bookRoleToInt
    p

{-

>>> error v2
-----------------Buyer---------------Seller--------------Buyer2-----------------
                   S0                  S0                 S1 s
       Title       |        --->       |
                  S2 s                S2 s                S1 s
    ------------------------------BookNotFound------------------------------
                  S2 s          S2 BookNotFound           S1 s
    BookNotFoun    |        <---       |
            S1 BookNotFound           End                 S1 s
  SellerNotFoundB  |                  --->                 |
                  End                 End                 End
                                    Terminal

    -------------------------------BookFound--------------------------------
                  S2 s            S2 BookFound            S1 s
       Price       |        <---       |
              S1 BookFound            S3 s                S1 s
   PriceToBuyer2   |                  --->                 |
                   S4                 S3 s                 S4
     HalfPrice     |                  <---                 |
                  S3 s                S3 s                S5 s
        --------------------------EnoughBudget--------------------------
            S3 EnoughBudget           S3 s                S5 s
       Afford      |        --->       |
                   S6                  S6                 S5 s
        Data       |        <---       |
            S5 EnoughBudget           End                 S5 s
      Success      |                  --->                 |
                  End                 End                 End
                                    Terminal

        ------------------------NotEnoughBudget-------------------------
           S3 NotEnoughBudget         S3 s                S5 s
       NotBuy      |        --->       |
           S5 NotEnoughBudget         End                 S5 s
       Failed      |                  --->                 |
                  End                 End                 End
                                    Terminal

-}

```
