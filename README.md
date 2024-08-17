Automatically generate status for typed-communication-protocol.

```haskell
data Role = Client | Server | Counter
  deriving (Show, Eq, Ord, Enum, Bounded)

v1 :: Protocol Creat Role Bool
v1 =
  Label 0
    :> Branch
      Client
      [ BranchSt True $
          Msg "Ping" ["Int", "Int", "Int"] Client Server
            :> Msg "Pong" [] Server Client
            :> Msg "AddOne" [] Client Counter
            :> Goto 0
      , BranchSt False $
          Msg "Stop" [] Client Server
            :> Msg "CStop" [] Client Counter
            :> Terminal
      ]

{-
>>> error $ fromRight "" $ genAllDoc (StrFillEnv 20 10)  v1 "Role" "PingPong" "Bool" ["Type"]
-}

```

This will generate the following code:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Type where
import Data.IFunctor (Sing, SingI (sing))
import Data.Kind
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import TypedProtocol.Core
{-
---------------------------Client--------------Server-------------Counter
LABEL 0                     S0 s                S0 s                S1 s
  [Branch] Client           S0 s                S0 s                S1 s
    * BranchSt True
    Ping                 S0 True->             ->S0 s               S1 s
    Pong                    S2<-                <-S2                S1 s
    AddOne               S1 True->              S0 s               ->S1 s
    ^ Goto 0                S0 s                S0 s                S1 s
    * BranchSt False
    Stop                 S0 False->            ->S0 s               S1 s
    CStop                S1 False->             End                ->S1 s
    ~ Terminal              End                 End                 End
-}
data SRole :: Role -> Type where
  SClient :: SRole Client
  SServer :: SRole Server
  SCounter :: SRole Counter
type instance Sing = SRole
instance SingI Client  where
  sing = SClient
instance SingI Server  where
  sing = SServer
instance SingI Counter  where
  sing = SCounter
instance SingToInt Role where
  singToInt x = I# (dataToTag# x)
data PingPongSt
  = End
  | S0 Bool
  | S1 Bool
  | S2
data SPingPongSt :: PingPongSt -> Type where
  SEnd :: SPingPongSt End
  SS0 :: SPingPongSt (S0 s)
  SS1 :: SPingPongSt (S1 s)
  SS2 :: SPingPongSt S2
type instance Sing = SPingPongSt
instance SingI End where
  sing = SEnd
instance SingI (S0 s) where
  sing = SS0
instance SingI (S1 s) where
  sing = SS1
instance SingI S2 where
  sing = SS2
instance SingToInt PingPongSt where
  singToInt x = I# (dataToTag# x)
instance Protocol Role PingPongSt where
  type Done Client = End
  type Done Server = End
  type Done Counter = End
  data Msg Role PingPongSt from send recv where
    Ping :: Int->Int->Int -> Msg Role PingPongSt (S0 True) '(Client,S2) '(Server,S2)
    Pong ::   Msg Role PingPongSt (S2) '(Server,S0 s) '(Client,S1 True)
    AddOne ::   Msg Role PingPongSt (S1 True) '(Client,S0 s) '(Counter,S1 s)
    Stop ::   Msg Role PingPongSt (S0 False) '(Client,S1 False) '(Server,End)
    CStop ::   Msg Role PingPongSt (S1 False) '(Client,End) '(Counter,End)
```