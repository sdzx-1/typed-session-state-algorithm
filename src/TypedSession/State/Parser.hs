module TypedSession.State.Parser where

import TypedSession.State.Pattern
import TypedSession.State.Type (Creat, Protocol)

-- data PingPong

data PingPongRole = Client | Server | Counter
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data PingPongBranchSt = STrue | SFalse
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

{-

1. protocol name
2. role name
3. branch state (options)

-}

-- v1 :: Protocol Creat PingPongRole PingPongBranchSt
-- v1 =
--   Label 0
--     :> Branch
--       Client
--       [ BranchSt STrue $
--           Msg "Ping" ["Int", "Int", "Int"] Client Server
--             :> Msg "Pong" [] Server Client
--             :> Msg "AddOne" [] Client Counter
--             :> Goto 0
--       , BranchSt SFalse $
--           Msg "Stop" [] Client Server
--             :> Msg "CStop" [] Client Counter
--             :> Terminal
--       ]

{-

pingpongProtocol = protocol @PingPongRole @PingPongBranchSt "PingPong"

[pingpongProtocol |
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
-}