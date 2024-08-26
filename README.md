Automatically generate status for typed-communication-protocol.

```haskell
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
--   Msg <(1,2,0)> AddOne [] Client Counter
--   Msg <(2,3,1)> Ping [Int, Int, Int] Client Server
--   Msg <(3,4,2)> Pong [] Server Client
--   Goto 4 0
--   * BranchSt () SFalse
--   Msg <(5,6,0)> Stop [] Client Server
--   Msg <(6,7,1)> CStop [] Client Counter
--   Terminal 7
-- ,--------------------ReRank-----------------
-- fromList [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
-- ,--------------------Idx-----------------
-- Label 0 0
-- [Branch] 0 Client
--   * BranchSt () STrue
--   Msg <(1,2,0)> AddOne [] Client Counter
--   Msg <(2,3,1)> Ping [Int, Int, Int] Client Server
--   Msg <(3,4,2)> Pong [] Server Client
--   Goto 4 0
--   * BranchSt () SFalse
--   Msg <(5,6,0)> Stop [] Client Server
--   Msg <(6,7,1)> CStop [] Client Counter
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
-- ,--------------------VerifyResult Map-----------------
-- fromList [(1,(Client,Server)),(2,(Client,Counter)),(3,(Server,Client))]
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
--     AddOne                        {S2 STrue}->            S1 s               ->S2 s
--       Ping                         S1 STrue->            ->S1 s               S2 s
--       Pong                            S3<-                <-S3                S2 s
--       Goto 0                           S0                 S1 s                S2 s
--     Stop                         {S1 SFalse}->           ->S1 s               S2 s
--       CStop                       S2 SFalse->             End                ->S2 s
--       Terminal                        End                 End                 End
```