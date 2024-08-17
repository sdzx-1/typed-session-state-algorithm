{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module N.N3 where

import Data.Either (fromRight)
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits
import N.N1
import N.N2
import N.Render
import N.Type (Creat, Protocol, ProtocolError)
import N.Utils
import Prettyprinter
import Prettyprinter.Render.String (renderString)

genRole :: forall r ann. (Enum r, Bounded r, Show r) => String -> Doc ann
genRole rname =
  let rg = rRange @r
      sRole = pretty ("S" <> rname)
      header = "data" <+> sRole <+> "::" <+> pretty rname <+> "-> Type where"
   in vsep
        [ nest 2 $ vsep $ header : [(pretty ("S" <> show r) <+> "::" <+> sRole <+> pretty (show r)) | r <- rg]
        , "type" <+> "instance" <+> "Sing" <+> "=" <+> sRole
        ]
        <> line
        <> vsep
          [ nest
              2
              ( vsep
                  [ "instance" <+> "SingI" <+> pretty (show r) <+> " where"
                  , "sing" <+> "=" <+> pretty ("S" <> show r)
                  ]
              )
          | r <- rg
          ]
        <> line
        <> ( nest 2 $
              vsep
                [ "instance SingToInt" <+> pretty rname <+> "where"
                , "singToInt x = I# (dataToTag# x)"
                ]
           )

genSt :: forall r bst ann. String -> String -> PipleResult r bst -> Doc ann
genSt protName bstName (PipleResult{dnySet, stBound = (start, end)}) =
  let
    protNameSt = protName <> "St"
    sProtNameSt = "S" <> protName <> "St"
    protSt =
      nest 2 $
        vsep $
          ["data" <+> pretty protNameSt]
            ++ [ if
                  | i == -1 -> "= End"
                  | i `Set.member` dnySet -> pretty ("| S" ++ show i) <+> pretty bstName
                  | otherwise -> pretty ("| S" ++ show i)
               | i <- [start .. end]
               ]
    protSSt =
      nest 2 $
        vsep $
          ["data" <+> pretty sProtNameSt <+> "::" <+> pretty protNameSt <+> "-> Type where"]
            ++ [ if
                  | i == -1 -> "SEnd ::" <+> pretty sProtNameSt <+> "End"
                  | i `Set.member` dnySet -> pretty ("SS" ++ show i ++ " ::") <+> pretty sProtNameSt <+> pretty ("(S" <> show i <> " s)")
                  | otherwise -> pretty ("SS" ++ show i ++ " ::") <+> pretty sProtNameSt <+> pretty ("S" <> show i)
               | i <- [start .. end]
               ]
    instVal i =
      nest 2 $
        vsep $
          [ "instance SingI"
              <+> ( if i == -1
                      then "End"
                      else
                        if i
                          `Set.member` dnySet
                          then parens (pretty ("S" <> show i) <> " s")
                          else pretty ("S" <> show i)
                  )
              <+> "where"
          , "sing =" <+> if i == -1 then "SEnd" else pretty ("SS" <> show i)
          ]
    instVals = vsep [instVal i | i <- [start .. end]]
    stoInt =
      ( nest 2 $
          vsep
            [ "instance SingToInt" <+> pretty protNameSt <+> "where"
            , "singToInt x = I# (dataToTag# x)"
            ]
      )
   in
    vsep
      [ protSt
      , protSSt
      , "type instance Sing =" <+> pretty sProtNameSt
      , instVals
      , stoInt
      ]

genProtIns :: forall r bst ann. (Enum r, Bounded r, Show bst, Show r) => String -> String -> PipleResult r bst -> Doc ann
genProtIns roleName protName PipleResult{msgT1} =
  let
    protNameSt = protName <> "St"
    typeDone = ["type Done" <+> pretty (show r) <+> "= End" | r <- rRange @r]
   in
    nest 2 $
      vsep
        [ "instance Protocol" <+> pretty roleName <+> pretty protNameSt <+> "where"
        , vsep typeDone
        , nest 2 $
            vsep
              [ "data Msg" <+> pretty roleName <+> pretty protNameSt <+> "from send recv where"
              , vsep (genDoc roleName protName msgT1)
              ]
        ]

genGraph :: (Enum r, Bounded r, Show bst, Ord r, Show r) => PipleResult r bst -> String
genGraph PipleResult{msgT} = runRender stMsgT msgT

genAllDoc'
  :: forall r bst ann
   . (Enum r, Bounded r, Ord r, Show r, Show bst)
  => Protocol Creat r bst
  -> String -- role name
  -> String -- protocol name
  -> String -- bst name
  -> [String] -- module Name
  -> Either (ProtocolError r bst) (Doc ann)
genAllDoc' prot rName pName bstName moduleNames = case piple prot of
  Left e -> Left e
  Right pipResult1 ->
    Right $
      vsep
        [ "{-# LANGUAGE DataKinds #-}"
        , "{-# LANGUAGE FlexibleInstances #-}"
        , "{-# LANGUAGE GADTs #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "{-# LANGUAGE MultiParamTypeClasses #-}"
        , "{-# LANGUAGE TypeFamilies #-}"
        , "module" <+> pretty (L.intercalate "." moduleNames) <+> "where"
        , "import Data.IFunctor (Sing, SingI (sing))"
        , "import Data.Kind"
        , "import GHC.Exts (dataToTag#)"
        , "import GHC.Int (Int (I#))"
        , "import TypedProtocol.Core"
        , "{-"
        , pretty $ genGraph pipResult1
        , "-}"
        , genRole @r rName
        , genSt pName bstName pipResult1
        , genProtIns rName pName pipResult1
        ]

genAllDoc
  :: forall r bst
   . (Enum r, Bounded r, Ord r, Show r, Show bst)
  => Protocol Creat r bst
  -> String -- role name
  -> String -- protocol name
  -> String -- bst name
  -> [String] -- module names
  -> Either (ProtocolError r bst) String
genAllDoc a b c d e =
  renderString . layoutPretty defaultLayoutOptions
    <$> genAllDoc' a b c d e

genAllFile
  :: forall r bst
   . (Enum r, Bounded r, Ord r, Show r, Show bst)
  => Protocol Creat r bst
  -> String -- role name
  -> String -- protocol name
  -> String -- bst name
  -> [String] -- module names
  -> IO ()
genAllFile a b c d e = case genAllDoc a b c d e of
  Left er -> print er
  Right st -> do
    let name = case e of
          [] -> "Type"
          xs -> last xs
    writeFile name st

---------------------------example---------------------------------------
-- data Role
--   = Buyer
--   | Seller
--   | Buyer2
--   deriving (Show, Eq, Ord, Enum, Bounded)

-- data BookBranchSt
--   = NotFound
--   | Found
--   | One
--   | Two
--   | Support
--   | NotSupport
--   | Enough
--   | NotEnough
--   deriving (Show)

-- v2 :: Protocol Creat Role BookBranchSt
-- v2 =
--   Label 0
--     :> Msg "Title" ["String"] Buyer Seller
--     :> Branch
--       Seller
--       [ BranchSt NotFound $
--           Msg "NoBook" [] Seller Buyer
--             :> Msg "SellerNoBook" [] Buyer Buyer2
--             :> Goto 0
--       , BranchSt Found $
--           Msg "Price" ["Int"] Seller Buyer
--             :> Branch
--               Buyer
--               [ BranchSt One $
--                   Msg "OneAfford" [] Buyer Buyer2
--                     :> Msg "OneAccept" [] Buyer Seller
--                     :> Msg "OneDate" ["Int"] Seller Buyer
--                     :> Msg "OneSuccess" ["Int"] Buyer Buyer2
--                     :> Goto 0
--               , BranchSt Two $
--                   Msg "PriceToBuyer2" ["Int"] Buyer Buyer2
--                     :> Branch
--                       Buyer2
--                       [ BranchSt NotSupport $
--                           Msg "NotSupport1" [] Buyer2 Buyer
--                             :> Msg "TwoNotBuy" [] Buyer Seller
--                             :> Goto 0
--                       , BranchSt Support $
--                           Msg "SupportVal" ["Int"] Buyer2 Buyer
--                             :> Branch
--                               Buyer
--                               [ BranchSt Enough $
--                                   Msg "TwoAccept" [] Buyer Seller
--                                     :> Msg "TwoDate" ["Int"] Seller Buyer
--                                     :> Msg "TwoSuccess" ["Int"] Buyer Buyer2
--                                     :> Goto 0
--                               , BranchSt NotEnough $
--                                   Msg "TwoNotBuy1" [] Buyer Seller
--                                     :> Msg "TwoFailed" [] Buyer Buyer2
--                                     :> Terminal
--                               ]
--                       ]
--               ]
--       ]

{-
>>> error $ fromRight "" $ genAllDoc v2 "Role" "Book" "BookBranchSt"

-}

---------------------------example---------------------------------------

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

--- >>>  genAllFile v1 "Role" "PingPong" "Bool" ["PingPong","Type"]

{-

>>> error $ fromRight "" $ genAllDoc v1 "Role" "PingPong" "Bool" ["Type"]
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
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                         S0 s                          S0 s                          S1 s
  [Branch] Client                               S0 s                          S0 s                          S1 s
    * BranchSt True
    Ping                                     S0 True->                       ->S0 s                         S1 s
    Pong                                        S2<-                          <-S2                          S1 s
    AddOne                                   S1 True->                        S0 s                         ->S1 s
    ^ Goto 0                                    S0 s                          S0 s                          S1 s
    * BranchSt False
    Stop                                     S0 False->                      ->S0 s                         S1 s
    CStop                                    S1 False->                       End                          ->S1 s
    ~ Terminal                                  End                           End                           End
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
    Ping ::   Msg Role PingPongSt (S0 True) '(Client,S2) '(Server,S2)
    Pong ::   Msg Role PingPongSt (S2) '(Server,S0 s) '(Client,S1 True)
    AddOne ::   Msg Role PingPongSt (S1 True) '(Client,S0 s) '(Counter,S1 s)
    Stop ::   Msg Role PingPongSt (S0 False) '(Client,S1 False) '(Server,End)
    CStop ::   Msg Role PingPongSt (S1 False) '(Client,End) '(Counter,End)
{-
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                         S0 s                          S0 s                          S1 s
  [Branch] Client                               S0 s                          S0 s                          S1 s
    * BranchSt True
    Ping                                     S0 True->                       ->S0 s                         S1 s
    Pong                                        S2<-                          <-S2                          S1 s
    AddOne                                   S1 True->                        S0 s                         ->S1 s
    ^ Goto 0                                    S0 s                          S0 s                          S1 s
    * BranchSt False
    Stop                                     S0 False->                      ->S0 s                         S1 s
    CStop                                    S1 False->                       End                          ->S1 s
    ~ Terminal                                  End                           End                           End
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
    Ping ::   Msg Role PingPongSt (S0 True) '(Client,S2) '(Server,S2)
    Pong ::   Msg Role PingPongSt (S2) '(Server,S0 s) '(Client,S1 True)
    AddOne ::   Msg Role PingPongSt (S1 True) '(Client,S0 s) '(Counter,S1 s)
    Stop ::   Msg Role PingPongSt (S0 False) '(Client,S1 False) '(Server,End)
    CStop ::   Msg Role PingPongSt (S1 False) '(Client,End) '(Counter,End)
{-
-----------------------------------------------Client------------------------Server-----------------------Counter
LABEL 0                                         S0 s                          S0 s                          S1 s
  [Branch] Client                               S0 s                          S0 s                          S1 s
    * BranchSt True
    Ping                                     S0 True->                       ->S0 s                         S1 s
    Pong                                        S2<-                          <-S2                          S1 s
    AddOne                                   S1 True->                        S0 s                         ->S1 s
    ^ Goto 0                                    S0 s                          S0 s                          S1 s
    * BranchSt False
    Stop                                     S0 False->                      ->S0 s                         S1 s
    CStop                                    S1 False->                       End                          ->S1 s
    ~ Terminal                                  End                           End                           End
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
    Ping ::   Msg Role PingPongSt (S0 True) '(Client,S2) '(Server,S2)
    Pong ::   Msg Role PingPongSt (S2) '(Server,S0 s) '(Client,S1 True)
    AddOne ::   Msg Role PingPongSt (S1 True) '(Client,S0 s) '(Counter,S1 s)
    Stop ::   Msg Role PingPongSt (S0 False) '(Client,S1 False) '(Server,End)
    CStop ::   Msg Role PingPongSt (S1 False) '(Client,End) '(Counter,End)

-}
