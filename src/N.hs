{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module N where

infixr 5 :>
data BranchSt r bst a = BranchSt a bst (M r bst a)
  deriving (Functor)

data MsgOrLabel eta r a
  = Msg a a String [String] r r
  | Label a Int
  deriving (Functor)

data M r bst a
  = (MsgOrLabel r bst a) :> (M r bst a)
  | Branch a r [BranchSt r bst a]
  | Goto a Int
  | Terminal a
  deriving (Functor)

bottomToUp :: M r bst a -> M r bst b
bottomToUp = undefined

{-
0 - - - - - - - - Label 0
1 - - - - - - - - Msg String [String] r r
_ - - - - - - - - Label 1
2 - - - - - - - - Msg String [String] r r
3 - - - - - - - - Goto 0
4 - - - - - - - - Msg String [String] r r

m a               label
a -> m a          Msg
-}

------------------------------------------
-- data BookRole
--   = Buyer
--   | Seller
--   | Buyer2
--   deriving (Show, Enum, Bounded, Eq, Ord, Ix)

-- >>> pppp
-- [Buyer,Seller,Buyer2]

-- pppp = [minBound @BookRole .. maxBound]

-- bookRoleToInt :: BookRole -> Int
-- bookRoleToInt = \case
--   Buyer -> 0
--   Seller -> 1
--   Buyer2 -> 2

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

-- pp :: M BookRole BookBranchSt
-- pp =
--   Label 0
--     :> Msg "Msg1" [] Buyer Seller
--     :> Msg "Msg2" [] Seller Buyer
--     :> Branch1
--       Seller
--       [ BranchSt One $
--           Msg "Msg3" [] Seller Buyer2
--             :> Msg "Msg4" [] Buyer2 Seller
--             :> Terminal1
--       , BranchSt Two $
--           Msg "Msg5" [] Seller Buyer2
--             :> Msg "Msg6" [] Buyer2 Seller
--             :> Terminal1
--       ]

-- interpret :: (Monad m) => M r bst -> m ()
-- interpret (Label n) = undefined
-- interpret (Msg constName params from to) = undefined
-- interpret (a :>> b) = undefined
-- interpret _ = undefined
