{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Type where

infixr 5 :>

data Arrow role' ann = Arrow ann role' role' String
  deriving (Functor)

data BranchVal role' ann = BranchVal String (R role' ann)

instance Functor (BranchVal role') where
  fmap f (BranchVal st r) = BranchVal st (fmap f r)

data R role' ann
  = Terminal ann
  | Arrow role' ann :> R role' ann
  | Branch ann [BranchVal role' ann]
  deriving (Functor)

(-->) :: role' -> role' -> String -> Arrow role' ()
a --> b = Arrow () a b

(<--) :: role' -> role' -> String -> Arrow role' ()
b <-- a = a --> b

terminal :: R role' ()
terminal = Terminal ()

branchVal :: String -> R role' () -> BranchVal role' ()
branchVal = BranchVal

branch :: [BranchVal role' ()] -> R role' ()
branch = Branch ()

firstArrowAnn :: BranchVal role' ann -> ann
firstArrowAnn (BranchVal _ (Arrow ann _ _ _ :> _)) = ann
firstArrowAnn _ = error "np"

rAnnToList :: R role' ann -> [ann]
rAnnToList = \case
  Terminal ann -> [ann]
  Arrow ann _ _ _ :> r -> ann : rAnnToList r
  Branch ann vs -> ann : concatMap (\(BranchVal _ r) -> rAnnToList r) vs

rToAnn :: R role' ann -> ann
rToAnn = \case
  Terminal ann -> ann
  Arrow ann _ _ _ :> _ -> ann
  Branch ann _ -> ann
