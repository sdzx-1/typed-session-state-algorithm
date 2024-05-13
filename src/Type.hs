{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Type where

infixr 5 :>

data Arrow role' ann = Arrow ann role' role' String
  deriving (Functor)

data BranchVal role' bSt ann = BranchVal bSt (R role' bSt ann)

instance Functor (BranchVal role' bSt) where
  fmap f (BranchVal st r) = BranchVal st (fmap f r)

data R role' bSt ann
  = Terminal ann
  | Arrow role' ann :> R role' bSt ann
  | Branch ann [BranchVal role' bSt ann]
  deriving (Functor)

(-->) :: role' -> role' -> String -> Arrow role' ()
a --> b = Arrow () a b

(<--) :: role' -> role' -> String -> Arrow role' ()
b <-- a = a --> b

terminal :: R role' bSt ()
terminal = Terminal ()

branchVal :: bSt -> R role' bSt () -> BranchVal role' bSt ()
branchVal = BranchVal

branch :: [BranchVal role' bSt ()] -> R role' bSt ()
branch = Branch ()

firstArrowAnn :: BranchVal role' bSt ann -> ann
firstArrowAnn (BranchVal _ (Arrow ann _ _ _ :> _)) = ann
firstArrowAnn _ = error "np"

rAnnToList :: R role' bSt ann -> [ann]
rAnnToList = \case
  Terminal ann -> [ann]
  Arrow ann _ _ _ :> r -> ann : rAnnToList r
  Branch ann vs -> ann : concatMap (\(BranchVal _ r) -> rAnnToList r) vs

rToAnn :: R role' bSt ann -> ann
rToAnn = \case
  Terminal ann -> ann
  Arrow ann _ _ _ :> _ -> ann
  Branch ann _ -> ann
