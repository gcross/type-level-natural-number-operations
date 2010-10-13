{- Copyright (c) 2010, Gregory M. Crosswhite. All rights reserved. -}

{-# LANGUAGE TypeFamilies #-}

module TypeLevel.NaturalNumber.Operations where

import TypeLevel.NaturalNumber

-- | The 'Plus' type family provides a function that adds two
-- type-level natural numbers.  This function is implemented by
-- induction on the first argument --- a fact that one should be aware
-- of when using it in function signatures.
type family Plus m n
-- | base case
type instance Plus Zero n = n
-- | inductive case
type instance Plus (SuccessorTo m) n = SuccessorTo (Plus m n)

-- | The 'Plus' type family provides a function that subtracts two
-- type-level natural numbers.  This function is implemented by
-- induction on both arguments, with the base case being when the
-- second argument is Zero.
type family Minus m n
-- | base case
type instance Minus m Zero = m
-- | inductive case
type instance Minus (SuccessorTo m) (SuccessorTo n) = Minus m n
