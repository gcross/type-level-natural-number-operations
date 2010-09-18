{- Copyright (c) 2010, Gregory M. Crosswhite. All rights reserved. -}

{-# LANGUAGE TypeFamilies #-}

module TypeLevel.NaturalNumber.Operations where

import TypeLevel.NaturalNumber

type family Plus m n
type instance Plus Zero n = n
type instance Plus (SuccessorTo m) n = SuccessorTo (Plus m n)

type family Minus m n
type instance Minus m Zero = m
type instance Minus (SuccessorTo m) (SuccessorTo n) = Minus m n
