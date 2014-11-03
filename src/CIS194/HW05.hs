{-
Name: Myles Megyesi
Collaborators: none
Notes: I'm not in the class bro, I just like Haskell
-}

module CIS194.HW05
    ( Mod5(MkMod)
    ) where

import CIS194.Ring
    ( Parsable
    , Ring

    , add
    , addId
    , addInv
    , mul
    , mulId
    , parse
    )
-- import CIS194.Parser

data Mod5 = MkMod Integer
            deriving (Show, Eq)

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod i) = MkMod (negate i)
  mulId = MkMod 1

  add (MkMod i1) (MkMod i2) = MkMod ((i1 + i2) `mod` 5)
  mul (MkMod i1) (MkMod i2) = MkMod ((i1 * i2) `mod` 5)

instance Parsable Mod5 where
  parse s =
      case intParseResult of
          Just (i, leftover) -> Just ((MkMod i), leftover)
          Nothing -> Nothing
    where intParseResult = parse s
