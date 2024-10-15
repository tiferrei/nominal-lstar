{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Examples.Adversarial where

{- In this file we define the running example of the paper
   The language is L_n = { ww | w \in A } for any alphabet A,
   as long as
   In terms of orbits, the minimal acceptor is quite large,
   but in terms of FO definable sets it is quite small.
-}

import           NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import           Prelude     (Eq, Ord, Show, Read, Maybe)
import           Data.List    ()

import           GHC.Generics (Generic)

a :: Atom
a = constant 1

d :: Atom
d = constant 4

e :: Atom
e = constant 5

data AdvAlpha a = Common a | Special a
  deriving (Eq, Ord, Show, Read, Generic, Nominal, Contextual)

advAlpha :: Set (AdvAlpha Atom)
advAlpha =  map Special specials
    `union` map Common (difference atoms specials)
    where specials = fromList [d]

data Adversarial a = AdvInitial | AdvStore a | AdvAccept
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

data Extended a = ExtInitial | ExtStore a | ExtAccept | ExtD | ExtDD
  deriving (Eq, Ord, Show, Generic, Nominal, Contextual)

mapNotSpecial :: (Nominal a) => (Atom -> a) -> Set (AdvAlpha Atom) -> Set a
mapNotSpecial func =
  mapFilter (\case
     Special _ -> nothing
     Common c -> just (func c))

adversarial :: Automaton (Maybe (Adversarial Atom)) Atom
adversarial = automatonWithTrashCan
    (singleton AdvInitial
        `union` mapNotSpecial AdvStore advAlpha
        `union` singleton AdvAccept)
    atoms
    (mapNotSpecial (\at -> (AdvInitial, at, AdvStore at)) advAlpha
        `union` mapNotSpecial (\at -> (AdvStore at, at, AdvAccept)) advAlpha)
    (singleton AdvInitial)
    (singleton AdvAccept)

extended :: Automaton (Maybe (Extended Atom)) Atom
extended = automatonWithTrashCan
    (singleton ExtInitial
        `union` mapNotSpecial ExtStore advAlpha
        `union` singleton ExtAccept
        `union` singleton ExtD
        `union` singleton ExtDD)
    atoms
    (mapNotSpecial (\at -> (ExtInitial, at, ExtStore at)) advAlpha
        `union` singleton (ExtInitial, d, ExtD)
        `union` singleton (ExtD, d, ExtDD)
        `union` singleton (ExtDD, d, ExtAccept)
        `union` mapNotSpecial (\at -> (ExtStore at, at, ExtAccept)) advAlpha)
    (singleton ExtInitial)
    (singleton ExtAccept)

