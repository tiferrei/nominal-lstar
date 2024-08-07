{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Examples.Adversarial where

{- In this file we define the running example of the paper
   The language is L_n = { ww | w \in A } for any alphabet A,
   as long as
   In terms of orbits, the minimal acceptor is quite large,
   but in terms of FO definable sets it is quite small.
-}

import           NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import           Data.List    (reverse)
import           Prelude      (Eq, Ord, Show, ($), (.), (-))
import qualified Prelude      ()

import           GHC.Generics (Generic)

a :: Atom
a = constant 1

d :: Atom
d = constant 4

e :: Atom
e = constant 5

data Adversarial = AdvInitial | AdvStore Atom | AdvAccept
  deriving (Eq, Ord, Show, Generic, Nominal)

data Extended = ExtInitial | ExtStore Atom | ExtAccept | ExtD | ExtDD
  deriving (Eq, Ord, Show, Generic, Nominal)

mapNotD :: (Nominal a) => (Atom -> a) -> Set Atom -> Set a
mapNotD func =
  mapFilter (\a -> maybeIf (a `neq` d) (func a))

adversarial = automatonWithTrashCan
    (singleton AdvInitial
        `union` mapNotD AdvStore atoms
        `union` singleton AdvAccept)
    atoms
    (mapNotD (\a -> (AdvInitial, a, AdvStore a)) atoms
        `union` mapNotD (\a -> (AdvStore a, a, AdvAccept)) atoms)
    (singleton AdvInitial)
    (singleton AdvAccept)

amicableAux = automatonWithTrashCan
  (singleton AdvInitial
        `union` map AdvStore (delete d atoms)
        `union` singleton AdvAccept)
    (delete d atoms)
    (map (\a -> (AdvInitial, a, AdvStore a)) (delete d atoms)
        `union` map (\a -> (AdvStore a, a, AdvAccept)) (delete d atoms))
    (singleton AdvInitial)
    (singleton AdvAccept)

amicable =
  let aut@(Automaton q a t i f) = amicableAux in
  Automaton q (delete d atoms) t i f

extended = automatonWithTrashCan
    (singleton ExtInitial
        `union` map ExtStore atoms
        `union` singleton ExtAccept
        `union` singleton ExtD
        `union` singleton ExtDD)
    atoms
    (mapNotD (\a -> (ExtInitial, a, ExtStore a)) atoms
        `union` mapNotD (\a -> (ExtStore a, a, ExtAccept)) atoms
        `union` singleton (ExtInitial, d, ExtD)
        `union` singleton (ExtD, d, ExtDD)
        `union` singleton (ExtDD, d, ExtAccept))
    (singleton ExtInitial)
    (singleton ExtAccept)

