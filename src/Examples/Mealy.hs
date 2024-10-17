{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Examples.Mealy where

import           NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import           Prelude     (Eq, Ord, Show, Read, uncurry)

import           GHC.Generics (Generic)

data LRU a = LRUSInit | LRUS1 a | LRUS2 (a, a)
  deriving (Eq, Ord, Show, Read, Generic, Nominal, Contextual)

data LRUOut = Hit | Miss
  deriving (Eq, Ord, Show, Read, Generic, Nominal, Contextual)

lru1 :: Mealy (LRU Atom) Atom LRUOut
lru1 = mealy
    (singleton LRUSInit
        `union` map LRUS1 atoms)
    LRUSInit
    atoms
    (fromList [Hit, Miss])
    (map (\a -> (LRUSInit, a, Miss, LRUS1 a)) atoms
        `union` map (\a -> (LRUS1 a, a, Hit, LRUS1 a)) atoms
        `union` map (\(a, b) -> (LRUS1 a, b, Miss, LRUS1 b)) differentAtomsPairs)

lru2 :: Mealy (LRU Atom) Atom LRUOut
lru2 = mealy
    (singleton LRUSInit
        `union` map LRUS1 atoms
        `union` map LRUS2 atomsPairs)
    LRUSInit
    atoms
    (fromList [Hit, Miss])
    (map (\a -> (LRUSInit, a, Miss, LRUS1 a)) atoms
        `union` map (\a -> (LRUS1 a, a, Hit, LRUS1 a)) atoms
        `union` map (\(a, b) -> (LRUS1 a, b, Miss, LRUS2 (a, b))) differentAtomsPairs
        `union` map (\a -> (LRUS2 (a, a), a, Hit, LRUS2 (a, a))) atoms
        `union` map (\(a, b) -> (LRUS2 (a, b), a, Hit, LRUS2 (b, a))) differentAtomsPairs
        `union` map (\(a, b) -> (LRUS2 (a, b), b, Hit, LRUS2 (a, b))) differentAtomsPairs
        `union` map (\(a, b, c) -> (LRUS2 (a, b), c, Miss, LRUS2 (b, c))) differentAtomsTriples)
    where
        differentAtomsTriples = triplesWithFilter (\a b c -> maybeIf (not (eq a b \/ eq b c \/ eq a c)) (a, b, c)) atoms atoms atoms
