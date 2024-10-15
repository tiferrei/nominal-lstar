{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Examples.Mealy where

import           NLambda

-- Explicit Prelude, as NLambda has quite some clashes
import           Prelude     (Eq, Ord, Show, Read)

import           GHC.Generics (Generic)

data LRUState a = LRUInit | LRU1 a | LRU2 (a, a)
  deriving (Eq, Ord, Show, Read, Generic, Nominal, Contextual)

data LRUOut = Hit | Miss
  deriving (Eq, Ord, Show, Read, Generic, Nominal, Contextual)

lru1 :: Mealy (LRUState Atom) Atom LRUOut
lru1 = mealy
    (singleton LRUInit
        `union` map LRU1 atoms)
    LRUInit
    atoms
    (fromList [Hit, Miss])
    (map (\a -> (LRUInit, a, Miss, LRU1 a)) atoms
        `union` map (\a -> (LRU1 a, a, Hit, LRU1 a)) atoms
        `union` map (\(a, b) -> (LRU1 a, b, Miss, LRU1 b)) differentAtomsPairs)

lru2 :: Mealy (LRUState Atom) Atom LRUOut
lru2 = mealy
    (singleton LRUInit
        `union` map LRU1 atoms
        `union` map LRU2 atomsPairs)
    LRUInit
    atoms
    (fromList [Hit, Miss])
    (map (\a -> (LRUInit, a, Miss, LRU1 a)) atoms
        `union` map (\a -> (LRU1 a, a, Hit, LRU1 a)) atoms
        `union` map (\(a, b) -> (LRU1 a, b, Miss, LRU2 (a, b))) differentAtomsPairs
        `union` map (\(a, b) -> (LRU2 (a, b), a, Hit, LRU2 (b, a))) atomsPairs
        `union` map (\(a, b) -> (LRU2 (a, b), b, Hit, LRU2 (a, b))) atomsPairs
        `union` map (\(a, b, c) -> (LRU2 (a, b), c, Miss, LRU2 (b, c))) freshQ)
    where
        freshQ = triplesWithFilter (\a b c -> maybeIf (not (eq a c \/ eq a b)) (a, b, c)) atoms atoms atoms

lruBad :: Mealy (LRUState Atom) Atom LRUOut
lruBad = mealy
    (singleton LRUInit
        `union` map LRU1 atoms
        `union` map LRU2 atomsPairs)
    LRUInit
    atoms
    (fromList [Hit, Miss])
    (map (\a -> (LRUInit, a, Miss, LRU1 a)) atoms
        `union` map (\a -> (LRU1 a, a, Hit, LRU1 a)) atoms
        `union` map (\(a, b) -> (LRU1 a, b, Miss, LRU2 (a, b))) differentAtomsPairs
        `union` map (\(a, b) -> (LRU2 (a, b), a, Hit, LRU2 (a, b))) atomsPairs
        `union` map (\(a, b) -> (LRU2 (a, b), b, Hit, LRU2 (a, b))) atomsPairs
        `union` map (\(a, b, c) -> (LRU2 (a, b), c, Miss, LRU2 (b, c))) freshQ
        )
    where
        freshQ = triplesWithFilter (\a b c -> maybeIf (not (eq a c \/ eq a b)) (a, b, c)) atoms atoms atoms

