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

lru1Faulty :: Atom -> Mealy (LRU Atom) Atom LRUOut
lru1Faulty special = mealy
    ss
    LRUSInit
    atoms
    (fromList [Hit, Miss])
    (map (\q -> (q, special, Miss, q)) ss
        `union` map (\a -> (LRUSInit, a, Miss, LRUS1 a)) commons
        `union` map (\a -> (LRUS1 a, a, Hit, LRUS1 a)) commons
        `union` map (\(a, b) -> (LRUS1 a, b, Miss, LRUS1 b)) differentCommonsPairs)
    where
        commons = filter (`neq` special) atoms
        differentCommonsPairs = pairsWithFilter (\a b -> maybeIf (a`neq` b) (a, b)) commons commons
        ss = singleton LRUSInit
            `union` map LRUS1 commons

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

-- A type of LRU2 that has a fault whereby it does not cache a specific element.
lru2Faulty :: Atom -> Mealy (LRU Atom) Atom LRUOut
lru2Faulty special = mealy
    ss
    LRUSInit
    atoms
    (fromList [Hit, Miss])
    (map (\q -> (q, special, Miss, q)) ss
        `union` map (\a -> (LRUSInit, a, Miss, LRUS1 a)) commons
        `union` map (\a -> (LRUS1 a, a, Hit, LRUS1 a)) commons
        `union` map (\(a, b) -> (LRUS1 a, b, Miss, LRUS2 (a, b))) differentCommonsPairs
        `union` map (\a -> (LRUS2 (a, a), a, Hit, LRUS2 (a, a))) commons
        `union` map (\(a, b) -> (LRUS2 (a, b), a, Hit, LRUS2 (b, a))) differentCommonsPairs
        `union` map (\(a, b) -> (LRUS2 (a, b), b, Hit, LRUS2 (a, b))) differentCommonsPairs
        `union` map (\(a, b, c) -> (LRUS2 (a, b), c, Miss, LRUS2 (b, c))) differentCommonsTriples)
    where
        commons = filter (`neq` special) atoms
        commonsPairs = pairs commons commons
        differentCommonsPairs = pairsWithFilter (\a b -> maybeIf (a`neq` b) (a, b)) commons commons
        differentCommonsTriples = triplesWithFilter (\a b c -> maybeIf (not (eq a b \/ eq b c \/ eq a c)) (a, b, c)) commons commons commons
        ss = singleton LRUSInit
            `union` map LRUS1 commons
            `union` map LRUS2 commonsPairs
