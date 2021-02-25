{-# language FlexibleInstances #-}
{-# language RankNTypes #-}

module Teacher
    ( module Teachers.Teacher
    , mqToBool
    , teacherWithTarget
    , teacherWithTargetNonDet
    , teacherWithIO
    , teacherWithIO2
    , teacherWithTargetAndIO
    ) where

import Teachers.Teacher
import Teachers.Terminal
import Teachers.Whitebox

import NLambda hiding (alphabet)
import qualified NLambda (alphabet)
import Prelude hiding (map)


-- The teacher interface is slightly inconvenient
-- But this is for a good reason. The type [i] -> o
-- doesn't work well in nlambda
mqToBool :: NominalType i => Teacher i -> Set [i] -> Set ([i], Bool)
mqToBool teacher qs = answer
    where
        realQ = membership teacher qs
        (inw, outw) = partition snd realQ
        answer = map (setB True) inw `union` map (setB False) outw
        setB b (w, _) = (w, b)


-- We provide three ways to construct teachers:
-- 1. Fully automatic
-- 2. Fully interactive (via IO)
-- 3. Automatic membership, but interactive equivalence tests
-- Furthermore we provide a teacher which counts and then passes the query
-- to a delegate.

-- 1. This is a fully automatic teacher, which has an internal automaton
-- Only works for DFAs for now, as those can be checked for equivalence
teacherWithTarget :: (NominalType i, NominalType q) => Automaton q i -> Teacher i
teacherWithTarget aut = Teacher
    { membership = foreachQuery $ accepts aut
    , equivalent = automaticEquivalent bisim aut
    , alphabet   = NLambda.alphabet aut
    }

-- 1b. This is a fully automatic teacher, which has an internal automaton
-- NFA have undecidable equivalence, n is a bound on deoth of bisimulation.
teacherWithTargetNonDet :: (Show i, Show q, NominalType i, NominalType q) => Int -> Automaton q i -> Teacher i
teacherWithTargetNonDet n aut = Teacher
    { membership = foreachQuery $ accepts aut
    , equivalent = automaticEquivalent (bisimNonDet n) aut
    , alphabet   = NLambda.alphabet aut
    }

-- 2. Will ask everything to someone reading the terminal
-- For the moment only Atom as input type
-- Note that parsing is very unforgiving, one mistake, and there is no way back
-- Atoms are referenced by Ints. When the user provides a counter example, we
-- consider the whole orbit generated by it.
teacherWithIO :: (Show i, Read i, NominalType i, Contextual i) => Set i -> Teacher i
teacherWithIO alph = Teacher
    { membership = ioMembership
    , equivalent = ioEquivalent
    , alphabet   = alph
    }

-- 2b. Same as above. But with machine readable queries (except for EQs maybe)
teacherWithIO2 :: (Show i, Read i, NominalType i, Contextual i) => Set i -> Teacher i
teacherWithIO2 alph = Teacher
    { membership = ioMembership2
    , equivalent = ioEquivalent2
    , alphabet   = alph
    }

-- 3. A teacher uses a target for the mebership queries, but you for equivalence
-- Useful as long as you don't have an equivalence check
-- used for NFAs when there was no bounded bisimulation yet
teacherWithTargetAndIO :: (Show i, Read i, NominalType i, Contextual i, NominalType q) => Automaton q i -> Teacher i
teacherWithTargetAndIO aut = Teacher
    { membership = foreachQuery $ accepts aut
    , equivalent = ioEquivalent
    , alphabet   = NLambda.alphabet aut
    }

automaticEquivalent :: (p1 -> p2 -> Set a) -> p1 -> p2 -> Maybe (Set a)
automaticEquivalent bisimlator aut hypo = case solve isEq of
        Nothing    -> error "should be solved"
        Just True  -> Nothing
        Just False -> Just bisimRes
        where
            bisimRes = bisimlator aut hypo
            isEq = isEmpty bisimRes
