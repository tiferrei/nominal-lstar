{-# language RankNTypes #-}
module Teachers.Teacher where

import Data.IORef

import NLambda
import Prelude hiding (map)

-- If during a query, the oracle detects an inconsistency, the new constant
-- (Left a) is returned, otherwise (Right out) is returned.

-- Abstract teacher type. Maybe this will be generalized to some monad, so that
-- the teacher can have state (such as a cache).
-- TODO: add a notion of state, so that Teachers can maintain a cache, or
--       a socket, or file, or whatever (as long as it stays deterministic).
data Teacher i = Teacher
    -- A teacher provides a way to answer membership queries. You'd expect
    -- a function of type [i] -> Bool. But in order to implement some teachers
    -- more efficiently, we provide Set [i] as input, and the teacher is
    -- supposed to answer each element in the set.
    { membership :: Set [i] -> Set ([i], Formula)
    -- Given a hypothesis, returns Nothing when equivalence or a (equivariant)
    -- set of counter examples. Needs to be quantified over q, because the
    -- learner may choose the type of the state space.
    , equivalent :: forall q. (Show q, Nominal q) => Automaton q i -> Either [Atom] (Maybe (Set [i]))
    -- Returns the alphabet to the learner
    , alphabet   :: Set i
    -- Keeps track of isolated constants
    , constants :: IORef [Atom]
    }

-- Often a membership query is defined by a function [i] -> Formula. This wraps
-- such a function to the required type for a membership query (see above).
foreachQuery :: Nominal i => ([i] -> Formula) -> Set[i] -> Set ([i], Formula)
foreachQuery f = map (\q -> (q, f q))
