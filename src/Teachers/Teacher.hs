{-# language RankNTypes #-}
module Teachers.Teacher where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.List (find)

import NLambda
import Prelude hiding (map, sum)
import qualified Prelude (map)

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

cacheOracle :: (Show i, Nominal i) => ([i] -> Formula) -> [i] -> Formula
cacheOracle mem q = unsafePerformIO $ do
    cache <- readIORef cacheState
    case find (\(i, _) -> i == q) cache of
        Nothing -> do
            let o = mem q
            putStrLn $ "[DEBUG] cache miss: " ++ show q ++ " -> " ++ show o
            writeIORef cacheState ((q, o) : cache)
            return o
        Just (_, o) -> do
            putStrLn $ "[DEBUG] cache hit: " ++ show q ++ " -> " ++ show o
            return o
    where
        cacheState = unsafePerformIO $ newIORef []

mqGeneraliser :: (Show i, Nominal i, Contextual i) => IORef [Atom] -> ([i] -> Formula) -> Set [i] -> Set ([i], Formula)
mqGeneraliser constsState mem qs = unsafePerformIO $ do
    consts <- readIORef constsState
    let oracle = cacheOracle mem
    let queries = toList . mapFilter id . setOrbitsRepresentatives $ qs
    let answers = Prelude.map (\q -> orbit consts (q, oracle q)) queries
    return . simplify . sum . fromList $ answers

eqGeneraliser :: (Show i, Nominal i, Contextual i) => IORef [Atom] -> ([i] -> Formula) -> (Automaton q i -> Maybe [i]) -> Automaton q i -> Either [Atom] (Maybe (Set [i]))
eqGeneraliser constsState mem equiv hyp = unsafePerformIO $ do
    let answer = equiv hyp
    let oracle = cacheOracle mem
    case answer of
        Nothing -> return (Right Nothing)
        Just cex -> do
            -- I will add state into whatever i want.
            consts <- readIORef constsState
            putStrLn ("[DEBUG] current constants: " ++ show consts)
            putStrLn ("[DEBUG] cex given: " ++ show cex)
            let abstract = orbit consts cex
                rep = head . toList . mapFilter id . setOrbitsRepresentatives $ abstract
            putStrLn ("[DEBUG] abstracted: " ++ show abstract)
            putStrLn ("[DEBUG] representative: " ++ show rep)
            -- FIXME: Hopefully this is short-circuiting.
            if cex == rep || isTrue (oracle cex <==> oracle rep) then
                return (Right (Just (orbit consts cex)))
            else do
                -- FIXME: Need to work out the logic to isolate the constant.
                -- For now, just add everything. Not minimal / efficient but
                -- should be correct.
                let newConstants = consts ++ leastSupport cex ++ leastSupport rep
                putStrLn ("[DEBUG] new constants: " ++ show newConstants)
                writeIORef constsState newConstants
                return (Left newConstants)
