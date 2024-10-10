{-# language RankNTypes #-}
module Teachers.Teacher where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.List (find)

import NLambda
import Prelude hiding (map, sum)

-- Abstract teacher type. Maybe this will be generalized to some monad, so that
-- the teacher can have state (such as a cache).
-- TODO: add a notion of state, so that Teachers can maintain a cache, or
--       a socket, or file, or whatever (as long as it stays deterministic).
data Teacher i = Teacher
    -- A teacher provides a way to answer membership queries.
    { membership :: [i] -> Bool
    -- Given a hypothesis, returns Nothing when equivalence or a (equivariant)
    -- set of counter examples. Needs to be quantified over q, because the
    -- learner may choose the type of the state space.
    , equivalent :: forall q. (Show q, Nominal q) => Automaton q i -> Maybe ([Atom], [i])
    -- Returns the alphabet to the learner
    , alphabet   :: Set i
    -- Keeps track of isolated constants
    , constants :: IORef [Atom]
    }

cacheOracle :: (Show i, Nominal i) => ([i] -> Bool) -> [i] -> Bool
cacheOracle mem q = unsafePerformIO $ do
    cache <- readIORef cacheState
    out <- case find (\(i, _) -> i == q) cache of
        Nothing -> do
            let o = mem q
            putStrLn $ "[DEBUG] cache miss: " ++ show q ++ " -> " ++ show o
            writeIORef cacheState ((q, o) : cache)
            count <- readIORef counter
            writeIORef counter (count + 1)
            return o
        Just (_, o) -> do
            putStrLn $ "[DEBUG] cache hit: " ++ show q ++ " -> " ++ show o
            return o
    count <- readIORef counter
    putStrLn $ "[DEBUG] QUERIES: " ++ show count
    return out
    where
        cacheState = unsafePerformIO $ newIORef []
        counter :: IORef Integer
        counter = unsafePerformIO $ newIORef 0

eqGeneraliser :: (Show i, Nominal i, Contextual i) => IORef [Atom] -> ([i] -> Bool) -> (Automaton q i -> Maybe [i]) -> Automaton q i -> Maybe ([Atom], [i])
eqGeneraliser constsState mem equiv hyp = unsafePerformIO $ do
    let answer = equiv hyp
    let oracle = cacheOracle mem
    case answer of
        Nothing -> return Nothing
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
            if cex == rep || (oracle cex == oracle rep) then
                return (Just ([], cex))
            else do
                -- FIXME: Need to work out the logic to isolate the constant.
                -- For now, just add everything. Not minimal / efficient but
                -- should be correct.
                let newConstants = consts ++ leastSupport cex ++ leastSupport rep
                putStrLn ("[DEBUG] new constants: " ++ show newConstants)
                writeIORef constsState newConstants
                return (Just (newConstants, cex))
