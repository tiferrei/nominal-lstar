{-# language ExistentialQuantification #-}
{-# language PartialTypeSignatures #-}
import AngluinDFA
import AngluinMealy
import Bollig
import Examples
import Teacher

import NLambda hiding (automaton)
import Prelude hiding (map)
import System.Environment

data Learner
  = NomLStar     -- nominal L* for nominal automata
  | NomLStarCol  -- nominal L* with counterexamples as columns (suffix closed)
  | NomNLStar    -- NL* for nominal automata, counterexamples as columns (suffix closed)
  deriving (Show, Read)

data Teacher
  = EqDFA         -- Automatic teacher with membership and equivalence (only for DFAs)
  | EqNFA Int     -- Automatic teacher with membership and bounded equivalence
  | EquivalenceIO -- Teacher with automatic membership but manual equivalence
  | ManualDFA     -- Teacher with manual membership and equivalence
  | EqMealy
  deriving (Show, Read)

data Aut = Running Int | Adversarial | Extended | NFA1 | Bollig Int | NonResidual | LRU1 | LRU2 | LRU1Faulty | LRU2Faulty
  deriving (Show, Read)

-- existential wrapper
data A = forall q . (Nominal q, Show q) => A (Automaton q Atom)
data M = forall q o . (Nominal q, Show q, Show o, Nominal o, Contextual o) => M (Mealy q Atom o)

testDFA :: _ => [i] -> _ -> _ -> IO ()
testDFA str target learned =
    putStrLn $ "Test: " ++ show str ++ " -> " ++ show (t, m)
    where
        t = automaticMembership target str
        m = automaticMembership learned str

testMealy :: _ => [i] -> _ -> _ -> IO ()
testMealy str target learned =
    putStrLn $ "Test: " ++ show str ++ " -> " ++ show (t, m)
    where
        t = automaticMembershipMealy target str
        m = automaticMembershipMealy learned str

mainDFAWithIO :: String -> IO ()
mainDFAWithIO learnerName = do
    let t = teacherWithIO atoms
    let target = case read learnerName of
            NomLStar    -> AngluinDFA.learnAngluinRows t
            NomLStarCol -> AngluinDFA.learnAngluin t
            NomNLStar   -> learnBollig 0 0 t
    print target

{- HLINT ignore "Redundant $" -}
mainDFA :: String -> String -> String -> IO ()
mainDFA learnerName teacherName autName = do
    A target <- return $ case read autName of
            Running n   -> A $ Examples.runningExample atoms n
            Adversarial -> A $ Examples.adversarial
            Extended    -> A $ Examples.extended
            NFA1        -> A $ Examples.exampleNFA1
            Bollig n    -> A $ Examples.exampleNFA2 n
            NonResidual -> A $ Examples.exampleNonResidual
            _           -> error "Unsupported target!"
    putStrLn $ "Target to be learned:\n" ++ show target
    let teacher = case read teacherName of
            EqDFA         -> teacherWithTarget target
            EqNFA k       -> teacherWithTargetNonDet k target
            EquivalenceIO -> teacherWithTargetAndIO target
            ManualDFA     -> teacherWithIO (NLambda.alphabet target)
            _             -> error "Unsupported teacher!"
    let learned = case read learnerName of
            NomLStar    -> AngluinDFA.learnAngluinRows teacher
            NomLStarCol -> AngluinDFA.learnAngluin teacher
            NomNLStar   -> learnBollig 0 0 teacher
    print $ learned

    testDFA [a,a] target learned
    testDFA [a,a,a] target learned
    testDFA [d,d] target learned
    testDFA [d,d,d] target learned
    testDFA [e,e] target learned
    testDFA [e,e,e] target learned
    testDFA [d,e] target learned
    testDFA [d,e,e] target learned

{- HLINT ignore "Redundant $" -}
mainMealy :: String -> String -> String -> IO ()
mainMealy learnerName teacherName autName = do
    --showSmtInfo
    M target <- return $ case read autName of
            LRU1  -> M $ Examples.lru1
            LRU2  -> M $ Examples.lru2
            LRU1Faulty -> M $ Examples.lru1Faulty (constant 4)
            LRU2Faulty -> M $ Examples.lru2Faulty (constant 4)
            _     -> error "Unsupported target!"
    putStrLn $ "Target to be learned:\n" ++ show target
    let teacher = case read teacherName of
            EqMealy -> mealyTeacherWithTarget target
            _       -> error "Unsupported teacher!"
    let learned = case read learnerName of
            NomLStarCol -> AngluinMealy.learnAngluin teacher
            NomLStar -> AngluinMealy.learnAngluinRows teacher
            _           -> error "Unsupported learner!"
    print $ learned

    testMealy [constant 0] target learned
    testMealy [constant 0, constant 0] target learned
    testMealy [constant 0, constant 1] target learned
    testMealy [constant 0, constant 0, constant 0] target learned
    testMealy [constant 0, constant 0, constant 1] target learned
    testMealy [constant 0, constant 1, constant 0] target learned
    testMealy [constant 0, constant 1, constant 1] target learned
    testMealy [constant 1, constant 0, constant 0] target learned
    testMealy [constant 1, constant 0, constant 1] target learned
    testMealy [constant 1, constant 1, constant 0] target learned
    testMealy [constant 1, constant 1, constant 1] target learned

    testMealy [constant 4] target learned
    testMealy [constant 4, constant 4] target learned
    testMealy [constant 4, constant 1] target learned
    testMealy [constant 4, constant 4, constant 4] target learned
    testMealy [constant 4, constant 4, constant 1] target learned
    testMealy [constant 4, constant 1, constant 4] target learned
    testMealy [constant 4, constant 1, constant 1] target learned
    testMealy [constant 1, constant 4, constant 4] target learned
    testMealy [constant 1, constant 4, constant 1] target learned
    testMealy [constant 1, constant 1, constant 4] target learned
    testMealy [constant 1, constant 1, constant 1] target learned

main :: IO ()
main = do
    bla <- getArgs
    case bla of
        ["dfa", learnerName, teacherName, autName] -> mainDFA learnerName teacherName autName
        ["mealy", learnerName, teacherName, autName] -> mainMealy learnerName teacherName autName
        ["dfa", learnerName] -> mainDFAWithIO learnerName
        _ -> help

help :: IO ()
help = do
  putStrLn "Usage (for automated runs)"
  putStrLn ""
  putStrLn "    nominal-lstar <dfa|mealy> <learner> <teacher> <automaton>"
  putStrLn ""
  putStrLn "or (for manual runs)"
  putStrLn ""
  putStrLn "    nominal-lstar <dfa|mealy> <learner>"
  putStrLn ""
  putStrLn $ "where <learner> is any of " ++ show learners ++ ", <teacher> is any of " ++ show teachers ++ ", and <automaton> is any of " ++ show automata ++ ". (Replace 3 with any number you wish.)"
  where
    learners = [NomLStar, NomLStarCol, NomNLStar]
    teachers = [EqDFA, EqNFA 3, EquivalenceIO, EqMealy]
    automata = [Running 3, NFA1, Bollig 3, NonResidual, LRU1, LRU2]
