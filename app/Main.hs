{-# language ExistentialQuantification #-}
import Angluin
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
  deriving (Show, Read)

data Aut = Running Int | Adversarial | NFA1 | Bollig Int | NonResidual
  deriving (Show, Read)

-- existential wrapper
data A = forall q . (Nominal q, Show q) => A (Automaton q Atom)

{- HLINT ignore "Redundant $" -}
mainExample :: String -> String -> String -> IO ()
mainExample learnerName teacherName autName = do
    A automaton <- return $ case read autName of
            Running n   -> A $ Examples.runningExample atoms n
            Adversarial -> A $ Examples.adversarial
            NFA1        -> A $ Examples.exampleNFA1
            Bollig n    -> A $ Examples.exampleNFA2 n
            NonResidual -> A $ Examples.exampleNonResidual
    let teacher = case read teacherName of
            EqDFA         -> teacherWithTarget automaton
            EqNFA k       -> teacherWithTargetNonDet k automaton
            EquivalenceIO -> teacherWithTargetAndIO automaton
    let model = case read learnerName of
            NomLStar    -> learnAngluinRows teacher
            NomLStarCol -> learnAngluin teacher
            NomNLStar   -> learnBollig 0 0 teacher
    print model
    putStrLn $ "Test: [1, 1] -> " ++ show (accepts automaton [a,a], accepts model [a,a])

mainWithIO :: String -> IO ()
mainWithIO learnerName = do
    let t = teacherWithIO atoms
    let model = case read learnerName of
            NomLStar    -> learnAngluinRows t
            NomLStarCol -> learnAngluin t
            NomNLStar   -> learnBollig 0 0 t
    print model

main :: IO ()
main = do
    bla <- getArgs
    case bla of
        [learnerName, teacherName, autName] -> mainExample learnerName teacherName autName
        [learnerName] -> mainWithIO learnerName
        _ -> help

help :: IO ()
help = do
  putStrLn "Usage (for automated runs)"
  putStrLn ""
  putStrLn "    nominal-lstar <learner> <teacher> <automaton>"
  putStrLn ""
  putStrLn "or (for manual runs)"
  putStrLn ""
  putStrLn "    nominal-lstar <learner>"
  putStrLn ""
  putStrLn $ "where <learner> is any of " ++ show learners ++ ", <teacher> is any of " ++ show teachers ++ ", and <automaton> is any of " ++ show automata ++ ". (Replace 3 with any number you wish.)"
  where
    learners = [NomLStar, NomLStarCol, NomNLStar]
    teachers = [EqDFA, EqNFA 3, EquivalenceIO]
    automata = [Running 3, NFA1, Bollig 3, NonResidual]
