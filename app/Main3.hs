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
  | ManualDFA     -- Teacher with manual membership and equivalence
  deriving (Show, Read)

data Aut = Running Int | Adversarial | Extended | NFA1 | Bollig Int | NonResidual
  deriving (Show, Read)

-- existential wrapper
data A = forall q . (Nominal q, Show q) => A (Automaton q Atom)

test :: (Show q1, Show q2, Show a, Nominal q1, Nominal q2, Nominal a) =>
        [a] -> Automaton q2 a -> Automaton q1 a -> IO ()
test str target learned =
    putStrLn $ "Test: " ++ show str ++ " -> " ++ show (t, m)
    where t = simplify $ accepts target str
          m = simplify $ accepts learned str

{- HLINT ignore "Redundant $" -}
mainExample :: String -> String -> String -> IO ()
mainExample learnerName teacherName autName = do

    A target <- return $ case read autName of
            Running n   -> A $ Examples.runningExample atoms n
            Adversarial -> A $ Examples.adversarial
            Extended    -> A $ Examples.extended
            NFA1        -> A $ Examples.exampleNFA1
            Bollig n    -> A $ Examples.exampleNFA2 n
            NonResidual -> A $ Examples.exampleNonResidual
    putStrLn $ "Target to be learned:\n" ++ show target
    let teacher = case read teacherName of
            EqDFA         -> teacherWithTarget target
            EqNFA k       -> teacherWithTargetNonDet k target
            EquivalenceIO -> teacherWithTargetAndIO target
            ManualDFA     -> teacherWithIO (NLambda.alphabet target)
    let learned = case read learnerName of
            NomLStar    -> learnAngluinRows teacher
            NomLStarCol -> learnAngluin teacher
            NomNLStar   -> learnBollig 0 0 teacher
    print $ learned

    test [atom "a", atom "a"] target learned
    test [atom "a", atom "b"] target learned
    test [atom "a", atom "a", atom "a"] target learned
    test [atom "a", atom "a", atom "b"] target learned
    test [constant 0, constant 0] target learned
    test [constant 0, constant 0, constant 0] target learned
    test [a,a] target learned
    test [a,a,a] target learned
    test [d,d] target learned
    test [d,d,d] target learned
    test [e,e] target learned
    test [e,e,e] target learned
    test [d,e] target learned
    test [d,e,e] target learned

mainWithIO :: String -> IO ()
mainWithIO learnerName = do
    let t = teacherWithIO atoms
    let target = case read learnerName of
            NomLStar    -> learnAngluinRows t
            NomLStarCol -> learnAngluin t
            NomNLStar   -> learnBollig 0 0 t
    print target

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
