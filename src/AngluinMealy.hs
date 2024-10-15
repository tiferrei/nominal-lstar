{-# language FlexibleContexts #-}
{-# language PartialTypeSignatures #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module AngluinMealy where

import AbstractLStar
import ObservationTableClass
import qualified EnhancedObservationTable as OT
import Teacher

import Data.List (inits, tails)
import Debug.Trace
import NLambda hiding (alphabet)
import Prelude (Bool (..), Maybe (..), error, show, ($), (++), (.), null, fst, snd)


-- This returns all witnesses (of the form sa) for non-closedness
closednessTest :: (Nominal i, _) => table -> TestResult i
closednessTest t = case solve (traceShowId $ isEmpty defect) of
    Just True  -> Succes
    Just False -> Failed defect empty
    Nothing    -> let err = error "@@@ Unsolvable (closednessTest) @@@" in Failed err err
    where
        allRows = map (row t) (rows t)
        hasEqRow = contains allRows . row t
        defect = filter (not . hasEqRow) (rowsExt t)

-- We look for inconsistencies and return columns witnessing it
consistencyTestDirect :: (Nominal i, _) => table -> TestResult i
consistencyTestDirect t = case solve (isEmpty defect) of
    Just True  -> Succes
    Just False -> trace "Not consistent" $ Failed empty defect
    Nothing    -> let err = error "@@@ Unsolvable (consistencyTestDirect) @@@" in Failed err err
    where
        ssRows = map (\u -> (u, row t u)) (rows t)
        candidates = pairsWithFilter (\(u1,r1) (u2,r2) -> maybeIf (u1 `neq` u2 /\ r1 `eq` r2) (u1, u2)) ssRows ssRows
        defect = triplesWithFilter (\(u1, u2) a v -> maybeIf (tableAt t (u1 ++ [a]) v `neq` tableAt t (u2 ++ [a]) v) (a:v)) candidates (alph t) (cols t)

-- Given a C&C table, constructs an automaton. The states are given by 2^E (not
-- necessarily equivariant functions)
constructHypothesis :: (Nominal i, _) => table -> alpha -> Mealy (Row table) i o
constructHypothesis t = simplify $ mealy q i (fst alpha) (snd alpha) d
    where
        q = map (row t) (rows t)
        i = rowEps t
        d = sum $ pairsWith (\s a -> map (\b -> (row t s, a, b, row t (s++[a]))) (tableAt t s a)) (rows t) (alph t)

-- Extends the table with all prefixes of a set of counter examples.
useCounterExampleAngluin :: (Nominal i, _) => MealyTeacher i o -> [i] -> table -> table
useCounterExampleAngluin teacher ces t =
    let newRows = fromList $ inits ces
        newRowsRed = newRows \\ rows t
     in addRows (mealyMembership teacher) newRowsRed t

-- This is the variant by Maler and Pnueli: Adds all suffixes as columns
useCounterExampleMP :: (Nominal i, _) => MealyTeacher i o -> [i] -> table -> table
useCounterExampleMP teacher ces t =
    let newColumns = fromList $ tails ces
        newColumnsRed = newColumns \\ cols t
     in addColumns (mealyMembership teacher) newColumnsRed t

-- Default: use counter examples in columns, which is slightly faster
learnAngluin :: (Nominal i, _) => MealyTeacher i o -> Mealy _ i o
learnAngluin teacher = learnLoop useCounterExampleMP teacher (OT.initialTableWith (mealyMembership teacher) (fst $ mealyAlphabet teacher) empty (fst $ mealyAlphabet teacher))

-- The "classical" version, where counter examples are added as rows
learnAngluinRows :: (Nominal i, _) => MealyTeacher i o -> Mealy _ i o
learnAngluinRows teacher = learnLoop useCounterExampleAngluin teacher (OT.initialTableWith (mealyMembership teacher) (fst $ mealyAlphabet teacher) empty (fst $ mealyAlphabet teacher))

learnLoop :: (Nominal i, ObservationTable table i [o], _) => _ -> MealyTeacher i o -> table -> Mealy (Row table) i o
learnLoop cexHandler teacher t =
    trace "1. Making it closed" $
    case closednessTest t of
        Failed newRows _ ->
            let state2 = addRows (membership teacher) newRows t in
            trace ("newrows = " ++ show (simplify newRows)) $
            learnLoop cexHandler teacher state2
        Succes ->
            trace "2. Making it consistent" $
            case consistencyTestDirect t of
                Failed _ newColumns ->
                    let state2 = addColumns (membership teacher) newColumns t in
                    trace ("newcols = " ++ show (simplify newColumns)) $
                    learnLoop cexHandler teacher state2
                Succes ->
                    traceShow hyp $
                    trace "3. Equivalent? " $
                    eqloop t hyp
    where
        hyp = constructHypothesis t (mealyAlphabet teacher)
        eqloop s2 h = case equivalent teacher h of
                        Nothing -> trace "Yes" h
                        Just (consts, ces) -> trace "No" $
                            let s3 = if null consts then s2 else addConstants consts s2 in
                            let s4 = cexHandler teacher ces s3 in
                            trace ("Using ce: " ++ show ces) $
                            learnLoop cexHandler teacher s4
