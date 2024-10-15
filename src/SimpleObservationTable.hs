{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PartialTypeSignatures #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module SimpleObservationTable where

import ObservationTableClass

import Data.Coerce (coerce)
import GHC.Generics (Generic)
import NLambda
import Prelude (Bool (..), Eq, Int, Ord, Show (..), fst, (++), ($), (.), head, id)
import qualified Prelude ()
import Debug.Trace (traceShowId)

toPairs :: (Nominal i, Contextual i) => [Atom] -> Set i -> Set (Set i, i)
toPairs consts s = map (\o -> (o, reps o)) orbits
    where
        -- Note: setOrbits s == (map (orbit []) (mapFilter id . setOrbitsRepresentatives $ s))
        -- But they actually give different reps, and setOrbits crashes often!
        orbits = map (orbit consts) (mapFilter id . setOrbitsRepresentatives $ s)
        reps = head . toList . mapFilter id . setOrbitsRepresentatives

ofPairs :: Nominal a => Set (Set a, a) -> Set a
ofPairs = sum . map fst

-- We represent functions as their graphs
-- Except when o = Bool, more on that later
type Fun i o = Set (i, o)

dom :: (Nominal a, Nominal b) => Set (Set (a, b), (a, b)) -> Set (Set a, a)
dom = map (\(a, c) -> (map fst a, fst c))

mqToPairs :: (Nominal i, Nominal o) => [Atom] -> ([i] -> o) -> Set (Set [i], [i]) -> Set (Set ([i], o), ([i], o))
mqToPairs consts mq = map (\(_, c) -> (orbit consts (c, mq c), (c, mq c)))

-- A table is nothing more than a part of the language.
-- Invariant: content is always defined for elements in
-- `rows * columns` and `rows * alph * columns`.
data Table i o = Table
    { content    :: Set (Set ([i], o), ([i], o))
    , rowIndices :: Set (Set (RowIndex i), RowIndex i)
    , colIndices :: Set (Set (ColumnIndex i), ColumnIndex i)
    , aa         :: Set i
    , consts     :: [Atom]
    }
    deriving (Show, Ord, Eq, Generic, Nominal, Contextual)

instance (Show i, Nominal i, Contextual i, Show o, Nominal o, Contextual o) => ObservationTable (Table i o) i o where
    type Row (Table i o) = Fun [i] o
    rows = ofPairs . rowIndices
    cols = ofPairs . colIndices
    alph = aa
    row Table{..} r = pairsWithFilter (\e (a, b) -> maybeIf (a `eq` (r ++ e)) (e, b)) (ofPairs colIndices) (ofPairs content)
    tableAt Table{..} r c = mapFilter (\(i, o) -> maybeIf ((r ++ c) `eq` i) o) (ofPairs content)

    -- Assumption: newRows is disjoint from rows (for efficiency)
    addRows mq newRows t@Table{..} =
        traceShowId t { content = content `union` newContent
          , rowIndices = rowIndices `union` toPairs consts newRows
          }
        where
            newRowsExt = pairsWith (\r a -> r ++ [a]) newRows aa
            newPart = toPairs consts $ pairsWith (++) (newRows `union` newRowsExt) (ofPairs colIndices)
            newPartRed = newPart \\ dom content
            newContent = mqToPairs consts mq newPartRed

    -- Assumption: newColumns is disjoint from columns (for efficiency)
    addColumns mq newColumns t@Table{..} =
        traceShowId t { content = content `union` newContent
          , colIndices = colIndices `union` toPairs consts newColumns
          }
        where
            newColumnsExt = pairsWith (:) aa newColumns
            newPart = toPairs consts $ pairsWith (++) (ofPairs rowIndices) (newColumns `union` newColumnsExt)
            newPartRed = newPart \\ dom content
            newContent = mqToPairs consts mq newPartRed

    addConstants addConsts t@Table{..} =
        traceShowId t {
            -- FIXME: Can probably be optimised.
            content = toPairs newConsts (sum $ map fst content),
            rowIndices = toPairs newConsts (sum $ map fst rowIndices),
            colIndices = toPairs newConsts (sum $ map fst colIndices),
            consts = newConsts }
        where
            newConsts = consts ++ addConsts

-- We can reuse the above tables for the Boolean case and
-- perform some minor optimisations.
newtype Boolean table = B { unB :: table }
    deriving (Show, Ord, Eq, Generic, Nominal, Conditional, Contextual)

type BTable i = Boolean (Table i Bool)

instance (Nominal i, Show i, Contextual i) => ObservationTable (BTable i) i Bool where
    -- Special case of a boolean: functions to Booleans are subsets
    type Row (BTable i) = Set [i]

    -- All the reusable functions are simply coerced
    rows = coerce (rows :: _ => Table i Bool -> _)
    cols = coerce (cols :: _ => Table i Bool -> _)
    rowsExt = coerce (rowsExt :: _ => Table i Bool -> _)
    colsExt = coerce (colsExt :: _ => Table i Bool -> _)
    alph = coerce (alph :: _ => Table i Bool -> _)
    tableAt = coerce (tableAt :: _ => Table i Bool -> _)
    addRows = coerce (addRows :: _ => _ -> _ -> Table i Bool -> Table i Bool)
    addColumns = coerce (addColumns :: _ => _ -> _ -> Table i Bool -> Table i Bool)
    addConstants = coerce (addConstants :: _ => _ -> Table i Bool -> Table i Bool)

    -- These are specific to our representation of Row
    row (B Table{..}) r = let lang = mapFilter (\(i, o) -> maybeIf (fromBool o) i) (ofPairs content)
                          in filter (\a -> lang `contains` (r ++ a)) (ofPairs colIndices)
    rowEps (B Table{..}) = mapFilter (\(i, o) -> maybeIf (fromBool o /\ i `member` ofPairs colIndices) i) (ofPairs content)


initialTableWith :: (Nominal i, Contextual i, Nominal o, Contextual o) => MQ i o -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> Table i o
initialTableWith mq alphabet newRows newColumns = Table
    { content = content
    , rowIndices = toPairs [] newRows
    , colIndices = toPairs [] newColumns
    , aa = alphabet
    , consts = []
    }
    where
        newColumnsExt = pairsWith (:) alphabet newColumns
        domain = toPairs [] $ pairsWith (++) newRows (newColumns `union` newColumnsExt)
        content = mqToPairs [] mq domain

initialTable :: (Nominal i, Contextual i, Nominal o, Contextual o) => MQ i o -> Set i -> Table i o
initialTable mq alphabet = initialTableWith mq alphabet (singleton []) (singleton [])

initialTableSize :: (Nominal i, Contextual i, Nominal o, Contextual o) => MQ i o -> Set i -> Int -> Int -> Table i o
initialTableSize mq alphabet rs cs = initialTableWith mq alphabet (replicateSetUntil rs alphabet) (replicateSetUntil cs alphabet)

initialBTableWith :: (Nominal i, Contextual i) => MQ i Bool -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> BTable i
initialBTableWith = coerce initialTableWith

initialBTable :: (Nominal i, Contextual i) => MQ i Bool -> Set i -> BTable i
initialBTable = coerce initialTable

initialBTableSize :: (Nominal i, Contextual i) => MQ i Bool -> Set i -> Int -> Int -> BTable i
initialBTableSize = coerce initialTableSize
