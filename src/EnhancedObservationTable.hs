{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}

module EnhancedObservationTable where

import ObservationTableClass

import GHC.Generics (Generic)
import NLambda
import Prelude (Bool (..), Eq, Int, Ord, Show (..), (++), (.), ($), fst, id, head)
import qualified Prelude ()
import Debug.Trace (traceShowId)

-- Helper function
mqToSubset :: Nominal i => (Set [i] -> Set ([i], Bool)) -> Set [i] -> Set [i]
mqToSubset mq = mapFilter (\(i, o) -> maybeIf (fromBool o) i) . mq

toPairs :: (Nominal i, Contextual i) => [Atom] -> Set i -> Set (Set i, i)
toPairs consts s = map (\o -> (o, reps o)) orbits
    where
        -- Note: setOrbits s == (map (orbit []) (mapFilter id . setOrbitsRepresentatives $ s))
        -- But they actually give different reps, and setOrbits crashes often!
        orbits = map (orbit consts) (mapFilter id . setOrbitsRepresentatives $ s)
        reps = head . toList . mapFilter id . setOrbitsRepresentatives

-- A table is nothing more than a part of the language.
-- Invariant: content is always a subset of
-- `domain` = `rows * columns` union `rows * alph * columns`.
data Table i = Table
    --                  (abs,      conc)
    { content    :: Set (Set [i], [i])
    , domain     :: Set (Set [i], [i])
    , rowIndices :: Set (Set [i], [i])
    , colIndices :: Set (Set [i], [i])
    , aa         :: Set i
    , consts     :: [Atom]
    }
    deriving (Show, Ord, Eq, Generic, Nominal, Contextual)

instance (Show i, Nominal i, Contextual i) => ObservationTable (Table i) i Bool where
    type Row (Table i) = Set [i]

    rows = sum . map fst . rowIndices
    cols = sum . map fst . colIndices
    alph = aa

    row Table{..} r = filter (\e -> (r ++ e) `member` sum (map fst content)) (sum $ map fst colIndices)
    rowEps Table{..} = intersection (sum $ map fst content) (sum $ map fst colIndices)
    tableAt Table{..} r c = ite ((r ++ c) `member` sum (map fst content)) (singleton True) (singleton False)

    addRows mq newRows t@Table{..} =
        traceShowId t { content = content `union` toPairs consts newContent
          , domain = domain `union` toPairs consts newPart
          , rowIndices = rowIndices `union` toPairs consts newRows
          }
        where
            newRowsExt = pairsWith (\r a -> r ++ [a]) newRows aa
            newPart = pairsWith (++) (newRows `union` newRowsExt) (sum $ map fst colIndices)
            newContent = mqToSubset mq newPart

    addColumns mq newColumns t@Table{..} =
        traceShowId t { content = content `union` toPairs consts newContent
          , domain = domain `union` toPairs consts newPart
          , colIndices = colIndices `union` toPairs consts newColumns
          }
        where
            newColumnsExt = pairsWith (:) aa newColumns
            newPart = pairsWith (++) (sum $ map fst rowIndices) (newColumns `union` newColumnsExt)
            newContent = mqToSubset mq newPart

    addConstants addConsts t@Table{..} =
        traceShowId t {
            content = toPairs newConsts (sum $ map fst content),
            domain = toPairs newConsts (sum $ map fst domain),
            rowIndices = toPairs newConsts (sum $ map fst rowIndices),
            colIndices = toPairs newConsts (sum $ map fst colIndices),
            consts = newConsts }
        where
            newConsts = consts ++ addConsts

initialBTableWith :: (Nominal i, Contextual i) => MQ i Bool -> Set i -> Set (RowIndex i) -> Set (ColumnIndex i) -> Table i
initialBTableWith mq alphabet newRows newColumns = Table
    { content = toPairs [] content
    , domain = toPairs [] domain
    , rowIndices = toPairs [] newRows
    , colIndices = toPairs [] newColumns
    , aa = alphabet
    , consts = []
    }
    where
        newColumnsExt = mapFilter id . setOrbitsRepresentatives $ pairsWith (:) alphabet newColumns
        domain = pairsWith (++) newRows (newColumns `union` newColumnsExt)
        content = mqToSubset mq domain

initialBTable :: (Nominal i, Contextual i) => MQ i Bool -> Set i -> Table i
initialBTable mq alphabet = initialBTableWith mq alphabet (singleton []) (singleton [])

initialBTableSize :: (Nominal i, Contextual i) => MQ i Bool -> Set i -> Int -> Int -> Table i
initialBTableSize mq alphabet rs cs = initialBTableWith mq alphabet (replicateSetUntil rs alphabet) (replicateSetUntil cs alphabet)
