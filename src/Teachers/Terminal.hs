module Teachers.Terminal where

import Control.Monad
import Data.IORef
import NLambda
import Prelude hiding (and, filter, map, sum)
import System.Console.Haskeline
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- Posing a membership query to the terminal and waits for used to input a formula
ioMembership :: (Show i, Nominal i, Contextual i) => IORef [Atom] -> Set [i] -> Set ([i], Formula)
ioMembership constantsState queries = unsafePerformIO $ do
    -- FIXME: We probably want to cache at the concrete level.
    cache <- readIORef mqCache
    -- I will add state into whatever i want.
    constants <- readIORef constantsState
    let cachedAnswers = filter (\(a, _) -> a `member` queries) cache
    let newQueries = simplify $ queries \\ map fst cache
    let representedInputs = toList . mapFilter id . setOrbitsRepresentatives $ newQueries
    putStrLn "\n# Membership Queries:"
    putStrLn "# Please answer each query with \"True\" or \"False\" (\"^D\" for quit)"
    answers <- forM representedInputs $ \input -> do
        putStr "Q: "
        print input
        let loop = do
                x <- fmap readMaybe <$> getInputLine "A: "
                case x of
                    Nothing -> error "Bye bye, have a good day!"
                    Just Nothing -> do
                        outputStrLn "Unable to parse, try again"
                        loop
                    Just (Just f) -> return f
        answer <- runInputT defaultSettings loop
        return $ orbit constants (input, fromBool answer)
    let answersAsSet = simplify . sum . fromList $ answers
    -- FIXME: re-enable cache at concrete level.
    --writeIORef mqCache (simplify $ cache `union` answersAsSet)
    return (simplify $ cachedAnswers `union` answersAsSet)
    where
        -- We use a cache, so that questions will not be repeated.
        -- It is a bit hacky, as the Teacher interface does not allow state...
        {-# NOINLINE mqCache #-}
        mqCache = unsafePerformIO $ newIORef empty


-- Same as above, but with a machine-readable format
ioMembership2 :: (Show i, Nominal i, Contextual i) => IORef [Atom] -> Set [i] -> Set ([i], Formula)
ioMembership2 constantsState queries = unsafePerformIO $ do
    cache <- readIORef mqCache
    -- I will add state into whatever i want.
    constants <- readIORef constantsState
    let cachedAnswers = filter (\(a, _) -> a `member` queries) cache
    let newQueries = simplify $ queries \\ map fst cache
    let representedInputs = toList . mapFilter id . setOrbitsRepresentatives $ newQueries
    answers <- forM representedInputs $ \input -> do
        let str = unwords . fmap show $ input
        putStrLn $ "MQ \"" ++ str ++ "\""
        let askit = do
                x <- getInputLine ""
                case x of
                    Just "Y" -> return True
                    Just "N" -> return False
                    _        -> error "Unable to parse, or quit. Bye!"
        answer <- runInputT defaultSettings askit
        return $ orbit constants (input, fromBool answer)
    let answersAsSet = simplify . sum . fromList $ answers
    writeIORef mqCache (simplify $ cache `union` answersAsSet)
    return (simplify $ cachedAnswers `union` answersAsSet)
    where
        -- We use a cache, so that questions will not be repeated.
        -- It is a bit hacky, as the Teacher interface does not allow state...
        {-# NOINLINE mqCache #-}
        mqCache = unsafePerformIO $ newIORef empty


newtype TestIO i = T [i]
  deriving (Show, Read, Eq, Ord)

-- Poses a query to the terminal, waiting for the user to provide a counter example
-- User can pose a "test query" which is evaluated on the hypothesis
ioEquivalent :: (Show q, Nominal q, Show i, Read i, Nominal i, Contextual i) => IORef [Atom] -> ([i] -> Formula) -> Automaton q i -> Either [Atom] (Maybe (Set [i]))
ioEquivalent constantsState memOracle hypothesis = unsafePerformIO $ do
    putStrLn "\n# Is the following automaton correct?"
    putStr "# "
    print hypothesis
    putStrLn "# \"^D\" for equivalent; \"[...]\" for a counter example (eg \"[0,1,0]\"); \"T [...]\" for a test query."
    let loop = do
            resp <- getInputLine "> "
            case resp of
                Nothing -> do
                    outputStrLn "Ok, we're done"
                    return Nothing
                Just inp ->
                    case readMaybe inp of
                        Just (T w) -> do
                            let a = accepts hypothesis w
                            outputStrLn $ show a
                            loop
                        Nothing ->
                            case readMaybe inp of
                                Just cex -> return (Just cex)
                                Nothing -> do
                                    outputStrLn "Unable to parse (88), try again"
                                    loop
    answer <- runInputT defaultSettings loop
    case answer of
        Nothing -> return (Right Nothing)
        Just cex -> do
            -- I will add state into whatever i want.
            constants <- readIORef constantsState
            let abstract = orbit constants cex
                rep = head . toList . mapFilter id . setOrbitsRepresentatives $ abstract
            if cex == rep then
                return (Right (Just (orbit constants cex)))
            else
                if memOracle cex == memOracle rep then
                    return (Right (Just (orbit constants cex)))
                else do
                    -- FIXME: Need to work out the logic to isolate the constant.
                    -- For now, just add everything. Not minimal / efficient but
                    -- should be correct.
                    let newConstants = constants ++ leastSupport cex ++ leastSupport rep
                    writeIORef constantsState newConstants
                    return (Left newConstants)

-- Same as above but in different format.
-- This is used for automation and benchmarking different nominal tools
ioEquivalent2 :: (Show q, Nominal q, Show i, Read i, Nominal i, Contextual i) => IORef [Atom] -> ([i] -> Formula) -> Automaton q i -> Either [Atom] (Maybe (Set [i]))
ioEquivalent2 constantsState memOracle hypothesis = unsafePerformIO $ do
    putStrLn "EQ\n\"Is the following automaton correct?"
    print hypothesis
    putStrLn "\""
    let loop = do
            x <- getInputLine ""
            case x of
                Just "Y" -> return Nothing
                Just ('N' : ' ' : ce) -> return (Just (readCE ce))
                _ -> error "Unable to parse (104), or quit. Bye!"
    answer <- runInputT defaultSettings loop
    case answer of
        Nothing -> return (Right Nothing)
        Just cex -> do
            -- I will add state into whatever i want.
            constants <- readIORef constantsState
            let abstract = orbit constants cex
                rep = head . toList . mapFilter id . setOrbitsRepresentatives $ abstract
            if cex == rep then
                return (Right (Just (orbit constants cex)))
            else
                if memOracle cex == memOracle rep then
                    return (Right (Just (orbit constants cex)))
                else do
                    -- FIXME: Need to work out the logic to isolate the constant.
                    -- For now, just add everything. Not minimal / efficient but
                    -- should be correct.
                    let newConstants = constants ++ leastSupport cex ++ leastSupport rep
                    writeIORef constantsState newConstants
                    return (Left newConstants)
    where
        readCE [] = []
        readCE (' ' : xs) = readCE xs
        readCE xs = case reads xs of
            [(a, str)] -> a : readCE str
            _          -> error "Unable to parse (113)"
