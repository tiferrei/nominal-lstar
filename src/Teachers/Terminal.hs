module Teachers.Terminal where

import Control.Monad
import NLambda
import Prelude hiding (and, filter, map, sum)
import System.Console.Haskeline
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- Posing a membership query to the terminal and waits for used to input a formula
ioMembership :: (Show i, Nominal i, Contextual i) => [i] -> Bool
ioMembership query = unsafePerformIO $ do
    putStr "Q: "
    print query
    let loop = do
            x <- fmap readMaybe <$> getInputLine "A: "
            case x of
                Nothing -> error "Bye bye, have a good day!"
                Just Nothing -> do
                    outputStrLn "Unable to parse, try again"
                    loop
                Just (Just f) -> return f
    runInputT defaultSettings loop

-- Same as above, but with a machine-readable format
ioMembership2 :: (Show i, Nominal i, Contextual i) => [i] -> Bool
ioMembership2 query = unsafePerformIO $ do
    let str = unwords . fmap show $ query
    putStrLn $ "MQ \"" ++ str ++ "\""
    let askit = do
            x <- getInputLine ""
            case x of
                Just "Y" -> return True
                Just "N" -> return False
                _        -> error "Unable to parse, or quit. Bye!"
    runInputT defaultSettings askit

newtype TestIO i = T [i]
  deriving (Show, Read, Eq, Ord)

-- Poses a query to the terminal, waiting for the user to provide a counter example
-- User can pose a "test query" which is evaluated on the hypothesis
ioEquivalent :: (Show q, Nominal q, Show i, Read i, Nominal i, Contextual i) => Automaton q i -> Maybe [i]
ioEquivalent hypothesis = unsafePerformIO $ do
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
    runInputT defaultSettings loop

-- Same as above but in different format.
-- This is used for automation and benchmarking different nominal tools
ioEquivalent2 :: (Show q, Nominal q, Show i, Read i, Nominal i, Contextual i) => Automaton q i -> Maybe [i]
ioEquivalent2 hypothesis = unsafePerformIO $ do
    putStrLn "EQ\n\"Is the following automaton correct?"
    print hypothesis
    putStrLn "\""
    let loop = do
            x <- getInputLine ""
            case x of
                Just "Y" -> return Nothing
                Just ('N' : ' ' : ce) -> return (Just (readCE ce))
                _ -> error "Unable to parse (104), or quit. Bye!"
    runInputT defaultSettings loop
    where
        readCE [] = []
        readCE (' ' : xs) = readCE xs
        readCE xs = case reads xs of
            [(a, str)] -> a : readCE str
            _          -> error "Unable to parse (113)"
