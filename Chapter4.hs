import System.Environment (getArgs)
import Data.Char (isDigit, digitToInt)

interactWith :: (String -> String) -> String -> String -> IO ()
interactWith f i o = do
    content <- readFile i
    writeFile o (f content)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [i, o] -> interactWith f i o
        _      -> usage
    where f = id

usage :: IO ()
usage = putStrLn "Usage: interact infile outfile"

------------------------------------------------------------------------------
-- Functions that only have return values defined for a subset of valid inputs
-- are called partial functions.  Functions that have a return value for all
-- values of their input domain are called total functions.  Partial functions
-- are a large source of bugs in Haskell.  Some people even mark any partial
-- function with the prefix `unsafe`.
------------------------------------------------------------------------------

asInt :: String -> Maybe Int
asInt xs
    | null digits = Nothing
    | otherwise   = Just $ foldl (\a i -> a * 10 + digitToInt i) 0 digits
    where digits  = takeWhile isDigit xs
