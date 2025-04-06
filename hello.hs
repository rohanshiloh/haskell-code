



-- Function to generate the Fibonacci sequence
fibonacci :: Int -> [Int]
fibonacci n = take n $ fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Main function to print the first 10 Fibonacci numbers
-- This is the main function that prints numbers from 1 to 10
main :: IO ()
main = do
    putStrLn "Printing numbers from 1 to 10:"
    mapM_ print [1..10]

    putStrLn "The first 10 Fibonacci numbers are:"
    print (fibonacci 10)
    printStrLn "Ding Dong the Queen is Dead"

rohanFunction :: String -> IO ()
rohanFunction str = do
    putStrLn str
    putStrLn "This is a test function"
    
