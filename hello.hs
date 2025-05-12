-- Enter distraction-free Zen mode: Command+K Z
-- Open Keyboard Shortcuts: Command+K --> Command+S


-- The Sieve of Eratosthenes is an ancient algorithm for finding all prime numbers up to a specified integer.
-- It works by iteratively marking the multiples of each prime number starting from 2.
-- The numbers that remain unmarked are prime.
-- It uses a list comprehension to generate a list of prime numbers and
-- demonstrates the use of higher-order functions and lazy evaluation in Haskell.
-- The program is designed to be efficient and works with large numbers.
-- It also includes an example of using the `foldl` function to sum the prime numbers.
-- Sieve of Eratosthenes: Generate all prime numbers up to a given limit

-- compare this with the implementation here:
-- https://onlinecourses.nptel.ac.in/noc23_cs94/preview

sieveOfEratosthenes :: Int -> [Int]
sieveOfEratosthenes n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Example usage of the Sieve of Eratosthenes
exampleSieve :: IO ()
exampleSieve = do
    let limit = 50
    putStrLn $ "Prime numbers up to " ++ show limit ++ ":"
    print $ sieveOfEratosthenes limit


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
    rohanFunction "Hello, Rohan!"

    exampleQ
    exampleSieve

rohanFunction :: String -> IO ()
rohanFunction str = do
    putStrLn str
    putStrLn "This is a test function"





-- https://stackoverflow.com/questions/60161882/how-to-model-this-recurisve-structure-in-haskell
-- model the kdb/q "atoms and lists" through Haskell type system
data Atom = I Int
          | C Char
          | D Double 
          deriving Show

data Q a = QAtom a 
         | QList [Q a]
         deriving Show

instance Functor Q where
    fmap f (QAtom a) = QAtom (f a)
    fmap f (QList qs) = QList $ fmap (fmap f) qs

instance Foldable Q where
    foldMap f (QAtom a) = f a
    foldMap f (QList qs) = mconcat $ fmap (foldMap f) qs

instance Traversable Q where
    sequenceA (QAtom fa) = fmap QAtom fa
    sequenceA (QList []) = pure $ QList []
    sequenceA (QList (qfa:qfas)) = concatL <$> (sequenceA qfa) <*> (sequenceA (QList qfas))
        where
            concatL (QAtom a) (QList qas) = QList ((QAtom a):qas)

-- Example usage of Functor, Foldable, and Traversable with Q
exampleQ :: IO ()
exampleQ = do
    let q = QList [QAtom 1, QList [QAtom 2, QAtom 3], QAtom 4] :: Q Int
    -- Show the structure
    print q -- Output: QList [QAtom 1,QList [QAtom 2,QAtom 3],QAtom 4]
    -- Show the structure with a different type
    let q' = QList [QAtom 1.0, QList [QAtom 2.0, QAtom 3.0], QAtom 4.0] :: Q Double
    print q' -- Output: QList [QAtom 1.0,QList [QAtom 2.0,QAtom 3.0],QAtom 4.0]
    -- Show the structure with a different type
    let q'' = QList [QAtom 'a', QList [QAtom 'b', QAtom 'c'], QAtom 'd'] :: Q Char
    print q'' -- Output: QList [QAtom 'a',QList [QAtom 'b',QAtom 'c'],QAtom 'd']
    -- Show the structure with a different type
    let q''' = QList [QAtom "hello", QList [QAtom "world", QAtom "!"], QAtom ""] :: Q String
    print q''' -- Output: QList [QAtom "hello",QList [QAtom "world",QAtom "!"],QAtom ""]
    -- Show the structure with a different type
    let q'''' = QList [QAtom True, QList [QAtom False, QAtom True], QAtom False] :: Q Bool
    print q'''' -- Output: QList [QAtom True,QList [QAtom False,QAtom True],QAtom False]
    -- Show the structure with a different type
    let q''''' = QList [QAtom (1,2), QList [QAtom (3,4), QAtom (5,6)], QAtom (7,8)] :: Q (Int, Int)
    print q''''' -- Output: QList [QAtom (1,2),QList [QAtom (3,4),QAtom (5,6)],QAtom (7,8)]

    -- Functor: Apply a function to each element
    print $ fmap (*2) q -- Output: QList [QAtom 2,QList [QAtom 4,QAtom 6],QAtom 8]
    

    -- Foldable: Sum all elements
    print $ foldr (+) 0 q -- Output: 10

    -- Traversable: Traverse the structure with a function that wraps elements in Maybe
    print $ traverse (\x -> if x > 2 then Just x else Nothing) q 
    -- Output: Nothing (because 1 and 2 are less than or equal to 2)


