-- Enter distraction-free Zen mode: Command+K Z
-- Open Keyboard Shortcuts: Command+K Command+S


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

    -- Functor: Apply a function to convert integers to strings
    print $ fmap show q -- Output: QList [QAtom "1",QList [QAtom "2",QAtom "3"],QAtom "4"]

    -- Foldable: Sum all elements
    print $ foldr (+) 0 q -- Output: 10

    -- Foldable: Count the number of elements
    print $ foldr (\_ acc -> acc + 1) 0 q -- Output: 4
    -- Foldable: Check if any element is greater than 2
    print $ foldr (\x acc -> if x > 2 then True else acc) False q -- Output: True
    -- Foldable: Check if all elements are greater than 0
    print $ foldr (\x acc -> if x > 0 then True else acc) False q -- Output: True
    -- Foldable: Find the maximum element
    print $ foldr1 max q -- Output: 4
    -- Foldable: Find the minimum element
    print $ foldr1 min q -- Output: 1
    -- Foldable: Check if the structure is empty
    print $ null q -- Output: False

    -- Traversable: Traverse the structure with a function that wraps elements in Maybe
    print $ traverse (\x -> if x > 2 then Just x else Nothing) q 
    -- Output: Nothing (because 1 and 2 are less than or equal to 2)
    
    -- Traversable: Traverse the structure with a function that filters out odd numbers
    print $ traverse (\x -> if even x then Just x else Nothing) q
    -- Output: Nothing (because all elements are odd)



