-- Haskell Basics
-- https://www.cmi.ac.in/~spsuresh/teaching/prgh19/


--Defining functions: guards, pattern matching and recursion
factorial :: Int -> Int
factorial n
    | n == 0    = 1
    | n > 0     = n * factorial (n - 1)     | otherwise = error "Negative input not allowed"  fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3 == 0  = "Fizz"
    | n `mod` 5 == 0  = "Buzz"
    | otherwise       = show n

-- Basic types: Int, Float, Bool, Char
exampleInt :: Int
exampleInt = 42

exampleFloat :: Float
exampleFloat = 3.14

exampleBool :: Bool
exampleBool = True

exampleChar :: Char
exampleChar = 'H'

-- Basic type classes: Num, Ord, Enum, Show, Read
exampleNum :: Num a => a -> a -> a
exampleNum x y = x + y

exampleOrd :: Ord a => a -> a -> Bool
exampleOrd x y = x > y

exampleEnum :: [Int]
exampleEnum = [1..5]

exampleShow :: String
exampleShow = show 123

exampleRead :: Int
exampleRead = read "123" :: Int

-- Lists, strings and tuples
exampleList :: [Int]
exampleList = [1, 2, 3, 4, 5]

exampleString :: String
exampleString = "Hello, Haskell!"

exampleTuple :: (Int, String, Bool)
exampleTuple = (42, "Answer", True)

-- Types and polymorphism
identity :: a -> a
identity x = x

-- Higher order functions on lists: map, filter, list comprehension
exampleMap :: [Int]
exampleMap = map (*2) [1, 2, 3]

exampleFilter :: [Int]
exampleFilter = filter even [1, 2, 3, 4, 5]

exampleListComprehension :: [Int]
exampleListComprehension = [x * 2 | x <- [1..5], even x]

-- Computation as rewriting, lazy evaluation and infinite data structures

infiniteList :: [Int]
infiniteList = [1..]

takeTen :: [Int]
takeTen = take 10 infiniteList

-- Conditional polymorphism and type classes
class Eq a => MyEq a where
    isEqual :: a -> a -> Bool
    isEqual x y = x == y

instance MyEq Int

-- User defined datatypes: lists, queues, trees

data MyList a = Empty | Cons a (MyList a) deriving Show

data Queue a = Queue [a] [a] deriving Show

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- Input/output and the ghc compiler
exampleIO :: IO ()
exampleIO = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

-- Arrays
import Data.Array

exampleArray :: Array Int String
exampleArray = array (1, 3) [(1, "One"), (2, "Two"), (3, "Three")]
