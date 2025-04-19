#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- https://academy.fpblock.com/haskell/library/vector/

{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import           Data.Word                   (Word8)
import qualified Data.Vector as V
import qualified GHC.TypeNats as U

-- | This program reads bytes from stdin and counts the frequency of each byte.
-- | It uses a mutable vector to store the counts, and then prints the results.
-- | The program is designed to be efficient and works with large amounts of data.
-- | It uses the `Data.Vector` library for efficient vector operations.
-- | The program demonstrates the use of mutable vectors, lazy byte strings,
-- | and the `PrimMonad` type class for working with mutable state in Haskell.
-- | The program also includes examples of various vector operations such as
-- | filtering, mapping, zipping, and taking elements from the vector.
-- | The program is a good example of how to use the `vector` library in Haskell
-- | to perform efficient operations on large collections of data.
main :: IO ()
main = do

    let list = [1..10] :: [Int]
        vector = V.fromList list :: V.Vector Int
        vector2 = V.enumFromTo 1 10 :: V.Vector Int
    print $ vector == vector2 -- True
    print $ list == V.toList vector -- also True
    print $ V.filter odd vector -- 1,3,5,7,9
    print $ V.map (* 2) vector -- 2,4,6,...,20
    print $ V.zip vector vector -- (1,1),(2,2),...(10,10)
    print $ V.zipWith (*) vector vector -- (1,4,9,16,...,100)
    print $ V.reverse vector -- 10,9,...,1
    print $ V.takeWhile (< 6) vector -- 1,2,3,4,5
    print $ V.takeWhile odd vector -- 1
    print $ V.takeWhile even vector -- []
    print $ V.dropWhile (< 6) vector -- 6,7,8,9,10
    print $ V.head vector -- 1
    print $ V.tail vector -- 2,3,4,...,10
    -- print $ V.head $ V.takeWhile even vector -- exception! vector: index out of bounds (0,0)


    -- Get all of the contents from stdin
    lbs <- L.getContents
    -- Print the length of the input
    putStrLn $ "Length of input: " ++ show (L.length lbs)
    -- Print the first 10 bytes of the input
    putStrLn $ "First 10 bytes: " ++ show (L.take 10 lbs)
    -- Print the last 10 bytes of the input
    putStrLn $ "Last 10 bytes: " ++ show (L.take 10 (L.reverse lbs))

    -- Fill the vector with zeros
    mutable <- M.replicate 256 0

    -- Add all of the bytes from stdin
    addBytes mutable lbs

    -- Freeze to get an immutable version
    vector <- U.unsafeFreeze mutable

    -- Print the frequency of each byte
    -- In newer vectors: we can use imapM_
    -- In older vectors: we can use zipWithM_
    U.imapM_ printFreq vector
    -- U.zipWithM_ is a bit more efficient than U.imapM_
    U.zipWithM_ printFreq (U.enumFromTo 0 255) vector

-- Exercise 1: Try out some other functions available in the Data.Vector module. 
-- In particular, try some of the fold functions, which we haven't covered here.

    -- Sum of all elements using foldl
    print $ U.foldl (+) 0 vector -- Output: 55
    -- Product of all elements using foldl
    print $ U.foldl (*) 1 vector -- Output: 3628800
    -- Maximum element using foldl1
    print $ U.foldl1 max vector -- Output: 10
    -- Minimum element using foldl1
    print $ U.foldl1 min vector -- Output: 1
    -- Sum of all elements using foldr
    print $ U.foldr (+) 0 vector -- Output: 55
    -- Product of all elements using foldr
    print $ U.foldr (*) 1 vector -- Output: 3628800
    -- Check if all elements are greater than 0 using all
    print $ U.all (> 0) vector -- Output: True
    -- Check if any element is greater than 5 using any
    print $ U.any (> 5) vector -- Output: True
    -- Count the number of even elements using foldl'
    print $ U.foldl' (\acc x -> if even x then acc + 1 else acc) 0 vector -- Output: 5
    -- Concatenate all elements into a string using foldr
    print $ U.foldr (\x acc -> show x ++ acc) "" vector -- Output: "12345678910"

-- Exercise 2: Try using the Functor, Foldable, and Traversable versions of functions with a vector







-- Exercise 3: Use an unboxed (or storable) vector instead of the boxed vectors we were using above. 
-- What code did you have to change from the original example? Do all of your examples from exercise 2 still work?


-- There are also a number of functions in the Data.Vector module with no corresponding function in Prelude. 
-- Many of these are related to mutable vectors (which we'll cover shortly). Others are present to provide 
-- more efficient means of manipulating a vector, based on their special in-memory representation.



addBytes :: (PrimMonad m, M.MVector v Int)
         => v (PrimState m) Int
         -> L.ByteString
         -> m ()
addBytes v lbs = mapM_ (addByte v) (L.unpack lbs)

addByte :: (PrimMonad m, M.MVector v Int)
        => v (PrimState m) Int
        -> Word8
        -> m ()
addByte v w = do
    -- Read out the old count value
    oldCount <- M.read v index
    -- Write back the updated count value
    M.write v index (oldCount + 1)
  where
    -- Indices in vectors are always Ints. Our bytes come in as Word8, so we
    -- need to convert them.
    index :: Int
    index = fromIntegral w

printFreq :: Int -> Int -> IO ()
printFreq index count = putStrLn $ concat
    [ "Frequency of byte "
    , show index
    , ": "
    , show count
    ]