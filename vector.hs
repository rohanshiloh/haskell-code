{-# LANGUAGE FlexibleContexts #-}
-- import           Control.Monad.Primitive     (PrimMonad, PrimState)
-- import qualified Data.ByteString.Lazy        as L
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import           Data.Word                   (Word8)

-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial
-- https://www.schoolofhaskell.com/user/commercial/content/vector


main :: IO ()
main = do
    putStrLn "Printing numbers from 1 to 10:"

    -- let a = fromList [10, 20, 30, 40]
    -- print a

    -- Create a new 256-size mutable vector
    -- Fill the vector with zeros
    -- mutable <- M.replicate 256 0


    putStrLn "This is a test function"