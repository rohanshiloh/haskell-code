
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

    
