-- Haskell for Good of Human Beans
-- 1. ghcup is used to install and manage Haskell toolchain versions
-- 2. ghc is the Glasgow Haskell Compiler
-- 3. runhaskell or runghc is a command to run Haskell scripts directly
-- 4. stack is a Haskell build tool that manages project dependencies
-- 5. cabal is a system for building and packaging Haskell libraries and programs
--    so stack and cabal are both build tools, but they have different approaches
--    stack is more opinionated and provides a curated environment
--    while cabal is more flexible and allows for more customization
-- 6. ghci is the interactive environment for Haskell
-- 7. runghc see #3 above

-- HLC or the Haskell Language Server 
-- is a server that communicates with editors and IDEs using the Language Server Protocol (LSP).
-- Here are a few pieces of jargon that you may come across in the HLS docs or when discussing problems:
-- 1. Code action: A code action is a specific action triggered by a user on a particular region of code.   
--    Examples might include “add a type signature to this function”.
-- 2. Code lens: A pre-rendered edit or action shown in the body of the document itself, usually triggered with a click.
--    Examples might include “the type signature for a function, which is actually inserted on click”.
-- 3. Completion item: An item that can be inserted into the text, including its metadata.
-- 4. Diagnostic: Any information about the project that is shown in the editor, including errors, warnings, and hints from tools such as hlint.
-- 5. Semantic highlighting: Special syntax highlighting performed by the server.
-- 6. Method: A LSP method is a function in the LSP protocol that the client can invoke to perform some action, e.g. ask for completions at a point.
-- 7. LSP: Language Server Protocol, a protocol used by HLS to communicate with editors and IDEs.

-- Code Execution in Haskell
-- to run this code, use the command: ghc hello.hs -o hello && ./hello
-- or use this command: runhaskell hello.hs
-- or use this command: runghc hello.hs (same as runhaskell)
-- or use this command: ghci hello.hs
-- or use this command: ghci hello.hs -o hello && ./hello
-- Key Differences:
-- ghci: Interactive environment for testing and debugging.
-- runghc/runhaskell: Non-interactive, used to execute Haskell scripts directly without creating a compiled binary.
-- or use this command: stack runghc hello.hs NO CAREFUL - it will try to upgrade before execution !!
-- or use this command: cabal run hello.hs    NO CAREFUL - it will try to upgrade before execution !!

-- Haskell Version Management
-- cd ~/.ghcup/
-- cd ~/.ghcup/ghc
-- ghcup list
-- ghcup set ghc <version>

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

