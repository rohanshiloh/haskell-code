# Haskell for Good of Human Beans

<!-- We recommend installing both. Most Haskell projects can be built using Cabal, but some might require Stack. Installing both guarantees that you can use either, and while following a tutorial or book you can use whatever they recommend. -->


## Summary 
 - GHC will produce an executable when the source file satisfies both conditions:
 1. Defines the main function in the source file
 2. Defines the module name to be Main (this can be done by adding module Main where at the top of the file), or does not have a module declaration (which is then inferred as the module Main). Otherwise, it will only produce the .o and .hi files.
 3. Module: A module is a collection of Haskell code that can be imported and used in other modules.
 4. Package: A package is a collection of modules that are distributed together.
 5. Project: A project is a collection of packages that are built together.
 6. Stack: A build tool for Haskell that manages project dependencies and builds.
 7. Cabal: A system for building and packaging Haskell libraries and programs.

## Terminal
 1. ghcup is used to install and manage Haskell toolchain versions
 2. ghc is the Glasgow Haskell Compiler
 3. runhaskell or runghc is a command to run Haskell scripts directly
 4. stack is a Haskell build tool that manages project dependencies
 5. cabal is a system for building and packaging Haskell libraries and programs
    - so stack and cabal are both build tools, but they have different approaches
    - stack is more opinionated and provides a curated environment
    - while cabal is more flexible and allows for more customization
 6. ghci is the interactive environment for Haskell
 7. runghc see #3 above

 ## HLC or the Haskell Language Server 
 is a server that communicates with editors and IDEs using the Language Server Protocol (LSP).
 
 ### Here are a few pieces of jargon that you may come across in the HLS docs or when discussing problems:
 1. Code action: A code action is a specific action triggered by a user on a particular region of code.   
    - Examples might include “add a type signature to this function”.
 2. Code lens: A pre-rendered edit or action shown in the body of the document itself, usually triggered with a click.
    - Examples might include “the type signature for a function, which is actually inserted on click”.
 3. Completion item: An item that can be inserted into the text, including its metadata.
 4. Diagnostic: Any information about the project that is shown in the editor, including errors, warnings, and hints from tools such as hlint.
 5. Semantic highlighting: Special syntax highlighting performed by the server.
 6. Method: A LSP method is a function in the LSP protocol that the client can invoke to perform some action, e.g. ask for completions at a point.
 7. LSP: Language Server Protocol, a protocol used by HLS to communicate with editors and IDEs.
> [!NOTE]  
> Press Ctrl+Shift+P and click 'Haskell: Restart Haskell LSP Server' (start typing to find it).




 ## Code Execution in Haskell
 1. to start the interactive environment, use the command: `ghci`
 2. to load a file in the interactive environment, use the command: `:load hello.hs`
 3. to compile and run this code, use the command: `ghc hello.hs -o hello && ./hello`
 4. with warnings: `ghc -Wall hello.hs -fforce-recomp -o hello && ./hello`
 5. or use this command: `runhaskell hello.hs`
 6. or use this command: `runghc hello.hs (same as runhaskell)`
 7. or use this command: `ghci hello.hs`
 8. or use this command: `ghci hello.hs -o hello && ./hello`
 ### Key Differences:
 - ghci: Interactive environment for testing and debugging.
 - runghc & runhaskell: Non-interactive, used to execute Haskell scripts directly without creating a compiled binary.
 - or use this command: `stack runghc hello.hs` NO CAREFUL - it will try to upgrade before execution !!
 - or use this command: `cabal run hello.hs`    NO CAREFUL - this doesn't work

> [!WARNING]  
> Running any stack command will try to install/upgrade? maybe not if there is a stack.yaml file in the project?
> this is for initializing a package but don't run if there is already a stack.ayml file available
> stack init --force linear-regression  
> stack clean 
> stack build
> stack run








### Notes for Running the linear-regression Project
1. `stack init --force linear-regression` this is not be necessary if there is already a stack.yaml file
2. FOR ERROR: HLS does not support GHC 9.8.4 yet.
  - use LTS Haskell 21.24 (ghc-9.4.8)
  - stack.yaml: `snapshot: lts-21.24`
  or\
  - Control whether we use the GHC we find on the path: `stack.yaml: system-ghc: true`
3. `stack init --force linear-regression` will overwrite the existing stack.yaml and choose its own resolver
  - you want to use the resolver `lts-21.24` for `ghc-9.4.8`
  - or one of the other `ghc-9.4.8` resolvers from here: https://www.stackage.org/snapshots?page=10
4. Build & Run & Clean & Re-Run
  1. `stack clean` : deletes build artefacts for all project packages - run this to re-build
  2. `stack build` : build locally
  3. `stack run`







 ## Haskell Version Management
 - `cd ~/.ghcup/`
 - `cd ~/.ghcup/ghc`
 - `ghcup list`
 - `ghcup set ghc <version>`

 ## Linear Algebra : BLAS + LAPACK
 ### BLAS (Basic Linear Algebra Subprograms) and LAPACK (Linear Algebra Package) are libraries for performing linear algebra operations.
 - BLAS is a set of low-level routines for performing basic vector and matrix operations.
 - LAPACK is built on top of BLAS and provides higher-level routines for solving linear algebra problems.
 - BLAS and LAPACK are widely used in scientific computing, machine learning, and data analysis.
 ### To install BLAS and LAPACK on macOS using Homebrew, you can use the following commands:
 - `brew install openblas`
 - `brew install lapack`
 - openblas is keg-only, which means it was not symlinked into /opt/homebrew,
 - because macOS provides BLAS in Accelerate.framework.
 ### For compilers to find openblas you may need to set:
 - `export LDFLAGS="-L/opt/homebrew/opt/openblas/lib $LDFLAGS"`
 - `export CPPFLAGS="-I/opt/homebrew/opt/openblas/include $CPPFLAGS"`
 ### and for the Haskell compiler to find openblas you may need to set:
 - `export PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig $PKG_CONFIG_PATH"`

 - lapack is keg-only, which means it was not symlinked into `/opt/homebrew`,
 - because macOS provides LAPACK in Accelerate.framework.
 - For compilers to find lapack you may need to set:
 -  `export LDFLAGS="-L/opt/homebrew/opt/lapack/lib $LDFLAGS"`
 -  `export CPPFLAGS="-I/opt/homebrew/opt/lapack/include $CPPFLAGS"`
 - and for the Haskell compiler to find lapack you may need to set:
 -  `export PKG_CONFIG_PATH="/opt/homebrew/opt/lapack/lib/pkgconfig $PKG_CONFIG_PATH"`
 - ==> Summary
 -  `/opt/homebrew/Cellar/lapack/3.12.1`: 30 files, 16.2MB

$~~~~~~~~~~~~~~$

- [x] linear algebra in haskell
- [x] linear regression in haskell with a plot
- [ ] portfolio (quadratic) optimization in haskell
- [ ] neural networks in haskell
- [ ] cuda for GPU distribution in haskell 

> [!NOTE]  
> Highlights information that users should take into account, even when skimming.

> [!TIP]
> Optional information to help a user be more successful.

> [!IMPORTANT]  
> Crucial information necessary for users to succeed.

> [!WARNING]  
> Critical content demanding immediate user attention due to potential risks.

> [!CAUTION]
> Negative potential consequences of an action.


$~~~~~~~~~~~~~~~~~~~~$


 [www.cs.mcgill.ca/~rshah3](http://www.cs.mcgill.ca/~rshah3)


 ## References

 [Linear Regression Using Haskell - Sam Gardner](https://samcgardner.github.io/2018/10/06/linear-regression-in-haskell.html)