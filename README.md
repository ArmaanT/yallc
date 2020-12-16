# yallc

Yet another LLVM to LC4 compiler.

Allows you to compile a `.ll` file into an LC4 `.asm` as well as run the LL and LC4 programs through simulators.

A CIS 552 final project.

## Usage

Build and install the yallc executable:

``` bash
stack install
```

Then run the following to see how to use yallc

``` bash
yallc --help
```

## Libraries Used

* pretty - Used for pretty printing LC4
* parsec - Used for parsing LL + LC4 files
* QuickCheck-GenT - Provides a monad transformer for QuickCheck generators
* cmdargs - Used for the actual compiler cli

## Modules

### Main

The yallc CLI. Allows you to compile a single `.ll` file into an LC4 `.asm` file. Additionally provides options to run the code through either the LL simulator or LC4 simulator as well as a debug option that shows the ASTs of the two programs.

### LC4

A module representing the AST of an LC4 program, a parser to read LC4 files, a pretty printer to write `.asm` files, a simulator to run LC4 programs as well as a generator to generate LC4 programs.

### LL

A module representing the AST of an LL program, a parser to read LL files, a simulator to run LL programs as well as a generator to generate LL programs.

### Backend

The actual compiler. Lives entirely in the state monad. Compiles an LL program into an LC4 program with the same functionality.

### Misc

`Parser.hs` and `PP.hs` files contain helpful functions for both parsing and pretty-printing. `Utils.hs` contains a runSimulator function to run the two different simulators as well as definitions that allow `MonadState` and `MonadGen` to interact.

### Testing

Tests for the compiler and generators. Consists of unit tests for both simulators and the compiler as well as a quickcheck compiler property and a roundtrip parsing property for LC4.

## Unfinished work/Optimizations that could be made

* Structs in LLVM aren't supported, this is probably the largest missing feature in yallc
* The backend should probably be rewritten using `MonadError` on top of the State monad.
* More of the LL simulator should exist in the state monad so we don't have to explicitly pass around portions of the state.
