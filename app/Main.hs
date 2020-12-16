{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Backend (compileProg)
import LC4 (simulate)
import LL (progP, simulate)
import PP (indented)
import System.Console.CmdArgs
  ( Data,
    Default (def),
    Typeable,
    args,
    cmdArgs,
    help,
    program,
    summary,
    typFile,
    (&=),
  )
import Text.Parsec (parse)
import Utils (runSimulator)

main :: IO ()
main = do
  args <- cmdArgs cli
  let filepath = file args
  let ll = runll args
  let lc4 = runlc4 args
  str <- readFile filepath
  case parse progP filepath str of
    Left err -> print err
    Right prog ->
      let lc4Prog = compileProg prog
          asmFileName = replaceFileName filepath
       in do
            writeFile asmFileName (indented lc4Prog)
            if debug args
              then print prog >> print lc4Prog
              else pure ()
            if ll
              then
                putStrLn "Running LL:"
                  >> runSimulator LL.simulate prog
                  >> putStrLn ""
              else pure ()
            if lc4
              then
                putStrLn "Running LC4:"
                  >> runSimulator LC4.simulate lc4Prog
              else pure ()

data CLI = Cli {runll :: Bool, runlc4 :: Bool, debug :: Bool, file :: FilePath} deriving (Show, Data, Typeable)

cli :: CLI
cli =
  Cli
    { runll = False &= help "Run ll through simulator",
      runlc4 = False &= help "Run compiled lc4 through simulator",
      debug = False &= help "Show LL & LC4 ASTs",
      file = def &= args &= typFile
    }
    &= help "Compile an .ll file into an LC4 .asm file"
    &= summary "Yet Another LLVM to LC4 Compiler"
    &= program "yallc"

replaceFileName :: String -> String
replaceFileName ".ll" = ".asm"
replaceFileName (x : xs) = x : replaceFileName xs
replaceFileName _ = error "Invalid file. Must end with .ll"
