module TestLC4Simulator where

import LC4 (Program, programP, simulate)
import PP (indented)
import Parser (testFileParse)
import Test.HUnit
  ( Assertion,
    Test (TestCase),
    assertEqual,
    assertFailure,
    (~:),
  )
import TestUtils (testDirectory)
import Text.Parsec (parse)

tests :: Test
tests = "testlc4" ~: TestCase $ testDirectory testLC4 "lc4programs"
  where
    testLC4 :: FilePath -> Assertion
    testLC4 fp = do
      str <- readFile fp
      case parse (testFileParse LC4.programP ';') "" str of
        Left err -> assertFailure $ "Could not parse \"" ++ fp ++ "\" with error: " ++ show err
        Right (expected, prog) -> case simulate prog of
          Left err -> assertFailure $ "Simulator failed on \"" ++ fp ++ "\" with error: " ++ err
          Right i -> assertEqual fp expected i

prop_roundtrip :: Program -> Bool
prop_roundtrip s = parse programP "" (indented s) == Right s
