module TestLLSimulator where

import LL (progP, simulate)
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
tests = "testll" ~: TestCase $ testDirectory testLL "llprograms"
  where
    testLL :: FilePath -> Assertion
    testLL fp = do
      str <- readFile fp
      case parse (testFileParse LL.progP '/') "" str of
        Left err -> assertFailure $ "Could not parse \"" ++ fp ++ "\" with error: " ++ show err
        Right (expected, prog) -> case simulate prog of
          Left err -> assertFailure $ "Simulator failed on \"" ++ fp ++ "\" with error: " ++ err
          Right i -> assertEqual fp expected i
