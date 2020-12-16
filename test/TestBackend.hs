module TestBackend where

import Backend (compileProg)
import LC4 (simulate)
import LL (Prog, progP, simulate)
import Parser (testFileParse)
import Programs (lc4AddTwo, llAddTwo)
import Test.HUnit
  ( Assertable (assert),
    Assertion,
    Test (TestCase, TestList),
    assertEqual,
    (~:),
    (~?=),
  )
import Test.QuickCheck (Property, within)
import TestUtils (testDirectory)
import Text.Parsec (parse)

-- Unit tests
tests :: Test
tests = TestList [tCompileAdd, tLlTest]

tLlTest :: Test
tLlTest = "testll" ~: TestCase $ testDirectory testLL "llprograms"
  where
    testLL :: FilePath -> Assertion
    testLL fp = do
      str <- readFile fp
      case parse (testFileParse LL.progP '/') "" str of
        Left _ -> assert False
        Right (_, prog) -> case (LL.simulate prog, LC4.simulate (compileProg prog)) of
          (Right i, Right j) -> assertEqual fp i j
          _ -> assert False

tCompileAdd :: Test
tCompileAdd = lc4AddTwo ~?= compileProg llAddTwo

-- QuickCheck

prop_compile :: LL.Prog -> Property
prop_compile p = within 1000000 $ case (LL.simulate p, LC4.simulate (compileProg p)) of
  (Right i, Right j) -> i == j
  _ -> False
