module TestUtils where

import System.Directory (listDirectory)
import Test.HUnit (Assertion)

testDirectory :: (FilePath -> Assertion) -> FilePath -> Assertion
testDirectory test dir = do
  files <- listDirectory dir
  mapM_ (test . ((dir ++ "/") ++)) files
