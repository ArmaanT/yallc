import Test.HUnit (Test (TestList), runTestTT)
import Test.QuickCheck
  ( Args (maxSize, maxSuccess),
    Testable,
    quickCheckWith,
    stdArgs,
  )
import TestBackend (prop_compile, tests)
import TestLC4Simulator (prop_roundtrip, tests)
import TestLLSimulator (tests)

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

main :: IO ()
main = do
  putStrLn "Running unit tests:"
  runTestTT $
    TestList
      [ TestLC4Simulator.tests,
        TestLLSimulator.tests,
        TestBackend.tests
      ]
  putStrLn "Running quickcheck tests:"
  quickCheckN 500 TestBackend.prop_compile
  quickCheckN 500 TestLC4Simulator.prop_roundtrip
  return ()
