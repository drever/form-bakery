import Test.Tasty
import Test.Tasty.HUnit

maint = defaultMain tests

tests :: TestTree
tests = testGroup "Parsing" $
    map (\c -> testCase ("show . read $ " ++ c) $ show . read $ c @?= c) $ [
          "<>"
        , "<><>"
        , "<<>>"
        , "<<><>>"
        , "<<<><>><>>" ]

