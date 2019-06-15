import Test.Hspec
import Test.QuickCheck

import Common
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    mapM_ (\c -> it ("show . read is identity for: " ++ c) $ do
           (let pe = read c :: Expr
            in show pe) `shouldBe` c) [
                    ""
                  , "<>"
                  , "<><>"
                  , "<<>>"
                  , "<<><>>"
                  , "<<<><>><>>"
                  , "<<<>><><>>" ]
    it "show . read for arbitrary expressions id identity" $ do
        property $ \e -> let s = show (e :: Expr)
                          in (show . (read :: String -> Expr) $ s) == (s :: String)
  describe "Evaluation" $ do
     mapM_ (\(exp, a, b, r) ->
         it (unwords [exp, ", a = ", show a, ", b = ", show b, " => r = ", show r]) $ do
             let env = Map.fromList [('a', a), ('b', b)] :: Env
             eval env (read exp) == r)
        [ ("<<a><b>>", marked, marked, Just marked)
        , ("<<a><b>>", unmarked, marked, Just unmarked)
        -- , ("<<a><b>>", marked, unmarked, Just unmarked)
        -- , ("<<a><b>>", unmarked, unmarked, Just unmarked)
        ]






instance Arbitrary Expr where
    arbitrary = frequency [
            (35, return $ marked)
          , (1, Cross <$> arbitrary)
          , (1, Call <$> listOf arbitrary)
         ]



