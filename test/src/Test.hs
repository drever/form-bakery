import Test.Hspec
import Test.QuickCheck

import Common

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


instance Arbitrary Expr where
    arbitrary = frequency [
            (35, return $ marked)
          , (1, Cross <$> arbitrary)
          , (1, Call <$> listOf arbitrary)
         ]

