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
     describe "and" $ do
         checkTable "<<a><b>>" $ truthTable (&&)
     describe "or" $ do
         checkTable "ab" $ truthTable (||)
     describe "implication" $ do
         checkTable "<a>b" $ truthTable (\a b -> not a || b)


checkTable exp t = mapM_ (\(a, b, r) ->
         it (unwords [exp, ", a = ", show a, ", b = ", show b, " => r = ", show r]) $ do
             let env = Map.fromList [('a', a), ('b', b)] :: Env
             eval env (read exp) `shouldBe` r) $ t

truthTable f =
    let fromBool b = if b then marked else unmarked
     in  [(fromBool a, fromBool b, Right . fromBool $ a `f` b)
                           | a <- [False .. True]
                           , b <- [False .. True]]

instance Arbitrary Expr where
    arbitrary = frequency [
            (35, return $ marked)
          , (1, Cross <$> arbitrary)
          , (1, Call <$> listOf arbitrary)
         ]



