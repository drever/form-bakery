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
         checkTable $ truthTable "<<a><b>>" (&&)
     describe "or" $ do
         checkTable $ truthTable "ab" (||)
     describe "implies" $ do
         checkTable $ truthTable "<a>b" (\a b -> not a || b)




checkTable t = mapM_ (\(exp, a, b, r) ->
         it (unwords [exp, ", a = ", show a, ", b = ", show b, " => r = ", show r]) $ do
             let env = Map.fromList [('a', a), ('b', b)] :: Env
             eval env (read exp) == r) $ t

truthTable p f =
    let fromBool b = if b then marked else unmarked
     in  [(p, fromBool a, fromBool b, Right . fromBool $ a `f` b)
                           | a <- [False .. True]
                           , b <- [False .. True]]

instance Arbitrary Expr where
    arbitrary = frequency [
            (35, return $ marked)
          , (1, Cross <$> arbitrary)
          , (1, Call <$> listOf arbitrary)
         ]



