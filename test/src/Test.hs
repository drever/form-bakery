{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Common
import qualified Data.Map as Map
import qualified Data.Text as T

main :: IO ()
main = hspec $ sequence_ [
            parsing
          , evaluation
          , manipulation]

parsing =
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

evaluation =
  describe "Evaluation" $ do
     describe "and" $ do
         checkTable "<<a><b>>" $ truthTable (&&)
     describe "or" $ do
         checkTable "ab" $ truthTable (||)
     describe "implication" $ do
         checkTable "<a>b" $ truthTable (\a b -> not a || b)

manipulation =
   describe "Manipulation" $ do
      let check e p r = do
              it (unwords ["insertMarkAt", e, p, "should be", r]) $ do
                insertMarkAt (T.pack p) (read e) `shouldBe` (read r)
       in do check "" "B" "<>"
             check "<>" "CB" "<<>>"
             check "<>" "0CB" "<<>>"
             check "<<>>" "CCB" "<<<>>>"
             check "<><><>" "0CB" "<<>><><>"
             check "<><><>" "1CB" "<><<>><>"
             check "<><><>" "2CB" "<><><<>>"
             check "<<><><>>" "C2CB" "<<><><<>>>"
             check "<<><><b>>" "C2CB" "<<><><<b>>>"
             check "<<a>b>" "B" "<<<a>b>>"

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



