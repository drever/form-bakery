{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Common
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf (printf)

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
         checkTable2 "<<a><b>>" $ truthTable2 (&&)
     describe "or" $ do
         checkTable2 "ab" $ truthTable2 (||)
     describe "implication" $ do
         checkTable2 "<a>b" $ truthTable2 (\a b -> not a || b)
     describe "not" $ do
         checkTable1 "<a>" $ truthTable1 not
     describe "const b" $ do
         checkTable2 "<<<a>a><b>>" $ truthTable2 (\_ b -> b)
     describe "const a" $ do
         checkTable2 "<<<b>b><a>>" $ truthTable2 (\a _ -> a)

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

checkTable1 :: String
                      -> [(Expr, Either String Expr)]
                      -> Spec
checkTable1 exp t = mapM_ (\(a, r) ->
   let m = printf "%s, a = %2s => r = %s" exp (show a) (show r)
   in it m $
      let env = Map.fromList [('a', a)] :: Env
      in eval env (read exp) `shouldBe` r) $ t

checkTable2 :: String
                      -> [(Expr, Expr, Either String Expr)]
                      -> Spec
checkTable2 exp t = mapM_ (\(a, b, r) ->
   let m = printf "%s, a = %2s, b = %2s => r = %s" exp (show a) (show b) (show r)
   in it m $ -- (unwords [exp, ", a = ", show a, ", b = ", show b, " => r = ", show r]) $ do
     let env = Map.fromList [('a', a), ('b', b)] :: Env
     in eval env (read exp) `shouldBe` r) $ t


truthTable1 :: (Bool -> Bool)
                      -> [(Expr, Either a Expr)]
truthTable1 f =
    let fromBool b = if b then marked else unmarked
     in  [(fromBool a, Right . fromBool $ f a)
                           | a <- [False .. True]]

truthTable2 :: (Bool -> Bool -> Bool)
                      -> [(Expr, Expr, Either a Expr)]
truthTable2 f =
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



