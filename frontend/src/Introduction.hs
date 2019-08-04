{-# LANGUAGE OverloadedStrings #-}

module Introduction where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseAndRenderWidget, consequence, highlight, id')

primaryArithmetic :: (DomBuilder t m, PostBuild t m) => m ()
primaryArithmetic = do

  elAttr "h2" (id' "primary-arithmetic") $ text "The primary arithmetic"
  el "h3" $ text "Draw a distinction!"

  el "p" $ do text "Type "
              highlight "<>"
              text " (open and close brackets) into the textbox and draw a distinction!"
  parseAndRenderWidget ""
  el "p" $ do text "You should see a table with just one column. At the beginnig, the table was empty. When you started typing something you likely saw a parse error. Don't worry about this. When you drew the distinction by typing "
              highlight "<>"
              text " you will see the mark in the upper row, and the same mark in the lower row. The reason for this is, that the lower row is the evaluation of the expression in the upper row. If this doesn't make sense yet, that's ok. It should become clear in the next sections."

  elAttr "h3" (id' "pa-condense") $ text "Condense two marks"
  el "p" $ do text "Write two marks next to each other by typing "
              highlight "<><>"
              text ". You should see two marks in the upper row. The lower row contains just one mark. This means that the expression with two marks is evaluated to an expression with just one mark. Two marks can be condensed to one mark. This also works repeatedly, try typing additional marks, e.g. "
              highlight "<><><><><>"
  parseAndRenderWidget ""

  el "h3" $ text "Cancel two marks"
  el "p" $ do text "Now write one mark and cross this mark by typing "
              highlight "<<>>"
              text ". This will show a marked mark in the upper row. These two marks cancel out and the result, shown in the lower row, is the unmarked state."
  parseAndRenderWidget ""
  el "p" $ do text "Write arbitrary expressions, e.g. "
              highlight "<<<<><>><>><>><><<<><>><><><>><>"
              text ". Grab a pen and paper and evaluate the expressions by hand. Use the form bakery to confirm the results!"

  el "h3" $ text "Equality in the primary arithmetic"
  el "p" $ do text "Each expression has a value, which can be obtained in a finite number of steps. The value is either the marked state "
              highlight "<>"
              text " or the unmakred state "
              highlight " "
              text ". When two expressions evaluate to the same state they are equal. Here you can see two expressions side-by-side which evaluate to the same value: "
              el "p" $ consequence "<<<<>>><>><<>><>" "<><><<<>><<>>>"

              text "They both evaluate to the marked state "
              highlight "<>"
              text ". Here is another pair of expressions of equal value which evaluates to the unmarked state: "
              consequence "<<<<><><>>>>" "<<<<<<><><>>>>><<<<><><>>>>>"

primaryAlgebra :: (DomBuilder t m, PostBuild t m) => m ()
primaryAlgebra = do
  elAttr "h2" (mempty & at "id" ?~ "primary-algebra") $ text "The primary algebra"
  el "p" $ do text "Now it's time for variables! Write the expression "
              highlight "a"
              text ". The table got a new column and a new row. The column stands for the variable "
              highlight "a"
              text ". This might be a bit confusing, because the whole expression is also "
              highlight "a"
              text ". Add a second variable by typing the expression "
              highlight "ab"
              text "."
  parseAndRenderWidget ""
  el "p" $ do text "Now the table has three columns and four rows. (The first row is the header row and not counted any longer). The first column indicates all possible values for "
              highlight "a"
              text " and the second column indicates all possible values for "
              highlight "b"
              text ". The third column is the result column, it shows the evaluation of the expression. In each row the variables have the values shown in the respective row, e.g. the second row shows that "
              highlight "a = <>"
              text "and"
              highlight "b = "
              text "gives "
              highlight "ab = <>"
              text ". Thus, the table is very similar to a truth table. Write the truth table for "
              highlight "<a>b"
              text "on a piece of paper by going through all possible values for the variables. Then use the form bakery to confirm the results."

  el "h3" $ text "Position"
  el "p" $ do text "Consequences are shown by two truth tables side by side. Write the expression "
              highlight "<<p>p>"
              text " into the first truth table."
  consequence "" ""
  el "p" $ do text "The evaluated values of the expressions are the same for the two possible values of "
              highlight "p"
              text ", namely the unmarked state. You can see two expression side by side to confirm, that they are equivalent by comparing the truth tables. Thus "
              highlight "<<p>p> = "
              text "."

  el "h3" $ text "Transposition"
  el "p" $ text "Now there is an expression with three variables and thus there are eight possibilities for the values. There is an equivalent expression which can be found by collecting the `r`. Can you find it?"
  consequence "<<pr><qr>>" "<<pr><qr>>"
  el "p" $ text "Spoiler: It's " >> highlight "<<p><q>>r" >> text ". Type this expression into the right truth table in order to confirm the equality"




