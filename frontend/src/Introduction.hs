{-# LANGUAGE OverloadedStrings #-}

module Introduction where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T

import Markup (parseAndRenderWidget, consequence)

introduction :: (DomBuilder t m, PostBuild t m) => m ()
introduction = do
  el "h1" $ text "Introduction"

  el "h2" $ text "The primary arithmetic"
  el "h3" $ text "Draw a distinction!"

  el "p" $ text "This is a playground for the Laws of Form. Type `<>` into the textbox and draw a distinction!"
  parseAndRenderWidget ""
  el "p" $ text "You should see a table with just one column. At the beginnig, the table was empty. When you started typing something you likely saw a parse error. Don't worry about this. When you drew the distinction by typing `<>` you will see the mark in the upper row, and the same mark in the lower row. The reason for this is, that the lower row is the evaluation of the expression in the upper row. If this doesn't make sense yet, that's ok. It should become clear in the next sections."

  el "h3" $ text "Condense two marks"
  el "p" $ text "Write two marks next to each other by typing `<><>`. You should see two marks in the upper row. The lower row contains just one mark. This is because of Initial 1. Two marks can be condensed to one mark."
  parseAndRenderWidget ""

  el "h3" $ text "Cancel two marks"
  el "p" $ text "Now write one mark and cross this mark by typing `<<>>`. This will show a marked mark in the upper row. Following Initial 2 these two marks cancel out and the result, shown in the lower row, is the unmarked state."
  parseAndRenderWidget ""
  el "p" $ text "Write arbitrary expressions, e.g. `<<><>><>`. Grab a pen and paper and evaluate the expressions by hand."

  el "h2" $ text "The primary algebra"
  el "p" $ text "Now it's time for variables! Write the expression `a`. Now the table suddenly got a new column and a new row. The column stands for the variable `a`. This might be a bit confusing, because the whole expression is also `a`. Add a second variable by typing the expression `ab`."
  parseAndRenderWidget ""
  el "p" $ text "Now the table has three columns and four rows. (The first row is the header row and not counted). The first column indicates all possible values for `a` and the second column indicates all possible values for `b`. The third column is the result column, it shows the evaluation of the expression. In each row the variables have the values shown in the respective row, e.g. `a = <>` and `b = ` gives `ab = <>`. Thus, the table is very similar to a truth table. Write the truth table for `<a>b` on a piece of paper by going through all possible values for the variables. Then use the form bakery to confirm the results."

  el "h3" $ text "Position"
  el "p" $ text "Now there are two truth tables. Write the expression `<<p>p>` into the first truth table."
  consequence "" ""
  el "p" $ text "The evaluated values of the expressions are the same for the two possible values of `p`, namely the unmarked state. You can see two expression side by side to confirm, that they are equivalent by comparing the truth tables. Thus `<<p>p> = `."

  el "h3" $ text "Transposition"
  el "p" $ text "Now there is an expression with three variables and thus there are eight possibilities for the values. There is an equivalent expression which can be found by collecting the `r`. Can you find it?"
  consequence "<<pr><qr>>" "<<pr><qr>>"
  el "p" $ text "Spoiler: It's `<<p><q>>r`. Type this expression into the right truth table in order to confirm the equality"




