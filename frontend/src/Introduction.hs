{-# LANGUAGE OverloadedStrings #-}

module Introduction where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseAndRenderWidget, consequence, highlight, id', section, SubSection (..))

primaryArithmetic :: (DomBuilder t m, PostBuild t m) => m ()
primaryArithmetic = do
  elAttr "h2" (id' "primary-arithmetic") $ text "The primary arithmetic"
  section [SubSection "Draw a distinction!" "pa-distinction" $ do
               el "p" $ do
                   text "Type "
                   highlight "<>"
                   text " into the textbox and draw a distinction! The expression consists of an opening and closing angle bracket."
               parseAndRenderWidget ""
               el "p" $ do
                   text "You should see a table with just one column. At the beginnig, the table was empty. When you started typing something you likely saw a parse error. Don't worry about this. When you drew the distinction by typing "
                   highlight "<>"
                   text " you will see the mark in the upper row, and the same mark in the lower row. The reason for this is, that the lower row is the evaluation of the expression in the upper row. If this doesn't make sense yet, that's ok. It should become clear in the next sections."
          , SubSection "Condense two marks"  "pa-condense" $ do
                el "p" $ do text "Write two marks next to each other by typing "
                            highlight "<><>"
                            text ". You should see two marks in the upper row. The lower row contains just one mark. This means that the expression with two marks is evaluated to an expression with just one mark. Two marks can be condensed to one mark. This also works repeatedly, try typing additional marks, e.g. "
                            highlight "<><><><><>"
                parseAndRenderWidget ""
          , SubSection "Cancel two marks" "pa-cancel" $ do
                el "p" $ do text "Now write one mark and cross this mark by typing "
                            highlight "<<>>"
                            text ". This will show a marked mark in the upper row. These two marks cancel out and the result, shown in the lower row, is the unmarked state."
                parseAndRenderWidget ""
          , SubSection "Build complex expressions with copy and paste"  "pa-complex" $ do
                el "p" $ do text "Write arbitrary expressions, e.g. " >> highlight "<<<<><>><>><>><><<<><>><><><>><>" >> text ". You can select an expression and then copy and paste it into the text box. Grab a pen and paper and evaluate the expressions by hand. Use the form bakery to confirm the results! Try copying expressions into other expressions, e.g. " >> highlight "<>" >> text " copied into the second mark of " >> highlight "<<>><>" >> text " gives the new expression " >> highlight "<<>><<>>" >> text "."
                parseAndRenderWidget ""
           , SubSection "Equality in the primary arithmetic" "pa-equality" $ do
                el "p" $ do text "Each expression has a value. The value of expressions in the primary arithmetic can be obtained in a finite number of steps. It is either the marked state " >> highlight "<>" >> text " or the unmarked state " >> highlight " " >> text ". When two expressions evaluate to the same state they are equal. Here you can see two expressions side-by-side which evaluate to the same value: "
                el "p" $ consequence "<<<<>>><>><<>><>" "<><><<<>><<>>>"

                el "p" $ do text "They both evaluate to the marked state " >> highlight "<>" >> text ". Here is another pair of expressions of equal value which evaluate to the unmarked state: "
                consequence "<<<>><>>" "<<>>"
      ]

primaryAlgebra :: (DomBuilder t m, PostBuild t m) => m ()
primaryAlgebra = do
  elAttr "h2" (id' "primary-algebra") $ text "The primary algebra"
  section [SubSection "Variables" "#palg-variables" $ do
                el "p" $ do
                     text "The primary arithmetic has only two symbols, the marked state " >> highlight "<>" >> text " and the unmarked state " >> highlight " " >> text ". We have seen that we may copy an expression and paste it into another expression "
                     elAttr "a" (mempty & at "href" ?~ "#pa-complex") $ text "in the previous section"
                     text ". Lets say you pasted the expression " >> highlight "<>" >> text " into the second mark of the expression " >> highlight "<<>><>" >> text ", resulting in " >> highlight "<<>><<>>" >> text ". You could equally well say, that there was an expression with a hole called " >> highlight "a" >> text ", and that this hole was assigned with " >> highlight "<>" >> text ", i.e. " >> highlight "a = <>" >> text ". What does it mean to assign " >> highlight " " >> text " to " >> highlight "a" >> text "? Try it and see how the value of the whole expressions changes:"
                parseAndRenderWidget "<<>><<>>"
                el "p" $ text "Assigning the unmarked state to " >> highlight "a" >> text ", i.e. " >> highlight " a = " >> text " gives the expression " >> highlight "<<>><>" >> text ". This changes the value of the expresssion to the marked state " >> highlight "<>" >> text ". Thus, we could write the variable " >> highlight "a" >> text " in the second mark of the expression to get:"
                parseAndRenderWidget "<<>><a>"
                el "p" $ text "The table has changed now, a new column appeared. The first column shows all possible assignments for the variable " >> highlight "a" >> text ". The second column shows the value of the expression, given the assignemnt of the variable. As we have seen before, " >> highlight "a = <>" >> text " evaluates to the unmarked state and " >> highlight "a = " >> text " evaluates to the marked state."
                el "p" $ text "Here is the simple expression " >> highlight "a" >> text ". Add a second variable by typing the expression " >> highlight "ab" >> text "."
                parseAndRenderWidget "a"
                el "p" $ do text "Now the table has three columns and four rows. (The first row is the header row and not counted any longer). The first column indicates all possible values for " >> highlight "a" >> text " and the second column indicates all possible values for " >> highlight "b" >> text ". The third column is the result column, it shows the value of the expression. In each row the variables have the values shown in the respective row, e.g. the second row shows that " >> highlight "a = <>" >> text "and" >> highlight "b = " >> text "gives " >> highlight "ab = <>" >> text ". Thus, the table is very similar to a truth table. Write the truth table for " >> highlight "<a>b" >> text "on a piece of paper by going through all possible values for the variables. Then use the form bakery to confirm the results."
         , SubSection "Equality" "#palg-equality" $ do
                el "p" $ do text "As we have "
                            elAttr "a" (mempty & at "href" ?~ "#pa-equality") $ text "have seen before"
                            text " two arithmetic expressions are equal, if they evaluate to the same state. The same is true for algebraic expressions. The following two expressions evaluate to the same value. I.e. they evaluate to the same state for all possible assignment of variables."
                el "p" $ consequence "<ab>" "<<<a>><<b>>>"
        , SubSection "Position" "palg-position" $ do
                el "p" $ do text "Consequences are shown by two truth tables side by side. Write the expression "
                            highlight "<<p>p>"
                            text " into the first truth table."
                consequence "" ""
                el "p" $ do text "The evaluated values of the expressions are the same for the two possible values of "
                            highlight "p"
                            text ", namely the unmarked state. You can see two expression side by side to confirm that they are equivalent by comparing the truth tables. Thus "
                            highlight "<<p>p> = "
                            text "."
      , SubSection "Transposition" "palg-transposition" $ do
                el "p" $ text "Now there is an expression with three variables and thus there are eight possibilities for the values. There is an equivalent expression which can be found by collecting the " >> highlight "r" >> text ". Can you find it?"
                consequence "<<pr><qr>>" "<<pr><qr>>"
                el "p" $ text "Spoiler: It's " >> highlight "<<p><q>>r" >> text ". Type this expression into the right truth table in order to confirm the equality"
   ]





