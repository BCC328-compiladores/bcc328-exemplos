{-# OPTIONS_GHC -Wno-orphans#-}
module Imp.Frontend.Pretty.StatementPretty (prettyProgram) where

import Text.PrettyPrint

import Imp.Frontend.Syntax.Exp
import Imp.Frontend.Syntax.Statement
import Imp.Frontend.Typing.Ty
import Imp.Frontend.Pretty.ExpPretty


instance Pretty Ty where
  pprint TInt = text "int"
  pprint TBool = text "bool"

prettyInit :: Pretty a => Maybe (Exp a) -> Doc
prettyInit Nothing = text ""
prettyInit (Just e) = text "=" <+> pprint e

level :: Int
level = 3

instance Pretty a => Pretty (Block a) where
  pprintNest d
    = nest d . hcat . punctuate (text ";") . map (pprintNest d) . unBlock

instance Pretty a => Pretty (Stmt a) where
  pprint = prettyStmt' 0
  pprintNest = prettyStmt'

prettyElse :: Pretty a => Int -> Block a -> Doc
prettyElse _ (Block []) = text ""
prettyElse d s  = pprintNest d s

prettyStmt' :: Pretty a => Int -> Stmt a -> Doc
prettyStmt' _ Skip = text "skip"
prettyStmt' _ (Def t v e)
  = pprint t <+> pprint v <+> prettyInit e
prettyStmt' _ (v := e)
  = pprint v <+> text ":=" <+> pprint e
prettyStmt' _ (Print e)
  = text "print" <+> pprint e
prettyStmt' _ (SRead v)
  = text "read" <+> pprint v
prettyStmt' d (If e s1 s2)
  = text "if"   <+> pprint e <+>
    text "then" <+> pprintNest (d + level) s1 <+>
    prettyElse (d + level) s2
prettyStmt' d (While e s)
  = text "while" <+> pprint e <+>
    text "do" <+> pprintNest (d + level) s


prettyProgram :: Pretty a => Program a -> String
prettyProgram = render . pprint
