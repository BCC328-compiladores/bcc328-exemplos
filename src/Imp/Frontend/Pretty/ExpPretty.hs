{-# LANGUAGE FlexibleInstances #-}
module Imp.Frontend.Pretty.ExpPretty ( Pretty (..)
                                     , render
                                     ) where

import Text.PrettyPrint

import Imp.Frontend.Syntax.Exp

class Pretty a where
  pprint :: a -> Doc

  pprintNest :: Int -> a -> Doc

  pprint = pprintNest 0
  pprintNest _ = pprint

instance Pretty Value where
  pprint (EInt v) = int v
  pprint (EBool b) = text $ if b then "true" else "false"

instance Pretty String where
  pprint = text

instance Pretty Int where
  pprint = int

instance Pretty a => Pretty (Var a) where
  pprint (Var v) = pprint v

instance Pretty a => Pretty (Exp a) where
  pprint = pprExpr 0

pprExpr :: Pretty a => Int -> Exp a -> Doc
pprExpr _ (EValue v) = pprint v
pprExpr _ (EVar v) = pprint v
pprExpr p (e1 :+: e2)
  = parensWhen (p > 3) $ pprExpr 4 e1 <+> char '+' <+> pprExpr 3 e2
pprExpr p (e1 :*: e2)
  = parensWhen (p > 4) $ pprExpr 5 e1 <+> char '*' <+> pprExpr 4 e2
pprExpr p (e1 :-: e2)
  = parensWhen (p > 3) $ pprExpr 4 e1 <+> char '-' <+> pprExpr 3 e2
pprExpr p (e1 :/: e2)
  = parensWhen (p > 4) $ pprExpr 5 e1 <+> char '/' <+> pprExpr 4 e2
pprExpr p (e1 :==: e2)
  = parensWhen (p > 1) $ pprExpr 2 e1 <+> text "==" <+> pprExpr 1 e2
pprExpr p (e1 :<: e2)
  = parensWhen (p > 1) $ pprExpr 2 e1 <+> text "<" <+> pprExpr 1 e2
pprExpr p (ENot e1)
  = parensWhen (p > 2) $ text "!" <+> pprExpr 1 e1
pprExpr p (e1 :&: e2)
  = parensWhen (p > 2) $ pprExpr 2 e1 <+> text "&&" <+> pprExpr 1 e2 

parensWhen :: Bool -> Doc -> Doc
parensWhen True d = parens d
parensWhen False d = d
