module Parsing.LL.Expr (exprGrammar) where

import Parsing.LL.Grammar

exprGrammar :: Grammar
exprGrammar = Grammar prods e
  where
    e = NT "E"
    e' = NT "E'"
    t  = NT "T"
    t' = NT "T'"
    f  = NT "F"
    plus = T "+"
    times = T "*"
    lparen = T "("
    rparen = T ")"
    tid = T "id"
    prods = [ e  +-> [ Var t , Var e']
            , e' +-> [ Symb plus
                     , Var t
                     , Var e']
            , e' +-> [Symb Lambda]
            , t  +-> [Var f , Var t']
            , t' +-> [ Symb times
                     , Var f
                     , Var t']
            , t' +-> [ Symb Lambda ]
            , f  +-> [ Symb lparen
                     , Var e
                     , Symb rparen ]
            , f +->  [Symb tid ]
            ]
