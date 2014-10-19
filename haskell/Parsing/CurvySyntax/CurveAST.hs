-- | Grammar G
-- Program ::= Defs
-- Defs    ::= Def
---         |  Def Defs
-- Def   ::= Ident '=' Curve
---       |  Ident '=' Curve 'where' '{' Defs '}'
-- Curve ::= Curve '++' Curve
---       |  Curve '^' Curve
---       |  Curve '->' Point
---       |  Curve '**' Expr
---       |  Curve 'refv' Expr
---       |  Curve 'refh' Expr
---       |  Curve 'rot' Expr
---       |  '(' Curve ')'
---       |  Point
---       |  Ident
-- Point ::= '(' Expr ',' Expr ')'
-- Expr  ::= Expr '+' Expr
-- Expr  '*' Expr
--        | 'width' Curve
--        | 'height' Curve
--        | Number
--        | '(' Expr ')'

-- | AST
module CurvySyntax.CurveAST where

type Program = [Def]

data Def = Def Ident Curve [Def] deriving (Eq, Show)

data Curve = Connect Curve Curve
           | Over Curve Curve
           | Translate Curve Point
           | Scale Curve Expr
           | Refv Curve Expr
           | Refh Curve Expr
           | Rot Curve Expr
           | Single Point
           | Id Ident
           deriving (Eq, Show)

data Point = Point Expr Expr deriving (Eq, Show)
data Expr  = Mult Expr Expr
           | Add Expr Expr
           | Width Curve
           | Height Curve
           | Const Number
           deriving (Eq, Show)

type Ident  = String
type Number = Double
