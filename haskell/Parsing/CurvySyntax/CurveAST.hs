{---- Original Grammar ----}
{-
Program -> Defs
Defs    -> Def
         | Def Defs
Def     -> Ident '=' Curve
         | Ident '=' Curve 'where' '{' Defs '}'
Curve   -> Curve '++' Curve
         | Curve '^' Curve

         | Curve '->' Point
         | Curve '**' Expr
         | Curve 'refv' Expr
         | Curve 'refh' Expr
         | Curve 'rot' Expr

         | '(' Curve ')'
         | Point
         | Ident
Point   -> '(' Expr ',' Expr ')'
Expr    -> Expr '+' Expr
         | Expr '*' Expr
         | 'width' Curve
         | 'height' Curve
         | Number
         | '(' Expr ')'
-}

{---- Rewritten Gramar ----}

{- Not rewritten
Program -> Defs
-}

{- Defs Rewritten by left-factorization
        ( alternatives start with the same non-terminal ) 

Defs    -> Def
         | Def Defs
Def     -> Ident '=' Curve Dopt
Dopt    -> 'where' '{' Defs '}'
         | epsilon.
-}

{- Curve Rewritten for immediate left-recursive
   
Curve -> Point         CurveOpt
       | Ident         CurveOpt
       | '(' Curve ')' CurveOpt

CurveOpt -> "++"   Curve CurveOpt
          | "^"    Curve CurveOpt
          | "->"   Point CurveOpt
          | "**"   Expr  CurveOpt
          | "refv" Expr  CurveOpt
          | "refh" Expr  CurveOpt
          | "rot"  Expr  CurveOpt
-}

{- Not rewritten
Point -> '(' Expr ',' Expr ')'
-}

{- Expt rewritten for immediate left-recursion and operator precedense

Expr -> Expr '+' Expr
      | Expr '*' Expr
      | 'width' Curve
      | 'height' Curve
      | Number
      | '(' Expr ')'

-- Precedece !This is enough if using chainl1
Expr -> Expr + Term   | Term
Term -> Term * Factor | Factor
Factor -> 'width'  Curve
        | 'height' Curve
        | Number
        | '(' Expr ')'

-- Left recursion removed !This is for more "direct" translation
Expr    -> Term ExprOpt
ExprOpt -> + Term ExprOpt | epsilon
Term    -> Factor TermOpt
TermOpt -> * Factor TermOpt | epsilon
Factor  -> 'width'  Curve
         | 'height' Curve
         | Number
         | '(' Expr ')'
-}

-- | AST Datatypes
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
