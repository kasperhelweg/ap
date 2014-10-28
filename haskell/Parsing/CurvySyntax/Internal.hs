-- In Haskell, create a Molamil Docker API to set up virtual machines.
-- In erlang create a push api and the julebryg game. push API using map reduce?

module CurvySyntax.Internal where
import Debug.Trace

import Data.Char

import Parser.SimpleParse
import CurvySyntax.CurveAST

import Control.Monad(liftM)
import Control.Applicative ((<|>))
--import Control.Applicative ((<|>),(<$>))

-- |
-- Types
--
data ErrorType = ParseError
               deriving (Show, Read, Eq)

data Error = Error { errorType :: ErrorType }
           deriving (Show, Eq)

-- |
-- API
--

--- Accepting parser
parseString' input = parse ( defs >> eof ) input

--- Attibuted Parser
--parseString :: String -> Either Error Expr2
parseString input = case liftM fst $ parseEof ( defs >>= \result -> return result ) input of
                     []     -> Left Error { errorType = ParseError }
                     [s]    -> Right s
                     (x:xs) -> trace ("calling f with x = " ++ show (x:xs)) Right x

--- parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

-- |
-- Implementation
--

-- Redundant...

reserved = ["where", "refv", "refh", "rot", "width", "height"]

program = do defs >>= \ds -> return ds

defs = do many1 def >>= \ds -> return ds
       
def = do id <- ident
         schar '='
         curve <- curves
         dopt  <- dopt

         return $ Def id curve dopt
  
dopt = do symbol "where"
          schar '{'
          ds <- defs
          schar '}'
          
          return ds
       <|> return []

{-
One can use this : Remove the ++ and ^ alternative from curveopt, and rename
current curves function to curve.

cop0 = do symbol "++"
          return Connect
cop1 = do schar '^'
          return Over

curves  = chainl1 curves' cop0
          <++ curve
curves' = chainl1 curve   cop1
-}
        
-- C0 : Curves without left recursion, but no precedense
curves = do c  <- curvet
            cv <- curveopt c
            return cv
            
curvet = do p <- point
            return (Single p)
         <|> do id <- ident
                return (Id id)
         <|> do schar '('
                c <- curves
                schar ')'
                return c

curveopt inval = do symbol "++"
                    c  <- curvet
                    cv <- curveopt (Connect inval c)
                     
                    return cv
                 <|> do symbol "^"
                        c  <- curvet
                        cv <- curveopt (Over inval c)
                        
                        return cv
                 <|> do symbol "->"
                        p  <- point
                        cv <- curveopt (Translate inval p)
                        
                        return cv
                 <|> do symbol "**"
                        e  <- expr
                        cv <- curveopt (Scale inval e)
                        
                        return cv
                 <|> do symbol "refv"
                        e  <- expr
                        cv <- curveopt (Refv inval e)

                        return cv
                 <|> do symbol "refh"
                        e  <- expr
                        cv <- curveopt (Refh inval e)

                        return cv
                 <|> do symbol "rot"
                        e  <- expr
                        cv <- curveopt (Rot inval e)

                        return cv
                 <|> return inval

-- Expressions using chainl1
{-
expr = chainl1 term op0
term = chainl1 factor op1

op0 = do schar '+'
         return Add
         
op1 = do schar '*'
         return Mult

factor = do e <- number
            return (Const e)
         <|> do symbol "width"
                c <- curve

                return (Width c)
         <|> do symbol "height"
                c <- curve

                return (Height c)
         <|> do schar '('
                e <- expr
                schar ')'

                return e
-}

-- Expressins using direct grammar rewriting
expr = do t <- term
          exprOpt t
                    
exprOpt inval = do schar '+'
                   t    <- term
                   eopt <- exprOpt (Add inval t)

                   return eopt
                <|> return inval

term = do f    <- factor
          topt <- termOpt f
          return topt

termOpt inval = do schar '*'
                   f    <- factor
                   topt <- termOpt (Mult inval f)
                   
                   return topt
                <|> return inval
                
factor = do e <- number
            return (Const e)
         <|> do symbol "width"
                c <- curves

                return (Width c)
         <|> do symbol "height"
                c <- curves

                return (Height c)
         <|> do schar '('
                e <- expr
                schar ')'

                return e

point = do schar '('
           e1 <- expr
           schar ','
           e2 <- expr
           schar ')'

           return $ Point e1 e2

ident = do token $
             many1 ( letter <|> num <|> underscore )
             >>= \id -> case (elem id reserved) of
                         True      -> reject
                         otherwise -> return id
                         
  where letter     = satisfy isLetter
        num        = satisfy isDigit
        underscore = schar '_'
        
number = token (do pre <- digits
                   char '.'
                   post <- digits
                   return $ read $ pre ++ "." ++ post
                <|> do num <- digits
                       return $ read num )
         
  where digits = many1 (satisfy isDigit)

----------------------------EXPERIMENTS AND STUFF
-------
{-
data Expr2 = Zeroterm
          | Oneterm
          | Minus Expr2 Expr2
          deriving (Eq, Show)
                   
e = do tv <- t
       ev <- eopt tv
       return ev
       
eopt inval = (do schar '-'
                 tv <- t
                 ev <- eopt (Minus inval tv)
                 return ev)
             <|> return inval

t = (do schar '0'
        return Zeroterm)
    <|> (do schar '1'
            return Oneterm)
-}

-------------------
{-
curve = do schar '('
           c <- curve
           schar ')'
           return c
        <|> do p <- point
               c <- curve' p
               return c

curve' op = do symbol "++"
             c <- curve
             traceM $ "x: " ++ show c
             return $ Connect lop c





t = do op <- point
       return $ Single op

-}
{-
curve = do chainl1 curve' op

curve' = do schar '('
            c <- curve
            schar ')'
            return c
         <|> do p <- point
                return $ Single p
         <|> do id <- ident
                return $ Id id

         

op = do symbol "++"
        return Connect
     <|> do schar '^'
            return Over
     <|> do symbol "->"
            return Translate
     <|> do symbol "**"
            return Scale
     <|> do symbol "refv"
            return Refv
     <|> do symbol "refh"
            return Refh
     <|> do symbol "rot"
            return Rot
-}
