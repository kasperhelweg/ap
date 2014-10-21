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
parseString' input = parse ( e >> eof ) input

--- Attibuted Parser
--parseString :: String -> Either Error Expr2
parseString input = case liftM fst $ parseEof ( defs >>= \result -> return result ) input of
                     []  -> Left Error { errorType = ParseError }
                     [s] -> Right s

--- parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

-- |
-- Implementation
--

-- Redundant...
program = do defs >>= \ds -> return ds

defs = do many1 def >>= \ds -> return ds
       
def = do id <- ident
         schar '='
         curve <- curve
         dopt  <- dopt

         return $ Def id curve dopt
  
dopt = do symbol "where"
          schar '{'
          ds <- defs
          schar '}' 

          return ds
       <|> return []

curve = return $ Single (Point (Const 3) (Const 4))

point = return Point (Const 3) (Const 4)
expr  = undefined

ident = do token $ many1 ( letter <|> num <|> underscore ) >>= \id -> return id
  where letter     = satisfy isLetter
        num        = satisfy isDigit
        underscore = schar '_'

number = undefined


-------
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
