-- In Haskell, create a Molamil Docker API to set up virtual machines.
-- In erlang create a push api and the julebryg game.

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

data Expr2 = Zeroterm
          | Oneterm
          | Minus Expr2 Expr2
          deriving (Eq, Show)

-- |
-- API
--

--- Accepting parser
parseString' input = parse ( e >> eof ) input

--- Attibuted Parser
--parseString :: String -> Either Error Expr2
parseString input = case liftM fst $ parseEof ( program >>= \result -> return result ) input of
                     []  -> Left Error { errorType = ParseError }
                     [s] -> Right s

--- parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

-- |
-- Implementation
--

program = do ds <- defs
             return ds

defs = do d  <- def
          ds <- defs
          let Def id curve ds' = d in
           return $ Def id curve ds:ds'
       <|> return []
       
def = do id <- ident; schar '='; curve <- curve;
         return $ Def id curve []
              
ident = do token $ many1 ( letter <|> num <|> underscore ) >>= \id -> return id
  where letter     = satisfy isLetter
        num        = satisfy isDigit
        underscore = schar '_'

curve = return $ Single (Point (Const 3) (Const 4))

-------
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
