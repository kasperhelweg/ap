module CurvySyntax.Internal where
import Debug.Trace

import Parser.SimpleParse
import CurvySyntax.CurveAST


import Control.Monad(liftM)
import Control.Applicative ((<|>),(<$>))

-- |
-- Types
--
data ErrorType = ParseError
               | UnallocatedRegister Int
               | RegisterAlreadyAllocated
               | RegisterEmpty Int
               | InvalidPC
               | Unspec String
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
parseString :: String -> Either Error Expr2
parseString input = case liftM fst $ parseEof ( e >>= \result -> return result ) input of
                      []  -> Left Error { errorType = ParseError }
                      [s] -> Right s 

--- parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

-- |
-- Implementation
--
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
