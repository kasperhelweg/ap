module Fast.Internal.Parser where

import Fast.AST.FastAST
import Lib.SimpleParse

-- debug
import Debug.Trace

-- stdlib
import Data.Char
import Control.Monad(liftM)
import Control.Applicative ((<|>))
--import Control.Applicative ((<|>),(<$>))


-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
data ErrorType = ParseError
               deriving (Show, Read, Eq)

data Error = Error { errorType :: ErrorType, errorMsg :: String }
           deriving (Show, Eq)

parseString :: String -> Either Error Prog
parseString input = case liftM fst $ parseEof ( cldecls >>= \result -> return result ) input of
                     [s]    -> Right s
                     [ ]    -> Left Error { errorType = ParseError,
                                            errorMsg  = "Yes, there was an error." }
                     -- Ambiguity debug
                     (x:xs) -> trace ("calling f with x = " ++ show (x:xs)) Right x

parseFile :: FilePath -> IO (Either Error Prog)
parseFile filename = fmap parseString $ readFile filename

cldecls = undefined
cldecl  = undefined

cnsdecl = undefined

nmdecls = undefined
nmdecl  = undefined

rcvdecl = undefined

params  = undefined

-- left factorze
params' = undefined
param   = undefined

args  = undefined

-- left factorize
args' = undefined

exprs = undefined

-- left factorize (chainl1)
expr  = undefined

cases   = undefined
case'   = undefined

pattern = undefined
       
-- datastructures and tokens
reserved = ["self", "class", "new", "receive", "send", "match", "return", "set"]

-- these functions should use munch if possible
name :: Parser [Char]
name = do token $
            many1 ( letter <|> num <|> underscore )
            >>= \n -> case (elem n reserved) of
                       True      -> reject
                       otherwise -> return n
                         
  where letter     = satisfy isLetter
        num        = satisfy isDigit
        underscore = schar '_'

{-
number = token (do pre <- digits
                   char '.'
                   post <- digits
                   return $ read $ pre ++ "." ++ post
                <|> do num <- digits
                       return $ read num )
         
  where digits = many1 (satisfy isDigit)
-}
