module Fast.Internal.Parser where

import Fast.AST.FastAST
import Lib.SimpleParse

-- debug
import Debug.Trace

-- stdlib
import Data.Char
import Control.Monad(liftM)
import Control.Applicative
--import Control.Applicative ((<|>),(<$>))

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
data ErrorType = ParseError
               deriving (Show, Read, Eq)

data Error = Error { errorType :: ErrorType, errorMsg :: String }
           deriving (Show, Eq)

parseString :: String -> Either Error Prog
parseString input = case liftM fst $ parseEof ( prog >>= \result -> return result ) input of
                     [s] -> Right s
                     [ ] -> Left Error { errorType = ParseError,
                                         errorMsg  = "Yes, there was an error." }
                     -- Ambiguity debug
                     (x:xs) -> trace ("calling f with x = " ++ show (x:xs)) Right x

parseFile :: FilePath -> IO (Either Error Prog)
parseFile filename = fmap parseString $ readFile filename

prog :: Parser [ClassDecl]
prog    = cldecls

cldecls :: Parser [ClassDecl]
cldecls = many cldecl >>= \cd -> return cd

cldecl :: Parser ClassDecl
cldecl  = do symbol "class"
             n <- name
             schar '{'
             cd <- option cnsdecl
             nd <- nmdecls
             rd <- option rcvdecl
             schar '}'
          
             return $ ClassDecl { className        = n
                                , classConstructor = cd
                                , classMethods     = nd
                                , classReceive     = rd
                                }

cnsdecl :: Parser MethodDecl
cnsdecl = do symbol "new"
             schar '('; p <- params; schar ')'
             schar '{'; e <- exprs;  schar '}'

             return $ MethodDecl { methodParameters = p
                                 , methodBody       = e
                                 }

nmdecls :: Parser [NamedMethodDecl]
nmdecls = many nmdecl >>= \nd -> return nd

nmdecl :: Parser NamedMethodDecl
nmdecl  = do n <- name
             schar '('; p  <- params; schar ')'
             schar '{'; es <- exprs;  schar '}'

             return $ NamedMethodDecl n MethodDecl { methodParameters = p
                                                   , methodBody       = es
                                                   }
rcvdecl :: Parser ReceiveDecl
rcvdecl = do symbol "receive"
             schar '('; p  <- param; schar ')'
             schar '{'; es <- exprs; schar '}'

             return $ ReceiveDecl { receiveParam = p
                                  , receiveBody  = es
                                  }
-- REWRITE THIS
-- list of names
params :: Parser [[Char]]
params  =  params'
           <|> return [ ]
                     
-- left factorize
params' = do p <- param
             return [p]
          <|> do p <- param
                 schar ','
                 ps <- params'
                 return (p:ps)
                 
param = do n <- name
           return n

args  = do e <- expr
           a <- args'
           return $ (e:a)
        <|> return [ ]

-- left factorize
args' = do schar ','
           as <- args'
           return as
        <|> do e <- expr
               return [e]

eop0 = do schar '+'
          return Plus
       <|> do schar '-'
              return Minus
              
eop1 = do schar '*'
          return Times
       <|> do schar '/'
              return DividedBy

exprs = do e <- expr
           schar ';'
           es <- exprs
           return (e:es)
        <|> return [ ]

-- Look into the naming of these
expr  = chainl1 expr' eop0
         <++ expro
expr' = chainl1 expro eop1



-- some of this could be refactored alot. for example 'set'
expro = do i <- integer
           return $ IntConst i
        <|> do s <- stringg
               return $ StringConst s
        <|> do n <- name
               return $ ReadVar n
        <|> do n <- name
               schar '('; as <- args; schar ')'
               return $ TermLiteral n as
        <|> do symbol "self"
               return Self
        <|> do symbol "return"
               e <- expr
               return $ Return e
        <|> do symbol "set"
               symbol "self"
               schar '.'
               n <- name
               schar '='
               e <- expr
               return $ SetField n e
        <|> do symbol "set"
               n <- name
               schar '='
               e <- expr
               return $ SetVar n e
        <|> do symbol "match"
               e <- expr
               schar '{'; cs <- cases; schar '}'
               return $ Match e cs
        <|> do symbol "send"
               schar '{'
               e1 <- expr
               schar ','
               e2 <- expr;
               schar '}'
               return $ SendMessage e1 e2
        <|> do symbol "self"
               schar '.'
               n <- name
               return $ ReadField n
      --  <|> do e <- expr
      --         schar '.'
      --         n <- name
      --         schar '('; as <- args; schar ')'
      --         return $ CallMethod e n as
        <|> do symbol "new"
               n <- name
               schar '('; as <- args; schar ')'
               return $ New n as
        <|> do schar '('; e <- expr; schar ')'
               return e
               
cases   = many1 casee
          <|> return [ ]
          
casee   = do p <- pattern
             symbol "->"
             schar '{'
             es <- exprs
             schar '}'

             return (p, es)

-- Missing string
pattern = do i <- integer
             return $ ConstInt i
          <|> do s <- stringg
                 return $ ConstString s
          <|> do n <- name
                 schar '('
                 ps <- params
                 schar ')'
                 return $ TermPattern n ps
          <|> do n <- name
                 return $ AnyValue n

-- datastructures and tokens
reserved = ["self", "class", "new", "receive", "send", "match", "return", "set"]

-- Maybe the token can go elsewhere?
-- should return something!
integer = token $
          do i <- munch1 (\c -> isDigit c)
             return (read i :: Integer)

--regexp "[^"]*".
stringg = do schar '"'
             s <- munch (\c -> c /= '"')
             schar '"'
             return $ s
             
name :: Parser [Char]
name = token $
       do h <- satisfy isLetter
          t <- munch natoms
          case (elem (h:t) reserved) of
           True      -> reject
           otherwise -> return (h:t)

-- Make this prettier at some point
natoms :: Char -> Bool
natoms c | isLetter c = True | isDigit  c = True | c == '_' = True
         | otherwise  = False
