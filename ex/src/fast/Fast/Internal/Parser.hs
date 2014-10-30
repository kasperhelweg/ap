module Fast.Internal.Parser
       ( Error
       , parseString
       , parseFile
       )
       where

import Fast.AST.FastAST
import Lib.SimpleParse

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
type Error = ()

parseString :: String -> Either Error Prog
parseString = undefined

parseFile :: FilePath -> IO (Either Error Prog)
parseFile = undefined
