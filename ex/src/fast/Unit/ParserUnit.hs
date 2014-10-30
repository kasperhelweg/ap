{-
  FastParser Unit Tests

  Date: Sep 20, 2012
  Author: Kasper Helweg Jonassen <kasperhelweg@gmail.com>

  compile example :
  ghc -i../ -Wall -fwarn-incomplete-uni-patterns -fno-warn-unused-do-bind -o UNIT ./ParserUnit.hs
-}

import Lib.SimpleParse
import Fast.Internal.Parser
import Control.Monad(liftM)

main :: IO ()
main = do nameUS
          
-- nameF
nameF input = return $ parse ( name >> eof ) input

-- nameU Succes
nameUS = nameF "kasper" >>= \n -> case n of
                                   [] -> print "test failed"
                                   _  -> print "test OK"
-- nameU Failure
nameUF = undefined
