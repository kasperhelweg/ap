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
import Test.QuickCheck

main :: IO ()
main = do nameUS
          nameUF

          integerUS
          stringUS

--quickCheck ((\s -> s == s) :: [Char] -> Bool)

-------------- NAME -------------- 
nameF input = return $ parse ( name >> eof ) input

-- nameU Succes
nameUS = do nameF "kas3_pe_r" >>= \n -> case n of
                                         [] -> print "test failed"
                                         _  -> print "test OK"
            nameF "l" >>= \n -> case n of
                                 [] -> print "test failed"
                                 _  -> print "test OK"

-- nameU Failure
-------------- Base
            -- Reserved
nameUF = do nameF "class" >>= \n -> case n of
                                     [] -> print "test OK"
                                     _  -> print "test failed"
            
-------------- Edge
            -- empty
            nameF " " >>= \n -> case n of
                                 [] -> print "test OK"
                                 _  -> print "test failed"
            nameF "" >>= \n -> case n of
                                [] -> print "test OK"
                                _  -> print "test failed"
            -- not letter
            nameF "_var" >>= \n -> case n of
                                    [] -> print "test OK"
                                    _  -> print "test failed"
                                    
            nameF "3var" >>= \n -> case n of
                                    [] -> print "test OK"
                                    _  -> print "test failed"

-------------- INTEGER --------------
integerF input = return $ parse ( integer >> eof ) input

integerUS = do integerF " 1234333332221119999998885757574839" >>= \n -> case n of
                                                                         [] -> print "test failed"
                                                                         _  -> print "test OK"

-------------- STRING --------------
stringF input = return $ parse ( stringg >> eof ) input
stringUS = do stringF "\"123433asd2221!!99999##88575g574839\"" >>= \n -> case n of
                                                                        [] -> print "test failed"
                                                                        _  -> print "test OK"

stringUF = do stringF "\"123433332342&&111999\"99988asd57574839\"" >>= \n -> case n of
                                                                        [] -> print "test failed"
                                                                        _  -> print "test OK"
