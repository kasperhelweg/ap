{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where
import CurvySyntax
import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
           (c:i:_) -> case c of
                       "-s" -> print (parseString i)
                       "-f" -> do result <- (parseFile i)
                                  print result
                       _    -> error "argument not recognized"
           _       -> error "input parse error"
