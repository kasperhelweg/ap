module Sm where

import Control.Monad
import Control.Applicative

import qualified Data.Map as M
import Debug.Trace

type Stack = [Int]

type Prog = [Inst]
data Inst = PUSH Int | POP | ADD | HALT
          deriving (Show, Eq)

data Error = Fail
           | EpicFail
           deriving (Show, Eq)
                    
data State = State { prog  :: Prog,
                     pc    :: Int,
                     stack :: Stack }
           deriving (Show, Eq)

newtype SM a = SM ( State -> Either Error (a, State) )

instance Functor SM where
  -- fmap :: (a -> b) -> SM a -> SM b
  fmap f (SM m) = SM $ \s -> case m s of
                              Left e        -> Left e
                              Right (a, st) -> Right(f a, st)

instance Applicative SM where
  -- (<*>) :: SM (a -> b) -> SM a -> SM b
  (SM f) <*> (SM p) = SM $ \s -> case p s of
                                  Left e       -> Left e
                                  Right (a, t) -> case f t of
                                                    Left fe -> Left fe
                                                    Right (fa, ft) -> Right (fa a, ft)

  -- pure :: a -> SM a
  pure x = SM $ \s -> Right (x, s)

instance Monad SM where
  -- (>>=) :: SM a -> (a -> SM b) -> SM b
  SM m >>= f = SM $ \s -> case m s of
                           Left e        -> Left e
                           Right (a, st) -> let SM g = f a in g st
  
  -- return :: a -> SM a
  return a = SM $ \s -> Right (a, s)

initial :: Prog -> State
initial p = State { prog  = p,
                    pc    = 0,
                    stack = [] }

getInst :: SM Inst
getInst = SM $ \s -> let p  = prog s
                         ci = pc s
                     in if length p <= ci
                        then Left Fail
                        else Right (p !! ci, s)

modifySM :: (State -> State) -> SM ()
modifySM f = SM $ \s -> Right((), f s)

setSM :: State -> SM ()
setSM s = SM $ \_ -> Right((), s)

getSM :: SM State
getSM = SM $ \s -> Right (s, s)

incPC :: SM ()
incPC = modifySM $ \s -> s { pc = pc s + 1 }

push :: Int -> SM ()
push n = modifySM $ \s -> s { stack = n : stack s }

pop :: SM Int
pop = getSM >>= \s -> case stack s of
                       []     -> SM $ \_ -> Left EpicFail
                       (x:xs) -> setSM s { stack = xs } >> return x
                       --(x:xs) -> setSM s { stack = xs } >>= \result -> return x
                                
interpInst :: Inst -> SM Bool
interpInst inst = case inst of
                   (PUSH n) -> do push n 
                                  incPC      
                                  return True

                   (POP)    -> do _ <- pop 
                                  incPC      
                                  return True
                                  
                   HALT     -> return False

-- DeSugared Version
--interpInst :: Inst -> SM Bool
--interpInst inst = case inst of
--                   (PUSH n) -> push n >>= \result ->
--                                           incPC >>= \result ->
--                                                      return True
--                   HALT     -> return False

interp :: SM ()
interp = do inst   <- getInst
            result <- interpInst inst
            when result interp

-- DeSugared Version
--interp :: SM ()
--interp = getInst >>= \i ->
--                      interpInst i >>= \result ->
--                                        case result of True  -> interp
--                                                       False -> return () 

runSM :: Prog -> Either Error (Maybe Int)
runSM [] = Left EpicFail
runSM p  = let (SM f) = interp
           --in case fmap snd $ f $ initial p of
           in case fmap snd $ return (initial p) >>= f of
               _ -> Right (Just 2)

program   = [PUSH 1, PUSH 2, ADD, PUSH 10, POP, ADD, HALT]
program'  = [PUSH 1, PUSH 2, POP, HALT]
program'' = [PUSH 1]
