module ASM where

import Control.Monad.Fix

type Name = String

data StackCode =
      PushFn Name
    | PushInt Int
    | PushReal Double
    | PushBool Bool
    | PushVar Name
    | CallFn Name
    | PopTo Name
    deriving (Ord,Eq,Show)

data SSACode =
      LifeStart Name
    | Mov Name Name
    | Add Name Name Name
    | LifeEnd Name

type Reg = Int


newtype ASM a = ASM (Int -> ([StackCode], Int, a))
 -- Retrieve the inner function
asmFunction (ASM f) = f
 -- Evaluate the monad and get a list of bytes
asm start (ASM f) = bytes where (bytes, _, _) = f start



newtype AllocState = AllocState
    { freeRegs :: [Reg]
    }