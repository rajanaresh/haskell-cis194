{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser
import StackVM
  
eval :: ExprT -> Integer
eval (ExprT.Lit i)   = i
eval (ExprT.Add l r) = (eval l) + (eval r)
eval (ExprT.Mul l r) = (eval l) * (eval r)

evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp ExprT.Lit ExprT.Add ExprT.Mul s)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i
    | i<=0      = False
    | otherwise = True
  add           = (||)
  mul           = (&&)
  
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit i                      = MinMax i
  add (MinMax l) (MinMax r)  = MinMax (max l r)
  mul (MinMax l) (MinMax r)  = MinMax (min l r)

instance Expr Mod7 where
  lit i                 = Mod7 (mod (abs (i)) 7)
  add (Mod7 l) (Mod7 r) = Mod7 (mod (l+r) 7)
  mul (Mod7 l) (Mod7 r) = Mod7 (mod (l*r) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp 

testBool :: Maybe Bool
testBool = testExp 

testMM :: Maybe MinMax
testMM = testExp 

testSat :: Maybe Mod7
testSat = testExp 

instance Expr StackVM.Program where
  lit i    = [PushI i]
  add l r  = l++r++[StackVM.Add]
  mul l r  = l++r++[StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
