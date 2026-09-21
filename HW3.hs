{-|
Module       : HW3
Description  : Introduction to Haskell
Maintainer   : CS 131, Programming Languages
-}
module HW3 where

--------------------------------------------------------------------------------  
-- Lists --
--------------------------------------------------------------------------------  


-- | Returns every other element of a list, starting with the first (zero-th)
evens :: [a] -> [a]
evens [] = []
evens [x1] = [x1]
evens (x1 : x2: xs) = (x1 : evens(xs))


-- | Returns every other element of a list, starting with the second (one-th)
odds :: [a] -> [a]
odds [] = []
odds [x1] = []
odds (x1 : x2 :xs) = (x2: odds(xs))


-- | Partitions a list of elements into a tuple of two lists, where the first
--   item in the tuple is a list of the elements at even-numbered indices and
--   the second item in the tuple is a list of the elements at odd-numbered 
--   indices
evenodds :: [a] -> ([a], [a])
evenodds [] = ([],[])
evenodds [x1] = ([x1],[])
evenodds (x1:x2 :xs)= 
            let (even, odd) = evenodds(xs)
            in (x1 : even, x2 :odd)



-- | The inverse of 'evenodds'
riffle :: ([a], [a]) -> [a]
riffle ([],[]) = []
riffle ([x1], []) = [x1]
riffle ([], [x2])= [x2]
riffle ((a:as), (b:bs)) = (a : b: riffle(as, bs))


--------------------------------------------------------------------------------
-- Higher-order functions, currying and uncurrying
-- 
-- Functions curry and uncurry are already pre-defined in Haskell,
-- which is why you need to call your version 'myCurry' and 'myUncurry'.
--------------------------------------------------------------------------------


-- | Converts an uncurried function to a curried function
myCurry :: ( (a,b) -> c ) -> ( a -> b -> c )
myCurry f x y = f(x, y)


-- | Converts a curried function to an uncurried function
myUncurry :: ( a -> b -> c ) -> ( (a,b) -> c )
myUncurry f(x,y) = f x y


-- | A curried version of 'riffle'
riffle2 :: [a] -> [a] -> [a]
riffle2 = myCurry riffle


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- [define a datatype for TreeOfInt here]
data TreeOfInt 
    =   Empty 
    | Branch Int TreeOfInt TreeOfInt
    deriving (Show, Eq)


-- | Returns the minimum element of the tree. Gives an error if the tree is empty
least :: TreeOfInt -> Int
least Empty = error "empty tree has no min element"
least (Branch n Empty t2) = n
least (Branch n t1 t2) = least t1


--------------------------------------------------------------------------------
-- Stack machines
--------------------------------------------------------------------------------

-- | Arithmetic expressions
data Expr = Num   Double        -- ^ Represents a floating-point value
          | BinOp Expr Op Expr  -- ^ Represents a binary operation
    deriving (Show, Eq)


-- | Binary operators
data Op = PlusOp | MinusOp | TimesOp | DivOp
    deriving (Show, Eq)


-- | Stack instructions
data StackInstr = Push Double  -- ^ Push a number on the stack
                | DoOp Op      -- ^ Perform an operation, using the top two stack values
                | Swap         -- ^ Swap the top two stack values
    deriving (Show, Eq)


-- A stack is represented as a list of floating-point numbers;
-- the head of the list is the top of the stack.
type StackValue = Double
type Stack = [StackValue]


-- | Evaluate a list of stack instructions, given an initial stack
evalRPN :: [StackInstr] -> Stack -> StackValue
evalRPN [] (y:ys)= y 
evalRPN (Push x: xs) cs = evalRPN xs (x : cs)
evalRPN (Swap : xs) (a:b:cs) = evalRPN xs (b:a:cs)
evalRPN (DoOp x :xs) (a:b:cs)=  case x of 
                    PlusOp -> evalRPN xs (a+b: cs)
                    MinusOp -> evalRPN xs (b - a: cs)
                    TimesOp -> evalRPN xs (a * b :cs)
                    DivOp -> evalRPN xs (b / a: cs)

                     



-- | Translate an expression to stack operations
toRPN :: Expr -> [StackInstr]
toRPN (Num x) = [Push x]
toRPN (BinOp x y z) =  toRPN x ++ toRPN z ++ [DoOp y]



-- | Minimize the stack depth
toRPNopt :: Expr -> ([StackInstr], Integer)
toRPNopt (Num x) = ([Push x], 1)
toRPNopt (BinOp x y z) = 
                    let (a, b) = toRPNopt x
                        (c, d) = toRPNopt z
                    in if b >= d
                        then (a ++ c ++ [DoOp y], max b (d+1))
                        else (c ++ a ++ [Swap, DoOp y], max d (b+1))


 

--------------------------------------------------------------------------------
-- Example expressions. Define these as described in the assignment.
--------------------------------------------------------------------------------

depth3 :: Expr
depth3 = BinOp (BinOp (Num 2.0) PlusOp (Num 3.0)) PlusOp (BinOp (Num 2.0) PlusOp (Num 3.0))

depth4 :: Expr
depth4 = BinOp (BinOp (BinOp (Num 2.0) PlusOp (Num 3.0)) TimesOp (BinOp (Num 4.0) PlusOp (Num 5.0))) MinusOp (BinOp (BinOp (Num 1.0) PlusOp (Num 2.0)) TimesOp (BinOp (Num 3.0) PlusOp (Num 4.0)))