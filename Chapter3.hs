import Data.List (sortBy)
import Data.Function (on)

-- Type aliases are purely for code readability:
type ShorterName = (Float, Int, Float, Int)

-- Algebraic data types have more than one value constructor:
data MyAlgebraic = MyValueConstructor1
                 | MyValueConstructor2

-- Value constructors may take any concrete type as an argument:
data MyAlgebraic2 = MyValueConstructor3 Int String
                  | MyValueConstructor4 Float Char

-- A parameterized type includes the use of type variables.  The type
-- constructor of a parameterized type will accept any number of concrete types
-- as arguments.  This ends up making the parameterized type a concrete type
-- itself.
data MyMaybe a = MyJust a
               | MyNothing

myTest :: MyMaybe (Maybe Int) -> Int
myTest MyNothing = error "Cannot get int values of MyNothing or Nothing"
myTest (MyJust Nothing) = error "Cannot get int values of MyNothing or Nothing"
myTest (MyJust (Just x)) = x

------------------------------------------------------------------------------
-- Any type which has a kind of `*` is a concrete type.  It can take no types
-- as arguments.  A type with kind `* -> *` or `* -> * -> *` etc. is a type
-- constructor.  It must take concrete types as arguments before it can become
-- a concrete type itself.
--------------------------------------------------------------------------------

-- The `error` function causes a program to terminate immediately with a
-- specific error message.  It has a type of `String -> a` and so it can
-- satisfy any type requirement when applied to a String.
alwaysFail :: a
alwaysFail = error "error message"

-- Try it!
main :: IO ()
main = alwaysFail

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ a -> 1 + a) 0

mean :: (Real a, Fractional b) => [a] -> b
mean xs = sumXs / lenXs
    where sumXs = realToFrac $ sum xs
          lenXs = realToFrac $ length xs

palindrome :: [a] -> [a]
palindrome xs = foldr (:) (reverse xs) xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' x xs = tail $ foldr (\i a -> x:i:a) [] xs

------------------------------------------------------------------------------
-- Tree type and height function
------------------------------------------------------------------------------
data Tree a = Node a (Tree a) (Tree a)
            | EmptyNode deriving (Show)

testTree1 :: Tree Int
testTree1 =
    Node 1
        (Node 1
            (Node 1 EmptyNode EmptyNode)
            (Node 1 EmptyNode
                (Node 1 EmptyNode EmptyNode)
            )
        )
        (Node 1 EmptyNode EmptyNode)

testTree2 :: Tree Int
testTree2 =
    Node 1
        (Node 1
            (Node 1 EmptyNode EmptyNode)
            (Node 1 EmptyNode
                (Node 1 EmptyNode EmptyNode)
            )
        )
        (Node 1
            (Node 1 EmptyNode EmptyNode)
            (Node 1 EmptyNode
                (Node 1
                    (Node 1 EmptyNode EmptyNode)
                    EmptyNode
                )
            )
        )

testTree3 :: Tree Int
testTree3 =
    Node 1
        (Node 1
            (Node 1 EmptyNode EmptyNode)
            EmptyNode
        )
        EmptyNode

height :: Tree a -> Int
height EmptyNode = 0
height (Node _ l r) = 1 + max (height l) (height r)

------------------------------------------------------------------------------
-- Convex hull excercise
------------------------------------------------------------------------------
type P2D = (Double, Double)

data Direction = Straight
               | LeftTurn
               | RightTurn deriving (Show, Read, Eq)

slope :: P2D -> P2D -> Double
slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

direction :: P2D -> P2D -> P2D -> Direction
direction (x1, y1) (x2, y2) (x3, y3)
    | cross > 0 = LeftTurn
    | cross < 0 = RightTurn
    | otherwise = Straight
    where cross = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

directions :: [P2D] -> [Direction]
directions (a:rest@(b:c:_)) = direction a b c:directions rest
directions _ = []

ps :: [P2D]
ps = [ (1.0, 1.0)
     , (2.0, 2.0)
     , (3.0, 4.0)
     , (-1.0, 1.0)
     , (-2.0, 2.0)
     , (-3.0, 4.0)
     , (-1.0, -1.0)
     , (-2.0, -2.0)
     , (-3.0, -4.0)
     , (1.0, -1.0)
     , (2.0, -2.0)
     , (3.0, -4.0)
     ]
