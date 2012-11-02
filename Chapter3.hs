------------------------------------------------------------------------------
-- Type constructors are purely for code readability:
type ShorterName = (Float, Int, Float, Int)

------------------------------------------------------------------------------
-- Algebraic data types have more than one value constructor:
data MyAlgebraic = MyValueConstructor1
                 | MyValueConstructor2

------------------------------------------------------------------------------
-- Value constructors may take any concrete type as an argument:
data MyAlgebraic2 = MyValueConstructor3 Int String
                  | MyValueConstructor4 Float Char

------------------------------------------------------------------------------
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
