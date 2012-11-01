------------------------------------------------------------------------------
-- Notice how the type signature of this function implies that the function
-- must have only one behavior!!  If the function uses only type variables, it
-- cannot know anything about the actual types the function is applied to.  As
-- far as it's concerned, the concrete types are a black box.  Furthermore, no
-- type classes are specified in the signature either.  Therefore, the only
-- valid behavior that this function can have (without even inspecting the
-- code) is that it must simply return the first member of a tuple.  WOW!!!
myFst :: (a, b) -> a
myFst (a, _) = a
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Functions with a type signature of `[a] -> a` must take a list and return
-- one of the items in the list.  Can they create a value with the concrete
-- type that the function is applied to?  They can't.  They may return the
-- first value, or the next, or the next, etc. until the last item in the list.
-- Really, they may only reliably return the first or last item in the list.
-- Am I wrong?
myLast :: [a] -> a
myLast [] = error "Cannot get last of empty list"
myLast (x:xs) = if null xs then x else myLast xs

myFirst :: [a] -> a
myFirst [] = error "Cannot get first of empty list"
myFirst (x:_) = x
------------------------------------------------------------------------------

------------------------------------------------------------------------------
secondToLast :: [a] -> a
secondToLast [] = error "Cannot get second to last of empty list"
secondToLast (_:[]) = error "Cannot get second to last of one item list"
secondToLast (x:y:ys)
    | null ys   = x
    | otherwise = secondToLast (y:ys)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Haskell is special in that all functions in the language are pure by
-- default.  This means that they MUST produce the same output for a given set
-- of inputs.  They CANNOT perform IO or any action with side-effects unless
-- that is explicitly stated.
------------------------------------------------------------------------------
