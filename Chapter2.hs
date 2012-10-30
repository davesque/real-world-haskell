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
