------------------------------------------------------------------------------
-- The `error` function causes a program to terminate immediately with a
-- specific error message.  It has a type of `String -> a` and so it can
-- satisfy any type requirement when applied to a String.
alwaysFail :: a
alwaysFail = error "error message"

main :: IO ()
main = alwaysFail
