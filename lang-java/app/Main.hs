module Main where
import TypeCheck (runTC)
-- import Syntax

main :: IO ()
main = do
    print $ runTC example
