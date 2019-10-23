{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
someFunc :: IO ()
someFunc = putStrLn "someFunc"
