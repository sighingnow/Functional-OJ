{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , empty
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = Stack [Int]

newtype Parser a = Parser { runParser :: Text -> Either ForthError (a, Text) }

instance Monad Parser where
    return = Right $ Stack []
    pa >>= f = undefined


numberP :: Parser Int
numberP

forthP :: Parser ForthState
forthP = undefined

empty :: ForthState
empty = Stack []

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input = case runParser forthP input of
                   Left err -> Left err
                   Right (r, _) -> Right r

toList :: ForthState -> [Int]
toList (Stack xs) = xs
