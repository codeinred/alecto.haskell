{-# LANGUAGE NoMonomorphismRestriction #-}
module Parse.Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Bifunctor
import Data.List

newtype Parser m a = Parser (String -> m (a, String))
Parser x // str = x str
unwrap :: (Functor m) => Parser m a -> String -> m a
unwrap (Parser f) s = value <$> f s where value (val, _) = val

instance Functor m => Functor (Parser m) where
    fmap f (Parser readInput) = Parser (fmap (Data.Bifunctor.first f) . readInput)
instance Monad m => Applicative (Parser m) where
    pure a = Parser (\s -> pure (a, s))
    (Parser readFunc) <*> (Parser readInput) = Parser apply where
        apply s = do
            (f, s') <- readFunc s
            (input, s'') <- readInput s'
            return (f input, s'')
instance (Alternative m, Monad m) => Alternative (Parser m) where
    empty = Parser $ const empty
    Parser a <|> Parser b = Parser (\s -> a s <|> b s)
instance Monad m => Monad (Parser m) where
    (Parser parse) >>= f = Parser apply where
        apply s = do
            (input, s') <- parse s
            f input // s'
data StringLiteral = SQuote String | DQuote String deriving (Show, Eq)
data Ignore = Ignore deriving (Show, Eq)

splitWhile predicate [] = ([],[])
splitWhile predicate (x:xs) = if predicate x then rest (splitWhile predicate xs) else ([], x:xs) where
    rest (yee, nay) = (x : yee, nay)
splitWhile2 _ [] = ([],[])
splitWhile2 _ [x] = ([], [x])
splitWhile2 predicate (x:y:xs) = if predicate x y then rest (splitWhile2 predicate (y:xs)) else ([x], y:xs) where
    rest (yee, nay) = (x : yee, nay)

token = Parser (process . splitWhile isAlphaNum . dropWhile isSpace) where
    process ([], s') = empty
    process (name, s') = return (name, s')

tokenOn pred = Parser (process . splitWhile pred . dropWhile isSpace) where
    process ([], s') = empty
    process (name, s') = return (name, s')
dashedToken = tokenOn p where
    p c = isAlphaNum c || c == '-'
htmlTagToken = tokenOn p where
    p c = isAlphaNum c || c == '-' || c == '!'

matchC ch = Parser check where
    check [] = empty
    check (x:xs) = if x == ch then return (ch, xs) else empty
killSpace = Parser (\s -> return (Ignore, dropWhile isSpace s))

maybeParse p = (Just <$> p) <|> return Nothing
digits = tokenOn isDigit
getSpace = tokenOn isSpace -- Returns the space, incase it's needed
parseWhile test = Parser (return . splitWhile test)
parseWhile2 test = Parser (return . splitWhile2 test)
oneOrMore p = do
    first <- p
    rest <- zeroOrMore p
    return (first : rest)
zeroOrMore p = oneOrMore p <|> return []
stringLiteral = do
    delim <- matchC '"' <|> matchC '\''
    body <- parseWhile2 (\a b -> b /= delim || a == '\\')
    matchC delim -- Ensure that the string ended with the same character it started
    return (if delim == '"' then DQuote body else SQuote body)
prefix prefix = Parser (\s -> if prefix `isPrefixOf` s then return (prefix, drop len s) else empty) where
    len = length prefix
check prefix = Parser (\s -> if prefix `isPrefixOf` s then return (Ignore, drop len s) else empty) where
    len = length prefix

-- Creates a copy of the remainder of the string without actually taking
-- anything from the string
copyRemainder = Parser (\s -> return (s, s))
takeUntil halt item = (halt >> return []) <|> do
    first <- item
    (first :) <$> takeUntil halt item

escapeIf p = Parser (\s -> if null (p // s) then empty else return (Ignore, s))