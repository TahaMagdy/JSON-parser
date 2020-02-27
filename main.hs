module Main where

import Data.Char
import Control.Applicative

-- Abstract Syntax Tree
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- Supporting only integers
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)


-- Parser
-- `a`: represent the parsing structure
-- `a`: is the thing the parser parses
-- Parser Char: is a parser that parses a Char.
-- Parser : takes the implementation of the parsing.
-- * To construct the Parser Char you need to give the Parser value construct
--   an implementation that is of the type String -> Maybe (String, Char)
newtype Parser a = Parser
    {
        runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
                            (restInput, x) <- p input
                            Just (restInput, f x)


instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                   (input' , f) <- p1 input
                                   (input'', a) <- p2 input'
                                   Just (input'', f a)


instance Alternative Parser where
  empty = Parser $ (\_ -> Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
                                    p1 input <|> p2 input


charParser :: Char -> Parser Char
charParser x = Parser f
    where f (y:ys)
            | y == x    = Just (ys, y)
            | otherwise = Nothing
          f [] = Nothing


-- Trick: Parser needs to be Applicative to use sequenceA
stringParser :: String -> Parser String
stringParser string = sequenceA $ map charParser string

jsonNull :: Parser JsonValue
jsonNull = fmap (\_ -> JsonNull) $ stringParser "null"


-- Parser needs to be Alternative to use <|>
jsonBool :: Parser JsonValue
jsonBool = fmap f $ stringParser "true" <|> stringParser "false"
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          f _ = undefined


spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
                        let (tokenFromPredicate, rest) = span f input
                        in Just(rest, tokenFromPredicate)



-- Parser Combinator
notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) = Parser $ \input -> do
                                (input', parsed) <- p input
                                -- Making sure that we don't get
                                -- an empty string indicating not parsed
                                if null parsed then Nothing
                                -- pass on the parsed results
                                else Just (input', parsed)

-- this have a problem if digits was empty  `read` crashes.
jsonNumber :: Parser JsonValue
jsonNumber = f <$> notEmpty(spanP isDigit)
    where f digits = JsonNumber $ read digits


stringLitral :: Parser String
stringLitral = charParser '"' *> spanP (/= '"') <* charParser '"'


jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLitral


whiteSpacesParser :: Parser String
whiteSpacesParser = spanP isSpace

sebBy :: Parser a -> Parser b -> Parser [b]
sebBy sep element = (:) <$> element <*> many(sep *> element) <|> pure []


jsonArray :: Parser JsonValue
jsonArray = JsonArray <$>  (charParser '[' *> elements <* charParser ']')
    where elements = sebBy (whiteSpacesParser *> charParser ',' <* whiteSpacesParser) jsonValue



jsonObject :: Parser JsonValue
jsonObject = JsonObject <$>
             (charParser '{'
             *> whiteSpacesParser
             *> sebBy (whiteSpacesParser *> charParser ',' <* whiteSpacesParser) pairs
             <* whiteSpacesParser
             <* charParser '}')
         where pairs = (\key _ value -> (key, value)) -- needs a deeper look
                      <$> stringLitral
                      <*> (whiteSpacesParser *> charParser ':' <* whiteSpacesParser)
                      <*> jsonValue


jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
--

