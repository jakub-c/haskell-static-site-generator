module ParseMd
    ( parseMarkdown
    , MarkdownElement(..)
    , InlineElement(..)
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec (try)
-- Remove this line: import Control.Applicative ((<|>))

data MarkdownElement
    = Header Int String
    | Paragraph [InlineElement]
    deriving (Show, Eq)

data InlineElement
    = PlainText String
    | Bold String
    | Italic String
    deriving (Show, Eq)

parseMarkdown :: String -> Either ParseError [MarkdownElement]
parseMarkdown = parse document ""

document :: Parser [MarkdownElement]
document = many (header <|> paragraph <* optional (many1 newline))

header :: Parser MarkdownElement
header = do
    level <- length <$> many1 (char '#')
    space
    content <- manyTill anyChar endOfLine
    return $ Header level (strip content)

paragraph :: Parser MarkdownElement
paragraph = Paragraph <$> many1 inlineElement <* endOfLine

inlineElement :: Parser InlineElement
inlineElement = try italicText <|> plainText

plainText :: Parser InlineElement
plainText = PlainText <$> many1 (noneOf "*\n")

italicText :: Parser InlineElement
italicText = Italic <$> between (char '*') (char '*') (many1 (noneOf "*\n"))

strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
