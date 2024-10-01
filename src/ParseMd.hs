module ParseMd
    ( parseMarkdown
    , MarkdownElement(..)
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
-- Remove this line: import Control.Applicative ((<|>))

data MarkdownElement
    = Header Int String
    | Paragraph String
    | Bold String
    | Italic String
    deriving (Show, Eq)

parseMarkdown :: String -> Either ParseError [MarkdownElement]
parseMarkdown = parse document ""

document :: Parser [MarkdownElement]
document = many (header <|> paragraph)

header :: Parser MarkdownElement
header = do
    level <- length <$> many1 (char '#')
    space
    content <- manyTill anyChar endOfLine
    return $ Header level (strip content)

paragraph :: Parser MarkdownElement
paragraph = Paragraph <$> manyTill anyChar endOfLine

strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
