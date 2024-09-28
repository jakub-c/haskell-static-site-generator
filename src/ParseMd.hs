module ParseMd
    ( parseMarkdown
    , MarkdownElement(..)
    ) where

import Data.Char (isSpace)

data MarkdownElement
    = Header Int String
    | Paragraph String
    | Bold String
    deriving (Show, Eq)

parseMarkdown :: String -> [MarkdownElement]
parseMarkdown = parseLines . lines

parseLines :: [String] -> [MarkdownElement]
parseLines [] = []
parseLines (x:xs)
    | take 2 x == "# " = Header 1 (drop 2 x) : parseLines xs
    | take 3 x == "## " = Header 2 (drop 3 x) : parseLines xs
    | take 4 x == "### " = Header 3 (drop 4 x) : parseLines xs
    | all isSpace x = parseLines xs  -- Skip empty lines
    | otherwise = Paragraph (parseBold x) : parseLines xs

parseBold :: String -> String
parseBold [] = []
parseBold ('*':'*':xs) = 
    let (bold, rest) = span (/= '*') xs
    in "<strong>" ++ bold ++ "</strong>" ++ parseBold (drop 2 rest)
parseBold (x:xs) = x : parseBold xs
