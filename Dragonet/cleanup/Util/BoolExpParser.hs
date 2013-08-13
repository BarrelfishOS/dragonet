module Util.BoolExpParser(
    parseExp,
) where

import Util.BoolExp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


expr :: GenParser Char st BExp
expr = buildExpressionParser table term

term :: GenParser Char st BExp
term = parens expr <|> variable

variable :: GenParser Char st BExp
variable = do { a <-  many (alphaNum <|> char '=') ; return (BEVar a) }

parens :: GenParser Char st b -> GenParser Char st b
parens p = do { _ <- char '('; a <- p ; _ <- char ')' ; return a }

table :: [[Operator Char st BExp]]
table = [
        [prefix "!" BENot ],
        [binary "&" BEAnd AssocLeft ],
        [binary "|" BEOr AssocLeft ]
    ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator Char st a
binary  name fun assoc = Infix (do{ _ <- string name; return fun }) assoc

prefix :: String -> (a -> a) -> Operator Char st a
prefix  name fun       = Prefix (do{ _ <- string name; return fun })

-- Parses simple boolean expressions using (),!,&,| as operators
parseExp :: String -> Either String BExp
parseExp s =
    case parse expr "" s of
        Left err -> Left (show err)
        Right e -> Right e
