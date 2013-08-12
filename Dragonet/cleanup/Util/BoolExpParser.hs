module Util.BoolExpParser(
    parseExp,
) where

import Util.BoolExp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


expr = buildExpressionParser table term

term = parens expr <|> variable
variable = do { a <-  many (alphaNum <|> char '=') ; return (BEVar a) }
parens p = do { char '('; a <- p ; char ')' ; return a }

table = [
        [prefix "!" BENot ],
        [binary "&" BEAnd AssocLeft ],
        [binary "|" BEOr AssocLeft ]
    ]
binary  name fun assoc = Infix (do{ string name; return fun }) assoc
prefix  name fun       = Prefix (do{ string name; return fun })

-- Parses simple boolean expressions using (),!,&,| as operators
parseExp :: String -> Either String BExp
parseExp s =
    case parse expr "" s of
        Left err -> Left (show err)
        Right e -> Right e
