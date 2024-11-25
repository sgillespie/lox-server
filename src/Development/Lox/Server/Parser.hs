module Development.Lox.Server.Parser
  ( parseLoxFile,
    parseLox,
  ) where

import Development.Lox.Server.Types qualified as Types

import Control.Exception (throwIO)
import Relude.Unsafe (read)
import System.FilePath (takeFileName)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lex
import Prelude hiding (many, some)

type Parser = Parsec Void Text

parseLoxFile :: FilePath -> IO Types.LoxProgram
parseLoxFile file = do
  contents <- readFileBS file
  let res = parseLox (Just file) (decodeUtf8 contents)
  either throwIO pure res

parseLox :: Maybe FilePath -> Text -> Either Types.LoxError Types.LoxProgram
parseLox path input =
  first
    Types.mkLoxParsingError
    (runParser parseProgram file input)
  where
    file = maybe "<inline>" takeFileName path

parseProgram :: Parser Types.LoxProgram
parseProgram =
  Types.LoxProgram <$> (space *> parseStmts <* eof)

parseStmts :: Parser [Types.LoxStmt]
parseStmts = many parseStmt

parseStmt :: Parser Types.LoxStmt
parseStmt = parsePrintStmt <|> parseExprStmt

parsePrintStmt :: Parser Types.LoxStmt
parsePrintStmt = Types.PrintStmt <$> parsePrintExpr
  where
    parsePrintExpr = symbol "print" *> parseExpr <* symbol ";"

parseExprStmt :: Parser Types.LoxStmt
parseExprStmt = Types.ExprStmt <$> parseStmt'
  where
    parseStmt' = parseExpr <* symbol ";"

parseExpr :: Parser Types.LoxExpr
parseExpr = primary

primary :: Parser Types.LoxExpr
primary =
  superExpr
    <|> parens primary
    <|> (lexeme string <?> "string")
    <|> (lexeme number <?> "number")
    <|> (lexeme identifier <?> "variable")

superExpr :: Parser Types.LoxExpr
superExpr =
  symbol "super" *> symbol "." *> identifier

string :: Parser Types.LoxExpr
string = Types.LoxString . toText <$> betweenQuotes
  where
    betweenQuotes = Char.char '\"' *> manyTill Lex.charLiteral (Char.char '\"')

number :: Parser Types.LoxExpr
number =
  mkNumber
    <$> some Char.digitChar
    <*> optional (Char.char '.' *> some Char.digitChar)
  where
    mkNumber :: String -> Maybe String -> Types.LoxExpr
    mkNumber intPart Nothing = Types.LoxNumber $ read intPart
    mkNumber intPart (Just decPart) = Types.LoxNumber $ read $ intPart <> "." <> decPart

identifier :: Parser Types.LoxExpr
identifier =
  mkVar <$> Char.letterChar <*> many Char.alphaNumChar
  where
    mkVar :: Char -> [Char] -> Types.LoxExpr
    mkVar c cs = Types.LoxVar $ toText (c : cs)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

space :: Parser ()
space = Lex.space Char.space1 (Lex.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space
