module Development.Lox.Server.Parser
  ( parseLoxFile,
    parseLox,
  ) where

import Development.Lox.Server.Types qualified as Types

import Control.Exception (throwIO)
import System.FilePath (takeFileName)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lex
import Prelude hiding (many)

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
parseStmt = parsePrintStmt

parsePrintStmt :: Parser Types.LoxStmt
parsePrintStmt = Types.PrintStmt <$> parsePrintExpr
  where
    parsePrintExpr = symbol "print" *> parseExpr <* symbol ";"

parseExpr :: Parser Types.LoxExpr
parseExpr = parseStrExpr

parseStrExpr :: Parser Types.LoxExpr
parseStrExpr = Types.LoxString . toText <$> betweenQuotes
  where
    betweenQuotes = char '\"' *> manyTill Lex.charLiteral (char '\"')

space :: Parser ()
space = Lex.space space1 (Lex.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space
