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
import Prelude hiding (get, many, some)

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
parseExpr = unary

unary :: Parser Types.LoxExpr
unary =
  (Types.LoxUnary <$> exclaim <*> unary)
    <|> (Types.LoxUnary <$> dash <*> unary)
    <|> call
  where
    exclaim = symbol "!" $> Types.Exclamation
    dash = symbol "-" $> Types.Dash

call :: Parser Types.LoxExpr
call =
  try funCall
    <|> try get
    <|> primary

funCall :: Parser Types.LoxExpr
funCall = Types.LoxCall <$> primary <*> parens (commaSep parseExpr)

get :: Parser Types.LoxExpr
get = Types.LoxGet <$> primary <*> (symbol "." *> identifier)

primary :: Parser Types.LoxExpr
primary =
  superExpr
    <|> parens primary
    <|> (lexeme string <?> "string")
    <|> (lexeme number <?> "number")
    <|> (lexeme var <?> "variable")

superExpr :: Parser Types.LoxExpr
superExpr =
  symbol "super" *> symbol "." *> var

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

var :: Parser Types.LoxExpr
var = Types.LoxVar <$> identifier

identifier :: Parser Text
identifier =
  mkVar <$> Char.letterChar <*> many Char.alphaNumChar
  where
    mkVar :: Char -> [Char] -> Text
    mkVar c cs = toText (c : cs)

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` symbol ",")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

space :: Parser ()
space = Lex.space Char.space1 (Lex.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space
