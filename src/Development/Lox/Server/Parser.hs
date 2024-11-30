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
import Prelude hiding (div, get, many, some)

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
parseExpr = assignment

assignment :: Parser Types.LoxExpr
assignment =
  try (uncurry Types.LoxSet <$> get' <*> (symbol "=" *> logicalOr'))
    <|> try (Types.LoxAssign <$> identifier <*> (symbol "=" *> logicalOr'))
    <|> logicalOr'

logicalOr' :: Parser Types.LoxExpr
logicalOr' = binary logicalAnd' logicalOr logicalOr'

binary
  :: Parser Types.LoxExpr
  -> Parser Types.LoxBinaryOp
  -> Parser Types.LoxExpr
  -> Parser Types.LoxExpr
binary left op right = do
  left' <- left

  binOp <- optional $ do
    op' <- op
    right' <- right
    pure (op', right')

  case binOp of
    Just (op', right') -> pure (mkBinary left' op' right')
    Nothing -> pure left'

logicalAnd' :: Parser Types.LoxExpr
logicalAnd' = binary equality logicalAnd logicalAnd'

equality :: Parser Types.LoxExpr
equality = binary comparison eqOp equality
  where
    eqOp = eq <|> notEq

comparison :: Parser Types.LoxExpr
comparison = binary term cmpOp comparison
  where
    cmpOp = greaterEq <|> greater <|> lessEq <|> less

term :: Parser Types.LoxExpr
term = binary factor termOp term
  where
    termOp = add <|> sub

factor :: Parser Types.LoxExpr
factor = binary unary factorOp factor
  where
    factorOp :: Parser Types.LoxBinaryOp
    factorOp = mul <|> div

mkBinary
  :: Types.LoxExpr
  -> Types.LoxBinaryOp
  -> Types.LoxExpr
  -> Types.LoxExpr
mkBinary =
  flip Types.LoxBinary

unary :: Parser Types.LoxExpr
unary =
  (Types.LoxUnary <$> exclamation <*> unary)
    <|> (Types.LoxUnary <$> dash <*> unary)
    <|> call

call :: Parser Types.LoxExpr
call =
  try get
    <|> try funCall
    <|> primary

funCall :: Parser Types.LoxExpr
funCall = Types.LoxCall <$> primary <*> parens (commaSep parseExpr)

get :: Parser Types.LoxExpr
get = uncurry Types.LoxGet <$> get'

get' :: Parser (Types.LoxExpr, Text)
get' = (,) <$> primary <*> (symbol "." *> identifier)

primary :: Parser Types.LoxExpr
primary =
  superExpr
    <|> parens parseExpr
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

mul :: Parser Types.LoxBinaryOp
mul = symbol "*" $> Types.Mul

div :: Parser Types.LoxBinaryOp
div = symbol "/" $> Types.Div

add :: Parser Types.LoxBinaryOp
add = symbol "+" $> Types.Add

sub :: Parser Types.LoxBinaryOp
sub = symbol "-" $> Types.Sub

greater :: Parser Types.LoxBinaryOp
greater = symbol ">" $> Types.Greater

greaterEq :: Parser Types.LoxBinaryOp
greaterEq = symbol ">=" $> Types.GreaterEq

less :: Parser Types.LoxBinaryOp
less = symbol "<" $> Types.Less

lessEq :: Parser Types.LoxBinaryOp
lessEq = symbol "<=" $> Types.LessEq

eq :: Parser Types.LoxBinaryOp
eq = symbol "==" $> Types.Eq

notEq :: Parser Types.LoxBinaryOp
notEq = symbol "!=" $> Types.NotEq

logicalAnd :: Parser Types.LoxBinaryOp
logicalAnd = symbol "&&" $> Types.LogicalAnd

logicalOr :: Parser Types.LoxBinaryOp
logicalOr = symbol "||" $> Types.LogicalOr

exclamation :: Parser Types.LoxUnaryOp
exclamation = symbol "!" $> Types.Exclamation

dash :: Parser Types.LoxUnaryOp
dash = symbol "-" $> Types.Dash

identifier :: Parser Text
identifier =
  lexeme $ mkVar <$> Char.letterChar <*> many Char.alphaNumChar
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
