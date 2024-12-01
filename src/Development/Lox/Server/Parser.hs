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
  Types.LoxProgram <$> (space *> declarations <* eof)

declarations :: Parser [Types.LoxStmt]
declarations = many declaration

declaration :: Parser Types.LoxStmt
declaration =
  varStmt
    <|> functionStmt
    <|> classStmt
    <|> stmt

varStmt :: Parser Types.LoxStmt
varStmt =
  Types.VarStmt
    <$> (symbol "var" *> identifier)
    <*> optional (symbol "=" *> expr)
    <* symbol ";"

functionStmt :: Parser Types.LoxStmt
functionStmt = Types.FunctionStmt <$> (symbol "fun" *> function)

function :: Parser Types.LoxFunction
function =
  Types.LoxFunction
    <$> identifier
    <*> parens (commaSep identifier)
    <*> block

classStmt :: Parser Types.LoxStmt
classStmt =
  Types.ClassStmt
    <$> (symbol "class" *> identifier)
    <*> optional (symbol "<" *> identifier)
    <*> braces (many function)

stmt :: Parser Types.LoxStmt
stmt =
  printStmt
    <|> returnStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt
    <|> exprStmt
    <|> blockStmt

printStmt :: Parser Types.LoxStmt
printStmt = Types.PrintStmt <$> parsePrintExpr
  where
    parsePrintExpr = symbol "print" *> expr <* symbol ";"

returnStmt :: Parser Types.LoxStmt
returnStmt =
  Types.ReturnStmt <$> parseReturnStmt
  where
    parseReturnStmt = symbol "return" *> optional expr <* symbol ";"

ifStmt :: Parser Types.LoxStmt
ifStmt =
  Types.IfStmt
    <$> (symbol "if" *> parens expr)
    <*> stmt
    <*> optional (symbol "else" *> stmt)

whileStmt :: Parser Types.LoxStmt
whileStmt =
  Types.WhileStmt
    <$> (symbol "while" *> parens expr)
    <*> stmt

forStmt :: Parser Types.LoxStmt
forStmt = do
  void (symbol "for")
  (init', cond, incr) <-
    parens $ do
      init' <- varStmt <|> exprStmt
      cond <- optional expr <* symbol ";"
      incr <- optional expr
      pure (init', cond, incr)
  body <- stmt

  let cond' = fromMaybe (Types.LoxVar "true") cond
      incrStmt = Types.ExprStmt <$> incr
      body' =
        case incrStmt of
          Just incr' -> Types.BlockStmt (body : [incr'])
          Nothing -> body

  pure $ Types.BlockStmt [init', Types.WhileStmt cond' body']

exprStmt :: Parser Types.LoxStmt
exprStmt = Types.ExprStmt <$> parseStmt'
  where
    parseStmt' = expr <* symbol ";"

blockStmt :: Parser Types.LoxStmt
blockStmt = Types.BlockStmt <$> block

block :: Parser [Types.LoxStmt]
block = braces (many declaration)

expr :: Parser Types.LoxExpr
expr = assignment

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
funCall = Types.LoxCall <$> primary <*> parens (commaSep expr)

get :: Parser Types.LoxExpr
get = uncurry Types.LoxGet <$> get'

get' :: Parser (Types.LoxExpr, Text)
get' = (,) <$> primary <*> (symbol "." *> identifier)

primary :: Parser Types.LoxExpr
primary =
  superExpr
    <|> parens expr
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

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

space :: Parser ()
space = Lex.space Char.space1 (Lex.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space
