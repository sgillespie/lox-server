module Development.Lox.Server.Parser.Internal
  ( Parser,
    ParseError,
    runParseProgram,
    parseProgram,
    declarations,
    declaration,
    stmt,
    expr,
    var,
    string,
    number,
    identifier,
  ) where

import Development.Lox.Server.Span qualified as Span
import Development.Lox.Server.Types qualified as Types

import Control.Monad.Combinators.Expr
import Data.Foldable (foldr1)
import Language.LSP.Protocol.Types (Range (..))
import Relude.Unsafe (read)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lex
import Prelude hiding (div, get, many, some)

type Parser = Parsec Void Text
type ParseError = ParseErrorBundle Text Void

runParseProgram
  :: FilePath
  -> Text
  -> Either ParseError Span.LocatedLoxProgram
runParseProgram = runParser parseProgram

parseProgram :: Parser Span.LocatedLoxProgram
parseProgram = Types.LoxProgram <$> (space *> declarations <* eof)

declarations :: Parser [Span.LocatedLoxStmt]
declarations = catMaybes <$> many declarationWithRecovery

declarationWithRecovery :: Parser (Maybe Span.LocatedLoxStmt)
declarationWithRecovery = withRecovery onFail (Just <$> declaration)
  where
    onFail err = do
      -- Collect all errors until we've consumed all input
      registerParseError err
      -- Skip tokens until we reach the next statement
      void $ takeWhileP Nothing (/= ';')
      void (symbol ";")
      -- No declaration to construct
      pure Nothing

declaration :: Parser Span.LocatedLoxStmt
declaration =
  varStmt
    <|> functionStmt
    <|> classStmt
    <|> stmt

varStmt :: Parser Span.LocatedLoxStmt
varStmt =
  withRange $
    Types.VarStmt
      <$> (symbol "var" *> identifier)
      <*> optional (symbol "=" *> expr)
      <* symbol ";"

functionStmt :: Parser Span.LocatedLoxStmt
functionStmt =
  withRange $ Types.FunctionStmt <$> (symbol "fun" *> function)

function :: Parser Span.LocatedLoxFunction
function =
  Types.LoxFunction
    <$> identifier
    <*> parens (commaSep identifier)
    <*> block

classStmt :: Parser Span.LocatedLoxStmt
classStmt =
  withRange $
    Types.ClassStmt
      <$> (symbol "class" *> identifier)
      <*> optional (symbol "<" *> identifier)
      <*> braces (many function)

stmt :: Parser Span.LocatedLoxStmt
stmt =
  printStmt
    <|> returnStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt
    <|> exprStmt
    <|> blockStmt

fromSourcePos :: SourcePos -> SourcePos -> Span.Located
fromSourcePos s1 s2 =
  Span.mkLocated (toPosition s1) (toPosition s2)

toPosition :: SourcePos -> Span.Position
toPosition (SourcePos{sourceLine, sourceColumn}) =
  Span.Position (toUInt sourceLine) (toUInt sourceColumn)
  where
    toUInt = fromIntegral . pred . unPos

withRange :: Parser (Span.Located -> p) -> Parser p
withRange p = do
  sourcePos1 <- getSourcePos
  p' <- p
  sourcePos2 <- getSourcePos

  let range = fromSourcePos sourcePos1 sourcePos2

  pure (p' range)

printStmt :: Parser Span.LocatedLoxStmt
printStmt = do
  withRange $ Types.PrintStmt <$> parsePrintExpr
  where
    parsePrintExpr = symbol "print" *> expr <* symbol ";"

returnStmt :: Parser Span.LocatedLoxStmt
returnStmt =
  withRange $ Types.ReturnStmt <$> parseReturnStmt
  where
    parseReturnStmt = symbol "return" *> optional expr <* symbol ";"

ifStmt :: Parser Span.LocatedLoxStmt
ifStmt =
  withRange $
    Types.IfStmt
      <$> (symbol "if" *> parens expr)
      <*> stmt
      <*> optional (symbol "else" *> stmt)

whileStmt :: Parser Span.LocatedLoxStmt
whileStmt =
  withRange $
    Types.WhileStmt
      <$> (symbol "while" *> parens expr)
      <*> stmt

forStmt :: Parser Span.LocatedLoxStmt
forStmt = do
  ((init', cond, incr, body), range) <-
    withRange $ do
      void (symbol "for")
      (init', cond, incr) <-
        parens $ do
          init' <- varStmt <|> exprStmt
          cond <- optional expr <* symbol ";"
          incr <- optional expr
          pure (init', cond, incr)
      body <- stmt

      pure ((init', cond, incr, body),)

  -- Transform into a while
  let cond' = fromMaybe (Types.LoxVar "true" range) cond
      incrStmt = flip Types.ExprStmt range <$> incr
      body' =
        case incrStmt of
          Just incr' -> Types.BlockStmt (body : [incr']) range
          Nothing -> body

  pure $ Types.BlockStmt [init', Types.WhileStmt cond' body' range] range

exprStmt :: Parser Span.LocatedLoxStmt
exprStmt = withRange $ Types.ExprStmt <$> parseStmt'
  where
    parseStmt' = expr <* symbol ";"

blockStmt :: Parser Span.LocatedLoxStmt
blockStmt = withRange $ Types.BlockStmt <$> block

block :: Parser [Span.LocatedLoxStmt]
block = braces (many declaration)

expr :: Parser Span.LocatedLoxExpr
expr = assignment

assignment :: Parser Span.LocatedLoxExpr
assignment =
  try
    (withRange $ uncurry Types.LoxSet <$> get' <*> (symbol "=" *> opExpr))
    <|> try
      (withRange $ Types.LoxAssign <$> identifier <*> (symbol "=" *> opExpr))
    <|> opExpr

opExpr :: Parser Span.LocatedLoxExpr
opExpr = makeExprParser term table <?> "expression"
  where
    table =
      [ [ Prefix $ mkUnary (dash <|> exclamation)
        ],
        [ InfixL $ mkBinary <$> mul,
          InfixL $ mkBinary <$> div
        ],
        [ InfixL $ mkBinary <$> add,
          InfixL $ mkBinary <$> sub
        ],
        [ InfixL $ mkBinary <$> greaterEq,
          InfixL $ mkBinary <$> greater,
          InfixL $ mkBinary <$> lessEq,
          InfixL $ mkBinary <$> less
        ],
        [ InfixN $ mkBinary <$> eq,
          InfixN $ mkBinary <$> notEq
        ],
        [InfixL $ mkBinary <$> logicalAnd],
        [InfixL $ mkBinary <$> logicalOr]
      ]
    mkUnary p = manyUnary $ do
      p1 <- getSourcePos
      op <- p

      pure $ \expr' ->
        let (Span.Located (Range _ p2)) = Span.loc expr'
            loc = Span.mkLocated (toPosition p1) p2
        in Types.LoxUnary op expr' loc

    manyUnary p = foldr1 (.) <$> some p

    mkBinary op lhs rhs =
      let (Span.Located (Range p1 _)) = Span.loc lhs
          (Span.Located (Range _ p2)) = Span.loc rhs
      in Types.LoxBinary op lhs rhs (Span.Located (Range p1 p2))

term :: Parser Span.LocatedLoxExpr
term =
  try get
    <|> try funCall
    <|> primary

funCall :: Parser Span.LocatedLoxExpr
funCall =
  withRange $
    Types.LoxCall
      <$> primary
      <*> parens (commaSep expr)

get :: Parser Span.LocatedLoxExpr
get = withRange $ uncurry Types.LoxGet <$> get'

get' :: Parser (Span.LocatedLoxExpr, Text)
get' = (,) <$> primary <*> (symbol "." *> identifier)

primary :: Parser Span.LocatedLoxExpr
primary =
  superExpr
    <|> parens expr
    <|> (lexeme string <?> "string")
    <|> (lexeme number <?> "number")
    <|> (lexeme var <?> "variable")

superExpr :: Parser Span.LocatedLoxExpr
superExpr =
  withRange $
    Types.LoxSuper <$> (symbol "super" *> symbol "." *> identifier)

string :: Parser Span.LocatedLoxExpr
string = withRange (Types.LoxString . toText <$> betweenQuotes)
  where
    betweenQuotes = Char.char '\"' *> manyTill Lex.charLiteral (Char.char '\"')

number :: Parser Span.LocatedLoxExpr
number =
  withRange $
    mkNumber
      <$> some Char.digitChar
      <*> optional (Char.char '.' *> some Char.digitChar)
  where
    mkNumber :: String -> Maybe String -> Span.Located -> Span.LocatedLoxExpr
    mkNumber intPart Nothing r = Types.LoxNumber (read intPart) r
    mkNumber intPart (Just decPart) r =
      Types.LoxNumber (read $ intPart <> "." <> decPart) r

var :: Parser Span.LocatedLoxExpr
var = withRange (Types.LoxVar <$> identifier)

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
