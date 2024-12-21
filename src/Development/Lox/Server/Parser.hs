module Development.Lox.Server.Parser
  ( parseLoxFile,
    parseLox,
  ) where

import Development.Lox.Server.Types qualified as Types

import Control.Exception (throwIO)
import Control.Monad.Combinators.Expr
import Data.Foldable (foldr1)
import Data.Tuple.Extra (uncurry3)
import Relude.Unsafe (read)
import System.FilePath (takeFileName)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lex
import Prelude hiding (div, get, many, some)

type Parser = Parsec Void Text

parseLoxFile :: FilePath -> IO Types.LocatedLoxProgram
parseLoxFile file = do
  contents <- readFileBS file
  let res = parseLox (Just file) (decodeUtf8 contents)
  either throwIO pure res

parseLox :: Maybe FilePath -> Text -> Either Types.LoxError Types.LocatedLoxProgram
parseLox path input =
  first
    Types.fromParseErrorBundle
    (runParser parseProgram file input)
  where
    file = maybe "<inline>" takeFileName path

parseProgram :: Parser Types.LocatedLoxProgram
parseProgram = Types.LoxProgram <$> (space *> declarations <* eof)

declarations :: Parser [Types.LocatedLoxStmt]
declarations = catMaybes <$> many declarationWithRecovery

declarationWithRecovery :: Parser (Maybe Types.LocatedLoxStmt)
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

declaration :: Parser Types.LocatedLoxStmt
declaration =
  varStmt
    <|> functionStmt
    <|> classStmt
    <|> stmt

varStmt :: Parser Types.LocatedLoxStmt
varStmt =
  withRange (uncurry . Types.VarStmt) $
    (,)
      <$> (symbol "var" *> identifier)
      <*> optional (symbol "=" *> expr)
      <* symbol ";"

functionStmt :: Parser Types.LocatedLoxStmt
functionStmt =
  withRange Types.FunctionStmt (symbol "fun" *> function)

function :: Parser Types.LocatedLoxFunction
function =
  Types.LoxFunction
    <$> identifier
    <*> parens (commaSep identifier)
    <*> block

classStmt :: Parser Types.LocatedLoxStmt
classStmt =
  withRange (uncurry3 . Types.ClassStmt) $
    (,,)
      <$> (symbol "class" *> identifier)
      <*> optional (symbol "<" *> identifier)
      <*> braces (many function)

stmt :: Parser Types.LocatedLoxStmt
stmt =
  printStmt
    <|> returnStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt
    <|> exprStmt
    <|> blockStmt

fromSourcePos :: SourcePos -> SourcePos -> Types.Range
fromSourcePos s1 s2 =
  Types.Range (toPosition s1) (toPosition s2)
  where
    toPosition (SourcePos{sourceLine, sourceColumn}) =
      Types.Position (toUInt sourceLine) (toUInt sourceColumn)
    toUInt = fromIntegral . pred . unPos

withRange :: (Types.Range -> p -> p') -> Parser p -> Parser p'
withRange f p = do
  sourcePos1 <- getSourcePos
  p' <- p
  sourcePos2 <- getSourcePos

  pure $ f (fromSourcePos sourcePos1 sourcePos2) p'

printStmt :: Parser Types.LocatedLoxStmt
printStmt = do
  withRange Types.PrintStmt parsePrintExpr
  where
    parsePrintExpr = symbol "print" *> expr <* symbol ";"

returnStmt :: Parser Types.LocatedLoxStmt
returnStmt =
  withRange Types.ReturnStmt parseReturnStmt
  where
    parseReturnStmt = symbol "return" *> optional expr <* symbol ";"

ifStmt :: Parser Types.LocatedLoxStmt
ifStmt = do
  withRange (uncurry3 . Types.IfStmt) $
    (,,)
      <$> (symbol "if" *> parens expr)
      <*> stmt
      <*> optional (symbol "else" *> stmt)

whileStmt :: Parser Types.LocatedLoxStmt
whileStmt =
  withRange (uncurry . Types.WhileStmt) $
    (,)
      <$> (symbol "while" *> parens expr)
      <*> stmt

forStmt :: Parser Types.LocatedLoxStmt
forStmt = do
  (range, (init', cond, incr, body)) <- withRange (,) $ do
    void (symbol "for")
    (init', cond, incr) <-
      parens $ do
        init' <- varStmt <|> exprStmt
        cond <- optional expr <* symbol ";"
        incr <- optional expr
        pure (init', cond, incr)
    body <- stmt

    pure (init', cond, incr, body)

  -- Transform into a while
  let cond' = fromMaybe (Types.LoxVar range "true") cond
      incrStmt = Types.ExprStmt range <$> incr
      body' =
        case incrStmt of
          Just incr' -> Types.BlockStmt range (body : [incr'])
          Nothing -> body

  pure $ Types.BlockStmt range [init', Types.WhileStmt range cond' body']

exprStmt :: Parser Types.LocatedLoxStmt
exprStmt = withRange Types.ExprStmt parseStmt'
  where
    parseStmt' = expr <* symbol ";"

blockStmt :: Parser Types.LocatedLoxStmt
blockStmt = withRange Types.BlockStmt block

block :: Parser [Types.LocatedLoxStmt]
block = braces (many declaration)

expr :: Parser Types.LocatedLoxExpr
expr = assignment

assignment :: Parser Types.LocatedLoxExpr
assignment =
  try
    (withRange (uncurry . uncurry . Types.LoxSet) $ (,) <$> get' <*> (symbol "=" *> opExpr))
    <|> try
      (withRange (uncurry . Types.LoxAssign) $ (,) <$> identifier <*> (symbol "=" *> opExpr))
    <|> opExpr

opExpr :: Parser Types.LocatedLoxExpr
opExpr = makeExprParser term table <?> "expression"
  where
    table =
      [ [Prefix $ manyUnary (exclamation <|> dash)],
        [ InfixL $ withRange Types.LoxBinary mul,
          InfixL $ withRange Types.LoxBinary div
        ],
        [ InfixL $ withRange Types.LoxBinary add,
          InfixL $ withRange Types.LoxBinary sub
        ],
        [ InfixL $ withRange Types.LoxBinary greaterEq,
          InfixL $ withRange Types.LoxBinary greater,
          InfixL $ withRange Types.LoxBinary lessEq,
          InfixL $ withRange Types.LoxBinary less
        ],
        [ InfixN $ withRange Types.LoxBinary eq,
          InfixN $ withRange Types.LoxBinary notEq
        ],
        [InfixL $ withRange Types.LoxBinary logicalAnd],
        [InfixL $ withRange Types.LoxBinary logicalOr]
      ]
    manyUnary p = foldr1 (.) <$> some (withRange Types.LoxUnary p)

term :: Parser Types.LocatedLoxExpr
term =
  try get
    <|> try funCall
    <|> primary

funCall :: Parser Types.LocatedLoxExpr
funCall =
  withRange (uncurry . Types.LoxCall) $
    (,) <$> primary <*> parens (commaSep expr)

get :: Parser Types.LocatedLoxExpr
get =
  withRange (uncurry . Types.LoxGet) get'

get' :: Parser (Types.LocatedLoxExpr, Text)
get' = (,) <$> primary <*> (symbol "." *> identifier)

primary :: Parser Types.LocatedLoxExpr
primary =
  superExpr
    <|> parens expr
    <|> (lexeme string <?> "string")
    <|> (lexeme number <?> "number")
    <|> (lexeme var <?> "variable")

superExpr :: Parser Types.LocatedLoxExpr
superExpr =
  symbol "super" *> symbol "." *> var

string :: Parser Types.LocatedLoxExpr
string = withRange Types.LoxString (toText <$> betweenQuotes)
  where
    betweenQuotes = Char.char '\"' *> manyTill Lex.charLiteral (Char.char '\"')

number :: Parser Types.LocatedLoxExpr
number =
  withRange (uncurry . mkNumber) $
    (,)
      <$> some Char.digitChar
      <*> optional (Char.char '.' *> some Char.digitChar)
  where
    mkNumber :: Types.Range -> String -> Maybe String -> Types.LocatedLoxExpr
    mkNumber r intPart Nothing = Types.LoxNumber r (read intPart)
    mkNumber r intPart (Just decPart) =
      Types.LoxNumber r $ read $ intPart <> "." <> decPart

var :: Parser Types.LocatedLoxExpr
var = withRange Types.LoxVar identifier

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
