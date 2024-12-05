module Development.Lox.Server.Types
  ( LoxProgram (..),
    LoxStmt (..),
    LoxFunction (..),
    LoxExpr (..),
    LoxError (..),
    LoxParsingError (..),
    LoxUnaryOp (..),
    LoxBinaryOp (..),
    Range (..),
    Position (..),
    fromParseErrorBundle,
    parsingErrorRange,
    isLoxError,
    isLoxParsingError,
    isLoxFileNotFound,
  ) where

import Language.LSP.Protocol.Types (Position (..), Range (..), UInt)
import Text.Megaparsec qualified as Parsec

newtype LoxProgram = LoxProgram [LoxStmt]
  deriving stock (Eq, Show)

data LoxStmt
  = VarStmt Text (Maybe LoxExpr)
  | FunctionStmt LoxFunction
  | ClassStmt Text (Maybe Text) [LoxFunction]
  | PrintStmt LoxExpr
  | ReturnStmt (Maybe LoxExpr)
  | IfStmt LoxExpr LoxStmt (Maybe LoxStmt)
  | WhileStmt LoxExpr LoxStmt
  | ExprStmt LoxExpr
  | BlockStmt [LoxStmt]
  deriving stock (Eq, Show)

data LoxFunction = LoxFunction
  { functionName :: Text,
    functionParams :: [Text],
    functionBody :: [LoxStmt]
  }
  deriving stock (Eq, Show)

data LoxExpr
  = LoxString Text
  | LoxNumber Double
  | LoxVar Text
  | LoxCall LoxExpr [LoxExpr]
  | LoxGet LoxExpr Text
  | LoxUnary LoxUnaryOp LoxExpr
  | LoxBinary LoxBinaryOp LoxExpr LoxExpr
  | LoxAssign Text LoxExpr
  | LoxSet LoxExpr Text LoxExpr
  deriving stock (Eq, Show)

data LoxUnaryOp
  = Exclamation
  | Dash
  deriving (Eq, Show)

data LoxBinaryOp
  = Mul
  | Div
  | Add
  | Sub
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
  | LogicalAnd
  | LogicalOr
  deriving (Eq, Ord, Show)

data LoxError
  = LoxParsingErrors [LoxParsingError]
  | LoxFileNotFound
  deriving stock (Eq, Ord, Show)

data LoxParsingError = LoxParsingError Range Text
  deriving (Eq, Ord, Show)

instance Exception LoxError

fromParseErrorBundle
  :: Parsec.ParseErrorBundle Text Void
  -> LoxError
fromParseErrorBundle (Parsec.ParseErrorBundle errs pos) =
  evalState (fromParseErrors errs) pos

fromParseErrors
  :: NonEmpty (Parsec.ParseError Text Void)
  -> State (Parsec.PosState Text) LoxError
fromParseErrors errs =
  LoxParsingErrors <$> mapM fromParseError (toList errs)

fromParseError
  :: Parsec.ParseError Text Void
  -> State (Parsec.PosState Text) LoxParsingError
fromParseError err = do
  let errText = toText (Parsec.parseErrorTextPretty err)
  range <- parseErrorRange err

  pure (LoxParsingError range errText)

parseErrorRange
  :: Parsec.ParseError Text e
  -> State (Parsec.PosState Text) Range
parseErrorRange err = do
  posState <- get

  let offset = Parsec.errorOffset err
      newPosState = Parsec.reachOffsetNoLine offset posState
      line = fromPosState Parsec.sourceLine newPosState
      col = fromPosState Parsec.sourceColumn newPosState

  put newPosState

  pure $
    Range
      (Position (line - 1) (col - 1)) -- Parser position state
      (Position (line - 1) maxBound) -- Until the end of the line
  where
    fromPosState :: (Parsec.SourcePos -> Parsec.Pos) -> Parsec.PosState s -> UInt
    fromPosState f = fromIntegral . Parsec.unPos . f . Parsec.pstateSourcePos

parsingErrorRange :: LoxParsingError -> Range
parsingErrorRange (LoxParsingError range _) = range

isLoxError :: LoxError -> Bool
isLoxError = const True

isLoxParsingError :: LoxError -> Bool
isLoxParsingError (LoxParsingErrors _) = True
isLoxParsingError _ = False

isLoxFileNotFound :: LoxError -> Bool
isLoxFileNotFound LoxFileNotFound = True
isLoxFileNotFound _ = False
