module Development.Lox.Server.Types
  ( LoxProgram (..),
    LoxStmt (..),
    LoxFunction (..),
    LoxExpr (..),
    LoxError (..),
    LoxUnaryOp (..),
    LoxBinaryOp (..),
    Range (..),
    Position (..),
    mkLoxParsingError,
    parsingErrorRange,
    isLoxError,
    isLoxParsingError,
    isLoxFileNotFound,
    displayLoxError,
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
  = LoxParsingError Range Text
  | LoxFileNotFound
  deriving stock (Eq, Ord, Show)

instance Exception LoxError

mkLoxParsingError
  :: Parsec.ParseErrorBundle Text Void
  -> LoxError
mkLoxParsingError err =
  LoxParsingError (mkRange err) (toErrText err)
  where
    toErrText = toText . Parsec.errorBundlePretty

mkRange :: Parsec.ParseErrorBundle Text e -> Range
mkRange (Parsec.ParseErrorBundle{bundleErrors, bundlePosState}) =
  let offset = Parsec.errorOffset (head bundleErrors)
      newPosState = Parsec.reachOffsetNoLine offset bundlePosState
      line = fromPosState Parsec.sourceLine newPosState
      col = fromPosState Parsec.sourceColumn newPosState
  in Range
      (Position (line - 1) (col - 1)) -- Parser position state
      (Position (line - 1) maxBound) -- Until the end of the line
  where
    fromPosState :: (Parsec.SourcePos -> Parsec.Pos) -> Parsec.PosState s -> UInt
    fromPosState f = fromIntegral . Parsec.unPos . f . Parsec.pstateSourcePos

parsingErrorRange :: LoxError -> Maybe Range
parsingErrorRange (LoxParsingError range _) = Just range
parsingErrorRange _ = Nothing

displayLoxError :: LoxError -> Text
displayLoxError (LoxParsingError _ err) = err
displayLoxError LoxFileNotFound = "File not found"

isLoxError :: LoxError -> Bool
isLoxError = const True

isLoxParsingError :: LoxError -> Bool
isLoxParsingError (LoxParsingError _ _) = True
isLoxParsingError _ = False

isLoxFileNotFound :: LoxError -> Bool
isLoxFileNotFound LoxFileNotFound = True
isLoxFileNotFound _ = False
