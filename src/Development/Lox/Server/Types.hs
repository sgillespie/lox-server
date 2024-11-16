module Development.Lox.Server.Types
  ( LoxProgram (..),
    LoxStmt (..),
    LoxExpr (..),
    LoxError (..),
    Range (..),
    Position (..),
    mkLoxParsingError,
    parsingErrorRange,
    isLoxError,
    isLoxParsingError,
    isLoxFileNotFound,
    displayLoxError,
  ) where

import Language.LSP.Protocol.Types (Position (..), Range (..))
import Text.Megaparsec qualified as Parsec

newtype LoxProgram = LoxProgram [LoxStmt]
  deriving stock (Eq, Show)

newtype LoxStmt = PrintStmt LoxExpr
  deriving stock (Eq, Show)

newtype LoxExpr = LoxString Text
  deriving stock (Eq, Show)

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

mkRange :: Parsec.ParseErrorBundle s e -> Range
mkRange (Parsec.ParseErrorBundle{bundlePosState}) =
  Range
    (Position line (col - 1)) -- Parser position state
    (Position line maxBound) -- Until the end of the line
  where
    line = fromPosState Parsec.sourceLine
    col = fromPosState Parsec.sourceColumn
    fromPosState f =
      fromIntegral
        . Parsec.unPos
        . f
        . Parsec.pstateSourcePos
        $ bundlePosState

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
