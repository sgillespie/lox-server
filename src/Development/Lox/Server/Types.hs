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

import Language.LSP.Protocol.Types (Position (..), Range (..), UInt)
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

mkRange :: Parsec.ParseErrorBundle Text e -> Range
mkRange (Parsec.ParseErrorBundle{bundleErrors, bundlePosState}) =
  let offset = Parsec.errorOffset (head bundleErrors)
      newPosState = Parsec.reachOffsetNoLine offset bundlePosState
      line = fromPosState Parsec.sourceLine newPosState
      col = fromPosState Parsec.sourceColumn newPosState
  in Range
      (Position line (col - 1)) -- Parser position state
      (Position line maxBound) -- Until the end of the line
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
