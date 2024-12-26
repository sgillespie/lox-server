module Development.Lox.Server.Parser
  ( parseLoxFile,
    parseLox,
    readLoxFile,
  ) where

import Development.Lox.Server.Parser.Internal (parseProgram)
import Development.Lox.Server.Span qualified as Span
import Development.Lox.Server.Types (LoxError, fromParseErrorBundle)

import Control.Exception (throwIO)
import System.FilePath (takeFileName)
import Text.Megaparsec (runParser)
import Prelude hiding (div, get, many, some)

parseLoxFile :: FilePath -> IO Span.LocatedLoxProgram
parseLoxFile file = do
  contents <- readFileBS file
  let res = parseLox (Just file) (decodeUtf8 contents)
  either throwIO pure res

parseLox :: Maybe FilePath -> Text -> Either LoxError Span.LocatedLoxProgram
parseLox path input =
  first
    fromParseErrorBundle
    (runParser parseProgram file input)
  where
    file = maybe "<inline>" takeFileName path

readLoxFile :: FilePath -> IO Text
readLoxFile = fmap decodeUtf8 . readFileBS
