module Development.Lox.Server.Parser
  ( parseLoxFile,
    parseLox,
  ) where

import Control.Exception (throwIO)
import Development.Lox.Server.Types qualified as Types

parseLoxFile :: FilePath -> IO Types.LoxProgram
parseLoxFile file = do
  contents <- readFileBS file
  let res = parseLox (decodeUtf8 contents)
  either throwIO pure res

parseLox :: Text -> Either Types.LoxError Types.LoxProgram
parseLox _ =
  Right $
    Types.LoxProgram
      [ Types.PrintStmt (Types.LoxString "Hello, world!")
      ]
