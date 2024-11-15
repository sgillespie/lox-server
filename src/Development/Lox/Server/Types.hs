module Development.Lox.Server.Types
  ( LoxProgram (..),
    LoxStmt (..),
    LoxExpr (..),
    LoxError (..),
    isLoxError,
    isLoxParsingError,
    isLoxFileNotFound,
  ) where

newtype LoxProgram = LoxProgram [LoxStmt]
  deriving stock (Eq, Show)

newtype LoxStmt = PrintStmt LoxExpr
  deriving stock (Eq, Show)

newtype LoxExpr = LoxString Text
  deriving stock (Eq, Show)

data LoxError
  = LoxParsingError Text
  | LoxFileNotFound
  deriving stock (Eq, Ord, Show)

isLoxError :: LoxError -> Bool
isLoxError = const True

isLoxParsingError :: LoxError -> Bool
isLoxParsingError (LoxParsingError _) = True
isLoxParsingError _ = False

isLoxFileNotFound :: LoxError -> Bool
isLoxFileNotFound LoxFileNotFound = True
isLoxFileNotFound _ = False

instance Exception LoxError
