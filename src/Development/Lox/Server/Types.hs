module Development.Lox.Server.Types
  ( LoxProgram (..),
    LoxStmt (..),
    LoxExpr (..),
    LoxError (..),
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

instance Exception LoxError
