module Development.Lox.Server.Types
  ( LoxProgram (..),
    LoxStmt (..),
    LoxFunction (..),
    LoxExpr (..),
    LoxError (..),
    LoxParsingError (..),
    LoxUnaryOp (..),
    LoxBinaryOp (..),
    fromParseErrorBundle,
    parsingErrorRange,
    isLoxError,
    isLoxParsingError,
    isLoxFileNotFound,
  ) where

import Language.LSP.Protocol.Types (Position (..), Range (..), UInt)
import Prettyprinter
import Text.Megaparsec qualified as Parsec

newtype LoxProgram e = LoxProgram [LoxStmt e]
  deriving stock (Eq, Show)

data LoxStmt e
  = VarStmt Text (Maybe (LoxExpr e)) e
  | FunctionStmt (LoxFunction e) e
  | ClassStmt Text (Maybe Text) [LoxFunction e] e
  | PrintStmt (LoxExpr e) e
  | ReturnStmt (Maybe (LoxExpr e)) e
  | IfStmt (LoxExpr e) (LoxStmt e) (Maybe (LoxStmt e)) e
  | WhileStmt (LoxExpr e) (LoxStmt e) e
  | ExprStmt (LoxExpr e) e
  | BlockStmt [LoxStmt e] e
  deriving stock (Eq, Show)

instance Functor LoxStmt where
  fmap f stmt =
    case stmt of
      VarStmt txt expr e -> VarStmt txt (fmap (fmap f) expr) (f e)
      FunctionStmt fun e -> FunctionStmt (fmap f fun) (f e)
      ClassStmt name extends body e -> ClassStmt name extends (map (fmap f) body) (f e)
      PrintStmt expr e -> PrintStmt (fmap f expr) (f e)
      ReturnStmt expr e -> ReturnStmt (fmap (fmap f) expr) (f e)
      IfStmt cond then' else' e -> IfStmt (fmap f cond) (fmap f then') (fmap (fmap f) else') (f e)
      WhileStmt cond body e -> WhileStmt (fmap f cond) (fmap f body) (f e)
      ExprStmt expr e -> ExprStmt (fmap f expr) (f e)
      BlockStmt exprs e -> BlockStmt (map (fmap f) exprs) (f e)

instance Pretty e => Pretty (LoxStmt e) where
  pretty (VarStmt name v _) =
    "var" <+> pretty name <> maybe "" ((" =" <+>) . pretty) v
  pretty (FunctionStmt fun _) = pretty fun
  pretty (ClassStmt name extends _ _) =
    "class" <+> pretty name <+> maybe "" (("<" <+>) . pretty) extends
  pretty (PrintStmt expr _) = "print" <+> pretty expr
  pretty (ReturnStmt expr _) = "return" <+> pretty expr
  pretty (IfStmt cond _ _ _) = "if" <+> pretty cond
  pretty (WhileStmt cond _ _) = "while" <+> pretty cond
  pretty (ExprStmt expr _) = pretty expr
  pretty (BlockStmt _ _) = "{ ... }"

data LoxFunction e = LoxFunction
  { functionName :: Text,
    functionParams :: [Text],
    functionBody :: [LoxStmt e]
  }
  deriving stock (Eq, Show)

instance Functor LoxFunction where
  fmap f fun@(LoxFunction{functionBody}) =
    fun{functionBody = map (fmap f) functionBody}

instance Pretty e => Pretty (LoxFunction e) where
  pretty _ = "function"

data LoxExpr e
  = LoxString Text e
  | LoxNumber Double e
  | LoxVar Text e
  | LoxCall (LoxExpr e) [LoxExpr e] e
  | LoxGet (LoxExpr e) Text e
  | LoxUnary LoxUnaryOp (LoxExpr e) e
  | LoxBinary LoxBinaryOp (LoxExpr e) (LoxExpr e) e
  | LoxAssign Text (LoxExpr e) e
  | LoxSet (LoxExpr e) Text (LoxExpr e) e
  | LoxSuper Text e
  deriving stock (Eq, Show)

instance Functor LoxExpr where
  fmap f expr =
    case expr of
      LoxString txt e -> LoxString txt (f e)
      LoxNumber n e -> LoxNumber n (f e)
      LoxVar txt e -> LoxVar txt (f e)
      LoxCall expr' params e -> LoxCall (fmap f expr') (map (fmap f) params) (f e)
      LoxGet expr' name e -> LoxGet (fmap f expr') name (f e)
      LoxUnary op expr' e -> LoxUnary op (fmap f expr') (f e)
      LoxBinary op e1 e2 e -> LoxBinary op (fmap f e1) (fmap f e2) (f e)
      LoxAssign name expr' e -> LoxAssign name (fmap f expr') (f e)
      LoxSet expr' name val e -> LoxSet (fmap f expr') name (fmap f val) (f e)
      LoxSuper method e -> LoxSuper method (f e)

instance Pretty e => Pretty (LoxExpr e) where
  pretty (LoxString str _) = "\"" <> pretty str <> "\""
  pretty (LoxNumber n _) = pretty n
  pretty (LoxVar n _) = pretty n
  pretty (LoxCall obj params _) = pretty obj <> "(" <> pretty params <> ")"
  pretty (LoxGet obj name _) = pretty obj <> "." <> pretty name
  pretty (LoxUnary op expr _) = pretty op <> pretty expr
  pretty (LoxBinary op e1 e2 _) = pretty e1 <+> pretty op <+> pretty e2
  pretty (LoxAssign name expr _) = pretty name <+> "=" <+> pretty expr
  pretty (LoxSet obj name expr _) =
    pretty obj <> "." <> pretty name <+> "=" <+> pretty expr
  pretty (LoxSuper m _) = "super." <> pretty m

data LoxUnaryOp
  = Exclamation
  | Dash
  deriving (Eq, Show)

instance Pretty LoxUnaryOp where
  pretty Exclamation = "!"
  pretty Dash = "-"

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

instance Pretty LoxBinaryOp where
  pretty Mul = "*"
  pretty Div = "/"
  pretty Add = "+"
  pretty Sub = "-"
  pretty Greater = ">"
  pretty GreaterEq = ">="
  pretty Less = "<"
  pretty LessEq = "<="
  pretty Eq = "=="
  pretty NotEq = "!="
  pretty LogicalAnd = "&&"
  pretty LogicalOr = "||"

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
      posLine = fromPosState Parsec.sourceLine newPosState
      posCol = fromPosState Parsec.sourceColumn newPosState

  put newPosState

  pure $
    Range
      (Position (posLine - 1) (posCol - 1)) -- Parser position state
      (Position (posLine - 1) maxBound) -- Until the end of the line
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
