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
  = VarStmt e Text (Maybe (LoxExpr e))
  | FunctionStmt e (LoxFunction e)
  | ClassStmt e Text (Maybe Text) [LoxFunction e]
  | PrintStmt e (LoxExpr e)
  | ReturnStmt e (Maybe (LoxExpr e))
  | IfStmt e (LoxExpr e) (LoxStmt e) (Maybe (LoxStmt e))
  | WhileStmt e (LoxExpr e) (LoxStmt e)
  | ExprStmt e (LoxExpr e)
  | BlockStmt e [LoxStmt e]
  deriving stock (Eq, Show)

instance Functor LoxStmt where
  fmap f stmt =
    case stmt of
      VarStmt e txt expr -> VarStmt (f e) txt (fmap (fmap f) expr)
      FunctionStmt e fun -> FunctionStmt (f e) (fmap f fun)
      ClassStmt e name extends body -> ClassStmt (f e) name extends (map (fmap f) body)
      PrintStmt e expr -> PrintStmt (f e) (fmap f expr)
      ReturnStmt e expr -> ReturnStmt (f e) (fmap (fmap f) expr)
      IfStmt e cond then' else' -> IfStmt (f e) (fmap f cond) (fmap f then') (fmap (fmap f) else')
      WhileStmt e cond body -> WhileStmt (f e) (fmap f cond) (fmap f body)
      ExprStmt e expr -> ExprStmt (f e) (fmap f expr)
      BlockStmt e exprs -> BlockStmt (f e) (map (fmap f) exprs)

instance Pretty e => Pretty (LoxStmt e) where
  pretty (VarStmt _ name v) =
    "var" <+> pretty name <> maybe "" ((" =" <+>) . pretty) v
  pretty (FunctionStmt _ fun) = pretty fun
  pretty (ClassStmt _ name extends _) =
    "class" <+> pretty name <+> maybe "" (("<" <+>) . pretty) extends
  pretty (PrintStmt _ expr) = "print" <+> pretty expr
  pretty (ReturnStmt _ expr) = "return" <+> pretty expr
  pretty (IfStmt _ cond _ _) = "if" <+> pretty cond
  pretty (WhileStmt _ cond _) = "while" <+> pretty cond
  pretty (ExprStmt _ expr) = pretty expr
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
  = LoxString e Text
  | LoxNumber e Double
  | LoxVar e Text
  | LoxCall e (LoxExpr e) [LoxExpr e]
  | LoxGet e (LoxExpr e) Text
  | LoxUnary e LoxUnaryOp (LoxExpr e)
  | LoxBinary e LoxBinaryOp (LoxExpr e) (LoxExpr e)
  | LoxAssign e Text (LoxExpr e)
  | LoxSet e (LoxExpr e) Text (LoxExpr e)
  deriving stock (Eq, Show)

instance Functor LoxExpr where
  fmap f expr =
    case expr of
      LoxString e txt -> LoxString (f e) txt
      LoxNumber e n -> LoxNumber (f e) n
      LoxVar e txt -> LoxVar (f e) txt
      LoxCall e expr' params -> LoxCall (f e) (fmap f expr') (map (fmap f) params)
      LoxGet e expr' name -> LoxGet (f e) (fmap f expr') name
      LoxUnary e op expr' -> LoxUnary (f e) op (fmap f expr')
      LoxBinary e op e1 e2 -> LoxBinary (f e) op (fmap f e1) (fmap f e2)
      LoxAssign e name expr' -> LoxAssign (f e) name (fmap f expr')
      LoxSet e expr' name val -> LoxSet (f e) (fmap f expr') name (fmap f val)

instance Pretty e => Pretty (LoxExpr e) where
  pretty (LoxString _ str) = "\"" <> pretty str <> "\""
  pretty (LoxNumber _ n) = pretty n
  pretty (LoxVar _ n) = pretty n
  pretty (LoxCall _ obj params) = pretty obj <> "(" <> pretty params <> ")"
  pretty (LoxGet _ obj name) = pretty obj <> "." <> pretty name
  pretty (LoxUnary _ op expr) = pretty op <> pretty expr
  pretty (LoxBinary _ op e1 e2) = pretty e1 <+> pretty op <+> pretty e2
  pretty (LoxAssign _ name expr) = pretty name <+> "=" <+> pretty expr
  pretty (LoxSet _ obj name expr) =
    pretty obj <> "." <> pretty name <+> "=" <+> pretty expr

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
