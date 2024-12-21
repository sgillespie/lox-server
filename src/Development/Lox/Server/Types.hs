module Development.Lox.Server.Types
  ( LocatedLoxProgram,
    LocatedLoxStmt,
    LocatedLoxFunction,
    LocatedLoxExpr,
    LoxProgram (..),
    LoxStmt (..),
    LoxFunction (..),
    LoxExpr (..),
    LoxError (..),
    LoxParsingError (..),
    LoxUnaryOp (..),
    LoxBinaryOp (..),
    Range (..),
    Position (..),
    stmtRange,
    exprRange,
    fromParseErrorBundle,
    parsingErrorRange,
    isLoxError,
    isLoxParsingError,
    isLoxFileNotFound,
  ) where

import Language.LSP.Protocol.Types (Position (..), Range (..), UInt)
import Text.Megaparsec qualified as Parsec

type LocatedLoxProgram = LoxProgram Range
type LocatedLoxStmt = LoxStmt Range
type LocatedLoxFunction = LoxFunction Range
type LocatedLoxExpr = LoxExpr Range

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

data LoxFunction e = LoxFunction
  { functionName :: Text,
    functionParams :: [Text],
    functionBody :: [LoxStmt e]
  }
  deriving stock (Eq, Show)

instance Functor LoxFunction where
  fmap f fun@(LoxFunction{functionBody}) =
    fun{functionBody = map (fmap f) functionBody}

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

stmtRange :: LocatedLoxStmt -> Range
stmtRange (VarStmt r _ _) = r
stmtRange (FunctionStmt r _) = r
stmtRange (ClassStmt r _ _ _) = r
stmtRange (PrintStmt r _) = r
stmtRange (ReturnStmt r _) = r
stmtRange (IfStmt r _ _ _) = r
stmtRange (WhileStmt r _ _) = r
stmtRange (ExprStmt r _) = r
stmtRange (BlockStmt r _) = r

exprRange :: LocatedLoxExpr -> Range
exprRange (LoxString r _) = r
exprRange (LoxNumber r _) = r
exprRange (LoxVar r _) = r
exprRange (LoxCall r _ _) = r
exprRange (LoxGet r _ _) = r
exprRange (LoxUnary r _ _) = r
exprRange (LoxBinary r _ _ _) = r
exprRange (LoxAssign r _ _) = r
exprRange (LoxSet r _ _ _) = r

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
