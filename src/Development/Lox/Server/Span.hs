module Development.Lox.Server.Span
  ( LocatedLoxProgram,
    LocatedLoxStmt,
    LocatedLoxFunction,
    LocatedLoxExpr,
    Located (..),
    HasLocated (..),
    SpanResult (..),
    LSP.Position (..),
    mkLocated,
    spanAtPos,
  ) where

import Development.Lox.Server.Types
import Language.LSP.Protocol.Types qualified as LSP
import Prettyprinter (Pretty (..))

type LocatedLoxProgram = LoxProgram Located
type LocatedLoxStmt = LoxStmt Located
type LocatedLoxFunction = LoxFunction Located
type LocatedLoxExpr = LoxExpr Located

newtype Located = Located {unLocated :: LSP.Range}
  deriving stock (Eq, Show)

instance Pretty Located where
  pretty (Located (LSP.Range p1 p2)) =
    prettyPos p1 <> "-" <> prettyPos p2
    where
      prettyPos (LSP.Position l c) = pretty l <> ":" <> pretty c

mkLocated :: LSP.Position -> LSP.Position -> Located
mkLocated p1 p2 = Located (LSP.Range p1 p2)

class HasLocated a where
  loc :: a -> Located

instance HasLocated LocatedLoxStmt where
  loc (VarStmt r _ _) = r
  loc (FunctionStmt r _) = r
  loc (ClassStmt r _ _ _) = r
  loc (PrintStmt r _) = r
  loc (ReturnStmt r _) = r
  loc (IfStmt r _ _ _) = r
  loc (WhileStmt r _ _) = r
  loc (ExprStmt r _) = r
  loc (BlockStmt r _) = r

instance HasLocated LocatedLoxExpr where
  loc (LoxString r _) = r
  loc (LoxNumber r _) = r
  loc (LoxVar r _) = r
  loc (LoxCall r _ _) = r
  loc (LoxGet r _ _) = r
  loc (LoxUnary r _ _) = r
  loc (LoxBinary r _ _ _) = r
  loc (LoxAssign r _ _) = r
  loc (LoxSet r _ _ _) = r
  loc (LoxSuper r _) = r

data SpanResult
  = ResLoxStmt LocatedLoxStmt
  | ResLoxExpr LocatedLoxExpr
  | ResNothing
  deriving (Eq, Show)

spanAtPos
  :: LSP.Position
  -> LocatedLoxProgram
  -> SpanResult
spanAtPos pos (LoxProgram stmts) =
  case spanAtPos' stmts of
    Just (Left stmt) -> ResLoxStmt stmt
    Just (Right expr) -> ResLoxExpr expr
    Nothing -> ResNothing
  where
    spanAtPos' = findMaybe (findSpanStmt inRange inRange)

    inRange :: HasLocated a => a -> Bool
    inRange = LSP.positionInRange pos . unLocated . loc

findSpanStmt
  :: (LoxStmt e -> Bool)
  -> (LoxExpr e -> Bool)
  -> LoxStmt e
  -> Maybe (Either (LoxStmt e) (LoxExpr e))
findSpanStmt fs fe stmt =
  whenFs $
    case stmt of
      VarStmt _ _ (Just expr) -> foldExpr' expr <|> def
      VarStmt _ _ Nothing -> def
      FunctionStmt _ fun -> foldFunction' fun <|> def
      ClassStmt _ _ _ funcs ->
        (Left <$> findMaybe (findSpanFunction fs) funcs) <|> def
      PrintStmt _ expr -> foldExpr' expr <|> def
      ReturnStmt _ (Just expr) -> foldExpr' expr <|> def
      ReturnStmt _ Nothing -> def
      IfStmt _ cond then' else' ->
        foldExpr' cond
          <|> findSpanStmt fs fe then'
          <|> (findSpanStmt fs fe =<< else')
          <|> def
      WhileStmt _ cond body ->
        foldExpr' cond
          <|> findSpanStmt fs fe body
          <|> def
      ExprStmt _ expr -> foldExpr' expr <|> def
      BlockStmt _ stmts -> findMaybe (findSpanStmt fs fe) stmts <|> def
  where
    foldExpr' expr = fmap Right (findSpanExpr fe expr)
    foldFunction' fun = fmap Left (findSpanFunction fs fun)
    def = Just (Left stmt)
    whenFs f = if fs stmt then f else Nothing

findSpanFunction
  :: (LoxStmt e -> Bool)
  -> LoxFunction e
  -> Maybe (LoxStmt e)
findSpanFunction f LoxFunction{functionBody} =
  find f functionBody

findSpanExpr
  :: (LoxExpr e -> Bool)
  -> LoxExpr e
  -> Maybe (LoxExpr e)
findSpanExpr f expr =
  whenF $
    case expr of
      LoxString _ _ -> def
      LoxNumber _ _ -> def
      LoxVar _ _ -> def
      LoxCall _ expr' params -> findSpanExpr f expr' <|> find f params <|> def
      LoxGet _ obj _ -> findSpanExpr f obj <|> def
      LoxUnary _ _ expr' -> findSpanExpr f expr' <|> def
      LoxBinary _ _ e1 e2 -> findSpanExpr f e1 <|> findSpanExpr f e2 <|> def
      LoxAssign _ _ val -> findSpanExpr f val <|> def
      LoxSet _ obj _ val -> findSpanExpr f obj <|> findSpanExpr f val <|> def
      LoxSuper _ _ -> def
  where
    whenF c = if f expr then c else Nothing
    def = Just expr

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f xs = viaNonEmpty head (mapMaybe f xs)
