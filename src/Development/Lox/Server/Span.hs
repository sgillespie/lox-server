module Development.Lox.Server.Span
  ( LocatedLoxProgram,
    LocatedLoxStmt,
    LocatedLoxFunction,
    LocatedLoxExpr,
    Located (..),
    HasLocated (..),
    SpanResult (..),
    LSP.Position (..),
    LSP.UInt,
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
  loc (VarStmt _ _ r) = r
  loc (FunctionStmt _ r) = r
  loc (ClassStmt _ _ _ r) = r
  loc (PrintStmt _ r) = r
  loc (ReturnStmt _ r) = r
  loc (IfStmt _ _ _ r) = r
  loc (WhileStmt _ _ r) = r
  loc (ExprStmt _ r) = r
  loc (BlockStmt _ r) = r

instance HasLocated LocatedLoxExpr where
  loc (LoxString _ r) = r
  loc (LoxNumber _ r) = r
  loc (LoxVar _ r) = r
  loc (LoxCall _ _ r) = r
  loc (LoxGet _ _ r) = r
  loc (LoxUnary _ _ r) = r
  loc (LoxBinary _ _ _ r) = r
  loc (LoxAssign _ _ r) = r
  loc (LoxSet _ _ _ r) = r
  loc (LoxSuper _ r) = r

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
      VarStmt _ (Just expr) _ -> foldExpr' expr <|> def
      VarStmt _ Nothing _ -> def
      FunctionStmt fun _ -> foldFunction' fun <|> def
      ClassStmt _ _ funcs _ ->
        (Left <$> findMaybe (findSpanFunction fs) funcs) <|> def
      PrintStmt expr _ -> foldExpr' expr <|> def
      ReturnStmt (Just expr) _ -> foldExpr' expr <|> def
      ReturnStmt Nothing _ -> def
      IfStmt cond then' else' _ ->
        foldExpr' cond
          <|> findSpanStmt fs fe then'
          <|> (findSpanStmt fs fe =<< else')
          <|> def
      WhileStmt cond body _ ->
        foldExpr' cond
          <|> findSpanStmt fs fe body
          <|> def
      ExprStmt expr _ -> foldExpr' expr <|> def
      BlockStmt stmts _ -> findMaybe (findSpanStmt fs fe) stmts <|> def
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
      LoxCall expr' params _ -> findSpanExpr f expr' <|> find f params <|> def
      LoxGet obj _ _ -> findSpanExpr f obj <|> def
      LoxUnary _ expr' _ -> findSpanExpr f expr' <|> def
      LoxBinary _ e1 e2 _ -> findSpanExpr f e1 <|> findSpanExpr f e2 <|> def
      LoxAssign _ val _ -> findSpanExpr f val <|> def
      LoxSet obj _ val _ -> findSpanExpr f obj <|> findSpanExpr f val <|> def
      LoxSuper _ _ -> def
  where
    whenF c = if f expr then c else Nothing
    def = Just expr

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f xs = viaNonEmpty head (mapMaybe f xs)
