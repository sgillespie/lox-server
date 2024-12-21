module Development.Lox.Server.Span
  ( spanAtPos,
    SpanResult (..),
  ) where

import Development.Lox.Server.Types
import Language.LSP.Protocol.Types (positionInRange)

data SpanResult
  = ResLoxStmt LocatedLoxStmt
  | ResLoxExpr LocatedLoxExpr
  | ResNothing
  deriving (Eq, Show)

spanAtPos
  :: Position
  -> LocatedLoxProgram
  -> SpanResult
spanAtPos pos (LoxProgram stmts) =
  case spanAtPos' stmts of
    Just (Left stmt) -> ResLoxStmt stmt
    Just (Right expr) -> ResLoxExpr expr
    Nothing -> ResNothing
  where
    spanAtPos' = findMaybe (findSpanStmt stmtInRange exprInRange)
    stmtInRange = positionInRange pos . stmtRange
    exprInRange = positionInRange pos . exprRange

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
  where
    whenF c = if f expr then c else Nothing
    def = Just expr

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f xs = viaNonEmpty head (mapMaybe f xs)
