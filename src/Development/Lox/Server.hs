{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Development.Lox.Server
  ( runServer,
  ) where

import Development.Lox.Server.Parser (parseLox)
import Development.Lox.Server.Pretty
import Development.Lox.Server.Span qualified as Span
import Development.Lox.Server.Types
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as Lens
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lens.Micro
import Prettyprinter

runServer :: IO ()
runServer =
  void $
    LSP.runServer $
      LSP.ServerDefinition
        { LSP.defaultConfig = (),
          LSP.configSection = "lox",
          LSP.parseConfig = const . const $ Right (),
          LSP.onConfigChange = const $ pure (),
          LSP.doInitialize = const . pure . Right,
          LSP.staticHandlers = const handlers,
          LSP.interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO,
          LSP.options = lspOptions
        }

handlers :: LSP.Handlers (LSP.LspM ())
handlers =
  mconcat
    [ LSP.notificationHandler Message.SMethod_TextDocumentDidOpen handleDiagnostics,
      LSP.notificationHandler Message.SMethod_TextDocumentDidChange handleDiagnostics,
      LSP.requestHandler Message.SMethod_TextDocumentHover handleHover
    ]

handleDiagnostics
  :: ( Lens.HasParams msg params,
       Lens.HasTextDocument params doc,
       Lens.HasUri doc LSP.Uri,
       Lens.HasVersion doc Int32
     )
  => msg
  -> LSP.LspM c ()
handleDiagnostics msg = do
  let doc = msg ^. Lens.params . Lens.textDocument
      uri' = doc ^. Lens.uri
      version' = doc ^. Lens.version

  parseResult <- parseLoxSource uri'

  let diagnostics =
        either
          mkErrDiagnostics
          (const [])
          parseResult

  LSP.publishDiagnostics
    100
    (LSP.toNormalizedUri uri')
    (Just version')
    (partitionBySource diagnostics)

parseLoxSource
  :: LSP.Uri
  -> LSP.LspM c (Either LoxError Span.LocatedLoxProgram)
parseLoxSource uri' = runExceptT $ do
  let filePath = LSP.uriToFilePath uri'
  virtualFile <- ExceptT getVirtualFile

  hoistEither $
    parseLox filePath (VFS.virtualFileText virtualFile)
  where
    getVirtualFile = do
      res <- LSP.getVirtualFile (LSP.toNormalizedUri uri')
      pure (maybeToRight LoxFileNotFound res)

mkErrDiagnostics :: LoxError -> [LSP.Diagnostic]
mkErrDiagnostics = \case
  LoxParsingErrors errs ->
    map
      ( \(LoxParsingError range msg) ->
          mkDiagnostic range msg LSP.DiagnosticSeverity_Error
      )
      errs
  LoxFileNotFound ->
    [mkDiagnostic defaultRange "File not found" LSP.DiagnosticSeverity_Error]
  where
    defaultRange = LSP.Range (LSP.Position 0 0) (LSP.Position maxBound maxBound)

mkDiagnostic :: LSP.Range -> Text -> LSP.DiagnosticSeverity -> LSP.Diagnostic
mkDiagnostic range message severity =
  LSP.Diagnostic
    { LSP._range = range,
      LSP._severity = Just severity,
      LSP._code = Nothing,
      LSP._codeDescription = Nothing,
      LSP._source = Just "lox",
      LSP._message = message,
      LSP._tags = Nothing,
      LSP._relatedInformation = Nothing,
      LSP._data_ = Nothing
    }

type Response m =
  Either (Message.TResponseError m) (Message.MessageResult m)

handleHover
  :: Message.TRequestMessage 'Message.Method_TextDocumentHover
  -> (Response 'Message.Method_TextDocumentHover -> LSP.LspM c ())
  -> LSP.LspM c ()
handleHover req responder = do
  let (Message.TRequestMessage _ _ _ (LSP.HoverParams doc pos _)) = req
      uri = doc ^. Lens.uri

  parseResult <- parseLoxSource uri

  case parseResult of
    Left err -> responder . Right . LSP.InL $ hoverParseFailure pos err
    Right parsed -> responder . Right . LSP.InL $ hoverParseSuccess pos parsed

hoverParseFailure :: Span.Position -> LoxError -> LSP.Hover
hoverParseFailure _ err =
  LSP.Hover (LSP.InL (LSP.mkMarkdown (show err))) Nothing

hoverParseSuccess :: Span.Position -> Span.LocatedLoxProgram -> LSP.Hover
hoverParseSuccess pos lox =
  LSP.Hover (LSP.InL (LSP.mkMarkdown searchRes)) Nothing
  where
    suffix span' = "(" <> pretty (Span.loc span') <> ")"
    searchRes =
      case Span.spanAtPos pos lox of
        Span.ResNothing -> mempty
        Span.ResLoxStmt stmt ->
          showDoc $ "stmt: " <> pretty stmt <+> suffix stmt
        Span.ResLoxExpr expr ->
          showDoc $ "expr: " <> pretty expr <+> suffix expr

lspOptions :: LSP.Options
lspOptions =
  LSP.defaultOptions
    { LSP.optTextDocumentSync = Just docSyncOptions
    }

docSyncOptions :: LSP.TextDocumentSyncOptions
docSyncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Nothing,
      LSP._change = Just LSP.TextDocumentSyncKind_Full,
      LSP._willSave = Nothing,
      LSP._willSaveWaitUntil = Nothing,
      LSP._save = Nothing
    }
