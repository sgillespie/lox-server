{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Development.Lox.Server
  ( runServer,
  ) where

import Development.Lox.Server.Parser (parseLox)

import Development.Lox.Server.Types
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as Lens
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lens.Micro

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
    [ LSP.notificationHandler SMethod_TextDocumentDidOpen handleDiagnostics,
      LSP.notificationHandler SMethod_TextDocumentDidChange handleDiagnostics
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
  where
    getVirtualFile uri' = do
      res <- LSP.getVirtualFile (LSP.toNormalizedUri uri')
      pure (maybeToRight LoxFileNotFound res)

    parseLoxSource uri' = runExceptT $ do
      let filePath = LSP.uriToFilePath uri'

      f <- ExceptT $ getVirtualFile uri'
      hoistEither $ parseLox filePath (VFS.virtualFileText f)

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
