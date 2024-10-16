{-# LANGUAGE ExplicitNamespaces #-}

module Development.Lox.Server
  ( runServer,
  ) where

import Development.Lox.Server.Parser (parseLoxFile)

import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as Lens
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
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
      -- TODO: Use VFS
      file = LSP.uriToFilePath uri'

  diagnostics <-
    case file of
      Just file' -> do
        loxModule <- liftIO (parseLoxFile file')
        pure [mkDiagnostic (show loxModule)]
      Nothing -> pure []

  LSP.publishDiagnostics
    100
    (LSP.toNormalizedUri uri')
    (Just version')
    (partitionBySource diagnostics)

mkDiagnostic :: Text -> LSP.Diagnostic
mkDiagnostic message =
  LSP.Diagnostic
    { LSP._range = LSP.Range (LSP.Position 0 1) (LSP.Position 0 5),
      LSP._severity = Just LSP.DiagnosticSeverity_Warning,
      LSP._code = Nothing,
      LSP._codeDescription = Nothing,
      LSP._source = Just "sean",
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
