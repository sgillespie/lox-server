module Development.Lox.Server
  ( runServer,
  ) where

import Language.LSP.Server qualified as LSP

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
          LSP.options = LSP.defaultOptions
        }

handlers :: LSP.Handlers (LSP.LspM ())
handlers = mconcat []
