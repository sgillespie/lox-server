module Development.Lox.Server.Pretty
  ( layout,
    render,
    showDoc,
  ) where

import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

layout :: Doc ann -> SimpleDocStream ann
layout = layoutPretty defaultLayoutOptions

render :: SimpleDocStream ann -> Text
render = renderStrict

showDoc :: Doc ann -> Text
showDoc = render . layout
