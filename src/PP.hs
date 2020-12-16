module PP where

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

class PP a where
  pp :: a -> Doc

oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

indented :: PP a => a -> String
indented = PP.render . pp
