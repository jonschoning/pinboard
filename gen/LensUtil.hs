module LensUtil where

import Pinboard.ApiTypes
import Prelude
import Control.Lens
import Language.Haskell.TH

makeLensesCustom :: Name -> DecsQ
makeLensesCustom = makeLensesWith $ lensRules
  & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "L")]
