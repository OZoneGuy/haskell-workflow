{-# LANGUAGE TemplateHaskell #-}

import Data.Map (Map, assocs)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Dec (DataD))
import Types (NewResource (properties, resType), NewType (fields, name))

emptyBang :: Bang
emptyBang = Bang NoSourceUnpackedness NoSourceStrictness

declareType :: NewType -> Dec
declareType t =
  let recField :: (String, String) -> (Name, Bang, Type)
      recField (nm, tp) = (mkName nm, emptyBang, ConT (mkName tp))
      fieldsList = assocs $ fields t
      con = RecC tName $ map recField fieldsList
      tName = mkName $ name t
   in DataD [] tName [] Nothing [con] []

declareResource :: NewResource -> Dec
declareResource r =
  let recField :: (String, String) -> (Name, Bang, Type)
      recField (nm, tp) = (mkName nm, emptyBang, ConT (mkName tp))
      fieldsList = assocs $ properties r
      con = RecC tName $ map recField fieldsList
      tName = mkName $ resType r
   in DataD [] tName [] Nothing [con] []
