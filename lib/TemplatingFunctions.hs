{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Map (Map, assocs)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Dec (DataD))
import qualified Types as RT (NewResource(..), ResourceClass, Resource(..))
import qualified Types as TT (NewType(..), TypeClass, Type(..))

emptyBang :: TH.Bang
emptyBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

propFields :: (String, String) -> (TH.Name, TH.Bang, TH.Type)
propFields (nm, tp) = (TH.mkName nm, emptyBang, TH.ConT (TH.mkName tp))

instantiateClass :: TH.Type -> TH.Name -> Dec
instantiateClass ct tn = TH.InstanceD Nothing [] (f ct) []
  where
    f classType = TH.AppT classType $ TH.ConT tn

declareType :: TT.NewType -> Dec
declareType t =
  let fieldsList = assocs $ TT.fields t
      con = TH.RecC tName $ map propFields fieldsList
      tName = TH.mkName $ TT.name t
   in DataD [] tName [] Nothing [con] []

instantiateType :: TH.Name -> Dec
instantiateType = instantiateClass typeClass

typeClass :: TH.Type
typeClass = TH.ConT ''TT.TypeClass

declareResource :: RT.NewResource -> Dec
declareResource r =
  let fieldsList = assocs $ RT.properties r
      con = TH.RecC tName $ map propFields fieldsList
      tName = TH.mkName $ RT.resType r
   in DataD [] tName [] Nothing [con] []

instantiateResource :: TH.Name -> Dec
instantiateResource = instantiateClass resClass

resClass :: TH.Type
resClass = TH.ConT ''RT.ResourceClass
