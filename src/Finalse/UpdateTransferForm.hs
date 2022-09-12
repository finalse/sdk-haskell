{-
   Copyright © 2020 Finalse Cloud

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       https://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Finalse.UpdateTransferForm where
    import Data.Aeson
    import Data.List.NonEmpty
    import Data.Text (Text)
    import Finalse.Transfer hiding (id) 
    import GHC.Generics
    import Prelude hiding (id)
    import qualified Data.HashMap.Strict as HMap

    data Edit = SetIsElasticField Bool 
              | SetDescriptionField Text
              | SetForeignDataField Text deriving (Generic, Show, Eq)
    instance ToJSON Edit where
        toJSON (SetIsElasticField newValue) = object ["isElastic" .= newValue]
        toJSON (SetDescriptionField newValue) = object ["description" .= newValue]
        toJSON (SetForeignDataField newValue) = object ["foreignData" .= newValue]


    data RemovableField = DescriptionField 
                        | ForeignDataField deriving (Generic, Show, Eq)
    instance ToJSON RemovableField where
        toJSON DescriptionField = "description" 
        toJSON ForeignDataField = "foreignData" 

    data UpdateTransferForm = EditFieldsForm   { id :: Text, edit :: NonEmpty Edit } 
                            | RemoveFieldsForm { id :: Text, fields :: NonEmpty RemovableField }
                            | UpdateTransferForm { id :: Text, edit :: NonEmpty Edit, remove :: NonEmpty RemovableField }
                             deriving (Generic, Show, Eq)

    makeForm :: Text -> [Edit] -> [RemovableField] -> Maybe UpdateTransferForm
    makeForm id []     []     = Nothing
    makeForm id (e:es) []     = Just EditFieldsForm{id, edit = e :| es}
    makeForm id []     (f:fs) = Just RemoveFieldsForm{id, fields = f :| fs}
    makeForm id (e:es) (f:fs) = Just UpdateTransferForm{id, edit = e :| es, remove = f :| fs}

    body :: UpdateTransferForm -> Object
    body form =
       case form of
           EditFieldsForm   {id = _, edit} -> (editToJson edit)
           RemoveFieldsForm {id = _, fields} -> (removeToJson fields)
           UpdateTransferForm {id = _, edit, remove} -> (editToJson edit) `HMap.union` (removeToJson remove)
       where
           editToJson list   = HMap.singleton "set" $ toJSON list
           removeToJson list = HMap.singleton "remove" $ toJSON list
