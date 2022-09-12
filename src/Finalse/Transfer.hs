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

module Finalse.Transfer where
    import Data.Aeson
    import Data.Text (Text)
    import Finalse.TransferStatus
    import GHC.Generics

    data Transfer = Transfer { id :: Text, 
                               url :: Text, 
                               _type :: Text, 
                               status :: TransferStatus, 
                               isElastic :: Bool, 
                               description :: Maybe Text, 
                               foreignData :: Maybe Text }  deriving (Generic, Show, Eq)
    instance ToJSON Transfer where
        toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }
    instance FromJSON Transfer