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

module Finalse.Service where
    import Data.Aeson
    import Data.Text (Text)
    import Finalse.Client
    import Finalse.CreateTransferForm
    import Finalse.Http
    import Finalse.ListForm
    import Finalse.RestCollection
    import Finalse.Sdk (version)
    import Finalse.Transfer
    import qualified Finalse.UpdateTransferForm as Fin.UpdateTransferForm

    createTransfer :: Client -> CreateTransferForm -> IO Transfer
    createTransfer fin form =
        let converter = \result -> decode(result) :: Maybe Transfer
        in post ("/" <> version <> "/transfers") [] [] (Just $ encode form) converter (auth fin)

    deleteTransfer :: Client -> Text -> IO ()
    deleteTransfer fin id =
        let converter = \ignored -> Just ()
        in delete ("/" <> version <> "/transfers/" <> id) [] [] Nothing converter (auth fin)

    getTransfer :: Client -> Text -> IO Transfer
    getTransfer fin id =
        let converter = \result -> decode(result) :: Maybe Transfer
        in get ("/" <> version <> "/transfers/" <> id) [] [] converter (auth fin)

    listTransfer :: Client -> Maybe ListForm -> IO (RestCollection Transfer)
    listTransfer fin maybeForm =
        let converter = \result -> decode(result) :: Maybe (RestCollection Transfer)
        in list ("/" <> version <> "/transfers") [] (toQueryString maybeForm) converter (auth fin)

    updateTransfer :: Client -> Fin.UpdateTransferForm.UpdateTransferForm -> IO Transfer
    updateTransfer fin form =
        let converter = \result -> decode(result) :: Maybe Transfer
            id = Fin.UpdateTransferForm.id form
            body = Fin.UpdateTransferForm.body form
        in patch ("/" <> version <> "/transfers/" <> id) [] [] (Just $ encode body) converter (auth fin)
