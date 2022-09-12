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


{-# LANGUAGE OverloadedStrings #-}

module Main where

import Finalse.Auth
import Finalse.Client
import Finalse.Service
import Finalse.CreateTransferForm
import Finalse.Http
import Data.Aeson (encode, decode)


main :: IO ()
main =
    let auth = Auth{token = "1234", secretKey = "****"}
        client = Client{auth = auth}
        form = CreateTransferForm { isElastic = True, description = Nothing, foreignData = Nothing }
    in do
    --domain <- client `createDomain` form
    -- putStrLn (show domain)
    putStrLn (show client)

