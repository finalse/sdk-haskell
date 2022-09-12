{-# LANGUAGE OverloadedStrings #-}

module Finalse.HttpRequest where
   import qualified Data.Text as T
   import Data.Time.Clock.POSIX

   data HttpRequest = HttpRequest { nonce :: Int,
                                    signedHeaders :: [(T.Text, T.Text)],
                                    method :: T.Text,
                                    path :: T.Text,
                                    queryString :: [(T.Text, T.Text)],
                                    bodyHash :: Maybe T.Text } deriving (Show)

   createHttpRequest :: [(T.Text, T.Text)] -> T.Text -> T.Text -> [(T.Text, T.Text)] -> Maybe T.Text -> IO HttpRequest
   createHttpRequest signedHeaders method path queryString bodyHash = do
       nonce <- round `fmap` getPOSIXTime
       return HttpRequest {  nonce= nonce,
                             signedHeaders= signedHeaders,
                             method= method,
                             path= path,
                             queryString= queryString,
                             bodyHash= bodyHash }

   toMessage :: HttpRequest -> T.Text
   toMessage (HttpRequest nonce signedHeaders method path queryString bodyHash) =
       nonceStringified <> signedHeadersStringified signedHeaders  <> method <> path <> queryStringStringified queryString <> bodyHashStringified bodyHash
       where
       nonceStringified = T.pack (show nonce)
       signedHeadersStringified sh  = T.intercalate "" $ fmap (\h -> fst h <>  T.pack ":" <> snd h) sh
       queryStringStringified []    = T.pack ""
       queryStringStringified qs    = T.pack "?" <> (T.intercalate "&" $ fmap (\q -> fst q <> T.pack "=" <> snd q) qs)
       bodyHashStringified (Just b) = b
       bodyHashStringified Nothing  = ""


