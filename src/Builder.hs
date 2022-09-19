{-# LANGUAGE OverloadedStrings #-}

module Builder where


import qualified Crypto.Hash.SHA256
import           Data.ByteString.Builder    (toLazyByteString)
import qualified Data.ByteString.Char8      as BC
import           Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Types.Header
import           Network.Wreq

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type BaseURL = String

kucoin :: BaseURL
kucoin = "https://api.kucoin.com"


apiTimeStamp :: POSIXTime -> IO BC.ByteString
apiTimeStamp = return . BC.pack . take 13  . show . (1000 *)

apiPassphrase :: String -> BC.ByteString
apiPassphrase = U.encodeBase64' $ H.hmac (BC.pack C.apiSecret) BC.pack

apiKeyVersion :: String -> BC.ByteString
apiKeyVersion = BC.pack


apiSign :: String -> BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString-> IO BC.ByteString
apiSign apiSecret method endpoint body time = do
  let message = mconcat [time,method,endpoint,body]
  let hash = H.hmac (BC.pack apiSecret) message
  (return . encodeUtf8 . U.encodeBase64) hash


headerBuilder :: Method -> Endpoint -> Maybe Body -> IO [Header]
headerBuilder method endpoint body = do
  timeStamp <- apiTimeStamp =<< TI.getPOSIXTime
  let authenticationHeaders = zip ["KC-API-KEY","KC-API-PASSPHRASE", "KC-API-TIMESTAMP", "KC-API-KEY-VERSION"]
                                  [BC.pack C.apiKey, apiPassphrase, timeStamp, BC.pack C.apiVersion]
  case body of
    Just b -> do
      apiSignValue <- apiSign method endpoint (BL.toStrict . BL.toLazyByteString . A.fromEncoding $ b) timeStamp
      return $ [("Content-Type", "application/json"), ("KC-API-SIGN", apiSignValue)] <> authenticationHeaders
    Nothing -> do
      apiSignValue <- apiSign method endpoint "" timeStamp
      return $ cons ("KC-API-SIGN", apiSignValue) authenticationHeaders



getBuilder ::  Endpoint -> Params -> IO (Response BU.ByteString)
getBuilder endpoint parameters = do
  let paramString = genParamText endpoint parameters
  compHeaders <- headerBuilder "GET" paramString Nothing
  let opt = headers .~ compHeaders $ params .~ parameters $ defaults
  getWith opt (kucoin ++ BC.unpack endpoint)



postBuilder :: Endpoint -> Params -> Body -> IO (Response BU.ByteString)
postBuilder endpoint parameters body = do
  let paramString = genParamText endpoint parameters
  compHeaders <- headerBuilder "POST" paramString (Just body)
  let opt = headers .~ compHeaders $ params .~ parameters $ defaults
  postWith opt (kucoin ++ BC.unpack endpoint) body
