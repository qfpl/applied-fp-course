module FirstApp.Responses where

import           Network.Wai                (Response, responseLBS)

import           Network.HTTP.Types         (Status, hContentType, status200,
                                             status400, status404, status500)

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Aeson                 (ToJSON)
import qualified Data.Aeson                 as A
import           FirstApp.Types             (ContentType (JSON, PlainText),
                                             renderContentType)

mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct msg =
  responseLBS sts [(hContentType, renderContentType ct)] msg

resp200
  :: LBS.ByteString
  -> Response
resp200 =
  mkResponse status200 PlainText

resp404
  :: LBS.ByteString
  -> Response
resp404 =
  mkResponse status404 PlainText

resp400
  :: LBS.ByteString
  -> Response
resp400 =
  mkResponse status400 PlainText

-- Some new helpers for different statuses and content types
resp500
  :: LBS.ByteString
  -> Response
resp500 =
  mkResponse status500 PlainText

resp200Json
  :: ToJSON a
  => a
  -> Response
resp200Json =
  mkResponse status200 JSON . A.encode
