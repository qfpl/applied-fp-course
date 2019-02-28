module Level07.Responses where

import           Network.Wai                (Response, responseLBS)

import           Network.HTTP.Types         (Status, hContentType, status200,
                                             status400, status404, status500)

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text.Lazy.Encoding    (encodeUtf8)

import           Waargonaut.Encode          (Encoder')
import qualified Waargonaut.Encode          as E

import           Level07.Types              (ContentType (JSON),
                                             renderContentType)

mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct msg =
  responseLBS sts [(hContentType, renderContentType ct)] msg

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: Encoder' a
  -> a
  -> Response
resp200Json e =
  resp200 JSON . encodeUtf8 .
  E.simplePureEncodeTextNoSpaces e
