{-# LANGUAGE OverloadedStrings #-}

import           Network.Wai
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)
import           Data.Monoid              ((<>))

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import qualified Conf
import           Types

main :: IO ()
main = do
  -- Load up the configuration by providing a FilePath for the JSON config file.
  cfgE <- Conf.parseOptions "appconfig.json"
  -- Loading the configuration can fail, so we have to take that into account now.
  case cfgE of
    Left err  -> print err
    Right cfg -> run ( Conf.getPort $ Conf.port cfg ) ( app cfg )

-- | Just some helper functions to make our lives a little more DRY.
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
-- |

-- Now that we have our configuration, pass it where it needs to go.
app
  :: Conf.Conf
  -> Application
app cfg rq cb = mkRequest rq
  >>= fmap handleRespErr . handleRErr
  >>= cb
  where
    -- Does this seem clunky to you?
    handleRespErr =
      either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr =
      either ( pure . Left ) ( handleRequest cfg )

-- Now we have some config, we can pull our configured helloMsg off it and use it
-- in the response.
handleRequest
  :: Conf.Conf
  -> RqType
  -> IO (Either Error Response)
handleRequest cfg (AddRq _ _) =
  pure . Right . resp200
    . ( "App says: " <> )
    . Conf.getHelloMsg
    $ Conf.helloMsg cfg
handleRequest _ (ViewRq _) =
  pure . Right $ resp200 "Susan was ere"
handleRequest _ ListRq =
  pure . Right $ resp200 "[ \"Fred wuz ere\", \"Susan was ere\" ]"

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure mkUnknownRouteErr

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 $ LBS.toStrict c)

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkUnknownRouteErr
  :: Either Error RqType
mkUnknownRouteErr =
  Left UnknownRoute

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 "Empty Topic"

