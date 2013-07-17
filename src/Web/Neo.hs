{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Web.Neo (
    ) where

import Web.Rest (
    RestT,rest,
    runRestT,Hostname,Port,RestError,
    Request(Request),Method(POST),ContentType,
    Response(code,responseType,responseBody),
    ResponseCode)

import Control.Error (EitherT,runEitherT,left,tryRead)

import Data.Aeson (
    ToJSON,object,(.=),encode,
    FromJSON(parseJSON),withObject,withText,(.:),eitherDecode)

import Control.Monad (mzero)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,append,pack,unpack)
import qualified  Data.Text as Text (takeWhile,reverse)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict,fromStrict)

data NeoError = CreationResponseCodeError ResponseCode ByteString
              | CreationResponseTypeError (Maybe ContentType) ByteString
              | CreationResponseParseError String
              | CreationReadURIError Text

deriving instance Show NeoError

type NeoT m a = EitherT NeoError (RestT m) a

runNeoT :: (MonadIO m) => Hostname -> Port -> NeoT m a -> m (Either RestError (Either NeoError a))
runNeoT hostname port = runRestT hostname port . runEitherT

defaultRunNeoT :: (MonadIO m) => NeoT m a -> m (Either RestError (Either NeoError a))
defaultRunNeoT = runNeoT "localhost" 7474

data Node = Node Integer

deriving instance Show Node

data Edge = Edge Integer

deriving instance Show Edge

data CreationResponse = CreationResponse Text

instance FromJSON CreationResponse where
    parseJSON = withObject "CreationResponse" (\creationResponseObject -> do
        creationResponseObject .: "self" >>= withText "SelfURI" (\selfUriText -> do
            return (CreationResponse selfUriText)))

type Label = Text

create :: (Monad m) => Request -> NeoT m CreationResponse
create request = do
    response <- lift (rest request)
    assert (code response == (2,0,1))
        (CreationResponseCodeError (code response) (responseBody response))
    assert (responseType response == Just jsoncontent)
        (CreationResponseTypeError (responseType response) (responseBody response))
    either (left . CreationResponseParseError) return (strictEitherDecode (responseBody response))

extractId :: (Monad m) => Text -> EitherT NeoError m Integer
extractId uri = do

    let lastURIsegment = Text.reverse (Text.takeWhile (/= '/') (Text.reverse uri))

    tryRead (CreationReadURIError lastURIsegment) (unpack lastURIsegment)


newNode :: (Monad m) => NeoT m Node
newNode = do

    let newNodeRequest = Request POST "/db/data/node" jsoncontent jsoncontent ""

    CreationResponse selfUri <- create newNodeRequest

    nodeId <- extractId selfUri

    return (Node nodeId)

nodeURI :: Node -> Text
nodeURI (Node nodeid) = "/db/data/node/" `append` (pack (show nodeid))

newEdge :: (Monad m) => Label -> Node -> Node -> NeoT m Edge
newEdge label sourcenode targetnode = do

    let sourceuri = (nodeURI sourcenode `append` "/relationships")
        payload   = object [
            "to"   .= nodeURI targetnode,
            "type" .= label]
        newEdgeRequest = Request POST sourceuri jsoncontent jsoncontent (strictEncode payload)

    CreationResponse selfUri <- create newEdgeRequest

    edgeId <- extractId selfUri

    return (Edge edgeId)

assert :: (Monad m) => Bool -> NeoError -> NeoT m ()
assert True  _        = return ()
assert False neoerror = left neoerror

jsoncontent :: Text
jsoncontent = "application/json"

strictEncode :: (ToJSON a) => a -> ByteString
strictEncode = BL.toStrict . encode

strictEitherDecode :: (FromJSON a) => ByteString -> Either String a
strictEitherDecode = eitherDecode . BL.fromStrict
