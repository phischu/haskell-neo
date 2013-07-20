{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Web.Neo (
    ) where

import Web.Rest (
    RestT,rest,
    runRestT,Hostname,Port,RestError,
    Request(Request),Method(POST,PUT,GET),Location,ContentType,Body,
    Response(code,responseType,responseBody),
    ResponseCode)

import Control.Error (EitherT,runEitherT,left,tryRead)

import Data.Aeson (
    Value,Object,
    ToJSON,object,(.=),encode,
    FromJSON(parseJSON),
    withObject,withText,withArray,
    (.:),eitherDecode)
import Data.Aeson.Types (Parser)

import Control.Monad (mzero,(>=>))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,append,pack,unpack)
import qualified  Data.Text as Text (takeWhile,reverse)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict,fromStrict)
import qualified Data.Vector as Vector (toList)

data NeoError = CreationResponseCodeError ResponseCode Body
              | CreationResponseTypeError (Maybe ContentType) Body
              | CreationResponseParseError String
              | ExtractIdError Text
              | EmptyResponseCodeError ResponseCode Body
              | EdgesResponseCodeError ResponseCode Body
              | EdgesResponseTypeError (Maybe ContentType) Body
              | EdgesResponseParseError String
              | EdgeInfoResponseCodeError ResponseCode Body
              | EdgeInfoResponseParseError String

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
    parseJSON = extractSelfURI >=> return . CreationResponse

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

    tryRead (ExtractIdError lastURIsegment) (unpack lastURIsegment)

extractTextField :: Text -> Value -> Parser Text
extractTextField key = withObject "ResponseObject" (\o -> o .: key >>= withText "TextField" return)

extractSelfURI :: Value -> Parser Text
extractSelfURI = extractTextField "self"

jsonRequest :: Method -> Location -> Body -> Request
jsonRequest method location body = Request method location jsoncontent jsoncontent body

jsonGetRequest :: Location -> Request
jsonGetRequest location = jsonRequest GET location ""

newNode :: (Monad m) => NeoT m Node
newNode = do

    let newNodeRequest = jsonRequest POST "/db/data/node" ""

    CreationResponse selfUri <- create newNodeRequest

    nodeId <- extractId selfUri

    return (Node nodeId)

nodeURI :: Node -> Location
nodeURI (Node nodeid) = "/db/data/node/" `append` (pack (show nodeid))

edgeURI :: Edge -> Location
edgeURI (Edge edgeid) = "/db/data/relationship/" `append` (pack (show edgeid))

newEdge :: (Monad m) => Label -> Node -> Node -> NeoT m Edge
newEdge label sourcenode targetnode = do

    let sourceuri = (nodeURI sourcenode `append` "/relationships")
        payload   = object [
            "to"   .= nodeURI targetnode,
            "type" .= label]
        newEdgeRequest = jsonRequest POST sourceuri (strictEncode payload)

    CreationResponse selfUri <- create newEdgeRequest

    edgeId <- extractId selfUri

    return (Edge edgeId)

setProperty :: (Monad m) => Text -> Value -> Location -> NeoT m ()
setProperty key value uri = do

    let requestUri = uri `append` "/properties/" `append` key
        setPropertyRequest = jsonRequest PUT requestUri (strictEncode value)

    requestWithEmptyResponse setPropertyRequest

requestWithEmptyResponse :: (Monad m) => Request -> NeoT m ()
requestWithEmptyResponse request = do

    response <- lift (rest request)

    assert (code response == (2,0,4))
        (EmptyResponseCodeError (code response) (responseBody response))

setNodeProperty :: (Monad m) => Text -> Value -> Node -> NeoT m ()
setNodeProperty key value node = setProperty key value (nodeURI node) 

setEdgeProperty :: (Monad m) => Text -> Value -> Edge -> NeoT m ()
setEdgeProperty key value edge = setProperty key value (edgeURI edge)

addNodeLabel :: (Monad m) => Label -> Node -> NeoT m ()
addNodeLabel label node = do

    let addNodeLabelRequest = jsonRequest POST (nodeURI node `append` "/labels") (strictEncode label)

    requestWithEmptyResponse addNodeLabelRequest

data EdgesResponse = EdgesResponse [Text]

instance FromJSON EdgesResponse where
    parseJSON = withArray "EdgesResponse" (\edges -> do
        edgeuris <- mapM extractSelfURI (Vector.toList edges)
        return (EdgesResponse edgeuris))

edges :: (Monad m) => Node -> NeoT m [Edge]
edges node = do

    let allEdgesRequest = jsonGetRequest (nodeURI node `append` "/relationships/all")

    response <- lift (rest allEdgesRequest)

    assert (code response == (2,0,0))
        (EdgesResponseCodeError (code response) (responseBody response))
    assert (responseType response == Just jsoncontent)
        (EdgesResponseTypeError (responseType response) (responseBody response))

    EdgesResponse edgeuris <- (strictEitherDecode (responseBody response))
        `whenLeft` EdgesResponseParseError

    edgeids <- mapM extractId edgeuris

    return (map Edge edgeids)

whenLeft :: (Monad m) => Either a b -> (a -> e) -> EitherT e m b
whenLeft e f = either (left . f) return e

data EdgeInfo = EdgeInfo {edgeInfoStartUri :: Text,
                          edgeInfoEndUri   :: Text,
                          edgeInfoType     :: Text}

instance FromJSON EdgeInfo where
    parseJSON v = do
        edgeinfostart <- extractTextField "start" v
        edgeinfoend   <- extractTextField "end" v
        edgeinfotype  <- extractTextField "type" v
        return (EdgeInfo edgeinfostart edgeinfoend edgeinfotype)

edgeInfo :: (Monad m) => Edge -> NeoT m EdgeInfo
edgeInfo edge = do

    response <- lift (rest (jsonGetRequest (edgeURI edge)))

    assert (code response == (2,0,0))
        (EdgeInfoResponseCodeError (code response) (responseBody response))

    (strictEitherDecode (responseBody response))
        `whenLeft` EdgeInfoResponseParseError

source :: (Monad m) => Edge -> NeoT m Node
source = edgeInfo >=> extractId . edgeInfoStartUri >=> return . Node

target :: (Monad m) => Edge -> NeoT m Node
target = edgeInfo >=> extractId . edgeInfoEndUri >=> return . Node

edgeLabel :: (Monad m) => Edge -> NeoT m Label
edgeLabel = edgeInfo >=> return . edgeInfoType

assert :: (Monad m) => Bool -> NeoError -> NeoT m ()
assert True  _        = return ()
assert False neoerror = left neoerror

jsoncontent :: Text
jsoncontent = "application/json"

strictEncode :: (ToJSON a) => a -> ByteString
strictEncode = BL.toStrict . encode

strictEitherDecode :: (FromJSON a) => ByteString -> Either String a
strictEitherDecode = eitherDecode . BL.fromStrict
