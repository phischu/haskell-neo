{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Web.Neo where

import Web.Rest (
    RestT,rest,
    runRestT,Hostname,Port,RestError,
    Request(Request),Method(POST,PUT,GET),Location,ContentType,Body,
    Response(code,responseType,responseBody),
    ResponseCode)

import Control.Error (EitherT,runEitherT,left,readErr)

import Data.Aeson (
    Value,
    ToJSON,object,(.=),encode,
    FromJSON(parseJSON),
    withObject,withText,
    (.:),eitherDecode)
import Data.Aeson.Types (Parser)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,append,pack,unpack)
import qualified  Data.Text as Text (takeWhile,reverse)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict,fromStrict)
import Data.HashMap.Strict (HashMap)

data NeoError = ResponseCodeError ResponseCode Body
              | ResponseTypeError (Maybe ContentType) Body
              | ResponseParseError String
              | ExtractIdError Text

deriving instance Show NeoError

type NeoT m a = EitherT NeoError (RestT m) a

runNeoT :: (MonadIO m) => Hostname -> Port -> NeoT m a -> m (Either RestError (Either NeoError a))
runNeoT hostname port = runRestT hostname port . runEitherT

defaultRunNeoT :: (MonadIO m) => NeoT m a -> m (Either RestError (Either NeoError a))
defaultRunNeoT = runNeoT "localhost" 7474

data Node = Node {
    nodeId :: Integer,
    nodeData :: Properties}

deriving instance Show Node

instance FromJSON Node where
    parseJSON = withObject "NodeObject" (\o -> do
        selfid <- o .: "self" >>= parseSelfId
        nodedata <- o .: "data"
        return (Node selfid nodedata))

data Edge = Edge {
    edgeId :: Integer,
    edgeStart :: Integer,
    edgeEnd :: Integer,
    edgeType :: Label,
    edgeData :: Properties}

deriving instance Show Edge

instance FromJSON Edge where
    parseJSON = withObject "EdgeObject" (\o -> do
        selfid <- o .: "self" >>= parseSelfId
        startid <- o .: "start" >>= parseSelfId
        endid   <- o .: "end" >>= parseSelfId
        label <- o .: "type" >>= parseJSON
        edgedata <- o .: "data"
        return (Edge selfid startid endid label edgedata))

type Properties = HashMap Text Value
type Label = Text

newNode :: (Monad m) => NeoT m Node
newNode = call (jsonRequest POST "/db/data/node" "") (2,0,1)

newEdge :: (Monad m) => Label -> Node -> Node -> NeoT m Edge
newEdge label sourcenode targetnode = call newEdgeRequest (2,0,1) where
    newEdgeRequest = jsonRequest POST sourceuri (strictEncode payload)
    sourceuri      = (nodeURI sourcenode `append` "/relationships")
    payload        = object [
        "to"   .= nodeURI targetnode,
        "type" .= label]

setProperty :: (Monad m) => Text -> Value -> Location -> NeoT m ()
setProperty key value uri = emptyCall setPropertyRequest where
    requestUri         = uri `append` "/properties/" `append` key
    setPropertyRequest = jsonRequest PUT requestUri (strictEncode value)

setNodeProperty :: (Monad m) => Text -> Value -> Node -> NeoT m ()
setNodeProperty key value node = setProperty key value (nodeURI node) 

setEdgeProperty :: (Monad m) => Text -> Value -> Edge -> NeoT m ()
setEdgeProperty key value edge = setProperty key value (edgeURI edge)

addNodeLabel :: (Monad m) => Label -> Node -> NeoT m ()
addNodeLabel label node = emptyCall addNodeLabelRequest where
    addNodeLabelRequest = jsonRequest POST (nodeURI node `append` "/labels") (strictEncode label)

nodeById :: (Monad m) => Integer -> NeoT m Node
nodeById nodeid = call (jsonGetRequest ("/db/data/node/" `append` (pack (show nodeid)))) (2,0,0)

nodesByLabel :: (Monad m) => Label -> NeoT m [Node]
nodesByLabel label = call (jsonGetRequest ("/db/data/label/" `append` label `append` "/nodes")) (2,0,0)

edges :: (Monad m) => Node -> NeoT m [Edge]
edges node = call (jsonGetRequest (nodeURI node `append` "/relationships/all")) (2,0,0)

nodeLabels :: (Monad m) => Node -> NeoT m [Label]
nodeLabels node = call (jsonGetRequest (nodeURI node `append` "/labels")) (2,0,0)

nodeProperties :: (Monad m) => Node -> NeoT m Properties
nodeProperties = return . nodeData

source :: (Monad m) => Edge -> NeoT m Node
source = nodeById . edgeStart

target :: (Monad m) => Edge -> NeoT m Node
target = nodeById . edgeEnd

edgeLabel :: (Monad m) => Edge -> NeoT m Label
edgeLabel = return . edgeType

edgeProperties :: (Monad m) => Edge -> NeoT m Properties
edgeProperties = return . edgeData

assert :: (Monad m) => Bool -> NeoError -> NeoT m ()
assert True  _        = return ()
assert False neoerror = left neoerror

assertResponseCode :: (Monad m) => ResponseCode -> Response -> NeoT m ()
assertResponseCode expectedCode response = assert
    (code response == expectedCode)
    (ResponseCodeError (code response) (responseBody response))

assertResponseType :: (Monad m) => ContentType -> Response -> NeoT m ()
assertResponseType expectedType response = assert
    (responseType response == Just expectedType)
    (ResponseTypeError (responseType response) (responseBody response))

jsoncontent :: Text
jsoncontent = "application/json"

strictEncode :: (ToJSON a) => a -> ByteString
strictEncode = BL.toStrict . encode

strictEitherDecode :: (FromJSON a) => ByteString -> Either String a
strictEitherDecode = eitherDecode . BL.fromStrict

call :: (FromJSON a,Monad m) => Request -> ResponseCode -> NeoT m a
call request expectedCode = do

    response <- lift (rest request)

    assertResponseCode expectedCode response
    assertResponseType jsoncontent response

    parseResponse response

emptyCall :: (Monad m) => Request -> NeoT m ()
emptyCall request =  lift (rest request) >>= assertResponseCode (2,0,4)

parseResponse :: (FromJSON a,Monad m) => Response -> NeoT m a
parseResponse response = case strictEitherDecode (responseBody response) of
    Left errormessage -> left (ResponseParseError errormessage)
    Right result      -> return result

parseSelfId :: Value -> Parser Integer
parseSelfId = withText "URI" (\s -> case idSlug s of
    Left errormessage -> fail errormessage
    Right idslug      -> return idslug)

idSlug :: Text -> Either String Integer
idSlug uri = readErr ("Reading URI slug failed: " ++ uriSlug) uriSlug where
    uriSlug = unpack (Text.reverse (Text.takeWhile (/= '/') (Text.reverse uri)))

jsonRequest :: Method -> Location -> Body -> Request
jsonRequest method location body = Request method location jsoncontent jsoncontent body

jsonGetRequest :: Location -> Request
jsonGetRequest location = jsonRequest GET location ""

nodeURI :: Node -> Location
nodeURI node = "/db/data/node/" `append` (pack (show (nodeId node)))

edgeURI :: Edge -> Location
edgeURI edge = "/db/data/relationship/" `append` (pack (show (edgeId edge)))
