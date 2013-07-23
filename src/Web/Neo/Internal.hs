{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Web.Neo.Internal where

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

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,append,pack,unpack)
import qualified  Data.Text as Text (takeWhile,reverse)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict,fromStrict)
import Data.HashMap.Strict (HashMap)

-- | Run the given neo commands against the given hostname and port.
runNeoT :: (MonadIO m) => Hostname -> Port -> NeoT m a -> m (Either RestError (Either NeoError a))
runNeoT hostname port = runRestT hostname port . runEitherT

-- | Run the given neo commands against hostname "localhost" and port 7474.
defaultRunNeoT :: (MonadIO m) => NeoT m a -> m (Either RestError (Either NeoError a))
defaultRunNeoT = runNeoT "localhost" 7474

-- | The neo monad transformer. Catches errors and uses REST calls.
type NeoT m a = EitherT NeoError (RestT m) a

-- | Type of things that can go wrong when talking to neo4j via the REST API.
data NeoError = ResponseCodeError ResponseCode Body
              | ResponseTypeError (Maybe ContentType) Body
              | ResponseParseError String
              | ExtractIdError Text

deriving instance Show NeoError

-- | A neo4j node.
data Node = Node {
    nodeId :: Integer,
    nodeData :: Properties}

deriving instance Show Node

instance FromJSON Node where
    parseJSON = withObject "NodeObject" (\o -> do
        selfid   <- o .: "self" >>= parseSelfId
        nodedata <- o .: "data"
        return (Node selfid nodedata))

-- | A neo4j edge.
data Edge = Edge {
    edgeId :: Integer,
    edgeStart :: Integer,
    edgeEnd :: Integer,
    edgeType :: Label,
    edgeData :: Properties}

deriving instance Show Edge

instance FromJSON Edge where
    parseJSON = withObject "EdgeObject" (\o -> do
        selfid   <- o .: "self"  >>= parseSelfId
        startid  <- o .: "start" >>= parseSelfId
        endid    <- o .: "end"   >>= parseSelfId
        label    <- o .: "type"  >>= parseJSON
        edgedata <- o .: "data"
        return (Edge selfid startid endid label edgedata))

-- | The properties of either a node or an edge. A map from 'Text' keys to
--   json values.
type Properties = HashMap Text Value

-- | A label of either a node or an edge.
type Label = Text

-- | Create a new node.
newNode :: (Monad m) => NeoT m Node
newNode = call (jsonRequest POST "/db/data/node" "") (2,0,1)

-- | Set the property of the given node with the given key to the given value.
setNodeProperty :: (Monad m) => Text -> Value -> Node -> NeoT m ()
setNodeProperty key value node = setProperty key value (nodeURI node)

-- | Add a label to a node.
addNodeLabel :: (Monad m) => Label -> Node -> NeoT m ()
addNodeLabel label node = emptyCall addNodeLabelRequest where
    addNodeLabelRequest = jsonRequest POST (nodeURI node `append` "/labels") (strictEncode label)

-- | Create a new edge.
newEdge :: (Monad m) => Label -> Node -> Node -> NeoT m Edge
newEdge label sourcenode targetnode = call newEdgeRequest (2,0,1) where
    newEdgeRequest = jsonRequest POST sourceuri (strictEncode payload)
    sourceuri      = (nodeURI sourcenode `append` "/relationships")
    payload        = object [
        "to"   .= nodeURI targetnode,
        "type" .= label]

-- | Set the property of the given edge with the given key to the given value.
setEdgeProperty :: (Monad m) => Text -> Value -> Edge -> NeoT m ()
setEdgeProperty key value edge = setProperty key value (edgeURI edge)

-- | Get the node with the given neo4j internal ID.
nodeById :: (Monad m) => Integer -> NeoT m Node
nodeById nodeid = call (jsonGetRequest ("/db/data/node/" `append` (pack (show nodeid)))) (2,0,0)

-- | Get all nodes with the given label.
nodesByLabel :: (Monad m) => Label -> NeoT m [Node]
nodesByLabel label = call (jsonGetRequest ("/db/data/label/" `append` label `append` "/nodes")) (2,0,0)

-- | Get all edges (outgoing as well as incoming) of the given node.
allEdges :: (Monad m) => Node -> NeoT m [Edge]
allEdges node = call (jsonGetRequest (nodeURI node `append` "/relationships/all")) (2,0,0)

-- | Get all incoming edges of the given node.
incomingEdges :: (Monad m) => Node -> NeoT m [Edge]
incomingEdges node = call (jsonGetRequest (nodeURI node `append` "/relationships/in")) (2,0,0)

-- | Get all outgoing edges of the given node.
outgoingEdges :: (Monad m) => Node -> NeoT m [Edge]
outgoingEdges node = call (jsonGetRequest (nodeURI node `append` "/relationships/out")) (2,0,0)

-- | Get all labels of the given node.
nodeLabels :: (Monad m) => Node -> NeoT m [Label]
nodeLabels node = call (jsonGetRequest (nodeURI node `append` "/labels")) (2,0,0)

-- | Get the properties of the given node.
nodeProperties :: (Monad m) => Node -> NeoT m Properties
nodeProperties = return . nodeData

-- | Get the source node of the given edge.
source :: (Monad m) => Edge -> NeoT m Node
source = nodeById . edgeStart

-- | Get the target node of the given edge.
target :: (Monad m) => Edge -> NeoT m Node
target = nodeById . edgeEnd

-- | Get the label of the given edge.
edgeLabel :: (Monad m) => Edge -> NeoT m Label
edgeLabel = return . edgeType

-- | Get the properties of the given edge.
edgeProperties :: (Monad m) => Edge -> NeoT m Properties
edgeProperties = return . edgeData

-- | When the given expected 'ResponseCode' and the one of the given 'Response'
--   are not equal throw a 'ResponseCodeError'.
assertResponseCode :: (Monad m) => ResponseCode -> Response -> NeoT m ()
assertResponseCode expectedCode response = when
    (expectedCode /= code response)
    (left (ResponseCodeError (code response) (responseBody response)))

-- | When the 'ContentType' of the given 'Response' does not match the
--   given expected 'ContentType' or is not present at all throw a
--   'ResponseTypeError'.
assertResponseType :: (Monad m) => ContentType -> Response -> NeoT m ()
assertResponseType expectedType response = when
    (Just expectedType /= responseType response)
    (left (ResponseTypeError (responseType response) (responseBody response)))

-- | The content type of a json request or a json response.
jsoncontent :: ContentType
jsoncontent = "application/json"

-- | Encode the given value into a strict 'ByteString'
strictEncode :: (ToJSON a) => a -> ByteString
strictEncode = BL.toStrict . encode

-- | Decode the given strict 'ByteString'.
strictEitherDecode :: (FromJSON a) => ByteString -> Either String a
strictEitherDecode = eitherDecode . BL.fromStrict

-- | Make a REST call. Expect the given 'ResponseCode' and the response
--   to be json encoded.
call :: (FromJSON a,Monad m) => Request -> ResponseCode -> NeoT m a
call request expectedCode = do

    response <- lift (rest request)

    assertResponseCode expectedCode response
    assertResponseType jsoncontent response

    parseResponse response

-- | Make a REST call and expect an empty response.
emptyCall :: (Monad m) => Request -> NeoT m ()
emptyCall request =  lift (rest request) >>= assertResponseCode (2,0,4)

-- | Make a json request where the given request body as well as the expected response
--   body are both json encoded.
jsonRequest :: Method -> Location -> Body -> Request
jsonRequest method location body = Request method location jsoncontent jsoncontent body

-- | Make a get request to the given location. The expected response body should be
--   json encoded.
jsonGetRequest :: Location -> Request
jsonGetRequest location = jsonRequest GET location ""

-- | Find a node's URI.
nodeURI :: Node -> Location
nodeURI node = "/db/data/node/" `append` (pack (show (nodeId node)))

-- | Find an edge's URI.
edgeURI :: Edge -> Location
edgeURI edge = "/db/data/relationship/" `append` (pack (show (edgeId edge)))

-- | Set the property of either an edge or a node at the given location.
setProperty :: (Monad m) => Text -> Value -> Location -> NeoT m ()
setProperty key value uri = emptyCall setPropertyRequest where
    requestUri         = uri `append` "/properties/" `append` key
    setPropertyRequest = jsonRequest PUT requestUri (strictEncode value)

-- | Parse the body of a response but fail in the 'NeoT' monad instead of
--   returning an 'Either'.
parseResponse :: (FromJSON a,Monad m) => Response -> NeoT m a
parseResponse response = case strictEitherDecode (responseBody response) of
    Left errormessage -> left (ResponseParseError errormessage)
    Right result      -> return result

-- | Given a json value that should represent a URI parse the last part of
--   it as a number.
parseSelfId :: Value -> Parser Integer
parseSelfId = withText "URI" (\s -> case idSlug s of
    Left errormessage -> fail errormessage
    Right idslug      -> return idslug)

-- | Extract the last part of the given URI.
idSlug :: Text -> Either String Integer
idSlug uri = readErr ("Reading URI slug failed: " ++ uriSlug) uriSlug where
    uriSlug = unpack (Text.reverse (Text.takeWhile (/= '/') (Text.reverse uri)))
