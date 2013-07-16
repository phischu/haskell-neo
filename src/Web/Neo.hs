{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Web.Neo (
    ) where

import Web.Rest (
    RestT,rest,
    runRestT,Hostname,Port,RestError,
    Request(Request),Method(POST),ContentType,
    Response(code,responseType,responseBody),
    ResponseCode)

import Control.Error (EitherT,runEitherT,left)

import Data.Aeson (ToJSON,object,(.=),encode)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,append,pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (concat)
import qualified Data.ByteString.Lazy as BL (toChunks)

data NeoError = NewNodeResponseCodeError ResponseCode ByteString
              | NewNodeResponseTypeError (Maybe ContentType) ByteString

deriving instance Show NeoError

type NeoT m a = EitherT NeoError (RestT m) a

runNeoT :: (MonadIO m) => Hostname -> Port -> NeoT m a -> m (Either RestError (Either NeoError a))
runNeoT hostname port = runRestT hostname port . runEitherT

defaultRunNeoT :: (MonadIO m) => NeoT m a -> m (Either RestError (Either NeoError a))
defaultRunNeoT = runNeoT "localhost" 7474

data Node = Node Integer
deriving instance Show Node

type Label = Text

newNode :: (Monad m) => NeoT m String
newNode = do
    response <- lift (rest (Request POST "/db/data/node" jsoncontent jsoncontent ""))
    assert (code response == (2,0,1))
        (NewNodeResponseCodeError (code response) (responseBody response))
    assert (responseType response == Just jsoncontent)
        (NewNodeResponseTypeError (responseType response) (responseBody response))
    return (show response)

nodeURI :: Node -> Text
nodeURI (Node nodeid) = "/db/data/node/" `append` (pack (show nodeid))

newEdge :: (Monad m) => Label -> Node -> Node -> NeoT m String
newEdge label sourcenode targetnode = do
	let sourceuri = (nodeURI sourcenode `append` "/relationships")
	    payload   = object [
	        "to"   .= nodeURI targetnode,
	        "type" .= label]
	response <- lift (rest (Request POST sourceuri jsoncontent jsoncontent (strictEncode payload)))
	return (show response)

assert :: (Monad m) => Bool -> NeoError -> NeoT m ()
assert True  _        = return ()
assert False neoerror = left neoerror

jsoncontent :: Text
jsoncontent = "application/json"

strictEncode :: (ToJSON a) => a -> ByteString
strictEncode = B.concat . BL.toChunks . encode
