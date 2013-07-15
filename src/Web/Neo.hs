{-# LANGUAGE OverloadedStrings #-}
module Web.Neo (
    ) where

import Web.Rest (
    RestT,rest,
    Request(Request),Method(POST))

newNode :: (Monad m) => RestT m String
newNode = do
    result <- rest (Request POST "/db/data/node" "application/json" "application/json" "")
    return (show result)
