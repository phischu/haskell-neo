{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Web.Neo (
    -- * Run Neo Actions
    runNeoT,defaultRunNeoT,NeoT,NeoError(..),
    Hostname,Port,
    -- * Basic Types
    Node(..),Edge(..),Properties,Label,
    -- * Create Nodes and Edges
    newNode,setNodeProperty,addNodeLabel,
    newEdge,setEdgeProperty,
    -- * Get Nodes
    nodeById,nodesByLabel,
    -- * Get Edges
    edgeById,
    -- * Get Information about Nodes
    allEdges,incomingEdges,outgoingEdges,
    nodeLabels,nodeProperties,
    -- * Get Information about Edges
    source,target,edgeLabel,edgeProperties,
    -- * Cypher Queries
    cypher,CypherQuery,CypherParameters,CypherResult(..)
    ) where

import Web.Neo.Internal

import Web.Rest (Hostname,Port)
