module DynamicTctStrategy.Action.Ops
    ( actionDecomposition
    ) where

import           Control.Monad.Trans.Reader

import           ML.BORL

import           DynamicTctStrategy.Action.Type
import           DynamicTctStrategy.Type


-- actionDecomposition :: St -> Reader ActionConfig (ListOfActions, [Action St])
actionDecomposition :: Reader ActionConfig [Action St]
actionDecomposition = undefined
