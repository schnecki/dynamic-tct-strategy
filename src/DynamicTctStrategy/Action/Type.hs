

module DynamicTctStrategy.Action.Type
    ( ActionConfig (..)
    ) where

import qualified Data.Map                as M

import           DynamicTctStrategy.Type

-- import TcT

data ActionConfig =
  ActionConfig
    { actionMaxOperations :: !Int
    , actionMethodDomain  :: Either [DecompositionMethod] [AnalyserMethod]
    }
  deriving (Show, Eq, Ord)


