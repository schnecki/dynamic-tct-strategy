module DynamicTctStrategy.Type
    ( St (..)
    , TcTAgentType (..)
    , MDPState (..)
    , StrategyTree (..)
    , DecompositionMethod (..)
    , AnalyserMethod (..)
    , Operation (..)
    , mkConfig
    , allDecompositionMethods
    , allAnalyserMethods
    ) where

import           Control.Monad.Trans.Reader

import           Tct.Trs.Data.Problem
import           Tct.Trs.Data.Symbol


mkConfig :: Reader r a -> r -> a
mkConfig = runReader


data St =
  St
    { tctAgentType    :: TcTAgentType
    , mdpState        :: MDPState
    , currentStrategy :: Maybe StrategyTree
    , problem         :: Problem F V
    }


-- | Agent type. This is needed to reset the system state once an episode is finished.
data TcTAgentType = AgentDecomposition | AgentAnalyser
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Current state of the MDP.
data MDPState = ChooseAction
              | ChooseConcreteMethod
              | End
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Binary tree encoding the current strategy
data StrategyTree
  = Op Operation StrategyTree StrategyTree
  | Method ConcreteMethod
  deriving (Show, Eq, Ord)


-- | Supported operations
data Operation
  = OpSeq -- | In TcT: Seq, .>>>
  | OpAlt -- | In TcT: Alt, .<|>
  deriving (Show, Eq, Ord, Enum, Bounded)


type ConcreteMethod = Either DecompositionMethod AnalyserMethod


-- | Supported concrete decomposition methods.
data DecompositionMethod = UsableRules
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Supported concrete analyser methods.
data AnalyserMethod = TODO
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | All supported decomposition methods.
allDecompositionMethods :: [DecompositionMethod]
allDecompositionMethods = [minBound .. maxBound]


-- | All supported analyser methods.
allAnalyserMethods :: [AnalyserMethod]
allAnalyserMethods = [minBound .. maxBound]
