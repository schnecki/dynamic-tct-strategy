module DynamicTctStrategy.SettingsActions where

import           ML.BORL

import           DynamicTctStrategy.Action
import           DynamicTctStrategy.Type


actionsDecomposition :: [Action St]
actionsDecomposition = mkConfig actionDecomposition actionConfigDecomposition

actionConfigDecomposition :: ActionConfig
actionConfigDecomposition = ActionConfig 5 (Left allDecompositionMethods)

