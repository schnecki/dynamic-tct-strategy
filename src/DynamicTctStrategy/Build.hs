module DynamicTctStrategy.Build
    ( buildGrenade

    ) where

import           Control.Monad.IO.Class
import           Data.List                                   (genericLength)
import qualified Data.Vector.Storable                        as VS
import           Grenade
import           ML.BORL

import           DynamicTctStrategy.Type

import           DynamicTctStrategy.SettingsActions
import           DynamicTctStrategy.SettingsConfigParameters

actFilter :: St -> VS.Vector Bool
actFilter = undefined

modelBuilderGrenade :: [Action a] -> St -> Integer -> IO SpecConcreteNetwork
modelBuilderGrenade actions initState cols =
  buildModelWith UniformInit BuildSetup { printResultingSpecification = False } $
  inputLayer1D lenIn >>
  fullyConnected (2*lenIn) >> relu >>
  fullyConnected lenIn >> relu >>
  fullyConnected (2*lenOut) >> relu >>
  fullyConnected lenOut >> reshape (lenActs, cols, 1) >> tanhLayer
  where
    lenOut = lenActs * cols
    lenIn = fromIntegral $ VS.length (netInp initState)
    lenActs = genericLength actions


mkInitSt :: TcTAgentType -> AgentType -> IO St
mkInitSt tp _ = return $ St tp ChooseAction Nothing undefined


-- | Build the decomposition agent.
buildDecomposeGrenade :: IO (BORL St)
buildDecomposeGrenade = do
  initSt <- liftIO $ mkInitSt AgentDecomposition MainAgent
  flipObjective . setPrettyPrintElems <$>
    mkUnichainGrenade
      alg
      (return . const initSt)
      netInp
      actionsDecomposition
      actFilter
      borlParams
      decayParams
      (modelBuilderGrenade actionsDecomposition initSt)
      nnConfig
      borlSettings
      (Just initVals)

setPrettyPrintElems :: BORL St -> BORL St
setPrettyPrintElems = id

netInp :: St -> VS.Vector Float
netInp = undefined

