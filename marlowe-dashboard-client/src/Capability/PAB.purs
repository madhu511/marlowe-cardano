module Capability.PAB
  ( class ManagePAB
  , activateContract
  , deactivateContract
  , getContractInstanceClientState
  , getContractInstanceCurrentState
  , getContractInstanceObservableState
  , getContractInstanceHooks
  , invokeEndpoint
  , getWalletContractInstances
  , getAllContractInstances
  , getContractDefinitions
  ) where

import Prologue

import API.Lenses (_cicCurrentState, _hooks, _observableState)
import AppM (AppM)
import Control.Monad.Except (lift)
import Data.Argonaut (Json, encodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Lens (view)
import Data.WalletId (WalletId)
import Data.WalletId as WalletId
import Halogen (HalogenM)
import Marlowe.PAB (PlutusAppId)
import MarloweContract (MarloweContract)
import Plutus.Contract.Effects (ActiveEndpoint)
import Plutus.Contract.Resumable (Request)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Webserver as PAB
import Plutus.PAB.Webserver.Types
  ( ContractActivationArgs(..)
  , ContractInstanceClientState
  , ContractSignatureResponse
  )
import Types (AjaxResponse)
import Wallet.Emulator.Wallet (Wallet(..))

-- TODO (possibly): make `AppM` a `MonadError` and remove all the `runExceptT`s
class
  Monad m <=
  ManagePAB m where
  activateContract
    :: MarloweContract -> WalletId -> m (AjaxResponse PlutusAppId)
  deactivateContract :: PlutusAppId -> m (AjaxResponse Unit)
  getContractInstanceClientState
    :: PlutusAppId
    -> m (AjaxResponse (ContractInstanceClientState MarloweContract))
  getContractInstanceCurrentState
    :: PlutusAppId -> m (AjaxResponse (PartiallyDecodedResponse ActiveEndpoint))
  getContractInstanceObservableState :: PlutusAppId -> m (AjaxResponse Json)
  getContractInstanceHooks
    :: PlutusAppId -> m (AjaxResponse (Array (Request ActiveEndpoint)))
  invokeEndpoint
    :: forall d
     . EncodeJson d
    => PlutusAppId
    -> String
    -> d
    -> m (AjaxResponse Unit)
  getWalletContractInstances
    :: WalletId
    -> m (AjaxResponse (Array (ContractInstanceClientState MarloweContract)))
  getAllContractInstances :: m
    (AjaxResponse (Array (ContractInstanceClientState MarloweContract)))
  getContractDefinitions :: m
    (AjaxResponse (Array (ContractSignatureResponse MarloweContract)))

instance ManagePAB AppM where
  activateContract contractActivationId wallet =
    PAB.postApiContractActivate
      $ ContractActivationArgs
          { caID: contractActivationId
          , caWallet: Just $ Wallet { getWalletId: WalletId.toString wallet }
          }
  deactivateContract =
    PAB.putApiContractInstanceByContractinstanceidStop
  getContractInstanceClientState =
    PAB.getApiContractInstanceByContractinstanceidStatus
  getContractInstanceCurrentState plutusAppId = do
    clientState <- getContractInstanceClientState plutusAppId
    pure $ map (view _cicCurrentState) clientState
  getContractInstanceObservableState plutusAppId = do
    currentState <- getContractInstanceCurrentState plutusAppId
    pure $ map (view _observableState) currentState
  getContractInstanceHooks plutusAppId = do
    currentState <- getContractInstanceCurrentState plutusAppId
    pure $ map (view _hooks) currentState
  invokeEndpoint plutusAppId endpoint payload = do
    PAB.postApiContractInstanceByContractinstanceidEndpointByEndpointname
      (encodeJson payload)
      plutusAppId
      endpoint
  getWalletContractInstances wallet =
    PAB.getApiContractInstancesWalletByWalletid (WalletId.toString wallet)
      Nothing
  getAllContractInstances = PAB.getApiContractInstances Nothing
  getContractDefinitions = PAB.getApiContractDefinitions

instance ManagePAB m => ManagePAB (HalogenM state action slots msg m) where
  activateContract contractActivationId wallet = lift $ activateContract
    contractActivationId
    wallet
  deactivateContract = lift <<< deactivateContract
  getContractInstanceClientState = lift <<< getContractInstanceClientState
  getContractInstanceCurrentState = lift <<< getContractInstanceCurrentState
  getContractInstanceObservableState = lift <<<
    getContractInstanceObservableState
  getContractInstanceHooks = lift <<< getContractInstanceHooks
  invokeEndpoint plutusAppId endpointDescription payload = lift $ invokeEndpoint
    plutusAppId
    endpointDescription
    payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  getAllContractInstances = lift getAllContractInstances
  getContractDefinitions = lift getContractDefinitions
