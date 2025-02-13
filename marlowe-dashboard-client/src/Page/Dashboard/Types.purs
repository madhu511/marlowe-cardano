module Page.Dashboard.Types
  ( Action(..)
  , State
  , Card(..)
  , ContractFilter(..)
  , Input
  , WalletCompanionStatus(..)
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent, toEvent)
import Clipboard (Action) as Clipboard
import Component.ConfirmInput.Types as ConfirmInput
import Component.Contacts.Types (Action, State) as Contacts
import Component.Template.Types (Action, State) as Template
import Data.AddressBook (AddressBook)
import Data.Map (Map)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import Data.WalletNickname (WalletNickname)
import Marlowe.Client (ContractHistory)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweData, MarloweParams, Slot)
import Page.Contract.Types (Action, State) as Contract

type State =
  { contactsState :: Contacts.State
  , walletCompanionStatus :: WalletCompanionStatus
  , menuOpen :: Boolean
  , card :: Maybe Card
  -- TODO use HalogenStore for modals. It would sure be nice to have portals...
  , cardOpen :: Boolean -- see note [CardOpen] in Welcome.State (the same applies here)
  -- FIXME-3208: Refactor in progress, remove...
  -- TODO: SCP-3208 Move contract state to halogen store
  , contracts :: Map PlutusAppId Contract.State
  , contractFilter :: ContractFilter
  , selectedContractFollowerAppId :: Maybe PlutusAppId
  , templateState :: Template.State
  }

-- This represents the status of the wallet companion. When we start the application
-- we are waiting for the wallet companion to tell us of every Marlowe contract that
-- the user has. While we wait, we show a loading indicator in the dashboard.
-- After we have the initial status update, we show the state of every contract that
-- we were following, and we start to follow the new contracts that the user has.
data WalletCompanionStatus
  = WaitingToSync
  | WalletCompanionSynced

derive instance eqWalletCompanionStatus :: Eq WalletCompanionStatus

data Card
  = TutorialsCard
  | CurrentWalletCard
  | ContactsCard
  | ContractTemplateCard
  | ContractActionConfirmationCard PlutusAppId ConfirmInput.Input

data ContractFilter
  = Running
  | Completed

derive instance eqContractFilter :: Eq ContractFilter

type Input =
  { wallet :: PABConnectedWallet
  , addressBook :: AddressBook
  , currentSlot :: Slot
  , tzOffset :: Minutes
  }

data Action
  = DisconnectWallet
  | ContactsAction Contacts.Action
  | ToggleMenu
  | OpenCard Card
  | CloseCard
  | SetContractFilter ContractFilter
  | SelectContract (Maybe PlutusAppId)
  | UpdateFollowerApps (Map MarloweParams MarloweData)
  | UpdateContract PlutusAppId ContractHistory
  | RedeemPayments PlutusAppId
  | AdvanceTimedoutSteps
  | TemplateAction Template.Action
  | ContractAction PlutusAppId Contract.Action
  | SetContactForRole String WalletNickname
  | ClipboardAction Clipboard.Action

-- | Here we decide which top-level queries to track as GA events, and how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent DisconnectWallet = Just $ defaultEvent "DisconnectWallet"
  toEvent (ContactsAction contactsAction) = toEvent contactsAction
  toEvent ToggleMenu = Just $ defaultEvent "ToggleMenu"
  toEvent (OpenCard _) = Nothing
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
  toEvent CloseCard = Nothing
  toEvent (SetContractFilter _) = Just $ defaultEvent "FilterContracts"
  toEvent (SelectContract _) = Just $ defaultEvent "OpenContract"
  toEvent (UpdateFollowerApps _) = Nothing
  toEvent (UpdateContract _ _) = Nothing
  toEvent (RedeemPayments _) = Nothing
  toEvent AdvanceTimedoutSteps = Nothing
  toEvent (TemplateAction _) = Nothing
  toEvent (ContractAction _ contractAction) = toEvent contractAction
  toEvent (SetContactForRole _ _) = Nothing
