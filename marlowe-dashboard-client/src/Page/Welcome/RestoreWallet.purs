module Page.Welcome.RestoreWallet (component, _restoreWallet) where

import Prologue

import AppM (passphrase) as AppM
import Capability.Marlowe (class ManageMarlowe, restoreWallet)
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.AddressBook (AddressBook)
import Data.Lens (Setter', is, set, (^?))
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Variant (default, on) as Variant
import Data.Wallet (WalletDetails)
import Data.WalletNickname (WalletNickname)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.Injective (blank, project)
import Halogen.Form.Input (FieldState)
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), _Failure, _Loading)
import Page.Welcome.Forms.Render (mkMnemonicInput, mkNicknameInput, renderForm)
import Page.Welcome.RestoreWallet.Types
  ( Component
  , Input
  , Msg(..)
  , RestoreWalletFields
  , RestoreWalletParams
  , _mnemonic
  , _nickname
  )
import Store as Store
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

data Action
  = OnInit
  | OnReceive (Connected AddressBook Input)
  | OnUpdate (RestoreWalletFields -> RestoreWalletFields)
  | OnFormSubmit Event
  | OnCancel
  | OnRestore RestoreWalletParams

type State =
  { addressBook :: AddressBook
  , fields :: RestoreWalletFields
  , result :: Maybe RestoreWalletParams
  , walletDetails :: RemoteData String WalletDetails
  }

type ChildSlots =
  ( nickname :: Input.Slot Action WalletNickname Unit
  , mnemonic :: Input.Slot Action MnemonicPhrase Unit
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

_restoreWallet = Proxy :: Proxy "restoreWallet"

component
  :: forall m
   . MonadAff m
  => ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => Component m
component = connect (selectEq _.addressBook) $ H.mkComponent
  { initialState
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< OnReceive
      , initialize = Just OnInit
      }
  , render
  }

initialState :: Connected AddressBook Input -> State
initialState { context } =
  { addressBook: context
  , fields: blank
  , result: Nothing
  , walletDetails: NotAsked
  }

adaptInput
  :: forall a
   . Setter' RestoreWalletFields (FieldState a)
  -> Input.Msg Action a
  -> Action
adaptInput optic = case _ of
  Input.Updated field -> OnUpdate $ set optic field
  Input.Blurred -> OnUpdate identity
  Input.Focused -> OnUpdate identity
  Input.Emit a -> a

handleAction
  :: forall m. MonadEffect m => ManageMarlowe m => Action -> DSL m Unit
handleAction = case _ of
  OnInit -> do
    H.tell _nickname unit $ Input.Focus
  OnReceive input -> H.modify_ _ { addressBook = input.context }
  OnUpdate update -> do
    { fields: newFields } <- H.modify \s -> s { fields = update s.fields }
    H.modify_ _ { result = project newFields }
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnCancel -> H.raise CancelClicked
  OnRestore { nickname, mnemonic } -> do
    H.modify_ _ { walletDetails = Loading }
    response <- lift $ restoreWallet nickname mnemonic AppM.passphrase
    case response of
      Left err -> do
        H.modify_ _
          { walletDetails = err
              #
                ( Variant.default "Error from server."
                    # Variant.on
                        (Proxy :: Proxy "invalidMnemonic")
                        (const "Invalid mnemonic phrase.")
                )
              # Failure
          }
      Right walletDetails -> do
        H.modify_ _ { walletDetails = Success walletDetails }
        H.raise $ WalletRestored walletDetails

render
  :: forall m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML m
render { addressBook, result, fields, walletDetails } = do
  let serverError = fromMaybe "" $ walletDetails ^? _Failure
  let inProgress = is _Loading walletDetails
  renderForm
    { body:
        [ HH.form
            [ HE.onSubmit OnFormSubmit
            , classNames [ "relative", "space-y-4" ]
            ]
            [ nicknameInput addressBook fields.nickname
            , mnemonicInput fields.mnemonic
            ]
        , HH.p_
            [ HH.b_ [ HH.text "IMPORTANT:" ]
            -- FIXME: as part of SCP-3173, Write a section in the Marlowe Run documentation and add a link to it
            , HH.text "Do not use a real wallet phrase <read more>"
            ]
        -- TODO replace with progress buttons when refactored.
        , HH.p [ classNames Css.inputError ] [ HH.text serverError ]
        ]
    , inProgress
    , onCancel:
        { action: Just OnCancel
        , label: "Cancel"
        }
    , onSkip: Nothing
    , onSubmit:
        { action: OnRestore <$> result
        , label: "Restore wallet"
        }
    , title: "Restore testnet wallet"
    }

nicknameInput
  :: forall m
   . MonadEffect m
  => AddressBook
  -> FieldState WalletNickname
  -> ComponentHTML m
nicknameInput addressBook fieldState =
  HH.slot
    _nickname
    unit
    Input.component
    (mkNicknameInput addressBook fieldState)
    $ adaptInput
    $ prop _nickname

mnemonicInput
  :: forall m. MonadEffect m => FieldState MnemonicPhrase -> ComponentHTML m
mnemonicInput fieldState =
  HH.slot
    _mnemonic
    unit
    Input.component
    (mkMnemonicInput fieldState)
    $ adaptInput
    $ prop _mnemonic
