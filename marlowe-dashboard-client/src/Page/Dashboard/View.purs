module Page.Dashboard.View
  ( dashboardScreen
  , dashboardCard
  ) where

import Prologue hiding (Either(..), div)

import Clipboard (Action(..)) as Clipboard
import Component.Address.View (defaultInput, render) as Address
import Component.ConfirmInput.View as ConfirmInput
import Component.Contacts.State (adaToken, getAda)
import Component.Contacts.View (contactsCard)
import Component.ContractPreview.View (contractPreviewCard)
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Component.Popper (Placement(..))
import Component.Template.View (contractTemplateCard)
import Component.Tooltip.State (tooltip)
import Component.Tooltip.Types (ReferenceId(..))
import Css as Css
import Data.Address as A
import Data.Compactable (compact)
import Data.Int (round)
import Data.Lens (preview, view, (^.))
import Data.Map (Map, filter, isEmpty, toUnfoldable)
import Data.Maybe (isJust)
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _assets
  , _pubKeyHash
  , _syncStatus
  , _walletNickname
  )
import Data.PaymentPubKeyHash (_PaymentPubKeyHash)
import Data.String (take)
import Data.Tuple.Nested ((/\))
import Data.Wallet (SyncStatus(..))
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (applyWhen, classNames)
import Halogen.Extra (mapComponentAction, renderSubmodule)
import Halogen.HTML
  ( HTML
  , a
  , button
  , div
  , div_
  , footer
  , h2
  , h3
  , h4
  , header
  , img
  , main
  , nav
  , p
  , span
  , span_
  , text
  )
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (href, id, src)
import Halogen.Store.Monad (class MonadStore)
import Humanize (humanizeValue)
import Images (marloweRunNavLogo, marloweRunNavLogoDark)
import MainFrame.Types (ChildSlots)
import Marlowe.Execution.State (contractName) as Execution
import Marlowe.Execution.Types (State) as Execution
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (PubKey, Slot)
import Page.Contract.Lenses (_Started, _executionState)
import Page.Contract.Types (State) as Contract
import Page.Contract.View (contractScreen)
import Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _contactsState
  , _contractFilter
  , _menuOpen
  , _selectedContract
  , _selectedContractFollowerAppId
  , _templateState
  )
import Page.Dashboard.Types
  ( Action(..)
  , Card(..)
  , ContractFilter(..)
  , Input
  , State
  , WalletCompanionStatus(..)
  )
import Store as Store

-- TODO: We should be able to remove Input (tz and current slot) after we make each sub-component a proper component
dashboardScreen
  :: forall m. MonadAff m => Input -> State -> ComponentHTML Action ChildSlots m
dashboardScreen { currentSlot, tzOffset, wallet } state =
  let
    walletNickname = wallet ^. _walletNickname

    menuOpen = state ^. _menuOpen

    cardOpen = state ^. _cardOpen

    selectedContractFollowerAppId = state ^. _selectedContractFollowerAppId

    selectedContract = preview
      (_selectedContract <<< _Started <<< _executionState)
      state
  in
    div
      [ classNames
          $
            [ "h-full"
            , "grid"
            , "grid-rows-auto-1fr-auto"
            , "transition-all"
            , "duration-500"
            , "overflow-x-hidden"
            ]
              <> applyWhen cardOpen [ "lg:mr-sidebar" ]
      ]
      [ dashboardHeader (WN.toString walletNickname) menuOpen
      , div
          [ classNames [ "relative" ] ] -- this wrapper is relative because the mobile menu is absolutely positioned inside it
          [ mobileMenu menuOpen
          , div [ classNames [ "h-full", "grid", "grid-rows-auto-1fr" ] ]
              [ dashboardBreadcrumb selectedContract
              , main
                  [ classNames [ "relative" ] ]
                  case selectedContractFollowerAppId of
                    Just followerAppId ->
                      [ renderSubmodule
                          _selectedContract
                          (ContractAction followerAppId)
                          ( contractScreen
                              { currentSlot
                              , tzOffset
                              , wallet
                              , followerAppId
                              }
                          )
                          state
                      ]
                    _ -> [ contractsScreen currentSlot state ]
              ]
          ]
      , dashboardFooter
      ]

dashboardCard
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Input
  -> State
  -> ComponentHTML Action ChildSlots m
dashboardCard { addressBook, wallet } state = case view _card state of
  Just card ->
    let
      cardOpen = state ^. _cardOpen

      assets = wallet ^. _assets
    in
      div
        [ classNames $ Css.sidebarCardOverlay cardOpen ]
        [ div
            [ classNames $ Css.sidebarCard cardOpen ]
            $
              [ a
                  [ classNames [ "absolute", "top-4", "right-4" ]
                  , onClick_ CloseCard
                  ]
                  [ icon_ Icon.Close ]
              , case card of
                  TutorialsCard -> tutorialsCard
                  CurrentWalletCard -> currentWalletCard wallet
                  ContactsCard -> renderSubmodule _contactsState ContactsAction
                    (contactsCard addressBook wallet)
                    state
                  ContractTemplateCard -> renderSubmodule _templateState
                    TemplateAction
                    (contractTemplateCard assets)
                    state
                  ContractActionConfirmationCard contractId input ->
                    mapComponentAction
                      (ContractAction contractId)
                      (ConfirmInput.render input)
              ]
        ]
  Nothing -> div_ []

------------------------------------------------------------
dashboardHeader
  :: forall m
   . MonadAff m
  => PubKey
  -> Boolean
  -> ComponentHTML Action ChildSlots m
dashboardHeader walletNickname menuOpen =
  header
    [ classNames
        $ [ "relative", "border-gray", "transition-colors", "duration-200" ]
            <>
              if menuOpen then
                [ "border-0"
                , "bg-black"
                , "text-white"
                , "md:border-b"
                , "md:bg-transparent"
                , "md:text-black"
                ]
              else [ "border-b", "text-black" ]
    -- ^ in case the menu is open when the user makes their window wider, we make sure the menuOpen styles only apply on small screens ...
    ]
    [ div
        [ classNames $ Css.maxWidthContainer <>
            [ "flex"
            , "justify-between"
            , "items-center"
            , "leading-none"
            , "py-3"
            , "md:py-1"
            ]
        ]
        [ a
            [ onClick_ $ SelectContract Nothing ]
            [ img
                [ classNames [ "w-16", "md:hidden" ]
                , src
                    if menuOpen then marloweRunNavLogoDark
                    else marloweRunNavLogo
                ]
            -- ... and provide an alternative logo for wider screens that always has black text
            , img
                [ classNames [ "w-16", "hidden", "md:inline" ]
                , src marloweRunNavLogo
                ]
            ]
        , nav
            [ classNames [ "flex", "items-center" ] ]
            [ navigation (OpenCard ContactsCard) Icon.Contacts "contactsHeader"
            , tooltip "Contacts" (RefId "contactsHeader") Bottom
            , navigation (OpenCard TutorialsCard) Icon.Tutorials
                "tutorialsHeader"
            , tooltip "Tutorials" (RefId "tutorialsHeader") Bottom
            , a
                [ classNames [ "ml-6", "font-bold", "text-sm" ]
                , id "currentWalletHeader"
                , onClick_ $ OpenCard CurrentWalletCard
                ]
                [ span
                    [ classNames $
                        [ "flex"
                        , "items-baseline"
                        , "gap-2"
                        , "bg-white"
                        , "rounded-lg"
                        , "p-4"
                        , "leading-none"
                        ]
                    ]
                    [ span
                        [ classNames $
                            [ "-m-1"
                            , "rounded-full"
                            , "text-white"
                            , "w-5"
                            , "h-5"
                            , "flex"
                            , "justify-center"
                            , "items-center"
                            , "uppercase"
                            , "font-semibold"
                            ] <> Css.bgBlueGradient
                        ]
                        [ text $ take 1 walletNickname ]
                    , span
                        [ classNames
                            [ "hidden", "md:inline", "truncate", "max-w-16" ]
                        ]
                        [ text walletNickname ]
                    ]
                ]
            , tooltip "Your wallet" (RefId "currentWalletHeader") Bottom
            , a
                [ classNames [ "ml-4", "md:hidden" ]
                , onClick_ ToggleMenu
                ]
                [ if menuOpen then icon_ Icon.Close else icon_ Icon.Menu ]
            ]
        ]
    ]
  where
  navigation action icon' refId =
    a
      [ classNames [ "ml-6", "font-bold", "text-sm" ]
      , id refId
      , onClick_ action
      ]
      [ icon_ icon' ]

mobileMenu :: forall p. Boolean -> HTML p Action
mobileMenu menuOpen =
  nav
    [ classNames
        $
          [ "md:hidden"
          , "absolute"
          , "inset-0"
          , "z-30"
          , "bg-black"
          , "text-white"
          , "text-lg"
          , "overflow-auto"
          , "flex"
          , "flex-col"
          , "justify-between"
          , "pt-8"
          , "pb-4"
          , "transition-all"
          , "duration-200"
          ]
            <>
              if menuOpen then [ "opacity-100" ]
              else [ "opacity-0", "pointer-events-none" ]
    ]
    [ div
        [ classNames [ "flex", "flex-col" ] ]
        dashboardLinks
    , div
        [ classNames [ "flex", "flex-col" ] ]
        iohkLinks
    ]

dashboardBreadcrumb
  :: forall m
   . MonadAff m
  => (Maybe Execution.State)
  -> ComponentHTML Action ChildSlots m
dashboardBreadcrumb mSelectedContractState =
  div [ classNames [ "border-b", "border-gray" ] ]
    [ nav [ classNames $ Css.maxWidthContainer <> [ "flex", "gap-2", "py-2" ] ]
        $
          [ a
              ( compact
                  [ Just $ id "goToDashboard"
                  , mSelectedContractState $> onClick
                      (const $ SelectContract Nothing)
                  , Just
                      $ classNames
                          if (isJust mSelectedContractState) then
                            [ "text-lightpurple", "font-bold" ]
                          else
                            [ "cursor-default" ]
                  ]
              )
              [ text "Dashboard" ]
          ]
            <> case mSelectedContractState of
              Just state ->
                [ icon_ Icon.Next
                , tooltip "Go to dashboard" (RefId "goToDashboard") Bottom
                , span_
                    [ text nickname
                    ]
                ]
                where
                nickname = Execution.contractName state
              Nothing -> []
    ]

dashboardFooter :: forall p. HTML p Action
dashboardFooter =
  footer
    [ classNames [ "hidden", "md:block", "border-t", "border-gray" ] ]
    [ div
        [ classNames $ Css.maxWidthContainer <>
            [ "flex", "justify-between", "py-2", "text-sm" ]
        ]
        [ nav
            [ classNames [ "flex", "-ml-4" ] ] -- -ml-4 to offset the padding of the first link
            dashboardLinks
        , nav
            [ classNames [ "flex", "-mr-4" ] ] -- -mr-4 to offset the padding of the last link
            iohkLinks
        ]
    ]

dashboardLinks :: forall p. Array (HTML p Action)
dashboardLinks =
  -- FIXME: SCP-2589 Add link to Docs
  [ link "Docs" ""
  , link "marlowe-finance.io" "https://marlowe-finance.io"
  , link "play.marlowe-finance.io" "https://play.marlowe-finance.io"
  {- disabled for phase 1, link "Market" ""
  , link "Support" "" -}
  ]

iohkLinks :: forall p. Array (HTML p Action)
iohkLinks =
  [ link "cardano.org" "https://cardano.org"
  , link "iohk.io" "https://iohk.io"
  ]

link :: forall p. String -> String -> HTML p Action
link label url =
  a
    [ classNames [ "px-4", "py-2", "font-bold", "cursor-pointer" ]
    , href url
    ]
    [ text label ]

------------------------------------------------------------
contractsScreen
  :: forall m. MonadAff m => Slot -> State -> ComponentHTML Action ChildSlots m
contractsScreen currentSlot state =
  let
    contractFilter = view _contractFilter state
  in
    -- This convoluted combination of absolute and relative elements is the only way I could find
    -- to get the contract navigation element to be have a fixed position relative to a max-width
    -- container, at the same time as having the vertical scroll appear to be on the whole div.
    -- If not for the scroll, it could have just had position absolute (with a relative parent).
    -- And if not for the max width container, it could have just had position fixed.
    div
      [ classNames [ "h-full", "relative" ] ]
      [ div
          -- overflow-x here can occur when the sidebar is open
          [ classNames
              [ "absolute"
              , "z-10"
              , "inset-0"
              , "h-full"
              , "overflow-y-auto"
              , "overflow-x-hidden"
              ]
          ]
          [ div
              [ classNames $ Css.maxWidthContainer <> [ "relative", "h-full" ] ]
              [ contractCards currentSlot state ]
          ]
      , div
          [ classNames [ "absolute", "inset-0" ] ]
          [ div
              [ classNames $ Css.maxWidthContainer <> [ "relative", "h-full" ] ]
              [ contractNavigation contractFilter ]
          ]
      ]

contractNavigation
  :: forall m. MonadAff m => ContractFilter -> ComponentHTML Action ChildSlots m
contractNavigation contractFilter =
  let
    navClasses =
      [ "inline-flex"
      , "gap-4"
      , "overflow-hidden"
      , "px-3"
      , "lg:px-0"
      , "lg:py-3"
      , "lg:flex-col"
      , "bg-white"
      , "rounded"
      , "shadow"
      ]

    navItemClasses active =
      [ "leading-none"
      , "pt-2+2px"
      , "pb-2"
      , "border-b-2"
      , "lg:py-0"
      , "lg:pr-2+2px"
      , "lg:pl-2"
      , "lg:border-b-0"
      , "lg:border-l-2"
      , "border-transparent"
      ] <> applyWhen active [ "border-black" ]
  in
    div
      [ classNames
          [ "absolute"
          , "z-20"
          , "left-4"
          , "bottom-4"
          , "right-4"
          , "lg:right-auto"
          , "lg:top-0"
          , "grid"
          , "grid-cols-1fr-auto-1fr"
          , "lg:grid-cols-none"
          , "lg:grid-rows-1fr-auto-1fr"
          ]
      ]
      [ div
          [ classNames
              [ "row-start-1"
              , "col-start-2"
              , "lg:row-start-2"
              , "lg:col-start-1"
              ]
          ]
          [ nav
              [ classNames navClasses ]
              [ a
                  [ classNames $ navItemClasses $ contractFilter == Running
                  , onClick_ $ SetContractFilter Running
                  , id "runningContractsFilter"
                  ]
                  [ icon_ Icon.Running ]
              , tooltip "Running contracts" (RefId "runningContractsFilter")
                  Right
              , a
                  [ classNames $ navItemClasses $ contractFilter == Completed
                  , onClick_ $ SetContractFilter Completed
                  , id "completedContractsFilter"
                  ]
                  [ icon_ Icon.History ]
              , tooltip "Completed contracts" (RefId "completedContractsFilter")
                  Right
              , a
                  [ classNames $ navItemClasses false
                  , onClick_ $ OpenCard ContractTemplateCard
                  , id "newContractButton"
                  ]
                  [ icon Icon.AddBox [ "text-purple" ] ]
              , tooltip "Create a new contract" (RefId "newContractButton")
                  Right
              ]
          ]
      , div
          [ classNames
              [ "row-start-1", "col-start-1", "lg:row-start-3", "lg:self-end" ]
          ]
          [ nav
              [ classNames navClasses ]
              [ a
                  [ classNames $ navItemClasses false
                  , onClick_ $ OpenCard TutorialsCard
                  , id "tutorialsButton"
                  ]
                  [ icon Icon.Help [ "text-purple" ] ]
              , tooltip "Tutorials" (RefId "tutorialsButton") Right
              ]
          ]
      ]

contractCards
  :: forall m. MonadAff m => Slot -> State -> ComponentHTML Action ChildSlots m
contractCards
  currentSlot
  { walletCompanionStatus, contractFilter: Running, contracts } =
  case walletCompanionStatus of
    WalletCompanionSynced ->
      let
        -- FIXME-3208: Change the Dashboard state to include two Maps/List, one for
        --        runningContracts and one for completedContracts
        -- runningContracts = filter (not isContractClosed) contracts
        runningContracts = filter (const true) contracts
      in
        if isEmpty runningContracts then
          noContractsMessage Running
        else
          contractGrid currentSlot Running runningContracts
    WaitingToSync ->
      div
        [ classNames
            [ "h-full", "flex", "flex-col", "justify-center", "items-center" ]
        ]
        [ icon Icon.Contract [ "text-big-icon", "text-gray" ]
        , p
            [ classNames [ "flex", "items-center", "gap-2" ] ]
            [ icon Icon.Sync [ "animate-spin" ]
            , text "Checking for new contracts..."
            ]
        ]

contractCards currentSlot { contractFilter: Completed, contracts } =
  let
    -- FIXME-3208: Same as `runningContracts`
    -- completedContracts = filter isContractClosed contracts
    completedContracts = filter (const false) contracts
  in
    if isEmpty completedContracts then
      noContractsMessage Completed
    else
      contractGrid currentSlot Completed completedContracts

noContractsMessage :: forall p. ContractFilter -> HTML p Action
noContractsMessage contractFilter =
  div
    [ classNames
        [ "h-full", "flex", "flex-col", "justify-center", "items-center" ]
    ]
    $ [ icon Icon.Contract [ "text-big-icon", "text-gray" ] ]
        <> case contractFilter of
          Running ->
            [ p
                [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
                [ text "You have no running contracts." ]
            , p
                [ classNames [ "text-lg", "mb-4" ] ]
                [ text "Choose a template to begin." ]
            , button
                [ classNames Css.primaryButton
                , onClick_ $ OpenCard ContractTemplateCard
                ]
                [ text "Choose a template" ]
            ]
          Completed ->
            [ p
                [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
                [ text "You have no completed contracts." ]
            ]

contractGrid
  :: forall m
   . MonadAff m
  => Slot
  -> ContractFilter
  -> Map PlutusAppId Contract.State
  -> ComponentHTML Action ChildSlots m
contractGrid currentSlot contractFilter contracts =
  div
    [ classNames
        [ "grid"
        , "pt-4"
        , "pb-20"
        , "lg:pb-4"
        , "gap-8"
        , "auto-rows-min"
        , "mx-auto"
        , "max-w-contracts-grid-sm"
        , "md:max-w-none"
        , "md:w-contracts-grid-md"
        , "md:grid-cols-2"
        , "lg:w-contracts-grid-lg"
        , "lg:grid-cols-3"
        ]
    ]
    $
      case contractFilter of
        Running -> [ newContractCard ]
        Completed -> []
        <> (dashboardContractCard <$> toUnfoldable contracts)
  where
  newContractCard =
    a
      [ classNames
          [ "hidden"
          , "md:flex"
          , "flex-col"
          , "justify-center"
          , "items-center"
          , "rounded"
          , "border-2"
          , "border-darkgray"
          , "border-dashed"
          , "p-4"
          ]
      , onClick_ $ OpenCard ContractTemplateCard
      ]
      [ icon_ Icon.AddCircle
      , span_ [ text "New smart contract from template" ]
      ]

  dashboardContractCard (followerAppId /\ contractState) =
    mapComponentAction (ContractAction followerAppId) $ contractPreviewCard
      currentSlot
      contractState

currentWalletCard :: forall p. PABConnectedWallet -> HTML p Action
currentWalletCard wallet =
  let
    walletNickname = view _walletNickname wallet

    address = view (_pubKeyHash <<< _PaymentPubKeyHash)
      wallet

    assets = view _assets wallet

    syncStatus = view _syncStatus wallet

    copyAddress = ClipboardAction <<< Clipboard.CopyToClipboard <<< A.toString
  in
    div
      [ classNames
          [ "h-full"
          , "grid"
          , "grid-rows-auto-1fr-auto"
          , "divide-y"
          , "divide-gray"
          ]
      ]
      [ h2
          [ classNames Css.cardHeader ]
          [ text "My wallet" ]
      , div
          [ classNames
              [ "p-4", "overflow-y-auto", "overflow-x-hidden", "space-y-4" ]
          ]
          [ h3
              [ classNames [ "font-semibold", "text-lg" ] ]
              [ text $ WN.toString walletNickname ]
          , copyAddress <$> Address.render
              (Address.defaultInput $ A.fromPubKeyHash address)
          , div_
              [ h4
                  [ classNames [ "font-semibold" ] ]
                  [ text "Balance:" ]
              , p
                  [ classNames Css.funds ]
                  [ text $ humanizeValue adaToken $ getAda assets ]
              ]
          , div [ classNames [ "space-y-2" ] ]
              [ h4
                  [ classNames [ "font-semibold" ] ]
                  [ text "Status:" ]
              , case syncStatus of
                  OutOfSync ->
                    p [ classNames [ "text-red" ] ] [ text "Out of sync" ]
                  Synchronizing progress ->
                    p []
                      [ text "Syncrhonizing ("
                      , text $ show $ round $ progress * 100.0
                      , text "%)"
                      ]
                  Synchronized ->
                    p [ classNames [ "text-green" ] ]
                      [ text "Syncrhonized" ]
              ]
          ]
      , div
          [ classNames [ "p-4", "flex", "gap-4" ] ]
          [ button
              [ classNames $ Css.secondaryButton <> [ "flex-1" ]
              , onClick_ CloseCard
              ]
              [ text "Cancel" ]
          , button
              [ classNames $ Css.primaryButton <> [ "flex-1" ]
              , onClick_ DisconnectWallet
              ]
              [ text "Drop wallet" ]
          ]
      ]

-- FIXME: add a proper tutorials card (possibly a whole tutorials module)
tutorialsCard :: forall p. HTML p Action
tutorialsCard =
  div
    [ classNames [ "p-4" ] ]
    [ h2
        [ classNames [ "font-semibold", "text-lg", "mb-4" ] ]
        [ text "Tutorials" ]
    ]
