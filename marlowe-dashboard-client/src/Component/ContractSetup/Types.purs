module Component.ContractSetup.Types where

import Data.Address (Address)
import Data.ContractNickname (ContractNickname)
import Data.ContractTimeout (ContractTimeout)
import Data.ContractValue (ContractValue)
import Data.Map (Map)
import Data.Set (Set)
import Halogen as H
import Halogen.Form.Input (FieldState)
import Marlowe.Extended.Metadata (NumberFormat)
import Marlowe.Semantics (TokenName)
import Type.Proxy (Proxy(..))

type Input =
  { templateRoles :: Set TokenName
  , templateTimeouts :: Map String ContractTimeout
  , templateValues :: Map String NumberFormat
  , templateName :: String
  , fields :: ContractFields
  }

data Msg
  = BackClicked
  | ReviewClicked ContractParams
  | FieldsUpdated ContractFields

data Query (a :: Type)

type Slot slot = H.Slot Query Msg slot

type Component m = H.Component Query Input Msg m

type ContractParams =
  { nickname :: ContractNickname
  , roles :: Map TokenName Address
  , timeouts :: Map String ContractTimeout
  , values :: Map String ContractValue
  }

type ContractFields =
  { nickname :: FieldState ContractNickname
  , roles :: Map TokenName (FieldState Address)
  , timeouts :: Map String (FieldState ContractTimeout)
  , values :: Map String (FieldState ContractValue)
  }

_nickname = Proxy :: Proxy "nickname"
_roles = Proxy :: Proxy "roles"
_timeouts = Proxy :: Proxy "timeouts"
_values = Proxy :: Proxy "values"
