-- File auto generated by purescript-bridge! --
module Wallet.Emulator.Wallet where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(Proxy))

newtype Wallet = Wallet { getWalletId :: String }

derive instance eqWallet :: Eq Wallet

instance showWallet :: Show Wallet where
  show a = genericShow a

instance encodeJsonWallet :: EncodeJson Wallet where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { getWalletId: E.value :: _ String }
    )

instance decodeJsonWallet :: DecodeJson Wallet where
  decodeJson = defer \_ -> D.decode $
    (Wallet <$> D.record "Wallet" { getWalletId: D.value :: _ String })

derive instance genericWallet :: Generic Wallet _

derive instance newtypeWallet :: Newtype Wallet _

--------------------------------------------------------------------------------

_Wallet :: Iso' Wallet { getWalletId :: String }
_Wallet = _Newtype