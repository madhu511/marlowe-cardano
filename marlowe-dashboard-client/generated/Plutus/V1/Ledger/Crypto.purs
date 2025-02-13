-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Crypto where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
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

newtype PubKey = PubKey { getPubKey :: String }

derive instance Eq PubKey

derive instance Ord PubKey

instance Show PubKey where
  show a = genericShow a

instance EncodeJson PubKey where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { getPubKey: E.value :: _ String }
    )

instance DecodeJson PubKey where
  decodeJson = defer \_ -> D.decode $
    (PubKey <$> D.record "PubKey" { getPubKey: D.value :: _ String })

derive instance Generic PubKey _

derive instance Newtype PubKey _

--------------------------------------------------------------------------------

_PubKey :: Iso' PubKey { getPubKey :: String }
_PubKey = _Newtype

--------------------------------------------------------------------------------

newtype Signature = Signature { getSignature :: String }

instance Show Signature where
  show a = genericShow a

derive instance Eq Signature

derive instance Ord Signature

instance EncodeJson Signature where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { getSignature: E.value :: _ String }
    )

instance DecodeJson Signature where
  decodeJson = defer \_ -> D.decode $
    (Signature <$> D.record "Signature" { getSignature: D.value :: _ String })

derive instance Generic Signature _

derive instance Newtype Signature _

--------------------------------------------------------------------------------

_Signature :: Iso' Signature { getSignature :: String }
_Signature = _Newtype
