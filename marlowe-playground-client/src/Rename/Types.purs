module Rename.Types where

import Prologue

import Analytics (class IsEvent)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

data Action
  = ChangeInput String
  | SaveProject

instance isEventAction :: IsEvent Action where
  toEvent (ChangeInput _) = Nothing
  toEvent SaveProject = Just
    { category: Just "Rename"
    , action: "SaveProject"
    , label: Nothing
    , value: Nothing
    }

type State
  =
  { projectName :: String
  , error :: Maybe String
  }

emptyState :: State
emptyState = { projectName: "New Project", error: Nothing }

_error :: Lens' State (Maybe String)
_error = prop (Proxy :: _ "error")
