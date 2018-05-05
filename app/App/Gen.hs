module App.Gen where

import Hedgehog

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

field :: MonadGen m => m String
field = G.string (R.linear 0 20) (G.choice [G.alphaNum, G.element "\r\n,\" \t"])
