module State (
    focusedBuf
  , Offset
  , Coord(..)
) where

import Data.Monoid
import Control.Lens
import Data.Default (def, Default(..))
import qualified Data.Text as T
import qualified Data.Vault.Lazy as Vlt

import Buffer
import Types

buffer :: T.Text -> Buffer Offset
buffer t = Buffer {
        _text=t
      , _cursor=0
}

instance Default St where
    def = St {
            _buffers=fmap buffer ["Buffer 0\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?", "Buffer 1"]
          , _focused=0
             }


focusedBuf :: Lens' St (Buffer Offset)
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            view (buffers. to (!! foc))

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a
