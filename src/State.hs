module State (
    focusedBuf
) where

import Control.Lens
import Data.Default (def, Default(..))

import Buffer
import Types

instance Default St where
    def = St {
            _buffers=fmap buffer ["Buffer 0\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?", "Buffer 1"]
          , _focused=0
             }


focusedBuf :: Lens' St (Buffer Offset)
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            -- TODO use ix here and make it safe??
            view (buffers.to (!! foc))

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a
