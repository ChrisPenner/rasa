module State (
    St
  , focusedBuf
  , focused
  , buffers
  , exiting
) where

import Control.Lens
import Data.Default (def, Default(..))

import Buffer

data St = St {
    _buffers :: [Buffer Offset]
  , _focused :: Int
  , _exiting :: Bool
}

makeLenses ''St

instance Default St where
    def = St {
            _buffers=fmap buffer ["Buffer 0\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?", "Buffer 1"]
          , _focused=0
          , _exiting=False
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

