module Rasa.Ext.Views
  ( viewports
  , viewable
  , splitRule
  , active
  , scrollPos
  , getViews
  , A.rotate
  , A.closeInactive
  , A.focusViewLeft
  , A.focusViewRight
  , A.focusViewAbove
  , A.focusViewBelow
  , A.hSplit
  , A.vSplit
  , A.addSplit
  , A.nextBuf
  , A.prevBuf
  , A.focusDo
  , A.focusDo_
  , A.focusedBufs
  , A.isFocused
  , A.scrollBy
  , Dir(..)
  , SplitRule(..)
  , Window
  , Split(..)
  , View(..)
  , Views(..)
  , BiTree(..)
  , BiTreeF(..)

  -- * Creating Widgets
  , Widgets
  , RenderWidgets(..)
  , addTopBar
  , addBottomBar
  , addLeftBar
  , addRightBar
  , topBar
  , bottomBar
  , leftBar
  , rightBar

  -- * Provided Widgets
  , enableLineNumbers
  , disableLineNumbers
  , toggleLineNumbers
  , checkLineNumbers
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.BiTree
import Rasa.Ext.Views.Internal.Views
import Rasa.Ext.Views.Internal.Widgets
import Rasa.Ext.Views.Internal.LineNumbers
import Rasa.Ext.Views.Internal.ActiveBar
import Rasa.Ext.Views.Internal.Actions as A

-- | Main export from the views extension, add this to your rasa config.
viewports :: Action ()
viewports = do
  onBufAdded_ A.addSplit
  lineNumbers
