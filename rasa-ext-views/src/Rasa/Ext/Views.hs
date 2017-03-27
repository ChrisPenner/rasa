module Rasa.Ext.Views
  ( viewports
  -- * Working with Views
  , View(..)
  , Viewable(..)
  , ViewAction
  , viewable
  , splitRule
  , active
  , scrollPos

  -- * View Structure
  -- | Views are stored as a Tree, with 'Split's determining
  -- the layout of each branch.
  , Split(..)
  , Dir(..)
  , SplitRule(..)
  , Window
  , BiTree(..)
  , BiTreeF(..)
  , viewTree

  -- * Provided Actions
  , A.rotate
  , A.closeInactive
  , A.focusViewLeft
  , A.focusViewRight
  , A.focusViewAbove
  , A.focusViewBelow
  , A.addRenderableSplit
  , A.nextBuf
  , A.prevBuf
  , A.focusDo
  , A.focusDo_
  , A.focusedBufs
  , A.isFocused
  , A.activeViewsDo
  , A.activeViewsDo_
  , A.viewBufDo
  , A.viewBufDo_
  , scrollBy
  , hSplit
  , vSplit

  -- * Creating Widgets
  , Widgets

  -- | Lenses for accessing parts of 'Widgets':
  , topBar
  , bottomBar
  , leftBar
  , rightBar
  , HasWidgets(..)

  -- ** Providing Widgets
  -- | The following functions register a BufAction which yields some renderable;
  -- On each render that renderable will be used as a top/bottom/left/right bar respectively.
  , addTopBar
  , addBottomBar
  , addLeftBar
  , addRightBar

  -- * Provided Widgets
  , enableLineNumbers
  , disableLineNumbers
  , toggleLineNumbers
  , checkLineNumbers

  , addTopStatus
  , addBottomStatus
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.BiTree
import Rasa.Ext.Views.Internal.Views
import Rasa.Ext.Views.Internal.Widgets
import Rasa.Ext.Views.Internal.LineNumbers
import Rasa.Ext.Views.Internal.StatusBar
import Rasa.Ext.Views.Internal.Actions as A

-- | Main export from the views extension, add this to your rasa config.
viewports :: App ()
viewports = do
  onBufAdded_ A.autoAddBufSplit
  lineNumbers
