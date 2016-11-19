module Extensions (extensions) where

import State (St, ExtensionState, VimSt)
import Directives (Directive)
import Ext.Vim (toDirective)

import Events (Event)

extensions :: [St -> Event -> (VimSt, [Directive])]
extensions = [toDirective]
