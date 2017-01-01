Configuring Rasa
================

This package represents a template for a working rasa configuration.

Rasa is designed to be easy to configure, both when adding extensions provided
by the community, and when writing your own user-scripts.

Rasa is written in Haskell, and the configuration is done in the Haskell
language, don't let that scare you though, you can script Rasa and add
extensions without knowing much haskell!

The basic configuration will look something like this:

```haskell
import Rasa
import Rasa.Ext.Vim
import Rasa.Ext.Cursors
import Rasa.Ext.Slate
import Rasa.Ext.Style

main :: IO ()
main = rasa $ do
  vim
  cursors
  slate
  style
```

You can see that we import a few extensions at the top, then just tell rasa
which ones we want to use. Easy peasy! Any extensions you add to this list
can dispatch or listen for events!

Now for the fun part, adding your own scripts and customizations!

It turns out that extensions are just organized groups of user-scripts, so if
you learn one you learn the other and there's a great [Rasa Extension
Guide](https://github.com/ChrisPenner/rasa/blob/master/docs/Building-An-Extension.md)
to bring you through the process! Give it a go!
