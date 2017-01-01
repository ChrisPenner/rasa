Vim
===

This extension more-or-less describes a Vim-style keymapping for Rasa. It's
also a good example of how to write your own keymap for Rasa.

A keymap isn't different from any other extension, it just listens for events
(keypresses in this case) and enacts changes on the editor as a result. To make
these changes a keymap depends on other extensions which export actions for
actually making changes. The cursors extension provides basic editing
capability, so we depend on that to do the dirty-work. It's recommended that
your rasa-config should only have one key-map extension; that prevents them
from stepping on each-other's toes.

The Vim keymap also stores a bit of state to keep track of which mode the user
is in; so that's an option too! Take a look around to see how it all works!
