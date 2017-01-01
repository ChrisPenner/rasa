Slate
=====
See the docs on [Hackage](hackage.haskell.org/package/rasa-ext-slate)

**Slate** is a **Renderer** for Rasa, that basically means that it helps you
see and interact with the editor. That all sounds impressive, but behind the
scenes it actually operates just like any other extension; it can listen for
and respond to events, and can dispatch events in response to user actions.

There's nothing special about the Slate renderer, it just happens to be the first
one. You could write your own renderer or event listener if you like! In fact
you could even write a separate extension to listen for events from the extension
that renders to screen! Or mix and match more than one renderer and event source;
Endless possibilities!

There are a few things that Renderers do that *most* other extensions don't;
for one they tend to display things to screen (either the terminal, or by 
sending data to a GUI app). Renderers also typically listen for user
interactions and fire events as a result. For example Slate listens for user
keypresses and dispatches an event each time a key is pressed.

Slate itself is implemented using the
[vty](http://hackage.haskell.org/package/vty) terminal library. It uses vty to
listen for keypresses and print text to the terminal. When the `OnRender` event
fires vty takes the current application state and renders it out to the screen.
There's nothing special about the way this happens, it's just regular ol' `IO`.

Feel free to look around and see how slate does things!
