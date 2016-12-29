Rasa (Rah-zah)
==============

[![Join the chat at https://gitter.im/rasa-editor/Lobby](https://badges.gitter.im/rasa-editor/Lobby.svg)](https://gitter.im/rasa-editor/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Embarrassingly modular customizable text editor built in Haskell.

What people are saying
----------------------

> Excessively Modular! - some bald guy

> I'm glad I'm unemployed so I have time to configure it! - my mate Steve

> You should go outside one of these days. - Mother

Installation
------------

At the moment you must build Rasa from source;

1. Install [stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
2. Clone this repo and `cd` into the directory
3. Run `stack build && stack exec rasa` (you may want to alias this to `rasa`)

Getting started
---------------

Here's a great guide on building a copy-paste extension from scratch! I definitely
recommend checking it out!

### [Building Your First Extension](docs/Building-An-Extension.md)

\^ That guide will walk you through installation and getting running! Once
you're running rasa you can experiment with creating your own adaptations. You
should customize your keymap to add a few mappings you like. It's a short step
from here to developing your own extensions. `Action`s like you'd use in an
extension can be registered to hooks in your `Main.hs`. You can build and
experiment with entire extensions in your config file and extract them as a
package when you're ready, kind of like a vimrc file. Again, just read the 
extension guide, it covers what you need to know!

If you have any issues (and I'm sure there'll be a few; it's a new project!)
please report them [here](https://github.com/ChrisPenner/rasa/issues).


Core Principles
---------------

Rasa is meant to be about as modular as an editor can be. The goal is for as
much code as possible to be extracted into composable extensions. If the core
editing facilities can't be implemented as extensions, then the extension
interface isn't powerful enough. I've taken this to its extreme, for instance
the following features are implemented as rasa extensions that anyone in the
community could have written.

- Loading and saving files 
- Key bindings
- Multiple cursors
- Rendering the editor to the terminal

This approach has some unique pros and cons:

### Pros

-   Implementing most core functionality as extensions ensures a powerful and
    elegant extension interface.
-   Flexibility; don't like the default cursor implementation? Write your own!
-   Adaptability; the core of Rasa is miniscule, you can mix and match
    extensions to build any editor you want.


### Cons

-   Module cross-dependencies makes the community infrastructure more fragile;
    We'll likely have to develop a solution to this as a community as time
    goes on.
-   Fragmentation; Not having a single implementation for a given feature means
    extensions that depend on a feature have to pick a specific implementation
    to augment. Over time data-structures and types will be standardized into
    Rasa's core to help alleviate this.


Core Features
-------------

As stated above, the editor itself focuses primarily on easy extensibility, so it doesn't have a lot of editing
features built in, instead it focuses on standardizing a good extension API.
We focus on creating a simple system so people can pick it up quickly.

Here are some features of that API:

### Event Hook System

All actions in the editor are triggered via an event/listener system.
Extensions may subscribe to events from the editor, or from another extension
and perform an action in response. The Event which triggered the hook is
available as an argument). Extensions may also dispatch any kind of event at
any time which other extensions may listen for.

### Actions/BufActions

Extensions define things that they'd like to do using a powerful set of
functions which they can embed in an `Action`. Within an action an extension
may perform IO, access the available buffers, store and access extension state,
and edit text.

Contributing
============

Things are moving quickly, but I'd love a hand! You can get a rough idea of
where you can help out at the
[Roadmap](https://github.com/ChrisPenner/rasa/issues/2), feel free to leave a
comment there asking any questions, I'm often free to chat, join our [gitter
here](https://gitter.im/rasa-editor/Lobby)!
