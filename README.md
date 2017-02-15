Rasa (Rah-zah)
==============

[![Join the chat at https://gitter.im/rasa-editor/Lobby](https://badges.gitter.im/rasa-editor/Lobby.svg)](https://gitter.im/rasa-editor/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Hackage](https://img.shields.io/badge/hackage-latest-green.svg)](https://hackage.haskell.org/package/rasa)

Embarrassingly modular customizable text editor built in Haskell.

![Rasa Editor](https://github.com/ChrisPenner/rasa/blob/master/docs/rasa.png "Rasa Editor")

A Rasa editing session with multiple cursors & viewports.

Documentation
-------------
You can find hackage documentation for rasa and some extensions here:

- [rasa](https://hackage.haskell.org/package/rasa)
- [rasa-ext-slate](https://hackage.haskell.org/package/rasa-ext-slate)
- [rasa-ext-views](https://hackage.haskell.org/package/rasa-ext-views)
- [rasa-ext-vim](https://hackage.haskell.org/package/rasa-ext-vim)
- [rasa-ext-cmd](https://hackage.haskell.org/package/rasa-ext-cmd)
- [rasa-ext-cursors](https://hackage.haskell.org/package/rasa-ext-cursors)
- [rasa-ext-files](https://hackage.haskell.org/package/rasa-ext-files)
- [rasa-ext-logger](https://hackage.haskell.org/package/rasa-ext-logger)
- [rasa-ext-status-bar](https://hackage.haskell.org/package/rasa-ext-status-bar)
- [rasa-ext-style](https://hackage.haskell.org/package/rasa-ext-style)

What people are saying
----------------------

> Excessively Modular! - some bald guy

> I'm glad I'm unemployed so I have time to configure it! - my mate Steve

> You should go outside one of these days. - Mother

Getting started
---------------

### Configuring Rasa

Rasa is designed to be easy to configure and script, both when adding extensions provided
by the community, and when writing your own user-scripts.

Rasa is written in Haskell, and the configuration is done in the Haskell
language, don't let that scare you though, you can script Rasa and add
extensions without knowing much haskell!

### [Building Your First Extension](https://github.com/ChrisPenner/rasa/blob/master/docs/Building-An-Extension.md)

\^ That guide will walk you through installation and getting running! Once
you're running rasa you can experiment with creating your own adaptations. You
should customize your keymap to add a few mappings you like. It's a short step
from here to developing your own extensions. `Action`s like you'd use in an
extension can be registered to listeners in your `Main.hs`. You can build and
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
- Listening for keyboard events
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
    We'll likely develop a solution to this as a community as time goes on.
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

### Event Listener System

All actions in the editor are triggered via an event/listener system.
Extensions may subscribe to events from the editor, or from another extension
and perform an action in response. The Event which triggered the listener is
available as an argument). Extensions may also dispatch any kind of event at
any time which other extensions may listen for.

### Actions/BufActions

Extensions define things that they'd like to do using a powerful set of
functions which they can embed in an `Action`. Within an action an extension
may perform IO, access the available buffers, store and access extension state,
and edit text.

Running Tests
-------------

Run all tests:

- `stack test`

Run only tests for core editor:

- `stack test rasa`


Installation
------------

At the moment you must build Rasa from source;

To provide reproducible builds, Rasa uses Stack & Nix.

1. Install [stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
2. Install [nix](https://nixos.org/nix/)
3. Clone this repo and `cd` into the directory
4. Run `stack build && stack exec rasa` (you may want to alias this to `rasa`)

### Troubleshooting

If you have issues with nix; you may try running rasa without it with `stack build --no-nix && stack exec rasa`;
You'll likely have to consider the following:

- You may need to install icu4c (`brew install icu4c`), it's a dependency of the rope library rasa uses.
- On linux, when the error message `Missing C libraries: icuuc, icui18n, icudata` appears, install `libicu-dev` (e.g. with `sudo apt install libicu-dev`).
- You'll need to point to the icu4c lib in your stack.yaml wherever it's stored on your system. If you install
    using brew on your Mac, then you can add the following to your stack.yaml:

```yaml
extra-lib-dirs:
- /usr/local/opt/icu4c/lib 
extra-include-dirs:
- /usr/local/opt/icu4c/include
```

- Depending on which LTS you're on, you'll likely also have to add each rasa package you use to your stack.yaml as
    extra-deps, here's an example:

```yaml
# in stack.yaml
extra-deps:
- rasa-0.1.0.0
- rasa-ext-cursors-0.1.0.0
- rasa-ext-logger-0.1.0.0
- rasa-ext-status-bar-0.1.0.0
- rasa-ext-vim-0.1.0.0
- text-lens-0.1.0.0
- rasa-ext-files-0.1.0.0
- rasa-ext-cmd-0.1.0.0
- rasa-ext-slate-0.1.0.0
- rasa-ext-style-0.1.0.0
- vty-5.14
```

Contributing
------------

Things are moving quickly, but I'd love a hand! You can get a rough idea of
where you can help out at the
[Roadmap](https://github.com/ChrisPenner/rasa/issues/2), feel free to leave a
comment there asking any questions. 

Chatting about features is a key part of Rasa's development; come join us in
the [Chat Room](https://gitter.im/rasa-editor/Lobby)!
