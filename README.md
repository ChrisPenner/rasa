Rasa
====

Exessively modular customizable text editor built in Haskell using Lens
combinators

What people are saying
----------------------

> Excessively Modular! - some bald guy

> I'm glad I'm unemployed so I have time to configure it! - my mate Steve

> Get that camera out of my face. - Mother

Core Principles
---------------

Rasa is meant to be about as modular as an editor can be, I mean it. the goal
is for as much code as possible to be extracted into composable extensions. If
the core editing facilities can't be implemented as extensions, then the
extension interface isn't powerful enough. I've taken this to its extreme, for
instance all the following tasks are simply extensions just like anyone could
write.

- Loading and saving files 
- Keybindings
- Syntax Highlighting
- Even the regular everyday cursor is just an extension!

This of course can result in a fractured eco-system really quickly so I'll
likely define a few standard typeclasses for things like cursor support so we
can all work on the same page.

Note that extensions can be dependent on (and import) other extensions, this
allows sharing of the workload, why implement multiple cursor support again
when some other extension has done it already?

Rasa exposes combinators and utilities from a small library of "Text Lenses"
that I've written. These lenses are available to plugins and provide high-level
abstractions for editing and navigating text. I'll be breaking it out into its
own independent library soon. A sampling of some operations it allows are:

- Perform function over text after the cursor
- Edit the text after index 10 and before index 20
- Alter every occurance of the string "Hello"
- Move the cursor to the next occurance of "World"

These base level operations can be combined in interesting ways, for example I've
built a small extension which emulates many of the basic Vim commands. The
extension interface allows you to combine the to express things like:

- Delete until end of line: `set (tillNext "\n") ""`
- Uppercase current word: `over currentWord (fmap toUpper)`

If the library is missing a combinator you need, open a PR! Let's keep making
it better.

Most text-editing is implemented through the use of lenses, for any given
operation over a block of text a lens (or composition of lenses) is defined to
'get' or 'set' that block of text, then any operation may be performed 'over'
that block of text. This allows us to maintain generality, allowing 'what' and
'how' to be specified separately and be re-used between commands.

For example a 'motion' may be defined as the 'next paragraph'. This lens could be defined as a composite lens using
other primitive lenses as follows:

```haskell
after :: Int -> Lens' T.Text T.Text
tillNext :: T.Text -> Lens' T.Text T.Text

nextParagraph :: T.Text T.Text
nextParagraph = tillNext "\n"
```

We then lift this lens using combinators to work over a Buffer (which has a cursor) to allow us to run it on the
text *after* the cursor

```haskell
withCursor :: (Int -> Lens' T.Text T.Text) -> Lens' (Buffer Offset) T.Text
after :: Int -> Lens' T.Text T.Text

nextParagraphInBuf :: Lens' (Buffer Offset) T.Text
nextParagraphInBuf = withCursor after.tillNext "\n"
```

In this case withCursor is a (slightly confusing) combinator which selects the cursor from the buffer and passes it to
the given function, creating a new lens which is contextualized using the cursor of the buffer.

Walking through this example, we say that we first want to select the text *after* the buffer's cursor: `withCursor
after` then we want to select the text until the next newline `tillNext "\n"`. Note that this is simply a lens and says
nothing about *what* we want to do to that text. Some things we may want to do:

```haskell
deleteNextParagraph = set nextParagraphInBuf ""
upperCaseNextParagraph = over nextParagraphInBuf T.toUpper
lengthOfNextParagraph = view $ nextParagraphInBuf.to T.length
```

Hopefully you can see that a lens based interface is quite a powerful abstraction. Note that with a bit of cleverness
you can even write lenses that operate over non-contiguous chunks of text:

```haskell
matching :: T.Text -> Lens' T.Text T.Text
-- This lens gets/sets all instances of 'pat' in a text where 'pat' is a Text.
matching pat = lens getter setter
          -- Getting all matches is equivalent to getting a list of n copies where n is the number of matches
    where getter = (`T.replicate` pat) . T.count pat
          -- Setting all the matches is the same as replacing them with the new value
          setter old new = T.replace pat new old
```

Note that almost all of these are *not* proper lenses since they edit the structure and length of the text, but in
practice they work just fine.


Installation
------------

At the moment you must build rasa from source, 

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone this repo as the `rasa` directory
3. Inside `rasa` run `stack build`
4. Run with `stack exec rasa-exe`

Contributing
============

Things are moving quickly, but I'd love a hand! You can get a rough idea of where
you can help out at the [Roadmap](https://github.com/ChrisPenner/rasa/issues/2), feel free to leave a comment there
asking any questions, I'm always free to chat.

Using Rasa
----------

At the moment things are changing very quickly, but you can get a rough idea of
the controls from
[here](https://github.com/ChrisPenner/rasa/blob/master/src/Directives.hs).
