# Rasa

Modular text editor built in Haskell.

[Here's the Roadmap](https://github.com/ChrisPenner/rasa/issues/2)

Core Principles
---------------

Rasa is meant to be about as modular as an editor an be, the goal is for as
much code as possible to be extracted into composable extensions. If the core
editing facilities can't be implemented as extensions, then the extension
interface isn't powerful enough.

Most functionality is implemented through the use of lenses, for any given operation over a block of text a lens
(or composition of lenses) is defined to 'get' or 'set' that block of text, then any operation may be performed
'over' that block of text. This allows us to maintain generality, allowing 'what' and 'how' to be specified
separately and be re-used between commands.

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


## Installation

At the moment you must build rasa from source, 

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone this repo as the `rasa` directory
3. Inside `rasa` run `stack build`
4. Run with `stack exec rasa-exe`

## Using Rasa

At the moment things are changing very quickly, but you can get a rough idea of the controls from [here](https://github.com/ChrisPenner/rasa/blob/master/src/Directives.hs).
