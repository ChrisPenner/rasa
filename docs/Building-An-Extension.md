Building Your First Rasa Extension
==================================

Glad you could join us! This will be a quick walkthrough of the steps involved
in building a simple extension. After completing this tutorial you should feel
comfortable building something of your own! Rasa is moving fast, so it's likely
this will go out of date from time to time, if you notice that something seems
a bit off; feel free to open an issue
[here](https://github.com/ChrisPenner/rasa/issues), or hop into our [gitter
chat](https://gitter.im/rasa-editor/Lobby)!

Downloading and Setting up Rasa
===============================

Rasa is a Haskell project, configured in Haskell; so we're going to need a
basic Haskell working environment if we're going to get anywhere. This guide
assumes at least a basic familiarity with both Haskell & [Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html).
Go ahead and install Stack now, I'll wait! (Psst! I'd recommend `brew update && brew install haskell-stack`). Make sure
you run `stack setup` before we get started.

Okay, so first clone the Rasa repository; go to your favourite project's
directory and run `git clone https://github.com/ChrisPenner/rasa`; this will
create a new `rasa` directory, go ahead and `cd rasa` into there.

Instead of going through the trouble of making a new package for our extension, we'll just
build the new functionality into our user-config. You can extract it later if you like!

Let's just edit the existing configuration for now, you can find it in `rasa-example-config/app/Main.hs`.
It should look something like this:

```haskell
import Rasa
-- ... some other imports
main :: IO ()
main = rasa [terminalEvents] $ do
  vim
  files
  cursors
  logger
  slate
  style
  -- some more plugins...
```

To get a feel for the different things we can do we'll be building
a simple copy-paste extension.

To get started let's add a simple action and see if we can even get it running. 
Let's make a new action to write "Hello world" to a logging file so we can see if it runs!

```haskell
helloWorld :: Action ()
helloWorld = writeFile "hello-world.txt" "Hello, World!"

main :: IO ()
main = rasa [terminalEvents] $ do
  -- some plugins...
  -- Add the new action here!
  helloWorld
```

Now we'll try to compile the project with `stack build && stack exec rasa`.

```
rasa/rasa-example-config/app/Main.hs:26:15: error:
    Not in scope: type constructor or class ‘Action’
```

Oops! Looks like the `Action` type isn't in scope, we'll have to import it.
Everything we'll need to build an extension is exported from `Rasa.Ext`, so
let's add that import line at the top with the other imports.

```haskell
import Rasa.Ext
```

Okay let's try again! `stack build && stack exec rasa` (you may want to alias that to something, we'll be running
it a lot!)

```haskell
rasa/rasa-example-config/app/Main.hs:28:14: error:
- Couldn't match expected type ‘Action ()’ with actual type ‘IO ()’
- In the expression: writeFile "hello-world.txt" "Hello, World!"
    In an equation for ‘helloWorld’:
        helloWorld = writeFile "hello-world.txt" "Hello, World!"
```

Oh of course! We've declared `helloWorld` to be type `Action ()`, which can only
contain statements of type `Action`. Luckily `Action` implements MonadIO so we can
embed IO actions in it by using the function `liftIO`. Don't worry too much if you don't
fully understand what's going on here, you can just follow along for now!

```haskell
-- new import at the top!
Control.Monad.IO.Class

-- Let's lift the writeFile call into an `Action`
helloWorld :: Action ()
helloWorld = liftIO $ writeFile "hello-world.txt" "Hello, World!"
```

Okay, that should take care of the IO error, let's run it again!

```haskell
rasa/rasa-example-config/app/Main.hs:28:3: error:
- Couldn't match type ‘Action’ with ‘Scheduler’
    Expected type: Scheduler ()
    Actual type: Action ()
```

Hrmm, what's that mean?
Looks like the rasa config doesn't take a list of `Action`s directly, I guess that makes
sense because how would Rasa know when we actually want to execute the actions?

The error says that it's expecting something of the type `Scheduler ()`.
I'd recommend reading more about it in the docs on Hackage, but basically it's 
just a wrapper around commands that say when to 'schedule' actions.

There's only a few commands we can use inside `Scheduler`, the main one is
`eventListener` which takes a function which uses some event to generate and action, and it registers it to be run
any time that action occurs. There are also a few handy wrappers around `eventListener` for 
some of the built-in rasa events. Let's use one called `onInit` now!

`onInit` has the signature `onInit :: Action () -> Scheduler ()`, which
suggests that it takes an action and 'schedules' it to run 'on init'. Which is
exactly what it does! Anything we pass to onInit will run when rasa starts up!
In reality it just sets up an eventListener for the `Init` event which is dispatched when rasa starts up, but it's
a nifty helper to make things simpler. Let's use it to schedule our `helloWorld` Action.

```haskell
main = rasa [terminalEvents] $ do
  -- other extensions
  onInit helloWorld
```

If we've done this right, then it when we rebuild the editor should compile and
start up!
Okay, so how can we see whether it worked? If you're using the Vim key-bindings then you can quit using `Ctrl-C`
and check your directory to see if you've got any new files!

```sh
$ cat hello-world.txt
Hello, World!
```

Awesome! It's working! Okay, we can run an action! what's next? I suppose it
would be nice to run an action in response to some keypress right? Let's change `helloWorld` into a function which
takes a `Keypress` and writes the pressed key to a file!

```haskell
helloWorld :: Keypress -> Action ()
helloWorld (Keypress char _) = liftIO $ appendFile "keylog.txt" ("You pressed: " ++ [char] ++ "\n")
```

Okay cool! Now it should record each keypress to the `keylog.txt` file!
Let's run it!

```
rasa/rasa-example-config/app/Main.hs:28:10: error:
- Couldn't match expected type ‘Action ()’
                with actual type ‘Keypress -> Action ()’
```

Hrmm, right! Now we don't want to use `onInit` anymore, now that we're listening for
a more general event we can use `eventListener`! Let's look at the type signature:

```haskell
eventListener :: forall a. Typeable a => (a -> Action ()) -> Scheduler ()
```

Whoah, what?? It's tough to unpack exactly what's going on here, but the basic
idea is that you can see it takes a function from ANY event `a` to an
`Action ()` and then returns a `Scheduler`. There's a bit of magic behind the
scenes that stores the function and calls it on any events that come that match
the type which that function expects, running the resulting Action. Check out
the source code behind `eventListener` if you're interested! It's pretty cool!
Okay! So we've got our function from our event type (Keypress), so let's try
embedding it using `eventListener`

```haskell
main = rasa [terminalEvents] $ do
  -- other extensions
  eventListener helloWorld
```

Okay, let's build that and run it, now in a separate terminal we'll run
`tail -f keylog.txt` to watch the key events come in! Go back to the editor and
hit a few keys and you should see them come streaming in! See how easy it is
to get things working?

Next up we'll try doing something useful. A key part of extensions in rasa is
that they can interact with other extensions! This means we can take advantage
of the work that others have done before us! The main way that we interact with
other extensions is to use `Action`s that they expose for us! 

We're doing a copy-paste sort of thing so we'll want to look at what the
cursors extension provides for us to use. If we take a look at the
documentation for `rasa-ext-cursors` we'll see a few things that we can do. We
should avoid accessing directly the state that the `cursors` extension has
stored, we should let it do the bookkeeping and just use the functions that
they expose for us. 

One thing that looks promising is `rangeDo`, it lets us run a function over
each range that the `cursors` extension has selected. `cursors` handles many
selections at the same time, which makes things a bit more complicated, but
let's just pretend for now that it only handles one at a time. `rangeDo` has
the following signature:

```haskell
rangeDo :: (Range -> BufAction a) -> BufAction [a]
```

A `Range` is a construct that's part of Rasa, it represents a range of text
within a buffer. We can see that it takes a function which accepts a range and
returns a `BufAction`, and runs it on each range, returning a list of the
results inside a `BufAction`. We can use this for our copy-paste extension to
go through and copy the text in each range so we can paste it later! To see if
we can get it working, first let's write a basic version that just writes out the 
contents of each range to a file! Things get a bit tougher here, but we're
going to power through it!

```haskell
-- Make sure we have these imports
import Rasa.Ext.Cursors
import Rasa.Control.Lens

main = rasa [terminalEvents] $ do
  -- other extensions
  cursors
  eventListener copyPasta

copyPasta :: Keypress -> Action ()
copyPasta (Keypress 'y' _) = focusDo $ rangeDo_ copier
  where
    copier :: Range -> BufAction ()
    copier r = do
      txt <- use (range r . asString)
      liftIO $ appendFile "copy-pasta.txt" ("You copied: " ++ txt ++ "\n")
copyPasta _ = return ()
```

Okay let's unpack it a bit! We're now listening specifically for the 'y'
character to be pressed, if anything else is pressed we'll just do a simple
`return ()` which does nothing. When someone presses 'y' then we we run
`focusDo` which lifts a `BufAction` into an `Action` which operates over only
the focused buffer. The `BufAction` that we're running is handled by
`rangeDo_`. You'll notice we're using `rangeDo_`, which is a version of
`rangeDo` which discards return values resulting in just `BufAction ()`.

As we discovered earlier `rangeDo` takes a function over a range, which we've
defined as `copier`. The next line `txt <- (use range r . asString)` may be new
to you even if you've written Haskell before, but it's using the [lens
library](https://hackage.haskell.org/package/lens) to access state from within
a `BufAction`. In this case we're using the `range` lens which accesses the
text in a buffer within a given range. There's a trick though, the text which
comes out is actually a `YiString` which comes from the `Yi.Rope` library, we
need to convert it to a string using `asString` so we can write it to file.
On the next line we just append the text to the `copy-pasta.txt` file.

Let's try it out. Hopefully it builds for you and you can test it out by moving
the cursor around and pressing 'y' to write out the current character to the
`copy-pasta.txt` file (you can watch it with `tail -f copy-pasta.txt`).

We've got a good start at this point! If you like you can add multiple cursors
using the cursors extension and should see that each cursor's contents is
copied to the file. The next thing we'll need to do is to store the last thing
we've copied so that we can paste it back later, we could just store it in a file,
but this gets tricky and inefficient for more complicated data-structures, so we'll
do it another way. Rasa provides a way for each extension to store state, in fact
you can store either state global to the entire editor, or we can store state local
to each buffer. In this case we want the latter, that way we can keep a different
copy-paste register for each buffer.





























There are a few other extensions included in this repo, so go ahead and take a
look at how they're doing things to get a few more ideas on how to do the
tricky stuff!
