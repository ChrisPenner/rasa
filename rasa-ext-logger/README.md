Logger
======

This is a basic logging extension, it's really quite useful for debugging when
working on extensions. If you add `logger` to your configuration then logger will
record a snapshot of the editor every time an event is fired into the file `logs.log`
in the directory where you run `rasa`. 

It also exports a few Actions which extensions can borrow to simplify their logging.

-   logInfo
-   logError

These functions simply write any strings passed to them to the `info.log` and
`error.log` files respectively.
