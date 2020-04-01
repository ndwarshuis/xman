# XMan
A daemon to manage [xcape](https://github.com/alols/xcape) instances based on
which app is in focus.

Xcape allows one to configure any key to send an arbitrary key upon release (for
instance, the Control key can function as itself on keypress and an Escape key
on key release).

This works beautifully except when one tries to switch to a virtual machine
(VM), particularly a windows machine with an Autohotkey script to replicate the
keymap in the host machine. In this case, any xcape'd keys will be sent twice.
The obvious (and somewhat crude solution) is to start and stop xcape depending
on which apps have focus.

# Usage

```
xcape-hs [-t TIMEOUT] BINDINGS REGEXP [[REGEXP] ...]
```

Where `TIMEOUT` and `BINDINGS` are the timeout argument and the key bindings to
be passed to the `-t` and `-b` flags respectively in `xcape` and `REGEXP` is a
POSIX-style regular expression matching a window app name for which xcape will
not be running when it is in focus. To find the app name for a window, use the
`xprop` program and look for the first member of the `WM_CLASS` atom.

# Dependencies

* xcape

# Installation

Clone this repo and run `stack install`.
