Run several commands in parallel
================================

`par` is a small utility that runs multiple commands in parallel and
by default exits with a failure status of a first failure it sees.

Use `--help` for command-line help.

Basic usage example
-------------------

    > par "echo foo; sleep 1; echo foo; sleep 1; echo foo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
    foo
    bar
    bar
    foo
    bar
    foo
    success
    > par "echo foo; sleep 1; foofoo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
    bar
    foo
    bar
    /bin/sh: foofoo: command not found
    bar

Adding prefix to output
-----------------------

    > par "PARPREFIX=[fooechoer] echo foo" "PARPREFIX=[bar] echo bar"
    [fooechoer] foo
    [bar] bar

Force success exit-code
-----------------------

    > par --succeed "foo" "bar" && echo 'wow'
    /bin/sh: foo: command not found
    /bin/sh: bar: command not found
    wow

Installation
------------

For Ubuntu 12.04, 14.04 and MacOS X download some release and put it
into $PATH. For others -- see "building from source" instructions.

https://github.com/k-bx/par/releases

Example:

```
cd /tmp
wget https://github.com/k-bx/par/releases/download/1.0.1/par-ubuntu-12.04
sudo mv ./par-ubuntu-12.04 /usr/local/bin/
```

Building from source
--------------------

1. Install [haskell stack tool](https://github.com/commercialhaskell/stack)
2. Run `stack install`. It'll build and install tool into `~/.local/bin/par`

Footnote on strings in bash/zsh
-------------------------------

Many people know that strings in bash and zsh are "weird", but not
many people know that there are good old ASCII-strings also present.

Double-quoted strings are interpolating variables and do other interesting
things like reacting on "!" sign, for example.

Single-quotes don't interpolate variables and don't react on "!" sign, but
they also don't let you quote neither single-quote nor double-quote.

Turns out good old ASCII-quotes are available as $'string' syntax! Example:

    > echo $'foo'
    foo
    > echo $'foo with "doublequotes and \'singletuoes\' inside"!'
    foo with "doublequotes and 'singletuoes' inside"!

You are a better person with this knowledge now. $'Enjoy!'
