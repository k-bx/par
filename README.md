Par – utility that runs commands in parallel
============================================

`par` is a small utility that runs multiple commands in parallel and
by default exits with a failure status of a first failure it sees.

Example usage:

```
➜  ~  ~/workspace/par/dist/build/par/par "echo foo; sleep 1; echo foo; sleep 1; echo foo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
foo
bar
bar
foo
bar
foo
success
➜  ~  ~/workspace/par/dist/build/par/par "echo foo; sleep 1; foofoo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
bar
foo
bar
/bin/sh: foofoo: command not found
bar
```

Installation
------------

TODO

Building from source
--------------------

1. [Install Haskell's GHC compiler](http://www.stackage.org/install)
2. run `make`

Executable will be inside `./dist/build/par/par`
