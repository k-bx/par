Par – utility that runs commands in parallel
============================================

`par` is a small utility that runs multiple commands in parallel and
by default exits with a failure status of a first failure it sees.

Example usage:

```
➜  ~  par "echo foo; sleep 1; echo foo; sleep 1; echo foo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
foo
bar
bar
foo
bar
foo
success
➜  ~  par "echo foo; sleep 1; foofoo" "echo bar; sleep 1; echo bar; sleep 1; echo bar" && echo "success"
bar
foo
bar
/bin/sh: foofoo: command not found
bar
```

Installation
------------

For Ubuntu 12.04, 14.04 and MacOS X download some release and put it into PATH. For others -- see "building from source" instructions.

https://github.com/k-bx/par/releases

Building from source
--------------------

1. [Install Haskell's GHC compiler](http://www.stackage.org/install)
2. run `make`

Executable will be inside `./dist/build/par/par`

Please note that `par` uses specific set of versions to build, setting
them via cabal.config file. If you want to remove these constraints,
just remove the file.
