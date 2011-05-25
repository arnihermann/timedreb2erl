This document describes how to get the `timedreb2erl` tool up and running.

Follow the rest of this document in order to produce an executable file.

All commands should be executed from the project root, i.e. the directory this
README file lies in.

Requirements
============

+ GHC >= 6.12
+ cabal >= 1.8
+ McErlang

McErlang is required to be able to simulate the generated Erlang code.

Install
=======

> cabal install 

And now we have a fresh binary `timedreb2erl` in our cabal bin directory.

Usage
=====

Given a timed rebeca model, we can generate Erlang source code for the model
such that we can simulate it with McErlang.


> timedreb2erl [OPTIONS] FILE
> 
> Common flags:
>   -s --simulate        
>   -m --monitor         
>   -o --outputdir=FOLDER
>   -? --help              Display help message
>   -V --version           Print version information

If no `outputdir` is specified the resulting Erlang code is printed to stdout.

The `simulate` flag outputs code that can be run with McErlang. If it is
omitted, then refined Erlang code is outputted.

The `monitor` flag has no effect unless `outputdir` is used.
