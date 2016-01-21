# blx.sortable

Solution to the [Sortable entity-resolution coding challenge](http://sortable.com/challenge/).

## Installation

This program uses Java and Leiningen.

If you don't have Leiningen installed, download the [`lein` script](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein), place it on your `$PATH` and make it executable.

## Usage

To run once, do

    $ lein run -- [OPTIONS]

in the root of this directory.

Alternatively, you can compile the program for faster repeated execution:

    $ lein uberjar

and then run it with

    $ java -jar target/uberjar/blx.sortable-0.1.0-standalone.jar [OPTIONS]

## Options

To specify the file paths for product input, listing input, and result output,
use the `-p` or `--products`, `-l` or `--listings`, and `-o` or `--output`
arguments.

The defaults, which are used for any unspecified keys, are

    resources/data/products.txt
    resources/data/listings.txt
    out.txt

Some info is logged using Timbre, mostly at the `:info` level. This is printed
by default, but can be entirely disabled at compile/run-time via environment
variable. For example, to silence log output, it is enough to do

    $ TIMBRE_LEVEL=:fatal lein run ...

## License

Copyright © 2015 Ben Cook

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
