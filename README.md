# blx.sortable

Solution to the [Sortable entity-resolution coding challenge](http://sortable.com/challenge/).

## Installation

This program uses Java and [Leiningen][lein].

If you don't have Leiningen installed, download the [`lein` script][lein-install],
place it on your `$PATH` and make it executable.

Leiningen will automatically download the required library dependencies when it's
first executed in this directory. See `project.clj` for specifics.

[lein]: https://github.com/technomancy/leiningen
[lein-install]: https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein

## Usage

To run once, do

    $ lein run -- [OPTIONS]

in the root of this directory.

Alternatively, you can compile the program for faster repeated execution:

    $ lein uberjar

and then run it with

    $ java -jar target/uberjar/blx.sortable-0.1.0-standalone.jar [OPTIONS]

I've assumed multiple processors (although single-threaded should be fine, just
slower) and at least ~1 GB of memory available to java.

On my quad-core MBP using the sample data, `time java -jar ...` averages
~2.8 seconds (clock time) and peaks at around 400 MB of memory usage.
Running from uncompiled source with `lein run` takes more like 7.5s.

## Options

To specify the file paths for product input, listing input, and result output,
use the `-p` or `--products`, `-l` or `--listings`, and `-o` or `--output`
arguments.

The defaults, which are used for any unspecified keys, are

    resources/data/products.txt
    resources/data/listings.txt
    out.txt

Some info is logged using [Timbre][], mostly at the `:info` level. This is printed
by default, but can be disabled (and in fact entirely elided from the compiled jar)
at compile/run-time via environment variable. For example, to silence log output,
just do

    $ TIMBRE_LEVEL=:fatal lein run ...

[Timbre]: https://github.com/ptaoussanis/timbre

## SQL

The main solution is in Clojure, but in `sql/` there is a Python script and 
MySQL bulk-loading scripts to load the products and listings files into MySQL
for exploration.

This made it easier to do things like

    select distinct manufacturer from listing
    order by manufacturer;

to help refine the matcher.

## License

Copyright © 2016 Ben Cook

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

Data in `resources/data/` are the property of Sortable.
