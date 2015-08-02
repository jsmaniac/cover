# Cover
[![Build Status](https://travis-ci.org/florence/cover.svg?branch=master)](https://travis-ci.org/florence/cover)
[![Coverage Status](https://coveralls.io/repos/florence/cover/badge.png?branch=master)](https://coveralls.io/r/florence/cover?branch=master)
[![Stories in Ready](https://badge.waffle.io/florence/cover.png?label=ready&title=Ready)](https://waffle.io/florence/cover)

This library is an extensible code coverage tool for racket. 

## How to install

Install via `raco pkg install cover`. To install for development, checkout the repository, `cd` into
the new directory and run `raco pkg install`.

## How to use

To view the arguments for Cover run `raco cover -h`.

Code coverage can be generated by specifying the `-f <format>` flag.

The only built in format is `html` simply generates html files for each source file containing
coverage information and highlighted source code.

For integration with coveralls see [cover-coveralls](https://github.com/rpless/cover-coveralls).

For more detailed usage see [the full documentation](http://pkg-build.racket-lang.org/doc/cover/index.html).

## Use with TravisCI

Cover works with Travis CI, however you may want to install an output format specialized to cover
coverage service, like [cover-coveralls](https://github.com/rpless/cover-coveralls).

## Internals

Cover comes with a racket API, which can be read about in
[the full documentation](http://pkg-build.racket-lang.org/doc/cover/index.html).
