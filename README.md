# What is this?

This is "stitchcraft", a collection of interoperable tools for generating cross-stitch patterns.

# How good is it?

"stitchcraft" is free-as-in-mattress. It is several bits of software the author created to satisfy her own needs; if it's useful to you, great!

# How can I install it?

### If you are familiar with building OCaml packages with opam:

`dune build` in the top-level directory of your cloned repository and follow any directions given in the `Error` lines until the build succeeds.  Once that happens, you should be able to `dune exec` the individual programs from the project's top-level directory.

Pinning the package with `opam pin` and then installing should work as well.

### If you are not familiar with this toolstack:

Stitchcraft is written in OCaml and uses several OCaml tools including the [dune](https://github.com/ocaml/dune) build system.  See documentation there to get started.

A better user experience for such folks is coming someday.

# How can I use it?

Stitchcraft bundles several executables for command-line use.  Many assume the presence of a `fonts.sqlite3` file in /tmp (location customizable with `--db`), which stores raster font information; if this file is missing, you're gonna have a bad time.  You can create entries for it with `otb2stitchfont`, or use the version distributed in this git repository.

## Pattern Generation and Manipulation

* assemble , for assembling color- and placement-free components into a full cross-stitch pattern
* c64stitch , for taking text input and making a cross-stitch pattern from it
* embellish , for composing cross-stitch patterns in various ways
* estimator , which gives very loose time and materials estimates for cross-stitch patterns
* listing , which makes supplementary files useful in listing cross-stitch patterns on Etsy
* otb2stitchfont , for importing otf bitmap fonts to the font database

## Pattern Viewing

* notty_canvas , for displaying patterns in the terminal
* stitchpattern , for converting patterns to PDFs

## Example Compositions

The `dune` file contains several test cases.  Have a look at the `brainfuck`, `butts`, `cards`, `be_gay_find_primes`, `verbs`, and `tops` targets.

## Importing Graphics

`ih` is a fabulous command-line tool for automatically generating cross-stitch patterns from raster images. There is a fork of `ih` available at [https://github.com/yomimono/ih/tree/stitchy_interchange](https://github.com/yomimono/ih/tree/stitchy_interchange) which adds a `-o json` output mode; the output of `ih` in this mode is a list of layers, which can be imported into a Stitchcraft workflow with the `assemble` tool.

# Design Goals

All tools are built around the central `stitchy` library and a simple JSON-based interchange format.  As a tool of last resort, it is possible to hand-build patterns understandable by Stitchcraft, but I hope it's easier to use the libraries to build tools instead.

# Current Features

* tries its best to not fuck up when confronted with unicode
* looks cool on your terminal
* generates usable PDFs

# Future Features

* nicer `listing` preview graphics
* Etsy API integration in `listing` (blocked on nice OAuth2 library :( )
* Zoom in `notty_canvas`
* sensible names or possibly subcommands
* autorefresh in notty_canvas
* ingestion of traditional pattern software files
* more full-featured composition binaries

# Anti-features / out-of-scope stuff

* taking raster images and converting them to patterns. `ih` does a great job and can interoperate with `stitchcraft`.
* vector font scaling and rasterization. `kxstitch` does this well.

# Acknowledgements

Most early work on this software was done at the [Recurse Center](https://recurse.com), and supported financially by a fellowship from that organization. I am deeply grateful to RC for all its support in the past, present, and future.

The fonts included with this software were originally packaged by VileR and are available in their entirety at [https://int10h.org](https://int10h.org). VileR has kindly packaged these fonts and provided them under a [CC-BY-SA](http://creativecommons.org/licenses/by-sa/4.0/) license.

This software depends heavily on [notty](https://github.com/pqwy/notty), [camlpdf](https://github.com/johnwhitington/camlpdf), and various small and useful libraries from [erratique.ch](https://erratique.ch/software).

Lastly, the authors and maintainers of the OCaml langauge and its tooling have made an environment that I find joyful to work in. Thank you for helping me make things!
