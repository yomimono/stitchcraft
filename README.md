# What is this?

This is "stitchcraft", a collection of interoperable tools for generating cross-stitch patterns.

# How good is it?

"stitchcraft" is free-as-in-mattress. It is several bits of software the author created to satisfy her own needs; if it's useful to you, great!

# How can I install it?

### If you are familiar with building OCaml packages with opam:

`dune build` in the top-level directory of your cloned repository and follow any directions given in the `Error` lines until the build succeeds.  Once that happens, you should be able to `dune exec` the individual programs from the project's top-level directory.

Installation via `opam` is not currently supported for version treadmill reasons; watch this space.

### If you are not familiar with this toolstack:

Stitchcraft is written in OCaml and uses several OCaml tools including the [dune](https://github.com/ocaml/dune) build system.  See documentation there to get started.

A better user experience for such folks is coming someday.

# How can I use it?

Stitchcraft bundles several executables for command-line use.  Many assume the presence of a `fonts.sqlite3` file in the current directory, which stores raster font information; if this file is missing, you're gonna have a bad time.  You can create entries for it with `otb2stitchfont`.

## Pattern Generation and Manipulation

* assemble , for assembling color- and placement-free components into a full cross-stitch pattern
* c64stitch , for taking text input and making a cross-stitch pattern from it
* embellish , for composing cross-stitch patterns in various ways
* estimator , which gives very loose time and materials estimates for cross-stitch patterns
* listing , which makes supplementary files useful in listing cross-stitch patterns on Etsy
* otb2stitchfont , for importing otf bitmap fonts to the font database

## Pattern Viewing

* notty_canvas , for displaying patterns in the terminal
* pattern , for converting patterns to PDFs

## Example Compositions

The `dune` file contains several test cases.  Have a look at the `brainfuck`, `butts`, `cards`, `be_gay_find_primes`, `verbs`, and `tops` targets.

# Design Goals

All tools are built around the central `stitchy` library and a simple JSON-based interchange format.  As a tool of last resort, it is possible to hand-build patterns understandable by Stitchcraft, but I hope it's easier to use the libraries to build tools instead.

# Current Features

* tries its best to not fuck up when confronted with unicode
* looks cool on your terminal

# Future Features

* pattern import from a raster-image-to-pattern program (likely one of those recommended below)
* nicer `listing` preview graphics
* Etsy API integration in `listing` (blocked on nice OAuth2 library :( )
* Zoom in `notty_canvas`
* sensible names or possibly subcommands
* prettier PDFs, including an optional "how to cross-stitch" page
* nicer installation and versioning story
* autorefresh in notty_canvas

# Anti-features / out-of-scope stuff

* taking raster images and converting them to patterns. There are many, many programs that already do this well. I recommend [kxstitch](https://kde.org/applications/graphics/kxstitch/) or [cstitch](https://github.com/kleintom/cstitch) but I'm sure there are other good ones out there that will fit well into your workflow.

# Acknowledgements

Most work on this software was done at the [Recurse Center](https://recurse.com), and supported financially by a fellowship from that organization. I am deeply grateful to RC for all its support in the past, present, and future.

The fonts included with this software were originally packaged by VileR and are available in their entirety at [https://int10h.org](https://int10h.org). VileR has kindly packaged these fonts and provided them under a [CC-BY-SA](http://creativecommons.org/licenses/by-sa/4.0/) license.

This software depends heavily on [notty](https://github.com/pqwy/notty), [camlpdf](https://github.com/johnwhitington/camlpdf), and various small and useful libraries from [erratique.ch](https://erratique.ch/software).
