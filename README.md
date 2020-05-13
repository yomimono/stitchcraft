# What is this?

This is "stitchcraft", a collection of interoperable tools for generating cross-stitch patterns.

# How can I install it?

### If you are familiar with building OCaml packages with opam:

You will need to pin:
* imagelib and imagelib-unix: `opam pin add https://github.com/cfcs/ocaml-imagelib.git#gif_prelim`.

Then `dune build` in the top-level directory.

Soon, you will be able to `opam install` stitchcraft.  For the moment, installation is not tested and you should just run everything with `dune exec`.

### If you are not familiar with this toolstack:

Stitchcraft is written in OCaml and uses several OCaml tools including the [dune](https://github.com/ocaml/dune) build system.  See documentation there to get started.

A better user experience for such folks is coming soon.

# How can I use it?

Stitchcraft bundles several executables for command-line use:

## Pattern Generation and Manipulation

* assemble , for assembling color- and placement-free components into a full cross-stitch pattern
* c64say , for taking text input and making a cross-stitch pattern from it
* embellish , for composing cross-stitch patterns in various ways
* estimator , which gives very loose time and materials estimates for cross-stitch patterns
* listing , which makes supplementary files useful in listing cross-stitch patterns on Etsy
* psf2stitchfont , for importing raster fonts

## Pattern Viewing

* notty_canvas , for displaying patterns in the terminal
* pattern , for converting patterns to PDFs

## Example Compositions

The `dune` file contains several test cases.  Have a look at the `brainfuck`, `butts`, `cards`, `be_gay_find_primes`, `verbs`, and `tops` targets.

# Design Goals

All tools are built around the central `stitchy` library and a simple JSON-based interchange format.  As a tool of last resort, it is possible to hand-build patterns understandable by Stitchcraft, but I hope it's easier to use the libraries to build tools instead.

# Features

* tries its best to not fuck up when confronted with unicode

# Anti-features / out-of-scope stuff

* taking raster images and converting them to patterns (there are 9327530758375329753 programs that already do this. I recommend [kxstitch](https://kde.org/applications/graphics/kxstitch/) or [cstitch](https://github.com/kleintom/cstitch) but I'm sure there are other good ones as well.
