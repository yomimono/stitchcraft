c64say: text -> png, c64 font representation (the text portion of a meme generator)
canvasy: doesn't do anything, can't remember what it was supposed to do
embellish: png -> png -> png -> png; add borders/corners to an existing png
  opam pin add imagelib git@github.com:cfcs/gif_prelim.git (and for imagelib-unix too); we need a little bit of hacking to use it with `embellish` since that really is using the Unix stuff.
examples: has a couple ribbon pngs in it
notty_canvas: needs stitchy which needs crowbar and ppx_deriving_crowbar which is no joke (ppx_deriving_crowbar should at some point get an update to ppx_deriving.4.2.1)
  builds fine with deps; minimap not working; doesn't take args yet
pattern: has a nice notes.txt in it, needs to be updated to work with imageutil fork/branch. image -> pattern pdf; needs symbol key and symbols (ask about unicode output?)
