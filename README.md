# lem-treesitter
    
# Structure of this project

- lem-treesitter-mode.lisp
  - intended entrypoint for the package
- src/utils.lisp
  - useful things in general
- src/filesystem.lisp
  - filesystem utilities
- src/languages.lisp
  - language install definitions live here
- src/treesitters
  - logic for fetching treesitter source, and compiling/installing them to the lem cache
- src/buffer
  - logic for interacting with lem buffers.

# todo:

- [x] - implement source fetching and compilation
  - [ ] - also implement situation where npm is needed to build from grammar
- [ ] - copy over the rest of the install definitions for src/languages.lisp
- [ ] - figure out how to include cl-treesitter from github? without qlfile?
- [ ] - implement treesitter-mode (or is it an extension of language mode?)
  - e.g. when we load a `c-sharp` file, we should to load the :c_sharp treesitter and parse the file
- [ ] - integrate treesitter support with lem buffers
    
There are also todos distributed through the rest of the codebase
    
# hacking

to load this from _within_ lem, put it in "~/common-lisp" (or otherwise make it available to asdf):
then: 
(ql:quickload :lem-treesitter-mode)

`hacking.lisp` is an ephemeral, non-library source file which
I'm using to manually exercise code with. Eventually this file
will not exist.
    
# fun can work
fun can work