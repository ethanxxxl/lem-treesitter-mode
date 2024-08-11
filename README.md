# lem-treesitter

# todo:

- [x]: implement source fetching and compilation
- [x]: define package with dependencies
- [ ]: figure out how to include cl-treesitter from github? without qlfile?
- [ ]: implement treesitter-mode (or is it an extension of language mode?)
  - e.g. when we load a `c-sharp` file, we should to load the :c_sharp treesitter and parse the file
- [ ]: integrate treesitter support with lem buffer
    
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