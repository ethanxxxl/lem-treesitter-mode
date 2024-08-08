(defsystem "lem-treesitter-mode"
  :depends-on ("cl-treesitter" "cffi-toolchain" "pathname-utils")
  :components ((:file "scratch")))
