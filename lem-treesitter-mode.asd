(defsystem "lem-treesitter-mode"
  :depends-on ("cl-treesitter" "cffi-toolchain" "pathname-utils")
  :components ((:file "utils")
               (:file "languages")
               (:file "filesystem")
               (:file "treesitters")
               (:file "buffer")
               (:file "lem-treesitter-mode")))
