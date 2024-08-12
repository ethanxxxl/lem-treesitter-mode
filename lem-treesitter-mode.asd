(defsystem "lem-treesitter-mode"
  :depends-on ("cl-treesitter" "cffi-toolchain" "pathname-utils")
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "languages")
                             (:file "filesystem")
                             (:file "treesitters")
                             (:file "buffer")
                             (:file "commands")))
               (:file "lem-treesitter-mode")))
