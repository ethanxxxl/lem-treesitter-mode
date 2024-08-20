(defsystem "lem-treesitter-mode"
  :depends-on ("cl-treesitter" "pathname-utils")
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "languages")
                             (:file "filesystem")
                             (:file "treesitters")
                             (:file "parser")
                             (:file "buffer")
                             (:file "commands")))
               (:file "lem-treesitter-mode")))
