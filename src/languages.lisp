; this file describes installation information for treesitters, by language
(defpackage #:lem-treesitter-mode/languages
  (:use #:cl)
  (:export 
   #:*treesitter-install-defs*
   #:get-install-def
   #:language-treesitter-repo
   #:language-treesitter-sourcefiles))

(in-package #:lem-treesitter-mode/languages)

; this file provides *treesitter-install-defs*, for now
; I'm  getting these from "https://github.com/nvim-treesitter/nvim-treesitter/blob/master/lua/nvim-treesitter/parsers.lua"
; Where are they getting them?
; TODO:
; [ ] - get the rest of these 
(defparameter *treesitter-install-defs*
  '(:c_sharp
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-c-sharp"
                    :files ("src/parser.c" "src/scanner.c"))
     :filetype "cs"
     :maintainers ("@Luxed"))
    :c
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-c"
                    :files ("src/parser.c"))
     :maintainers ("@amaanq"))
    :rust
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-rust"
                    :files ("src/parser.c" "src/scanner.c"))
     :maintainers ("@amaanq"))
    :bash
    (:install_info (:url "https://github.com/tree-sitter/tree-sitter-bash"
                    :files ("src/parser.c" "src/scanner.c"))
     :filetype "sh"
     :maintainers ("@TravonteD"))))

; TODO:
; [ ] : use clos to abstract over the language spec stuff

(defun get-ts-repo (install-def)
  (getf (getf install-def :install_info) :url))

(defun get-ts-files (install-def)
  (getf (getf install-def :install_info) :files))

(defun get-install-def (language)
  (getf *treesitter-install-defs* language))

(defun language-treesitter-repo (language)
  (get-ts-repo (get-install-def language)))

(defun language-treesitter-sourcefiles (language)
  (mapcar #'uiop:parse-unix-namestring 
          (get-ts-files (get-install-def language))))