; this file defines how to fetch and load treesitter for a given language
(defpackage :lem-treesitter-mode/treesitters
  (:use :cl 
        :lem-treesitter-mode/languages
        :lem-treesitter-mode/filesystem
        :lem-treesitter-mode/utils)
  (:export
   #:load-treesitter
   #:ensure-parser
   #:*treesitters*
   #:*parsers*))

; (defpackage :lem-treesitter-mode
;   (:use :cl :lem))
(in-package :lem-treesitter-mode/treesitters)

; this file is the entrypoint to the treesitter package

; 1. Loading a treesitter
;   - fetching & compiling a treesitter lib if it's not found locally
;   - loading a treesitter lib from a particular directory (lem cache?)
;     - do this on-demand when we enter a particular dir?
; 
; How does neovim do this?
; - mason (no, this is for LSPs)


(defun fetch-source (lang cache-pathname)
  "Fetches the source files for a particular ts-loc
     TODO:
     [ ]: typecheck lang, needs to be a keyword symbol
     [ ]: throw if we don't get a lang-spec successfully?
     [x]: make this thread safe? see uiop:with-current-directory [refactored to not use uiop:with-current-directory]
     [ ]: make output stream somehow, to show in a lem status bar? or something?
     [x]: actually get repo name somehow [Decided to clone to lang-str]
     [x]: handle instance where dir already exists? [see download-source]
     [x]: cross-platform copy of source files [see copy-to-dir]
     [ ]: get source tarball and extract instead of cloning?
     [ ]: factor out the dependency on \"pathname-utils\"

     Returns the relative path to the source repository.
"
  (with-message-buffer-stream (*standard-output*)
    (let* ((lang-str (string-downcase (symbol-name lang)))
           (repo (language-treesitter-repo lang))
           (local-repo-pathname (join-directory cache-pathname lang-str))
           (local-repo-namestring (pathname-utils:native-namestring local-repo-pathname)))
      (if (uiop:directory-exists-p local-repo-pathname)
          (format t "Directory ~a already exists. Refusing to clone.~%" local-repo-pathname)
          (uiop:run-program `("git" "clone" ,repo ,local-repo-namestring) 
                            :error-output *standard-output* 
                            :output *standard-output*))
      local-repo-pathname)))



(defun compile-lang (lang cache-pathname)
  "compiles treesitter language object and copies it to *lem-treesitter-library*"
  (with-message-buffer-stream (*error-output*)
      (let* ((lang-str (string-downcase (symbol-name lang)))
             (local-repo-pathname (join-directory cache-pathname lang-str))
             (input-files (language-treesitter-sourcefiles lang))
             (output-file (uiop:merge-pathnames* *lem-treesitter-library* 
                                                 (format nil "libtree-sitter-~a.so" lang-str)))
             (input-files-absolute (mapcar (lambda (file) 
                                             (join-pathnames local-repo-pathname file))
                                           input-files)))
        (lem:message "~a -> ~a~%" input-files-absolute output-file)
        (cffi-toolchain:link-shared-library output-file input-files-absolute)
        (lem:message "treesitter for ~a installed to ~a" lang output-file))))

(defun install-lang (lang)
  "Attempts to fetch and install the treesitter for a particular language"
  (let* ((tmpdir (make-temporary-directory))) 
    (fetch-source lang tmpdir)
    (compile-lang lang tmpdir)))


(defun ts-installed-p (lang)
  "Checks if a treesitter language object exists in *lem-treesitter-library*"
  (let* ((lang-str (string-downcase (symbol-name lang)))
         (lang-so-name (format nil "libtree-sitter-~a.so" lang-str))
         (lang-so-filepath (uiop:merge-pathnames* *lem-treesitter-library* lang-so-name)))
    (uiop:file-exists-p lang-so-filepath)))

(defun ensure-language-treesitter-installed (lang)
  "Determines if the treesitter library for the given language is installed. Installs it if it is not found."
  (unless (ts-installed-p lang)
    (install-lang lang)))

(defvar *treesitters* (make-hash-table :test #'equal))
(defvar *parsers* (make-hash-table :test #'equal))

(defmacro load-treesitter (lang &key (ensure-installed t))
  (let ((lang-str (string-downcase (symbol-name lang))))
    `(progn
       (when ,ensure-installed
         (ensure-language-treesitter-installed ,lang))
       (treesitter:include-language ,lang-str :search-path *lem-treesitter-library*)
       (setf (gethash ,lang *treesitters*) (treesitter:make-language ,lang-str)))))

(defun ensure-parser (language)
  (unless (gethash language *parsers*)
    (setf (gethash language *parsers*)
          (treesitter:make-parser :language (gethash language *treesitters*))))
  (gethash language *parsers*))