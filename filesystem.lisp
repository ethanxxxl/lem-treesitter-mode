; filesystem utilities
(defpackage :lem-treesitter-mode/filesystem
  (:use :cl)
  (:export
   #:*lem-cache-dir*
   #:*lem-treesitter-library*
   #:join-directory
   #:join-pathnames
   #:make-temporary-directory))

(in-package :lem-treesitter-mode/filesystem)

(defun join-directory (base-directory-pathname &rest sub-directory-path)
  (make-pathname :directory (append (pathname-directory base-directory-pathname)
                                    sub-directory-path)))

(defvar *lem-cache-dir* (uiop:parse-native-namestring (uiop:native-namestring "~/.local/share/lem") :ensure-directory t))

(defvar *lem-treesitter-library* (join-directory *lem-cache-dir* "treesitter" "compiled"))


(defun join-pathnames (base-pathname adjoining-pathname)
  "TODO:
   [ ]: make sure adjoining pathname is NOT an absolute directory"
  (let* ((adjoining-directory (rest (pathname-directory adjoining-pathname)))
         (adjoining-file (pathname-name adjoining-pathname))
         (adjoining-type (pathname-type adjoining-pathname)))
    (make-pathname :directory (append (pathname-directory base-pathname) adjoining-directory)
                   :name adjoining-file
                   :type adjoining-type)))

(defun make-temporary-directory ()
  (let ((tmpdir (make-pathname :directory (append (pathname-directory (uiop:temporary-directory)) 
                                                  '("lem-treesitter")))))
    (ensure-directories-exist tmpdir :verbose t :mode #o755)))