(defpackage :lem-treesitter-mode/utils
  (:use :cl)
  (:export
   #:with-message-buffer-stream))

(in-package :lem-treesitter-mode/utils)

(defmacro with-buffer-stream ((buffer stream) &body body)
  `(with-open-stream (,stream (lem:make-buffer-output-stream
                               (lem:buffer-end-point ,buffer)))
     ,@body))

(defmacro with-message-buffer-stream ((stream) &body body)
  "Opens a stream to the *Messages* buffer"
  `(with-buffer-stream ((lem:make-buffer "*Messages*")
                        ,stream)
     ,@body))