(defpackage :lem-treesitter-mode/utils
  (:use :cl)
  (:export
   #:with-buffer-stream
   #:with-message-buffer-stream
   #:clear-buffer))

(in-package :lem-treesitter-mode/utils)

(defun clear-buffer (buffer)
  (lem:delete-between-points (lem:buffer-start-point buffer) 
                         (lem:buffer-end-point buffer)))

(defmacro with-buffer-stream ((buffer stream) &body body)
  `(with-open-stream (,stream (lem:make-buffer-output-stream
                               (lem:buffer-end-point ,buffer)))
     ,@body))

(defmacro with-message-buffer-stream ((stream) &body body)
  "Opens a stream to the *Messages* buffer"
  `(with-buffer-stream ((lem:make-buffer "*Messages*")
                        ,stream)
     ,@body))