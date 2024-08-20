(defpackage :lem-treesitter-mode/utils
  (:use :cl)
  (:local-nicknames (:ts :treesitter))
  (:export
   #:debug-node
   #:with-buffer-stream
   #:with-message-buffer-stream
   #:with-diagnostic-buffer-stream
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

(defmacro with-diagnostic-buffer-stream ((stream) &body body)
  "Opens a stream to the *TSDiagnostic buffer"
  `(with-buffer-stream ((lem:make-buffer "*TSDiagnostic")
                        ,stream)
     ,@body))

(defun substring (start end string)
  (subseq string start (min (length string) end)))

(defun debug-node (node text)
  (format t "~&~5:A ,~5:A ,~30:A ,~30:A" 
          (if (ts:node-named-p node) "named" "")
          (if (and (ts:node-named-p node)
                   (= 0 (ts:node-child-count node))) "leaf" "") (ts:node-type node) (substring 0 50 (ts:node-text node text))))

; (defun update-lem-point (lem-point ts-point)
;   (lem:move-to-)
;   (lem:move-to-line lem-point (1+ (car ts-point)))
;   (lem:move-to-bytes))
