(defpackage :lem-treesitter-mode/commands
  (:use :cl
        :lem-treesitter-mode/treesitters
        :lem-treesitter-mode/utils
        :lem-treesitter-mode/buffer)
  (:local-nicknames (:ts :treesitter))
  (:export))

(in-package :lem-treesitter-mode/commands)

(lem:define-command treesitter-mode/diagnostics () ()
  (let* ((buffer (lem:current-buffer))
         (diagnostic (lem:make-buffer "*TSDiagnostic*"))
         (treesitter-enabled-for-buffer (lem:buffer-value buffer :tree)))
    (clear-buffer diagnostic)
    (with-buffer-stream (diagnostic diagnostic-stream)
      (format diagnostic-stream "~%current buffer is: ~a~%current point is: ~a" buffer (lem:buffer-point buffer))
      (if treesitter-enabled-for-buffer
          (let ((node (node-at-point buffer))
                (text (lem:buffer-text buffer)))
            (format diagnostic-stream "~%node type:~%~a~%node text:~%~a"
                    (ts:node-type node)
                    (ts:node-text node text)))
          (format diagnostic-stream "~%Treesitter is not initialized for this buffer.")))))