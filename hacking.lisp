(ql:quickload :lem-treesitter-mode)
;(ql:quickload :pathname-utils)
(ql:quickload :cl-treesitter)

(in-package :lem-user)
(rename-package :treesitter :ts '(:treesitter))

; creates cache directories. Start with this.
(lem-treesitter-mode:setup)

; any of these should work
(lem-treesitter-mode/treesitters:load-treesitter :c_sharp)
(lem-treesitter-mode/treesitters:load-treesitter :bash)
(lem-treesitter-mode/treesitters:load-treesitter :rust)
(lem-treesitter-mode/treesitters:load-treesitter :c)

; you need to have opened "example.cs" for this to work
(defparameter *b* (lem:get-buffer "example.cs"))
(lem-treesitter-mode/buffer:parse-buffer *b* :language :c_sharp)
; trace ts-node-delete

;what is a point in this context
; (ts:cursor-goto-first-child *cursor*)
; (ts:cursor-node *cursor*)
; (ts:node-text (ts:cursor-node *cursor*) (lem:buffer-text *b*))
; 
; (defparameter *encoding* (buffer-encoding *b*))
; (defparameter *point* (buffer-point *b*))
; (line-number-at-point *point*)
; (point-column *point*)
; (lem/)
; (lem:line-string *point*)
; (point-overlays *point*)
; (point-)
; (point-bytes *point*)
; 
; (lem:buffer-end-point )
; 
; ; how do we get the current byte of a buffer
; 
; (lem-treesitter-mode/buffer:parse-buffer *b* :language :c_sharp)
; 
; ; try some stuff
; (treesitter:tree-root-node 
;  (lem-treesitter-mode/buffer:parse-buffer *b* :language :c_sharp))
; 
; (ts:cursor-goto-first-child-for-point)
; 
; 
; (ts:cursor-goto-first-child-for-byte *cursor* (current-point))
; 
; (treesitter:node-string
;  (treesitter:tree-root-node 
;   (lem-treesitter-mode/buffer:parse-buffer 
;    *b* :language :c_sharp)))
; 
; (ts:node-start-point
;  (ts:tree-root-node (buffer-value *b* :tree)))
; 
; (lem-treesitter-mode/buffer:lem-point-to-ts-point (buffer-point *b*))
; (lem-treesitter-mode/buffer:buffer-ts-point *b*)
; 
; (lem-treesitter-mode/buffer:buffer-node-at-point *b*)
; 
; (lem-treesitter-mode/buffer:move-cursor-to-point *b*)
; (lem-treesitter-mode/buffer:node-text-at-point *b*)
; 
; (ts:node-string (ts:cursor-node (lem-treesitter-mode/buffer:get-cursor-at-point *b*)))


; aha
(ts:node-text (lem-treesitter-mode/buffer:node-at-point *b*)
              (buffer-text *b*))

(lem-treesitter-mode/buffer:node-text-at-point *b*)


; idea to push current node into the diagnostic buffer every input event
(defun handle-event (event)
  (let ((buffer (lem:current-buffer)))
    (when (equal (lem:buffer-name buffer) "example.cs")
      (let ((diagnostic (make-buffer "*TSDiagnostic*")))
        (clear-buffer diagnostic)
        (message "event is ~a" event)
        (with-open-stream (bstream (lem:make-buffer-output-stream
                                    (lem:buffer-end-point diagnostic)))
          (format bstream (lem-treesitter-mode/buffer:node-text-at-point *b*)))))))

;(add-hook *input-hook* #'handle-event)
;(remove-hook *input-hook* #'handle-event)

