(defpackage :lem-treesitter-mode/parser
  (:use :cl
        :lem-treesitter-mode/treesitters
        :lem-treesitter-mode/utils)
  (:local-nicknames (:ts :treesitter))
  (:export #:walk-tree
           #:get-treesitter-parser
           #:make-treesitter-syntax-parser
           #:make-treesitter-syntax-table))

(in-package :lem-treesitter-mode/parser)

; this is how markdown-mode does it
; this is pretty juicy:
; (lem/buffer/internal:current-syntax) ; gets the current table
; see also lem/buffer/internal:syntax-scan-region
; lem/buffer/internal:syntax-scan-region
; ^ this is what applies syntax "stuff " to a buffer
; ^ given a syntax-table, extract the syntax-table-parser and use that

; tmlanguage does something like get a "syntax-table-parser" given "current-syntax"
; so, what if when we construct the syntax table for a language,
; we construct the treesitter parser?
; I think that is the play

; so, for our purposes, we need to make a syntax parser that somehow
; knows about a tree, walks it, and applies syntax to the buffer

; (lem/buffer/internal:current-syntax)
(defclass syntax-parser () 
  ((language
    :initarg :language
    :accessor treesitter-parser-language)))

(defun get-treesitter-parser (syntax-parser)
  (ensure-parser (treesitter-parser-language syntax-parser)))

(defun make-treesitter-syntax-parser (language)
  "makes a treesitter syntax parser
   TODO: 
   [ ] - contain a reference to the actual treesitter parser object?"
  (make-instance 'syntax-parser
                 :language language))

(defun make-treesitter-syntax-table (language)
  (lem:make-syntax-table 
   :parser (make-treesitter-syntax-parser language)))


; another idea is to parse the buffer's syntax tree on the
; *before-syntax-scan-hook*
; and let the syntax parser work with that instead?
; e.g.
; (lem:add-hook lem/buffer/internal:before-syntax-scan-hook ...)

(defmethod lem/buffer/internal::%syntax-scan-region ((parser syntax-parser) start end)
  (lem:with-point ((start start)
               (end end)) 
    (let* ((treesitter-parser (get-treesitter-parser parser))
           (buffer (lem:point-buffer start))
           (text (lem:buffer-text buffer))
           (tree (ts:parser-parse-string treesitter-parser text)))
      (lem:buffer-start start)
      (lem:buffer-end end)
      (lem:remove-text-property start end :attribute)
      (walk-tree tree buffer))))


(defun named-leaf-p (node)
  (and  (ts:node-named-p node)
        (= 0 (ts:node-child-count node))))

(defun leaf-p (node)
  (= 0 (ts:node-child-count node)))

(defun walk-tree (tree buffer)
  "walks a treesitter tree, applying syntax to the nodes therein"
  (declare (optimize debug))
  (with-diagnostic-buffer-stream (*standard-output*)
    (lem:with-point ((start (lem:buffer-start-point buffer))
                     (end (lem:buffer-end-point buffer)))
      (let ((text (lem:buffer-text buffer)))
        (defun walker (cursor)
          (let ((node (ts:cursor-node cursor)))
            ;(debug-node node text)
            (cond 
              ((named-leaf-p node) (put-node-attribute node start end))
              ((leaf-p node) (put-node-attribute node start end))
              (t
               (ts:cursor-goto-first-child cursor)
               (walker cursor)
               (loop while (ts:cursor-goto-next-sibling cursor)
                     do (walker cursor))
               (ts:cursor-goto-parent cursor)))))
        (let ((cursor (ts:make-cursor (ts:tree-root-node tree))))
          (walker cursor)
          (format *standard-output* "~& walked buffer ~a" buffer))))))

  ;(defun debug-node (node text)
  ;  (format t "Node has ~:[no name~:;~a~]" (ts:node-name node text)))
  ;(defun debug-node (node text)
  ;  (format t "~&Node has type: ~s" (ts:node-type node))
  ;  (format t "~&Node is named?: ~s" (ts:node-named-p node))
  ;  ;(format t "~&Node starts on line/column: ~a" (ts:node-start-point node))
  ;  ;(format t "~&Node ends on line/column: ~a" (ts:node-end-point node))
  ;  (format t "~&Node begins with text: ~s" (str:substring 0 100 (ts:node-text node text))))

  ;(format t "~&Node ends with text: ~s" (str:substring (- (length text) 10) nil (ts:node-text node text)))
  

  ;identifiers might be modifiable based on their parents,
  ; e.g. `class Program`, `using System`

  ; leaves we seem to care about:
  ; identifier
  ; predefined_type
  ; comment
  ; string_literal
  ; integer_literal
  ; real_literal
  ; boolean literal
  ; (*_literal)
  ; using,class,static
  ; "(", ")", "\"", "\'", ";", these are all pieces of syntax
  ; (lem:put-text-property .. .)
(defun has-alphanum (string)
  (some #'alpha-char-p string))

(lem:syntax-variable-attribute)

(defun node-type-to-attribute (node)
  "todo, more intelligent node determinations based on parents"
  (let ((node-type (ts:node-type node)))
    (alexandria:switch (node-type :test #'equal)
      ("identifier" 'lem:syntax-variable-attribute)
      ("comment" 'lem:syntax-comment-attribute)
      ("predefined_type" 'lem:syntax-type-attribute)
      ("string_literal" 'lem:syntax-string-attribute)
      ("integer_literal" 'lem:syntax-constant-attribute)
      ("real_literal" 'lem:syntax-constant-attribute)
      ("boolean_literal" 'lem:syntax-constant-attribute)
      (t (when (has-alphanum node-type) 'lem:syntax-keyword-attribute)))))


(defun put-node-attribute (node start end)
  "node: treesitter node
   attribute: text attribute to apply
   start: lem buffer start point
   end: lem buffer end point"
  (lem:with-point ((start start)
                   (end end))
    (let ((attribute (node-type-to-attribute node)))
      (when attribute
        (lem:move-to-bytes start (1+ (ts:node-start-byte node)))
        (lem:move-to-bytes end (1+ (ts:node-end-byte node)))
        (lem:put-text-property start end :attribute attribute)))))