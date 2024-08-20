(ql:quickload :lem-treesitter-mode)

(in-package :lem-user)

(lem-treesitter-mode:setup)
(lem-treesitter-mode:load-treesitter :c_sharp)
(defvar *csharp-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                ;:symbol-chars '(#\_)
                :paren-pairs '((#\< . #\>)
                               (#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/")))))
    (set-syntax-parser table (lem-treesitter-mode:make-treesitter-syntax-parser :c_sharp))
    table))

(define-major-mode csharp-mode lem/language-mode:language-mode
    (:name "CSharp"
     :keymap *csharp-mode-keymap*
     :syntax-table *csharp-syntax-table*
     :mode-hook *csharp-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-file-type ("cs" "csx") csharp-mode)
