
(defpackage :lem-treesitter-mode
  (:use :cl :alexandria
        :lem-treesitter-mode/languages
        :lem-treesitter-mode/filesystem
        :lem-treesitter-mode/treesitters
        :lem-treesitter-mode/buffer
        :lem-treesitter-mode/parser)
  (:export
   #:setup
   ; re-export from filesystem.lisp
   #:*lem-cache-dir*
   #:*lem-treesitter-library*
   #:make-treesitter-syntax-parser
   #:load-treesitter))

(in-package :lem-treesitter-mode)



(lem:define-minor-mode treesitter-mode
    (:name "Treesitter"
     :hide-from-modeline nil
     :enable-hook 'enable
     :disable-hook 'disable))

;; TODO find the major mode symbolsfor these
(defvar *major-mode-parsers* '((:c_sharp ".cs")
                               (:lua)
                               (:rust)
                               (:python)
                               (:go)
                               (:java)
                               (:c)
                               (:cpp)
                               (:makefile)
                               (:ruby)))

(defparameter *treesitter-buffers* '()
  "Buffers that are managed by PARSE-BUFFERS-THREAD.")

(defconstant +treesitter-parser-timer+
  (lem:make-timer 'parse-buffers-cb
                  :name "Treesitter Parser")
  "Timer that will periodically call PARSE-BUFFERS-THREAD to parse any
treesitter buffers currently opened")

;; NOTE if the the timer is already running, changing this value might
;; not have an effect.
(defparameter *parse-interval* 500
  "How often treesitter will parse all the treesitter buffers.")

(defun parse-buffers-thread (&optional (period-ms 500))
  (dolist (buf *treesitter-buffers*)
    (parse-buffer buf :language (lem:buffer-value buf :treesitter-lang))))

(defun enable ()
  "Ensures tree sitter environment is set up correctly then starts the parser
thread."

  ;; ensure cache directories are setup
  (ensure-directories-exist *lem-cache-dir* :verbose t :mode #o755)
  (ensure-directories-exist *lem-treesitter-library* :verbose t :mode #o755)

  ;; TODO autofill based on the major mode
  ;(lem:buffer-major-mode (lem:current-buffer))
  (let ((language (read-from-string (lem:prompt-for-string "enter language (sym): ")))
        (buf (lem:current-buffer)))
    ;(lem:message "language: ~S" language)
    ;(lem:message "lib: ~S" *lem-treesitter-library*)
    ;(unless (load-treesitter language)
    ;  (error "ERROR! ~A is not a recognized language!" language))

    (parse-buffer (lem:current-buffer) :language language)

    ;; TODO ensure language is valid before pushing the buffer here.
    (push (lem:current-buffer) *treesitter-buffers*)

    (setf (lem:buffer-value buf :treesitter-lang) language)

    ;; Is there a race condition here if a buffer is started at exactly the same
    ;; time as when the timer turns over/repeats?
    (when (lem:timer-expired-p +treesitter-parser-timer+)
      (lem:start-timer +treesitter-parser-timer+ *parse-interval* :repeat t))))

(defun disable ()
  "Removes the current buffer from the list of buffers that needs to be parse and
stops the treesitter parse timer if there are no bufferes left to be parsed."
  (delete (lem:current-buffer) *treesitter-buffers*)

  (unless *treesitter-buffers*
    (lem:stop-timer +treesitter-parser-timer+)))