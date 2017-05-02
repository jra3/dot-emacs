;;; hack-mode.el --- Major mode for the Hack programming language -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Implements `hack-mode' for the Hack programming language.  This includes
;; basic support for Hack typechecker integration and the Hack autocompletion
;; service
;;
;; Indentation is currently provided solely by the CC indentation engine.
;; It does not handle indentation or highlighting of XHP expressions.

;;; Code:
(require 'icicles nil t)
(require 'auto-complete nil t)

(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)
(eval-when-compile
  (require 'regexp-opt))

;; Work around emacs bug#18845
(eval-and-compile
  (when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
    (require 'cl)))

(defgroup hack nil
  "Major mode `hack-mode' for editing Hack code."
  :prefix "hack-"
  :group 'languages)

(defcustom hack-fontify-function-calls t
  "Fontify function and method calls if this is non-nil."
  :type 'boolean
  :group 'hack)

(defface hack-default
  '((default
      (:inherit default)))
  "Default face in `hack-mode' buffers."
  :group 'hack)

(defface hack-dollar
  '((default
      (:inherit hack-default)))
  "Face for the dollar in variable names"
  :group 'hack)

(defface hack-constant
  '((default
      (:inherit (font-lock-constant-face hack-default))))
  "Face for constants"
  :group 'hack)

(defface hack-variable-name
  '((default
      (:inherit (font-lock-variable-name-face hack-default))))
  "Face for variable names"
  :group 'hack)

(defface hack-keyword
  '((default
      (:inherit (font-lock-keyword-face hack-default))))
  "Face for Hack keywords"
  :group 'hack)

(defface hack-attribute
  '((default
      (:inherit hack-keyword)))
  "Face for method and variable attributes"
  :group 'hack)

(defface hack-type
  '((default
      (:inherit (font-lock-type-face hack-default))))
  "Face for Hack type names"
  :group 'hack)

(defface hack-function-name
  '((default
      (:inherit (font-lock-function-name-face hack-default))))
  "Face for function names"
  :group 'hack)

(defface hack-special
  '((default
      (:inherit (font-lock-builtin-face hack-default))))
  "Face for special Hack names (__construct, __toString, etc)"
  :group 'hack)

(defface hack-builtin
  '((default
      (:inherit hack-special)))
  "Face for built-in hack functions"
  :group 'hack)

(defface hack-field-name
  '((default
      (:inherit (font-lock-variable-name-face hack-default))))
  "Face for field names in expressions like $foo->bar"
  :group 'hack)

(defface hack-function-call
  '((default
      (:inherit hack-function-name)))
  "Face for functions call expressions"
  :group 'hack)

(defface hack-method-call
  '((default
      (:inherit hack-function-call)))
  "Face for method call expressions"
  :group 'hack)

(defconst hack-builtins
  '("echo" "tuple" "list" "empty" "isset" "unset")
  "Hack builtins.")

(defconst hack-keywords
  '("exit" "die" "const" "return" "yield" "try" "catch" "finally"
    "throw" "if" "else" "while" "do" "for" "foreach" "instanceof"
    "as" "switch" "default" "attribute" "category"
    "children" "enum" "clone" "include" "include_once" "require"
    "require_once" "namespace" "use" "global" "await")
  "Hack Keywords.")

(defconst hack-keywords-2
  '("goto" "case" "continue")
  "Hack flow control"
  )

(defconst hack-special-methods
  '("__construct" "__destruct" "__toString" "__clone" "__sleep" "__wakeup")
  "Special Hack methods.")

(defconst hack-identifier-regexp "[[:alpha:]][[:alnum:]_]*")
(defconst hack-xhp-identifier-regexp "[[:alpha:]][[:alnum:]-_:]*")
(defconst hack-label-regexp hack-identifier-regexp)
(defconst hack-func-regexp (concat "\\_<function\\_>[[:space:]]*\\(" hack-identifier-regexp "\\)"))

;; types allow more chars than other things
(defconst hack-type-regexp "\\(?:\\<[[:alpha:]][[:alnum:]]*[\\]\\)?\\(?::\\|\\<\\)[[:alpha:]:_][[:alnum:]-:_]*")

(defconst hack-constants '("null" "true" "false"))

(defconst hack-types
  '("array" "bool" "char" "float" "int" "mixed" "string" "void"
    "Vector" "Map" "Set"
    "vec" "dict" "keyset")
  "Hack types.")

(require 'php-mode)

(defconst hack-mode-font-lock-keywords-1
  (append
   (list

    ;; keywords
    (cons
     (concat "\\(\\_<" (regexp-opt hack-keywords t) "\\_>\\)")
     '(1 'hack-keyword))

    ;; case, goto...
    (cons
     (concat "\\(\\_<" (regexp-opt hack-keywords-2 t) "\\_>\\)[[:space:]]+\\(-?[[:word:]]+\\)?")
     '(
       (1 'hack-keyword)
       (2 'hack-constant nil t)
       ))

    (cons
     (concat "\\(\\_<" (regexp-opt hack-builtins t) "\\_>\\)[[:space:]]*(")
     '(1 'hack-builtin))

    (cons
     (concat "\\(\\_<" (regexp-opt hack-constants t) "\\_>\\)")
     '(1 'hack-constant nil t))

    (cons
     (concat "\\_<" (regexp-opt hack-types t) "\\_>") '(0 'hack-type nil t))

    ;; PHP/Hack Tag including mode header
    '("<\\?\\(?:php\\|hh\\)[[:space:]]*?" (0 font-lock-preprocessor-face)
      ("//[[:space:]]+\\(partial\\|decl\\|strict\\)" nil nil (1 font-lock-warning-face t t)))

    )
   php-font-lock-keywords-1
   )
  "Level 1 Font Lock extending php-mode"
  )

(defconst hack-mode-font-lock-keywords-2
  (append
   (list

    ;; Class Attributes
    (cons
     (concat "<<\\(?:\\(" hack-type-regexp "\\),[[:space:]]*\\)*\\(" hack-type-regexp "\\)>>")
     '(
       (1 'hack-type nil t)
       (2 'hack-type nil t)
       ))

    ;; Type declarations
    (cons
     (concat "\\<\\(class\\|interface\\|trait\\|type\\|newtype\\)[[:space:]]+\\(" hack-type-regexp "\\)?")
     '(
       (1 'hack-keyword)
       (2 'hack-type nil t)
       ))

    ;; Tokens following certain keywords are known to be types
    (cons
     (concat "\\<\\(new\\|extends\\|implements\\)[[:space:]]+\\$?\\(" hack-type-regexp "\\)")
     '(
       (1 'hack-keyword)
       (2 'hack-type nil t)
       ))

    ;; ;; implements takes a list of types, handle it separately
    ;; '("\\<\\(implements\\)[[:space:]]+\\$?" (1 'hack-keyword t)
    ;;   (,hack-type-regexp nil nil (0 'hack-type- nil t)))

    ;; function
    (cons hack-func-regexp '(1 font-lock-function-name-face))

    ;; async must come before function keyword
    '("\\<\\(\\(?:async[[:space:]]+\\)?function\\)[[:space:]]*&?\\(\\sw+\\)?[[:space:]]*("
      (1 'hack-keyword)
      (2 'hack-function-name nil t))

    '("\\(?:[^$]\\|^\\)\\<\\(self\\|parent\\|static\\)\\>" (1 'hack-special nil nil))

    '("\\<\\(private\\|protected\\|public\\|static\\)[[:space:]]+\\$?\\sw+"
      (1 'hack-attribute t t))

    '("\\<\\(abstract\\|final\\)[[:space:]]+"
      (1 'hack-attribute t t))

    (cons
     (concat "\\(\\<use\\)[[:space:]]+\\(" hack-type-regexp "\\)")
     '(
      (1 'hack-keyword)
      (2 'hack-type nil t))
     )

    )
   hack-mode-font-lock-keywords-1
   php-font-lock-keywords-2)
  "Level 2 Font Lock extending php-mode"
  )

(defconst hack-mode-font-lock-keywords-3
  (append
   (list
    ;; XHP "<abcd ..." and "</abcd ..."
    (cons
     (concat "</?\\(" hack-type-regexp "\\)") '(1 'hack-type))

    ;; XML entities
    '("&\\w+;" . font-lock-constant-face)

    ;; fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 'hack-special))
    (cons
     (concat "\\(" hack-type-regexp "\\)[[:space:]]+\\$\\(\\sw+\\)")
     '((1 'hack-type t t)
       (2 'hack-variable-name t)
       )) ;; type $variable
    '("\\$\\(\\sw+\\)" (1 'hack-variable-name t)) ;; $variable
    '("->\\(\\sw+\\)\\s-*(" . (1 'hack-method-call nil t)) ;; ->function_call
    (cons
     (concat "->:\\(" hack-xhp-identifier-regexp "\\)")
     '(1 'hack-field-name nil t))
    '("->\\(\\sw+\\)" (1 'hack-field-name t t)) ;; ->variable
    '("::\\(\\sw+\\>\\)[^(]" . (1 'hack-field-name nil t)) ;; class::constant
    (cons
     (concat "\\(" hack-type-regexp"\\)::\\sw+\\s-*(?")  '(1 'hack-type)) ;; class::member
    ;; '("\\<\\sw+\\s-*[[(]" . 'hack-default) ;; word( or word[
    '("\\<[0-9]+" . 'hack-constant)

    ;; ;; Highlight types where they are easy to detect
    `(")\\s-*:\\s-*" (,hack-type-regexp nil nil (0 'hack-type nil t)))

    ;; Highlight special methods
    (cons
     (concat "\\<function\\s-+\\(" (regexp-opt hack-special-methods t) "\\)(")
     '(1 'hack-special t t))

    )
   hack-mode-font-lock-keywords-2
   php-font-lock-keywords-3
   )
  "Level 3 Font Lock extending php-mode"
)

(defconst hack-block-stmt-1-kwds '("do" "else" "finally" "try"))
(defconst hack-block-stmt-2-kwds
  '("for" "if" "while" "switch" "foreach"))

(defconst hack-block-stmt-1-key
  (regexp-opt hack-block-stmt-1-kwds))
(defconst hack-block-stmt-2-key
  (regexp-opt hack-block-stmt-2-kwds))

(defconst hack-class-decl-kwds '("class" "interface" "trait"))

(defconst hack-class-key
  (concat
   "\\(" (regexp-opt hack-class-decl-kwds) "\\)\\[[:space:]]+"
   (c-lang-const c-symbol-key c)                ;; Class name.
   "\\([[:space:]]+extends[[:space:]]+" (c-lang-const c-symbol-key c) "\\)?" ;; Name of superclass.
   "\\([[:space:]]+implements[[:space:]]+[^{]+{\\)?")) ;; List of any adopted protocols.

(defconst hack-client-binary "hh_client"
  "Hack client binary.")

(defvar hack-mode-abbrev-table nil)

(defun hack-get-completions (&optional prefix)
  "Symbol completion function for `hack-mode'."
  (when (executable-find hack-client-binary)
    (let* ((cur-buf (current-buffer)) ;; Save the current buffer
           (cur-point (point)) ;; Save the current point
           ;; Get the bounds of the symbol we're autocompleting
           (cur-symbol-bounds (if prefix
                                  (cons (- (point) (length prefix)) (point))
                                (bounds-of-thing-at-point 'symbol)))
           (complete-start (or (car cur-symbol-bounds) cur-point))
           (complete-end (or (cdr cur-symbol-bounds) cur-point))
           (candidates nil))
      ;; Use a temporary buffer for creating the input text and for storing the output
      (with-temp-buffer
        ;; Copy from the beginning of the buffer to the position of the point
        ;; Remove the properties, though I don't think it actually matters too much
        (insert-buffer-substring-no-properties cur-buf 1 cur-point)
        ;; Insert the autocomplete marker
        (insert "AUTO332")
        ;; Insert the rest of the buffer
        (insert-buffer-substring-no-properties cur-buf cur-point)
        ;; Call the hh_client binary, the input text is piped in through stdin, then deleted from the buffer.
        ;; The output then goes into the same temporary buffer
        (call-process-region 1 (buffer-size) hack-client-binary t t nil "--auto-complete")
        ;; Check that there were any valid completions
        (unless (= 0 (count-lines 1 (buffer-size)))
          ;; Put the point at the start of the buffer
          (goto-char (point-min))
          (let ((cur-candidate-symbol nil)
                (cur-candidate-type nil))
            ;; Each autocomplete candidate looks like this:
            ;;    <symbol> <type>
            ;; one per line
            (while (not (eobp))
              ;; Grab the symbol at the beginning of the list
              (setq cur-candidate-symbol (thing-at-point 'symbol))
              (when cur-candidate-symbol
                ;; Skip over the symbol name
                (forward-char (1+ (length cur-candidate-symbol)))
                (setq cur-candidate-type (buffer-substring-no-properties (point) (line-end-position)))
                (setq cur-candidate-symbol (propertize cur-candidate-symbol
                                                       'ac-hack-type cur-candidate-type))
                ;; Add it to the list
                (push cur-candidate-symbol candidates))
              (forward-line)))))
      (when candidates
        (list complete-start complete-end candidates)))))

(defun hack-completion ()
  (let ((candidates (hack-get-completions)))
    (when candidates
      (append candidates
              (list
               :annotation-function (lambda (candidate)
                                      (concat " " (get-text-property 0 'ac-hack-type candidate))))))))

;;;###autoload
(define-derived-mode hack-mode c-mode "Hack"
  "A major mode for Hack files\n\n\\{hack-mode-map}"
  (c-add-language 'hack-mode 'c-mode)

  (c-initialize-cc-mode t)
  (c-init-language-vars hack-mode)
  (c-common-init 'hack-mode)

  (setq-local c-opt-cpp-start "<\\?\\(?:php\\|hh\\)\\s-*?")
  (setq-local c-opt-cpp-prefix "<\\?\\(?:php\\|hh\\)\\s-*?")

  (c-set-offset 'cpp-macro 0)

  (setq-local c-block-stmt-1-key hack-block-stmt-1-key)
  (setq-local c-block-stmt-2-key hack-block-stmt-2-key)

  (setq-local c-class-key hack-class-key)

  (setq-local font-lock-defaults
			  '((hack-font-lock-keywords-1
				 hack-font-lock-keywords-2
				 hack-font-lock-keywords-3)
				nil
				t
				(("_" . "w")
				 ("'" . "\"")
				 ("`" . "\"")
				 ("$" . ".")
				 ("#" . "< b")
				 ("/" . ". 124b")
				 ("*" . ". 23")
				 (?\n . "> b"))
				nil))

  (setq font-lock-maximum-decoration t)
  (setq case-fold-search t)

  (setq-local compile-command (concat hack-client-binary " --from emacs"))

  (add-hook 'completion-at-point-functions 'hack-completion nil t)

  (run-hooks 'hack-mode-hooks))
(provide 'hack-mode)

;;; hack-mode.el ends here
