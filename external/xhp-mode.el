;;; xhp-mode.el --- major mode for editing PHP code with XHP support

;; Copyright 2004-present Facebook.  All Rights Reserved.

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'cl-functions)
    (byte-compile-disable-warning 'obsolete)
    (byte-compile-disable-warning 'interactive-only)))

(require 'cl)
(require 'cc-mode)
(require 'php-mode)
(require 'xhp-indent)
(require 'hack-mode)

(defcustom xhp-mode-hook nil
  "list of functions to be executed on entry to xhp-mode."
  :type 'hook
  :group 'php)


(defcustom xhp-mode-use-xhp-indent-p t
  "set to t to use the new xhp-indent library for indenting xhp"
  :group 'php)

;;;###autoload
(setq auto-mode-alist
      (nconc
       (mapcar
        (lambda (pat)
          (cons pat 'xhp-mode))
        '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.phpt?$"))
       auto-mode-alist))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("php" . xhp-mode))

(defcustom xhp-mode-use-xhp-find-tag-p t
  "set to t to use the new xhp-mode-find-tag-at-point"
  :group 'xhp)

(defun xhp-mode-xhp-context-p (start end)
  "Does the range include some XHP?"
  (if (< start end)
      (save-excursion
        (goto-char start)
        (re-search-forward "<[:a-zA-Z][-:a-zA-Z0-9]*" end t))
    nil))

;; This probably doesn't work for cases or labels because using ':' as a
;; statement separator is a bit tricky.  In practice, this means XHP indenting
;; inside a switch statement is still somewhat broken.
;; This also doesn't play nice with XML entities which include a ';'
(defun xhp-mode-beginning-of-statement ()
  "Find the point of the first character of the current statement."
  (let ((end-regexp "\\(;\\)\\|)\\s *{\\|<\\?php"))
    (save-excursion
      (re-search-backward end-regexp nil t)
      (let ((semicolon (match-string 1)))
        (re-search-forward end-regexp nil t)
        (re-search-forward (if semicolon "[\n }]*" "[\n ]*") nil t))
      (point))))

(defun xhp-mode-current-indent ()
  "Indentation level of the current statement."
  (save-excursion
    (goto-char (xhp-mode-beginning-of-statement))
    (current-column)))

(defun xhp-mode-back-to-indentation ()
  (let ((first-non-indent
         (save-excursion
           (back-to-indentation)
           (point))))
    (if (< (point) first-non-indent)
        (back-to-indentation))))

(defun xhp-mode-statement-first-line-p ()
  "Is this the first line of the current statement?"
  (let ((current-point (point)))
    (save-excursion
      (goto-char (xhp-mode-beginning-of-statement))
      (and (>= current-point (line-beginning-position))
           (<= current-point (line-end-position))))))

(defun xhp-mode-indent-line-xhp ()
  "Indent a line containing XHP."
  (let ((nesting-regex
         "\\(<[:a-zA-Z][-:a-zA-Z0-9]*\\|{\\)\\|\\(</\\|/>\\|}\\)\\|\\(=\\|return[ \n]\\)")
        (indent-from (line-beginning-position))
        (depth 0))
    (save-excursion
      (goto-char (xhp-mode-beginning-of-statement))
      (while (and (< (point) indent-from)
                  (re-search-forward nesting-regex indent-from t))
        (if (match-string 1) (incf depth))
        (if (match-string 2) (decf depth))
        (if (and (match-string 3) (equal depth 0)) (incf depth)))
      (goto-char indent-from)
      (indent-line-to
       (+ (xhp-mode-current-indent)
          (* 2 depth)
          (if (looking-at "\s*\\(?:</\\|/>\\)") -2 0))))
    (xhp-mode-back-to-indentation)))

;; This function uses a heuristic regexp parser to determine the current syntax
;; context which then determines the indentation method.  It is not always
;; correct.
(defun xhp-mode-indent-line ()
  "Modify indent for a line of PHP including support for XHP."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (cond
     ;; If a line contains XHP, use the custom indent function
     ((xhp-mode-xhp-context-p (xhp-mode-beginning-of-statement) (line-end-position))
      (xhp-mode-indent-line-xhp))

     ;; A line that does not include XHP will sometimes still be indented
     ;; incorrectly if it is the first line after an XHP statement because the
     ;; preceeding XHP confuses c-indent.
     ((save-excursion
        (and (xhp-mode-statement-first-line-p)
             (re-search-backward "\\([;:{}]\\)" nil t)
             (equal (match-string 1) ";") ;; other cases are handled correctly
             (xhp-mode-xhp-context-p (xhp-mode-beginning-of-statement) (point))))
      (indent-line-to
       (save-excursion
         (re-search-backward ";" nil t) ;; goto end of previous statement
         (xhp-mode-current-indent)))) ;; find indentation level

     ;; If none of these cases apply, hopefully the normal c-indent-line
     ;; function will work just fine
     (t (c-indent-line))))
  (xhp-mode-back-to-indentation))

(defun xhp-mode-indent-region (start end &optional quiet)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (search-forward "\n" nil t)
      (xhp-mode-indent-line))))

(defconst xhp-mode-font-lock-keywords-1
  (list
   ;; Keywords
   (cons
    (concat "[^_$]?\\<\\(" (regexp-opt hack-keywords t) "\\)\\>[^_]?")
    '(1 'hack-keyword))

   ;; Builtins
   (cons
    (concat "[^_$]?\\<\\(" (regexp-opt hack-builtins t) "\\)\\>[^_]?")
    '(1 'hack-builtin))

   '("\\<\\(break\\|case\\|continue\\)\\>\\s-+\\(-?\\sw+\\)?"
     (1 'hack-keyword) (2 'hack-constant nil t))
   '("^\\s-*\\(\\sw+\\):\\>" (1 'hack-constant nil t))

   ;; PHP/Hack Tag including mode header
   '("<\\?\\(?:php\\|hh\\)\\s-*?" (0 font-lock-preprocessor-face)
     ("//\\s-+\\(partial\\|decl\\|strict\\)" nil nil (1 font-lock-warning-face nil t))))
  "Level 1 Font Lock for XHP Mode.")

(defconst xhp-mode-font-lock-keywords-2
  (append
   xhp-mode-font-lock-keywords-1
   (list
    ;; Class Attributes
    '("<<\\([a-z_][a-z0-9_:]*\\>,\\s-*\\)*\\([a-z_][a-z0-9_:]*\\>\\)>>" ;; TODO use type regex
     (1 'hack-type nil t)
     (2 'hack-type nil t))
    ;; Type declarations
    '("\\<\\(class\\|interface\\|trait\\|type\\|newtype\\)\\s-+\\(\\sw+\\)?" ;; TODO use type regex
      (1 'hack-keyword) (2 'hack-type nil t))
    ;; Tokens following certain keywords are known to be types
    `("\\<\\(new\\|extends\\)\\s-+" (1 'hack-keyword)
      (,hack-type-regexp nil nil (0 'hack-type nil t)))
    ;; implements takes a list of types, handle it separately
    `("\\<\\(implements\\)\\s-+\\$?" (1 'hack-keyword t)
      (,hack-type-regexp nil nil (0 'hack-type nil t)))
    ;; Traits
    `("\\<\\(use\\)\\s-+\\$?" (1 'hack-keyword t)   ;; TODO support a list of types (syntactically?)
      (,hack-type-regexp nil nil (0 'hack-type nil t)))
    ;; async must come before function keyword
    '("\\<\\(\\(?:async\\s-+\\)?function\\)\\s-*&?\\(\\sw+\\)?\\s-*("  ;; TODO use hack-identifier regex
      (1 'hack-keyword)
      (2 'hack-function-name nil t))

    '("\\(?:[^$]\\|^\\)\\<\\(self\\|parent\\|static\\)\\>" (1 'hack-special nil nil))
    ;; method and variable attributes
    '("\\<\\(private\\|protected\\|public\\|static\\)\\s-+\\$?\\sw+"
      (1 'hack-attribute nil t))
    ;; method attributes
    '("\\<\\(abstract\\|final\\)\\s-+"
      (1 'hack-attribute nil t))

    (cons
     (concat "[^_$]?\\<\\(" (regexp-opt hack-types t) "\\)\\>[^_]?")
     '(1 'hack-type))
    ;; TODO: XHP declaration classes - 'attribute', 'children', 'category' keywords
    ))
  "Level 2 Font Lock for XHP Mode.")

(defconst xhp-mode-font-lock-keywords-3
  (append
   xhp-mode-font-lock-keywords-2
   (list
    ;; XHP "<abcd ..." and "</abcd ..."
    '("\\(</?\\)\\([a-zA-Z:\-]+\\)" (1 'hack-default) (2 font-lock-type-face))

    ;; XML entities
    '("&\\w+;" . font-lock-constant-face)

    ;; Allow => and ==> but warn if longer
    '("===+>" . font-lock-warning-face)

    '("\\<\\($\\)\\sw+\\>" (1 'hack-dollar))
    '("\\$\\(this\\)" (1 'hack-special))
    '("\\$\\(\\sw+\\)" (1 'hack-variable-name))
    '("\\<[0-9]+" . 'hack-constant)
    '("->\\(\\sw+\\)" (1 'hack-field-name))
    '("->\\(\\sw+\\)\\s-*(" (1 'hack-method-call))
    '("\\<\\([a-z\\_][a-z0-9\\_]*\\)\\s-*[[(]" (1 'hack-function-call))

    ;; Highlight types where they are easy to detect
    ;; Return types
    `(")\\s-*:\\s-*" (,hack-type-regexp nil nil (0 'hack-type nil t)))

    ;; Highlight special methods
    (cons
     (concat "\\<function\\s-+\\(" (regexp-opt hack-special-methods t) "\\)(")
     '(1 'hack-special nil t))

    )

   )
  "Level 3 Font Lock for XHP mode.")

(defun xhp-syntactic-fontify-detect-xhp-quote (limit)
  "This function is meant to be used in conjunction with the
standard font-lock syntax table so that it selectively marks
single quote characters to be font-locked as punctuation.

The way this works is that the font lock syntax table defines
single quotes as being class '|' which is emacs voodoo for
'special quote character that can be overridden', and then this
function figures out which quotes to mark as punctuation. (See
`font-lock-fontify-syntactic-keywords-region)

This function possibly cheats in that it sets the punctuation
property on all single-quotes in the range rather than returning
t or nil and letting font-lock-fontify-syntactically-region do
that, but performance requirements lead me to taking this
approach.

NOTE: this function assumes that it is never invoked with its
point inside an xhp block.
See `xhp-mode-font-lock-extend-region-around-xhp
"
  ;; useful debugging message, as you can't debug during display phase
  ;; (message "point %i limit %i" (point) limit)
  (let
      (
       (xhp-syntax)
       (res)
       )
    ;; if we find something that might be the start of xhp begin
    ;; looking for single quote in xhp
    (while (re-search-forward xhp-start-regex limit t)
      ;; while we're in xhp, markup any single quotes
      (while (and
              (re-search-forward "'" limit t)
              (save-match-data
                (backward-char) ;; move point to actual quote
                (setq xhp-syntax (xhp-indent-xhp-detect))
                (forward-char)
                (xhp-indent-syntax-has-attribute xhp-syntax 'xhp-indent-in-xhp)))
        ;; check that we're not between braces, i.e. in a single-line
        ;; php statement within xhp.
        (unless (xhp-indent-syntax-has-attribute xhp-syntax 'xhp-indent-php-in-xhp)
          (font-lock-apply-syntactic-highlight '(0 ".")))))
    ;; t means apply highlight, but we're already doing that in here, so always return false
    ;; the reason for this is that the expected way to use this is to return t for every quote and let
    ;; font-lock-fontify-syntactic-keywords-region apply the face, but that just isn't efficient.
    nil))

(defvar font-lock-beg)

(defun xhp-mode-font-lock-extend-region-above-xhp ()
  "Move fontification boundaries above any xhp block to make xhp parsing simpler"
  (save-match-data
    (let (xhp-start-pos changed-beg)
      ;; idea: if beg/end is in xhp , move back/forward until we're outside
      (goto-char font-lock-beg)
      (if (setq xhp-start-pos (xhp-indent-start-pos))
          (setq
           font-lock-beg xhp-start-pos
           changed-beg t))
      changed-beg)))

;;;###autoload
(define-derived-mode xhp-mode php-mode "XHP"
  "Major mode for editing PHP code with XHP.\n\n\\{xhp-mode-map}"
  (c-add-language 'xhp-mode 'c-mode)

  ;; Adapted from php-mode.el
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((xhp-mode-font-lock-keywords-1
           xhp-mode-font-lock-keywords-2
           xhp-mode-font-lock-keywords-3
           )
          nil                               ; KEYWORDS_ONLY
          t                                 ; CASE-FOLD
          (("_" . "w") (?# . "< b")
           ("'" . "\"")
           ("`" . "\"")
           ("$" . ".")
           ("#" . "< b")
           ("/" . ". 124b")
           ("*" . ". 23")
           (?\n . "> b"))
          ;; (("_" . "w") (?# . "< b")         ; SYNTAX-ALIST
          ;;  ("'" . "|")) ;; treat single quote as special string
          nil                               ; SYNTAX-BEGIN
          ; OTHER-VARS
          (font-lock-syntactic-keywords
           . ((xhp-syntactic-fontify-detect-xhp-quote 0 ".")))
          (font-lock-extend-region-functions
           . (xhp-mode-font-lock-extend-region-above-xhp))))
  (setq indent-line-function 'xhp-mode-indent-line)
  (setq indent-region-function 'xhp-mode-indent-region)
  (modify-syntax-entry ?< "_") ;; Treat '<' and '>' as syntactic whitespace
  (modify-syntax-entry ?> "_") ;; band-aid fix for user attributes on classes

  (when xhp-mode-use-xhp-indent-p
    (require 'xhp-indent)
    (xhp-indent-keybinds)
    (setq indent-line-function 'xhp-indent-line-or-region)
    (setq indent-region-function 'xhp-indent-line-or-region)
    ;; c-mode overrides indent-for-tab-command for some reason
    ;; (they even have a comment with 'is this the right thing to
    ;; do?' there). if we want xhp indentation to work for regions
    ;; as well we have to set this back. (previously this just used
    ;; c-indent AFAICT, so this bug was hidden)
    (substitute-key-definition
     'c-indent-line-or-region
     'indent-for-tab-command
     xhp-mode-map)
    )

  (when xhp-mode-use-xhp-find-tag-p
    (put 'xhp-mode 'find-tag-default-function 'xhp-mode-find-tag-at-point))

  (run-hooks 'xhp-mode-hook))

;; A variation of find-tag-default that enables <xhp:tags:like:this to match.
(defun xhp-mode-find-tag-at-point ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let* ((xhp "\\(?:\\w\\|[_-]\\)") ;; xhp other than ':'
         (re-symbol
          (concat
           ;; xhp (explicitly does not include "::")
           ":?\\(?:\\(?1:\\(?::" xhp "\\|" xhp "\\)+\\)"
           ;; optional static var after xhp class
           "\\(?:::\\(?2:\\(?:\\w\\|_\\)+\\)\\)?\\)\\|"
           ;; typical symbol (includes Static::refs)
           "\\(?1:\\(\\w\\|_\\)+\\)\\(?:::\\(?2:\\(?:\\w\\|_\\)+\\)\\)?"))
         (symbol-chars "a-zA-Z$_:-")
         (non-chars (concat "^" symbol-chars)))
    (flet ((skip-backward ()
             (skip-chars-backward non-chars (line-beginning-position)))
           (skip-forward ()
             (skip-chars-forward non-chars (line-end-position)))
           (test (initial-skip)
             (save-excursion
               (funcall initial-skip)
               (skip-chars-backward symbol-chars (line-beginning-position))
               (looking-at re-symbol))))
      (when (or (test (lambda ()))
                (test #'skip-backward)
                (test #'skip-forward))
        (let ((group (if (and (< (match-end 1) (point)) (match-end 2)) 2 1)))
          (buffer-substring-no-properties
           (match-beginning group) (match-end group)))))))

(provide 'xhp-mode)
