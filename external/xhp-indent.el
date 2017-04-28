;;; xhp-indent.el --- indent xhp code according to facebook style
;;; Copyright (C) 2012 Facebook Inc.
;;; Author: Aaron Brady <abrady0@fb.com>
;;; Usage:
;;   when coding TAB should indent a line of php or xhp according to
;;   style consistent with facebook style guidelines.
;;
;;   certain characters (semicolon, paren, etc.) will also
;;   automatically cause indentation
;;
;;   also of note is xhp-fill-paragraph, bound to ESC-q by default,
;;   that will break strings, xhp, functions with lots of params into
;;   multiple, properly indented lines
;;
;;   to enable this customize xhp-mode-use-xhp-indent-p to t and make
;;    sure your php files load in xhp mode.
;;
;; Also: you can always use
;; $ cmf --indent_php <file> to have your php indented properly

(require 'cc-mode)

(defvar xhp-start-regex "\\(return +\\|^ *\\|= *\\|( *\\)<[^<\\]"
  "the regex used to match the valid start of an xhp expression")

(defvar xhp-indent-close-tag-regex "\\(/>\\|</\\)"
  "regex used to match closing xhp tags. use this with
 e.g. xhp-indent-xhp-detect to figure out if you're at the end of
 xhp")

(defvar xhp-indent-debug-on nil)

(defvar xhp-indent-syntax-attributes
  '(
    xhp-indent-in-heredoc
    xhp-indent-in-attribute
    xhp-indent-in-mutiline-php-in-xhp-block
    xhp-indent-in-closing-elt
    xhp-indent-in-closing-stmt
    xhp-indent-in-closing-stmt
    xhp-indent-in-first-statement-after-xhp
    xhp-indent-php-in-xhp
    xhp-indent-in-xhp
    xhp-indent-in-php
    ))

(defun xhp-indent-debug (&rest args)
  (if xhp-indent-debug-on
      (apply 'message args)))

(defun xhp-indent-previous-semi (min)
  "helper for finding the previous semicolon not in a string or comment"
  (if (not min)
      (setq min (point-min)))
  (if (> min (point))
      nil ;; search/re-search error if this is true. stupid
    (let
        ((res))
      (save-excursion
        (while
            (if (search-backward ";" min t)
                (if (c-in-literal min)
                    t ;; keep searching
                  (setq res (point)) ;; semi found, done.
                  nil)))
        res))))

(defun xhp-in-heredoc ()
  "matches heredoc and nowdoc syntax e.g.
$foo = <<<FOO
  asdf
FOO;
$bar = <<<'BAR'
  asdf
BAR;"
  (save-match-data
    ;; find closest heredoc opening, potentially this could be in a heredoc as well
    ;; valid heredoc:
    ;; - a point is only inside heredoc if it is surrounded by a heredoc start and end
    ;; - the valid ways to be in a heredoc are to have a label <<<FOO above you that is *not* in heredoc and to have a FOO terminator
    (let
        (
         (cur (point))
         start
         end
         (heredoc-start-symbol
          (lambda ()
            ;; correctly find the start of a heredoc block. We're in
            ;; heredoc if we can find a heredoc start that is not
            ;; itself in:
            ;;: - not in a comment or a regular string
            ;;  - not itself in a heredoc
            (let
                (res)
              (while (and (not res) (re-search-backward "<<<'?\\(.+?\\)'? *$" nil t))
                (if (not (or (c-in-literal) (xhp-in-heredoc))) ;; RECURSION, yay!
                    (setq res (match-string 1))))
              (cons res (point)))))
         )
      (save-excursion
        (if (and
             ;; moves point to start of heredoc as well
             (car (setq start (funcall heredoc-start-symbol)))
             ;; given that point is correctly at start of a heredoc, a
             ;; simple search for the start symbol at bol is
             ;; sufficient to detect termination of heredoc.
             (> cur (cdr start))
             ;; if we don't find heredoc terminator assume user is in
             ;; the process of composing it
             (or
              (not (setq end (and (re-search-forward (format "^%s" (car start)) nil t) (point))))
              (< cur end))
             )
            (cons start end))
        ))))

(defun xhp-indent-heredoc-detect ()
  "Detect if in heredoc/nowdoc and determine indentation"
  (save-excursion
    ;; get out of anything being typed that might confuse the parsing
    (beginning-of-line)
    (c-save-buffer-state
        (heredoc-info)
      (if (xhp-in-heredoc)
          (list (current-indentation) 'xhp-indent-in-heredoc)
        ;; not in heredoc, see if previous statement was heredoc, if
        ;; so use the indentation at the start of it
        (progn
          (c-backward-syntactic-ws)
          (beginning-of-line)
          (if (setq heredoc-info (xhp-in-heredoc))
              (progn
                (goto-char (cdar heredoc-info))
                (list (current-indentation) 'xhp-indent-in-heredoc)))
          )
        )
      )
    ))

;; 1000 was chosen somewhat arbitrarily in that it didn't seem to
;; perform worse than 500 in a test file, but seems more than
;; sufficient to encompass a single xhp statement
(defconst xhp-indent-max-backtrack 1000
  "the maximum number of characters that xhp-indent will look
  backwards for xhp start. js_strings.php was the motivation for
  this")

(defun xhp-indent-xhp-detect ()
  "Determine if xhp around or above point will affect indentation"
  (save-excursion
    (c-save-buffer-state
        (
         (single-line-php-brace-pos (c-most-enclosing-brace (c-parse-state)))
         (min-brace
          (progn
            ;; get out of anything being typed that might confuse the parsing
            (beginning-of-line) ;; SIDE EFFECT
            (c-most-enclosing-brace (c-parse-state))))
         (min (save-excursion
                (or
                 (xhp-indent-previous-semi min-brace)
                 min-brace
                 (+ (point-min) 5) ;; skip past <?php
                 )))
         (max (point))
         base-indent
         xhp-start-pos
         )
      ;; STEP 1: find a previous xhp element, and derive the normal
      ;; indentation from it.
      (save-excursion
        (if (and
             (> (point) min)
             (re-search-backward xhp-start-regex min t)
             (not (c-in-literal)))
            (setq
             xhp-start-pos (point)
             base-indent
             ;; decide from this context if indentation should
             ;; be initially adjusted.
             (+
              ;; start with the indentation at this elt
              (current-indentation)
              ;; at the matched xhp element, figure out if the
              ;; indentation should be modified
              ;; TODO(abrady) too lazy to parse forward properly, these
              ;; work fine for now.
              (cond
               ;; CASE 1: matched elt is closed or self-closing e.g. <br />
               ;; or a 1-line enclosed stmt: <fbt:param>foo</fbt:param>
               ((save-excursion
                  (beginning-of-line)
                  (or
                   (re-search-forward "</" (line-end-position) t)
                   (re-search-forward "/> *,?$" max t)
                   (re-search-forward "--> *$" max t)))
                0)
               ;; DEFAULT: increase indent
               (t 2))
              ))))
      ;; STEP 2: indentation adjustment based on what user has typed so far
      (if base-indent
          ;; STEP 2.1: we found indentation to adjust. use the current
          ;; context to determine how it should be adjusted
          (progn
            (xhp-indent-debug "base indent %i" base-indent)
            (let
                ((res))
              (setq res
                    (cond
                     ;; CASE 0: indenting an attribute
                     ((looking-at "^ *[a-zA-Z_-]+")
                      (list base-indent 'xhp-indent-in-attribute))
                     ;; CASE 1: Terminating a multiline php block is a special
                     ;; case where we should default to php indentation as if we
                     ;; were inside the braces
                     ;; e.g. <div class={foo($a
                     ;;                      $b)}>
                     ((save-excursion
                        (and
                         (not (re-search-forward "^ *<" (line-end-position) t))
                         (re-search-forward "}> *$" (line-end-position) t)))
                      (xhp-indent-debug "terminating php block")
                      (list nil 'xhp-indent-in-mutiline-php-in-xhp-block))
                     ;; CASE 2: user is indenting a closing block, so out-dent
                     ;; e.g.
                     ;; <div>
                     ;; </div>
                     ((save-excursion
                        (re-search-forward "^ *</" (line-end-position) t))
                      (list (+ base-indent -2) 'xhp-indent-in-closing-elt))
                     ;; CASE 3: if this happens to be /> on its own
                     ;; line, reduce indent (coding standard)
                     ((save-excursion
                        (goto-char max)
                        (re-search-forward "^ */> *" (line-end-position) t))
                      (list (+ base-indent -2) 'xhp-indent-in-closing-stmt))
                     ;; CASE 4: close of xhp passed to a function, e.g.
                     ;; foo(
                     ;;   <xhp>
                     ;; );
                     ((save-excursion
                        (re-search-forward "^ *);" (line-end-position) t))
                      (list (+ base-indent -2) 'xhp-indent-in-closing-stmt))
                     ;; DEFAULT: no modification.
                     (t (list base-indent))))
              ;; already determined we're in xhp, if we have a
              ;; single-line brace it must be php in xhp.
              (if (and
                   single-line-php-brace-pos
                   min-brace
                   (< min-brace single-line-php-brace-pos))
                  (setq res (append res '(xhp-indent-php-in-xhp))))
              (append res '(xhp-indent-in-xhp) (list 'xhp-start-pos xhp-start-pos))
              ))
        ;; STEP 2.2: FIRST STATEMENT AFTER XHP. if we're after
        ;; the close of an xhp statement it still messes up the php
        ;; indentation, so check that here and override
        (cond
         ;; CASE 1: multiline self-enclosing tag or closing tag
         ;; e.g.
         ;; <div
         ;;   foo="bar"
         ;; />;
         ;; - or -
         ;; <div>
         ;;  ...
         ;; </div>;
         ((save-excursion
            (c-backward-syntactic-ws)
            (and
             (looking-back "\\(/>\\|</.*>\\);")
             ;; don't match single-line xhp $foo = <x:frag />;
             (not (re-search-backward "^ *\\$" (line-beginning-position) t))))
          ;; previous statement IS xhp. check what user has typed so
          ;; far
          (list
           (+
            (save-excursion (c-backward-syntactic-ws) (current-indentation))
            (cond
             ;; CASE 0: user typed a brace. outdent even more
             ((looking-at ".*}") -4)
             ;; CASE 1: close of case in a switch stmt, e.g. case FOO:
             ((looking-at ".*: *$") -4)
             ;; DEFAULT
             (t -2)))
           'xhp-indent-in-first-statement-after-xhp)
          )
         ;; DEFAULT: not first stmt after xhp, let c-indent figure
         ;; this out normally
         (t (list nil 'xhp-indent-in-php)))
        )
      )))

(defun xhp-indent-syntax-detect ()
  "emacs' built in syntax checking can only handle one or two character values for determining indentation. This function provides a more expensive way to detect indentation for contexts where the builtin syntax checking fails. There are currently two cases for this:
- inside xhp
- inside heredoc

returns a list of (indent-amount syntax-infos ...)
"
   (or
    (xhp-indent-heredoc-detect)
   (xhp-indent-xhp-detect)))

(defun xhp-indent-syntax-indent-amount (syntax)
  (car syntax))

(defun xhp-indent-syntax-has-attribute (syntax attribute)
  (or
   (not xhp-indent-debug-on)
   (memq attribute xhp-indent-syntax-attributes) ;; perf issue
   (error "invalid attribute %s" (symbol-name attribute)))
  (memq attribute (cdr syntax)))

(defun xhp-indent-start-pos (&optional xhp-indent-info)
  "helper for getting start position attribute from `xhp-indent-xhp-detect result"
  (cadr (xhp-indent-syntax-has-attribute
        (or xhp-indent-info (xhp-indent-xhp-detect)) 'xhp-start-pos)))

(defun xhp-indent-in-xhp ()
  "helper for detecting if point is in xhp"
  (xhp-indent-syntax-has-attribute (xhp-indent-xhp-detect) 'xhp-indent-in-xhp))

(defun xhp-indent-detect ()
  (interactive)
  (xhp-indent-syntax-indent-amount (xhp-indent-syntax-detect)))

(defun xhp-indent ()
  (interactive)
  (let
      ((indent (xhp-indent-detect)))
    (if indent
        (progn
          (xhp-indent-debug "xhp indent!!! %s" indent)
          ;; this is better than indent-to and indent-line-to because
          ;; it sets the point properly in a few different contexts.
          ;; e.g. when you've typed stuff, keep the point
          ;; but when you've typed nothing, go to end of line.
          (c-shift-line-indentation (- indent (current-indentation)))
          ))
    indent))

(defun xhp-cautious-indent-line ()
  "call xhp indent, or fallback to c-indent if not applicable"
  (if (not (xhp-indent))
      (funcall 'c-indent-line)))

(defun xhp-indent-line-or-region (&optional arg region)
  "Indent active region, current line, or block starting on this line.
In Transient Mark mode, when the region is active, reindent the region.
Otherwise, with a prefix argument, rigidly reindent the expression
starting on the current line.
Otherwise reindent just the current line."
  (interactive
   (list current-prefix-arg (use-region-p)))
  (if region
      (let
          (
           (lines-count (count-lines (region-beginning) (region-end)))
           )
      (save-excursion
        (goto-char (region-beginning))
        (dotimes (i lines-count)
          (xhp-cautious-indent-line)
          (forward-line 1))))
    (xhp-cautious-indent-line)))

;; Electric keys: override the built in C ones to use xhp-indent

(defun xhp-electric-semi&comma (arg)
  (interactive "*P")
  (if (and c-electric-flag (xhp-indent))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-semi&comma arg)))

(defun xhp-electric-brace (arg)
  (interactive "*P")
  (if (and c-electric-flag (xhp-indent))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-brace arg)))

(defun xhp-electric-brace-open (arg)
  (interactive "*P")
  (if (looking-back (concat xhp-start-regex ".*") (line-beginning-position))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-brace arg)))

(defun xhp-electric-colon (arg)
  (interactive "*P")
  (if (and c-electric-flag (xhp-indent))
      (self-insert-command (prefix-numeric-value arg))
    (c-electric-colon arg)))

;; fill-paragraph

(defun xhp-fill-paragraph (&optional arg)
  "attempt to break xhp into properly formatted and indented code"
  (interactive "*P")
  (let*
      (
       (line-too-long (lambda () (> (- (line-end-position) (line-beginning-position)) 80)))
       (break-line (lambda ()
                     (if (looking-back " +") (replace-match ""))
                     (insert "\n")
                     (xhp-indent-line-or-region)))
       (bol-looks-like
        (lambda (regex)
          (save-excursion
            (beginning-of-line)
            (and (not (c-in-literal)) (looking-at regex)))))
       (eol-looks-like
        (lambda (regex)
          (save-excursion
            (end-of-line)
            (and (not (c-in-literal)) (looking-back regex)))))
       (goto-last-fill-column
        (lambda ()
          (beginning-of-line)
          (forward-char 79)))
       (math-op " \\(!=\\|&&\\|||\\|==\\|\\*\\|+\\|-\\|/\\) ")
       (min (line-beginning-position))
       (max (copy-marker (line-end-position)))
       )
    (cond
     ;; CASE 0: if we're in a comment let the bultin function
     ;; do its thing
     ((let ((lit (c-in-literal))) (and lit (not (equal lit 'string))))
      (xhp-indent-debug "xhp-fill: in c literal")
      (c-fill-paragraph arg))
     ;; CASE 1: break up XHP
     ;; <foo a="foo" b={bar} ... >
     ;; becomes
     ;; <foo
     ;;   a="foo"
     ;;   b={bar}
     ;;   ...>
     ;;  (if ends with /> put on newline and outdent)
     ((funcall bol-looks-like xhp-start-regex)
      (xhp-indent-debug "xhp-fill: in xhp")
      ;; break up attributes
      (beginning-of-line)
      (while (re-search-forward " [a-z_-]+=\\(.\\)" (line-end-position) t)
        (let*
            (
             (php-attribute (equal (match-string 1) "{"))
             (beg (match-beginning 0))
             (end (copy-marker
                   (if (not php-attribute)
                       (and
                        (re-search-forward "\"" (line-end-position) t)
                        (point))
                     (forward-sexp)
                     (point))))
             )
          (goto-char beg)
          (funcall break-line)
          (goto-char end)
          )
        )
      (end-of-line)
      (if (looking-back "/>[;,)]*")
          (progn
            (goto-char (match-beginning 0))
            (funcall break-line)))
      )
     ;; CASE 2: long 'if()' stmt
     ;; break at boolean operators
     ((and (funcall line-too-long) (funcall bol-looks-like "^ *if (\\(.*\\)"))
      (xhp-indent-debug "xhp-fill: if stmt")
        (goto-char (match-beginning 1))
        (while (eq (c-forward-token-2 1 t max) 0)
          (if (and (looking-at "\\(||\\|&&\\)") (not (c-in-literal)))
              (funcall break-line))))
     ;; CASE 3: function decl/call too long:
     ;; foo(a,b,c,d,...) becomes
     ;; foo(
     ;;   a,
     ;;   b,
     ;;   ...
     ;; )
     ((funcall eol-looks-like "\\(,?.*)\\)\\([,;]\\|\\(: .+?\\)? {\\)?")
      (xhp-indent-debug "xhp-fill: func decl/call")
      (let
          ((func-decl-p (equal " {" (match-string 1))))
        (goto-char (match-end 1))
        (backward-sexp)
        (forward-char 1)
        (funcall break-line)
        (while (eq (c-forward-token-2 1 t max) 0)
          (if (and (looking-at ",") (not (c-in-literal)))
              (progn
                (forward-char 1)
                (funcall break-line))))
        (end-of-line)
        (if (not func-decl-p)
            (progn
              (re-search-backward ")")
              (funcall break-line)))))
     ;; CASE 4: long mathematical expression
     ((save-excursion (progn (end-of-line) (re-search-backward math-op min t)))
      (xhp-indent-debug "xhp-fill: math expr")
      (funcall goto-last-fill-column)
      (if (re-search-backward math-op min t)
          (progn
            (goto-char (match-beginning 1))
            (funcall break-line)))
      )
     ;; CASE 5: a bunch of string cat operators
     ;; 'a is '.$foo.' b is '.$bar.' etc.'
     ((save-excursion (end-of-line) (and (re-search-backward "['\"]\\." min t) (goto-char (match-end 0)) (not (c-in-literal))))
      (goto-char (match-end 0))
      (funcall break-line))
     ;; CASE 6: long string literal:
     ;; break at the '.' string concat
     ;; operators
     ((funcall eol-looks-like "\\(['\"]\\)[,;]?")
      (xhp-indent-debug "xhp-fill: long string literal")
      (let* (
             (quote-char (match-string 1))
             last
             )
        (funcall goto-last-fill-column)
        ;; try to break the string in a pretty way at spaces or punctuation
        (re-search-backward "[[:space:]]" (+ (line-beginning-position) 40) t)
        (insert quote-char)
        (funcall break-line)
        (insert "."quote-char)
        ))
     ;; CASE 7: chained method calls
     ;; a()->b()->c->d() etc.
     ((save-excursion (end-of-line) (and (re-search-backward "->" min t) (not (c-in-literal))))
      (xhp-indent-debug "xhp-fill: chained->method->calls")
      (goto-char (match-beginning 0))
      (funcall break-line))
     ;; DEFAULT: use the c paragraph filler
     ;;
     (t (c-fill-paragraph arg))
     )))

(defun xhp-indent-keybinds ()
  (local-set-key ";" 'xhp-electric-semi&comma)
  (local-set-key "," 'xhp-electric-semi&comma)
  (local-set-key "}" 'xhp-electric-brace)
  (local-set-key "{" 'xhp-electric-brace)
  (local-set-key ":" 'xhp-electric-colon)
  (local-set-key (kbd "ESC q") (quote xhp-fill-paragraph))
)

;; TODOS
;; arrays with xhp:
 ;; 'foo' =>
;;   <div>
;;     ...
;;   </div>,
;; 'bar'
;; php in xhp:
;; <ui:link
;;   href={$app->getAppCenterURL()}>
;;   {
;;     $this->getChildren()
;;       }
;; </ui:link>;
;; <br/> not on its own line:
;;; RunKeeper on your timeline<br/>
;;; <foo>
;; fails:
;; id(
;;   <fbt
;;     secret="appcenter"
;;     desc="platform type topnav filter, appcenter">
;;     All
;;   </fbt>
;; ), <= right here
(provide 'xhp-indent)
