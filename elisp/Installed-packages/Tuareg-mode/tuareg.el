;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tuareg.el - Caml mode for Emacs 19.xx and XEmacs 19.xx.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Copyright � 1997-1998 Albert Cohen, all rights reserved.
;;         Copying is covered by the GNU General Public License.

;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

(defconst tuareg-mode-version "Tuareg Version 1.36"
  "        Copyright � 1997-1998 Albert Cohen, all rights reserved.
         Copying is covered by the GNU General Public License.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Emacs versions support

(defconst tuareg-with-xemacs (string-match "XEmacs" emacs-version))

(if (not tuareg-with-xemacs)
    (defun match-string (num &optional string)
      "Return string of text matched by last search.

NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM
pairs.  Zero means the entire text matched by the whole regexp or
whole string.  STRING should be given if the last search was by
`string-match' on STRING."
      (let* ((data (match-data))
	     (begin (nth (* 2 num) data))
	     (end (nth (1+ (* 2 num)) data)))
	(if string (substring string begin end)
	  (buffer-substring-no-properties begin end)))))

(if (fboundp 'caar) ()
  (defun caar (l) (car (car l)))
  (defun cdar (l) (cdr (car l)))
  (defun cadr (l) (car (cdr l)))
  (defun cddr (l) (cdr (cdr l))))

(if (fboundp 'cadar) ()
  (defun cadar (l) (car (cdar l)))
  (defun cddar (l) (cdr (cdar l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

;; use `tuareg-mode-hook' to configure these variables, instead of patching
;; the mode itself!

;; comments

(defvar tuareg-indent-leading-comments t
  "*If true, indent leading comment lines (starting with `(*') like others.")
(defvar tuareg-indent-comments t
  "*If true, automatically align multi-line comments.")
(defvar tuareg-comment-end-extra-indent 0
  "*How many spaces to indent a leading comment end `*)'.
If you expect comments to be indented like
	(*
          ...
	 *)
even without leading `*', use `tuareg-comment-end-extra-indent' = 1.")
(defvar tuareg-support-leading-star-comments nil
  "*If true, allow automatic intentation of comments of the form
        (*
         * ...
         *)
If you still expect comments to be indented like
	(*
          ...
	 *)
without leading `*', use `tuareg-comment-end-extra-indent' = 1.")

;; indentation defaults

(defvar tuareg-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to.")

(defvar tuareg-lazy-= nil
  "*If true, indent `=' like a standard keyword (not `:=', `<='...).")

(defvar tuareg-lazy-paren nil
  "*If true, indent parentheses like a standard keyword.")

(defvar tuareg-let-always-indent t
  "*If true, enforce indentation is at least default after a let keyword.

As an example, set it to false when you have `tuareg-with-indent' set to 0,
and you want `let x = match ... with' and `match ... with' indent the
same way.")

(defvar tuareg-|-extra-unindent tuareg-default-indent
  "*Extra backward indent for Caml lines starting with the `|' operator.

It is automatically added to `function', `with', `parse' and some cases
of `type' keywords to leave enough space for `|' backward indentation.")

(defvar tuareg-class-indent tuareg-default-indent
  "*How many spaces to indent from a `class' keyword.")

(defvar tuareg-sig-struct-align t
  "*Align `sig' and `struct' keywords with `module'.")

(defvar tuareg-sig-struct-indent tuareg-default-indent
  "*How many spaces to indent from a `sig' or `struct' keyword.")

(defvar tuareg-method-indent tuareg-default-indent
  "*How many spaces to indent from a `method' keyword.")

(defvar tuareg-begin-indent tuareg-default-indent
  "*How many spaces to indent from a `begin' keyword.")

(defvar tuareg-for-while-indent tuareg-default-indent
  "*How many spaces to indent from a `for' or `while' keyword.")

(defvar tuareg-do-indent tuareg-default-indent
  "*How many spaces to indent from a `do' keyword.")

(defvar tuareg-fun-indent tuareg-default-indent
  "*How many spaces to indent from a `fun' keyword.")

(defvar tuareg-function-indent tuareg-default-indent
  "*How many spaces to indent from a `function' keyword.")

(defvar tuareg-if-then-else-indent tuareg-default-indent
  "*How many spaces to indent from an `if', `then' or `else' keyword
in Tuareg mode.")

(defvar tuareg-let-indent tuareg-default-indent
  "*How many spaces to indent from a `let' keyword.")

(defvar tuareg-in-indent tuareg-default-indent
  "*How many spaces to indent from a `in' keyword.
A lot of people like formatting `let' ... `in' expressions whithout
indentation:
        let x = 0 in
        blah x
Set this variable to 0 to get this behaviour.
However, nested declarations are always correctly handled:
        let x = 0 in                             let x = 0
        let y = 0 in              or             in let y = 0
        let z = 0 ...                            in let z = 0 ...")

(defvar tuareg-match-indent tuareg-default-indent
  "*How many spaces to indent from a `match' keyword.")

(defvar tuareg-try-indent tuareg-default-indent
  "*How many spaces to indent from a `try' keyword.")

(defvar tuareg-with-indent tuareg-default-indent
  "*How many spaces to indent from a `with' keyword.")

(defvar tuareg-rule-indent tuareg-default-indent
  "*How many spaces to indent from a `rule' keyword.")

(defvar tuareg-parse-indent tuareg-default-indent
  "*How many spaces to indent from a `parse' keyword.")

(defvar tuareg-parser-indent tuareg-default-indent
  "*How many spaces to indent from a `parser' keyword.")

(defvar tuareg-type-indent tuareg-default-indent
  "*How many spaces to indent from a `type' keyword.")

(defvar tuareg-val-indent tuareg-default-indent
  "*How many spaces to indent from a `val' keyword.")

;; automatic indentation
;; using abbrev-mode and electric keys

(defvar tuareg-use-abbrev-mode t
  "*Non-nil means electrically indent lines starting with leading keyword
such as `end', `done', `else' etc. It makes use of abbrev-mode.

Many people find eletric keywords irritating, so you can disable them in
setting this variable to nil.")
(make-variable-buffer-local 'tuareg-use-abbrev-mode)
(defvar tuareg-electric-indent t
  "*Non-nil means electrically indent lines starting with `|', `)', `]' or `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil.")
(make-variable-buffer-local 'tuareg-electric-indent)
(defvar tuareg-electric-close-vector t
  "*Non-nil means electrically insert a `|' before a vector-closing `]',
a `>' before a stream-closing `]' or before an object-closing `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil. You should probably have this on,
though, if you also have tuareg-electric-indent on.")
(make-variable-buffer-local 'tuareg-electric-close-vector)

;; Tuareg-Interactive
;; configure via `tuareg-mode-hook'

(defvar tuareg-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel.")

(defvar tuareg-interactive-read-only-input tuareg-with-xemacs
  "*Non-nil means input send to the Caml toplevel is read-only.")

(defvar tuareg-interactive-echo-phrase t
  "*Non-nil means echo phrases in the toplevel buffer when sending
them to the Caml toplevel.")

(defvar tuareg-interactive-input-font-lock t
  "*Non nil means Font-Lock for toplevel input phrases.")

(defvar tuareg-interactive-output-font-lock t
  "*Non nil means Font-Lock for toplevel output messages.")

(defvar tuareg-interactive-error-font-lock t
  "*Non nil means Font-Lock for toplevel error messages.")

(defvar tuareg-manual-url "http://pauillac.inria.fr/ocaml/htmlman/index.html"
  "*URL to the Caml reference manual.")

(defvar tuareg-browser 'tuareg-netscape-manual
  "*Name of function that displays the Caml reference manual.
Valid names are `tuareg-netscape-manual', `tuareg-mmm-manual'
and `tuareg-xemacs-w3-manual' (XEmacs only).")

(defvar tuareg-library-path "/usr/local/lib/ocaml/"
  "*Path to the Caml library.")

(defvar tuareg-definitions-max-items 30
  "*Maximum number of items a definitions menu can contain.")

(defvar tuareg-options-list
  '(("Lazy Parentheses Indentation" . 'tuareg-lazy-paren)
    ("Lazy `=' Indentation" . 'tuareg-lazy-=)
    ("Force indentation after `let'" . 'tuareg-let-always-indent)
    "---"
    ("Indent body of comments" . 'tuareg-indent-comments)
    ("Indent first line of comments" . 'tuareg-indent-leading-comments)
    ("Leading-`*' comment style" . 'tuareg-support-leading-star-comments)
    "---"
    ("Objective Labl Syntax" . 'tuareg-labl-support))
  "*List of menu-configurable Tuareg options")

(defvar tuareg-interactive-options-list
  '(("Skip phrase after evaluation" . 'tuareg-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'tuareg-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'tuareg-interactive-input-font-lock)
    ("Font-lock interactive output" . 'tuareg-interactive-output-font-lock)
    ("Font-lock interactive error" . 'tuareg-interactive-error-font-lock)
    "---"
    ("Read only input (XEmacs)" . 'tuareg-interactive-read-only-input))
  "*List of menu-configurable Tuareg options")

(defvar tuareg-interactive-program "ocaml"
  "*Default program name for invoking a Caml toplevel from Emacs.")
(make-variable-buffer-local 'tuareg-interactive-program)

;; customizable faces for Font-Lock mode
;; configure via `tuareg-mode-hook'

(defvar tuareg-font-lock-documentation '("goldenrod4" "goldenrod" t nil t nil)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-comment '("pink4" "moccasin" t nil t nil)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-string '("darkgreen" "aquamarine2" nil nil nil nil)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-module '("midnightblue" "lightblue" nil t nil t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-governing '("darkorange2" "orange" nil t nil t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-exception '("red" "red" nil t nil t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-preprocessor '("darkorchid" "yellow" nil nil nil nil)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-keyword '("blue3" "skyblue" nil t nil t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-function '("blue2" "cyan" nil t nil t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-operator '("brown" "lightgray" nil nil nil nil)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-reference '("firebrick" "plum1" nil nil t t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-function-name '("blue2" "cyan" nil nil t t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-exception-name '("red" "red" nil nil t t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-interactive-output '("blue4" "lightblue" t nil t nil)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")
(defvar tuareg-font-lock-interactive-error '("firebrick" "plum1" t t t t)
  "Face description: LIGHT DARK col-ITALIC col-BOLD font-ITALIC font-BOLD.")

;; end of customizable variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

(defvar tuareg-window-system
  (or (and (boundp 'window-system) window-system)
      (and (fboundp 'console-type) (or (eq (console-type) 'x)
				       (eq (console-type) 'win32))))
  "Are we running under a window system?")

(if (and (featurep 'font-lock)
	 (or tuareg-window-system tuareg-with-xemacs))
    (progn
      (if (boundp 'font-lock-use-colors)
	  (defvar tuareg-font-lock-use-colors font-lock-use-colors))
      (if (boundp 'font-lock-use-fonts)
	  (defvar tuareg-font-lock-use-fonts font-lock-use-fonts))

      (if (not tuareg-with-xemacs) (transient-mark-mode t)
	(defvar tuareg-ext-start nil "Temporary for fontification.")
	(make-variable-buffer-local 'tuareg-ext-start)
	(defvar tuareg-ext-end nil "Temporary for fontification.")
	(make-variable-buffer-local 'tuareg-ext-end)

	(defun tuareg-pre-idle-hook-first ()
	  (condition-case nil
	      (if font-lock-old-extent
		  (setq tuareg-ext-start
			(extent-start-position font-lock-old-extent)
			tuareg-ext-end
			(extent-end-position font-lock-old-extent))
		(setq tuareg-ext-start nil))
	    (error (warn "Error caught in `tuareg-pre-idle-hook-first'"))))

	(defun tuareg-pre-idle-hook-last ()
	  (condition-case nil
	      (if (and (eq major-mode 'tuareg-mode)
		       tuareg-ext-start)
		  (tuareg-fontify tuareg-ext-start tuareg-ext-end))
	    (error (warn "Error caught in `tuareg-pre-idle-hook-last'"))))

	(add-hook 'font-lock-after-fontify-buffer-hook 'tuareg-fontify-buffer)

	(defun tuareg-fontify-buffer ()
	  (if (eq major-mode 'tuareg-mode)
	      (tuareg-fontify (point-min) (point-max))))

	(defun tuareg-fontify (start end)
	  (save-excursion
	    (goto-char start) (beginning-of-line) (setq start (point))
	    (goto-char end) (end-of-line) (setq end (point))
	    (while (> end start)
	      (goto-char (1- end))
	      (tuareg-in-literal-or-comment)
	      (cond
	       ((cdr tuareg-last-loc)
		(tuareg-beginning-of-literal-or-comment)
		(font-lock-set-face (max start (point)) end
				    (if (looking-at "(\\*[tT][eE][xX]")
					'tuareg-font-lock-documentation-face
				      'tuareg-font-lock-comment-face))
		(setq end (1- (point))))
	       ((car tuareg-last-loc)
		(tuareg-beginning-of-literal-or-comment)
		(font-lock-set-face (max start (point)) end
				    'tuareg-font-lock-string-face)
		(setq end (point)))
	       (t (while (and tuareg-cache-local
			      (or (> (caar tuareg-cache-local) end)
				  (eq 'b (cadar tuareg-cache-local))))
		    (setq tuareg-cache-local (cdr tuareg-cache-local)))
		  (setq end (if tuareg-cache-local
				(caar tuareg-cache-local) start))))))))

      (defun tuareg-font-lock-hook ()
	"Function called by `font-lock-mode' for initialization purposes."
	(if (eq major-mode 'tuareg-mode)
	    (progn
	      (if (not tuareg-with-xemacs) ()
		(add-hook 'pre-idle-hook 'tuareg-pre-idle-hook-first)
		(add-hook 'pre-idle-hook 'tuareg-pre-idle-hook-last t)
		(if (or (and tuareg-font-lock-use-colors
			     font-lock-use-colors)
			(and tuareg-font-lock-use-fonts
			     font-lock-use-fonts)) ()
		  (setq tuareg-font-lock-use-colors font-lock-use-colors)
		  (setq tuareg-font-lock-use-fonts font-lock-use-fonts)
		  (tuareg-set-font-lock-faces))))))

      (defun tuareg-make-face-italic (face)
	(condition-case nil (make-face-italic face) (error nil)))
      (defun tuareg-make-face-bold (face)
	(condition-case nil (make-face-bold face) (error nil)))
      (defun tuareg-make-face-unitalic (face)
	(condition-case nil (make-face-unitalic face) (error nil)))
      (defun tuareg-make-face-unbold (face)
	(condition-case nil (make-face-unbold face) (error nil)))

      (defun tuareg-make-face (name)
	(let ((desc (eval (intern name)))
	      (face (intern (concat name "-face"))))
	  (if (not (facep face)) (make-face face))
	  (set-face-foreground face
			       (if use-fonts default-color
				 (if light-bg (nth 0 desc) (nth 1 desc))))
	  (if use-fonts
	      (progn
		(if (nth 4 desc) (tuareg-make-face-italic face)
		  (tuareg-make-face-unitalic face))
		(if (nth 5 desc) (tuareg-make-face-bold face)
		  (tuareg-make-face-unbold face)))
	    (if (nth 2 desc) (tuareg-make-face-italic face)
	      (tuareg-make-face-unitalic face))
	    (if (nth 3 desc) (tuareg-make-face-bold face)
	      (tuareg-make-face-unbold face)))))

      (defun tuareg-set-font-lock-faces ()
	"Set faces for Font-Lock mode."
	(let* ((use-fonts
		(or (and (boundp 'font-lock-use-fonts)
			 font-lock-use-fonts
			 (not (and (boundp 'font-lock-use-colors)
				   font-lock-use-colors)))
		    (and (fboundp 'device-class)
			 (eq (device-class) 'mono))
		    (and (not (fboundp 'device-class))
			 (fboundp 'x-display-color-p)
			 (not (x-display-color-p)))))
	       (light-bg
		(if tuareg-window-system
		    (if tuareg-with-xemacs
			(if (and (fboundp 'color-rgb-components)
				 (< (apply '+ (color-rgb-components
					       (make-color-specifier
						[default background])))
				    (* (apply '+
					      (color-rgb-components
					       (make-color-specifier "white")))
				       0.6))) nil t)
		      (let ((param (cdr (assq 'background-color
					      (frame-parameters)))))
			(cond
			 ((boundp 'font-lock-background-mode)
			  (if (eq font-lock-background-mode 'dark) nil t))
			 ((eq system-type 'ms-dos)
			  (if (string-match "light" param) t nil))
			 ((and param (fboundp 'x-color-values)
			       (< (apply '+ (x-color-values param))
				  (* (apply '+ (x-color-values "white"))
				     0.6))) nil)
			 (t t))))))
	       (default-color (if light-bg "black" "white")))
	  (mapcar 'tuareg-make-face
		  '("tuareg-font-lock-documentation"
		    "tuareg-font-lock-comment"
		    "tuareg-font-lock-string"
		    "tuareg-font-lock-module"
		    "tuareg-font-lock-governing"
		    "tuareg-font-lock-preprocessor"
		    "tuareg-font-lock-keyword"
		    "tuareg-font-lock-function"
		    "tuareg-font-lock-exception"
		    "tuareg-font-lock-reference"
		    "tuareg-font-lock-operator"
		    "tuareg-font-lock-function-name"
		    "tuareg-font-lock-exception-name"
		    "tuareg-font-lock-interactive-output"
		    "tuareg-font-lock-interactive-error"))))

      (defvar tuareg-font-lock-keywords
	(append
	 (if tuareg-with-xemacs ()
	   '(("\\(^\\|[^'`]\\)\"\\([^\"\n]\\|\\\\\"\\)*\\((\\*\\|\\*)\\)\\([^\"\n]\\|\\\\\"\\)*\""
	      0 'tuareg-font-lock-string-face t) ; big hack, especially the \n's
	     ("(\\*[tT][eE][xX]\\>\\([^*]\\|\\*+[^*)]\\)*\\(\\*+)\\|\\**$\\)"
	      0 'tuareg-font-lock-documentation-face t)
	     ("(\\*\\([^*]\\|\\*+[^*)]\\)*\\(\\*+)\\|\\**$\\)"
	      0 'tuareg-font-lock-comment-face t)
	     ("\\(^\\|[^'`]\\)\"\\([^\\\"]\\|\\\\.\\)*\"\\|'\\([^\\\n']\\|\\\\[\\a-z']\\|\\\\[0-9][0-9][0-9]\\)'\\|`\\([^\\\n`]\\|\\\\[\\a-z`]\\|\\\\[0-9][0-9][0-9]\\)`"
	      0 'tuareg-font-lock-string-face t)))
	 '(("^#[ \t]*[a-z][_a-z]*\\>\\|;;\\|\\<external\\>"
	    0 'tuareg-font-lock-preprocessor-face nil)
	   ("\\<\\(open\\|s\\(ig\\|truct\\)\\|module\\([ \t\n]+type\\)?\\|functor\\|\\(with\\|and\\|let\\)[ \t\n]+\\(type\\|module\\)\\)\\>"
	    0 'tuareg-font-lock-module-face nil)
	   ("\\<\\(v\\(al\\|irtual\\)\\|type\\|method\\|c\\(onstraint\\|lass\\)\\|in\\(herit\\|itializer\\)?\\|let\\|rec\\|and\\|begin\\|object\\|end\\)\\>"
	    0 'tuareg-font-lock-governing-face nil)
	   ("\\<\\(exception\\|raise\\|failwith\\|assert\\)\\>"
	    0 'tuareg-font-lock-exception-face nil)
	   ("\\<fun\\(ction\\)?\\>"
	    0 'tuareg-font-lock-function-face nil)
	   ("\\<\\(as\\|do\\(ne\\|wnto\\)?\\|else\\|for\\|if\\|let\\|m\\(atch\\|utable\\)\\|new\\|p\\(arser\\|rivate\\)\\|t\\(hen\\|o\\|ry\\)\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\|lazy\\)\\>"
	    0 'tuareg-font-lock-keyword-face nil)
	   ("\\<open\\>[ \t\n]*\\([_A-Za-z\277-\377]\\(\\w\\|\\.\\)*\\)"
	    1 'tuareg-font-lock-function-name-face nil)
	   ("\\<\\([_A-Za-z\277-\377]\\w*\\)\\>[ \t\n]*:\\<[^:=>]"
	    1 'tuareg-font-lock-function-name-face nil)
	   ("\\<\\(virtual\\|val\\([ \t\n]+mutable\\)?\\|inherit\\|private\\|constraint\\|external\\|method\\|module\\([ \t\n]+type\\)?\\|and\\|type\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t]*\\(\\(\\<['_A-Za-z\277-\377]\\w*\\>[ \t]*\\)*\\)\\>"
	    5 'tuareg-font-lock-function-name-face nil)
	   ("\\<exception\\>[ \t]*\\(\\<[_A-Za-z\277-\377]\\w*\\>\\)"
	    1 'tuareg-font-lock-exception-name-face nil)
	   ("\\<\\(as[lr]\\|false\\|l\\(and\\|xor\\|or\\|s[lr]\\)mod\\|not\\|ref\\|o[fr]\\|true\\|unit\\)\\>"
	    0 'tuareg-font-lock-reference-face nil)
	   ("[][;,()|{}@^!:#*=<>&/%+---]\\.?\\|\\.\\."
	    0 'tuareg-font-lock-operator-face nil)))
	"Font-Lock patterns for Tuareg mode.")

      (if (featurep 'sym-lock)
	  ;; to change this table, xfd -fn '-adobe-symbol-*--12-*' may be
	  ;; used to determine the symbol character codes.
	  (defvar tuareg-sym-lock-keywords
	    '(("<-" 0 1 172) ("->" 0 1 174)
	      (":=" 0 1 220) ("<=" 0 1 163)
	      (">=" 0 1 179) ("<>" 0 1 185)
	      ("==" 0 1 186) ("||" 0 1 218)
	      ("&&" 0 1 217) ("[^*]\\(\\*\\)\\." 1 8 180)
	      ("\\(/\\)\\." 1 3 184) (":>" 0 1 202)
	      (";;" 0 1 191) ("\\<_\\>" 0 3 188)
	      ("\\<sqrt\\>" 0 3 214) ("\\<unit\\>" 0 3 198)
	      ;; ("\\<fun\\>" 0 3 108)
	      ("\\<or\\>" 0 3 218) ("\\<not\\>" 0 3 216))
	    "If non nil: Overrides default Sym-Lock patterns for Tuareg."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar tuareg-labl-support nil
  "Do not modify this variable directly,
use function `tuareg-do-support-labl'.")

(defun tuareg-do-support-labl (&optional switch)
  "Switch from OCaml to OLabl syntax in modifying the syntax of operator `:'.
You may also use an argument `0' (resp. `1') to disable (resp. enable)
OLabl syntax."
  (interactive)
  (cond ((not switch) (setq tuareg-labl-support (not tuareg-labl-support)))
	((>= switch 1) (setq tuareg-labl-support t))
	(t (setq tuareg-labl-support nil)))
  (if tuareg-labl-support
      (progn
	(modify-syntax-entry ?: "w" tuareg-mode-syntax-table)
	(modify-syntax-entry ?` "w" tuareg-mode-syntax-table)
	(message "Objective Labl syntax now supported."))
    (modify-syntax-entry ?: "." tuareg-mode-syntax-table)
    (modify-syntax-entry ?` "\"" tuareg-mode-syntax-table)
    (message "Objective Caml syntax.")))

(defvar tuareg-mode-map nil
  "Keymap used in Tuareg mode.")
(setq tuareg-mode-map (make-sparse-keymap))
(define-key tuareg-mode-map "#" 'tuareg-electric)
(define-key tuareg-mode-map "%" 'tuareg-electric)
(define-key tuareg-mode-map "|" 'tuareg-electric)
(define-key tuareg-mode-map ")" 'tuareg-electric)
(define-key tuareg-mode-map "}" 'tuareg-electric-rc)
(define-key tuareg-mode-map "]" 'tuareg-electric-rb)
(define-key tuareg-mode-map "\t" 'tuareg-indent-command)
(define-key tuareg-mode-map "\M-\C-h" 'tuareg-mark-phrase)
(define-key tuareg-mode-map "\M-q" 'tuareg-indent-phrase)
(define-key tuareg-mode-map "\C-c\C-q" 'tuareg-indent-phrase)
(define-key tuareg-mode-map "\C-c\C-a" 'tuareg-find-alternate-file)
(define-key tuareg-mode-map "\C-c\C-c" 'compile)
(define-key tuareg-mode-map "\M-\C-x" 'tuareg-eval-phrase)
(define-key tuareg-mode-map "\C-x\C-e" 'tuareg-eval-phrase)
(define-key tuareg-mode-map "\C-c\C-e" 'tuareg-eval-phrase)
(define-key tuareg-mode-map "\C-c\C-r" 'tuareg-eval-region)
(define-key tuareg-mode-map "\C-c\C-b" 'tuareg-eval-buffer)
(define-key tuareg-mode-map "\C-c\C-s" 'tuareg-run-caml)
(define-key tuareg-mode-map "\C-c\C-i" 'tuareg-interrupt-caml)
(define-key tuareg-mode-map "\C-c\C-k" 'tuareg-kill-caml)
(define-key tuareg-mode-map "\C-c\C-n" 'tuareg-next-phrase)
(define-key tuareg-mode-map "\C-c\C-p" 'tuareg-previous-phrase)
(define-key tuareg-mode-map [(meta control down)]  'tuareg-next-phrase)
(define-key tuareg-mode-map [(meta control up)] 'tuareg-previous-phrase)
(define-key tuareg-mode-map "\C-c`" 'tuareg-interactive-next-error)
(define-key tuareg-mode-map "\C-cb" 'tuareg-insert-begin-form)
(define-key tuareg-mode-map "\C-cf" 'tuareg-insert-for-form)
(define-key tuareg-mode-map "\C-cw" 'tuareg-insert-while-form)
(define-key tuareg-mode-map "\C-ci" 'tuareg-insert-if-form)
(define-key tuareg-mode-map "\C-cl" 'tuareg-insert-let-form)
(define-key tuareg-mode-map "\C-cm" 'tuareg-insert-match-form)
(define-key tuareg-mode-map "\C-ct" 'tuareg-insert-try-form)

(defvar tuareg-mode-syntax-table ()
  "Syntax table in use in Tuareg mode buffers.")
(setq tuareg-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?_ "w" tuareg-mode-syntax-table)
(modify-syntax-entry ?: "." tuareg-mode-syntax-table)
(modify-syntax-entry ?' "w" tuareg-mode-syntax-table)
;; ' is part of words (for primes)
(modify-syntax-entry ?` "\"" tuareg-mode-syntax-table)
;; ` is a string delimiter (camllight compatibility)
(modify-syntax-entry ?\" "\"" tuareg-mode-syntax-table)
;; " is a string delimiter
(modify-syntax-entry ?\\ "\\" tuareg-mode-syntax-table)
(modify-syntax-entry ?\( "()1" tuareg-mode-syntax-table)
(modify-syntax-entry ?*  ".23" tuareg-mode-syntax-table)
(modify-syntax-entry ?\) ")(4" tuareg-mode-syntax-table)
(let ((i 192))
  (while (< i 256)
    (modify-syntax-entry i "w" tuareg-mode-syntax-table)
    (setq i (1+ i))))

(defconst tuareg-font-lock-syntax
  '((?` . ".") (?\" . "."))
  "Syntax changes for Font-Lock.")

(defvar tuareg-mode-abbrev-table ()
  "Abbrev table used for Tuareg mode buffers.")
(defun tuareg-define-abbrev (keyword)
  (define-abbrev tuareg-mode-abbrev-table keyword keyword 'tuareg-abbrev-hook))
(if tuareg-mode-abbrev-table ()
  (setq tuareg-mode-abbrev-table (make-abbrev-table))
  (mapcar 'tuareg-define-abbrev
	  '("module" "class" "object" "let" "type" "val" "inherit" "virtual"
	    "constraint" "exception" "external" "open" "method" "and"
	    "initializer" "to" "downto" "do" "done" "else" "end" "in"
	    "then" "with"))
  (setq abbrevs-changed nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

(defun tuareg-mode ()
  "Major mode for editing Caml code.

Dedicaced to Emacs and XEmacs. Works with Emacs version 19.30 and
higher, XEmacs version 19.13 and higher. Provides automatic
indentation and compilation interface. Performs font/color highlighting
using Font-Lock. It is designed for Objective Caml but handles
Objective Labl and Camllight as well.

Report bugs, remarks and questions to Albert.Cohen@prism.uvsq.fr.

If you use XEmacs, the Font-Lock minor-mode is used accordingly to
your customization options. Within Emacs, you may want to use
Font-Lock in addding the following lines to the configuration file:

  (if (and (boundp 'window-system) window-system)
      (require 'font-lock))

Within XEmacs (more generally, if you have variable-sized fonts
and `atomic-extents' supported) you may also want to use Sym-Lock:

  (if (and (boundp 'window-system) window-system
	   (string-match \"XEmacs\" emacs-version))
      (require 'sym-lock))

Emacs supports neither variable-sized fonts nor atomic sequences, perhaps
it is a good occasion for you to try XEmacs!

You have better byte-compile tuareg.el (and sym-lock.el if you use it)
because symbol highlighting is very time consuming.

For customization purposes, you should use `tuareg-mode-hook'
(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself. You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            '(lambda ()
               ... ; your customization code
             ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

A special case is Sym-Lock customization: You may set
`tuareg-sym-lock-keywords' in your `.emacs' configuration file
to override default Sym-Lock patterns.

`custom-tuareg.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

To enable OLabl support call `tuareg-do-support-labl', either
interactively or within `tuareg-mode-hook'. It is a switch when supplied with
no arguments, but you may use `0' (resp. `1') to disable (resp. enable)
OLabl syntax.

`M-x camldebug' FILE starts the Caml debugger camldebug on the executable
FILE, with input and output in an Emacs buffer named *camldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a toplevel is included.
Type `M-x tuareg-run-caml' or see special-keys below.

Some elementary rules have to be followed in order to get the best of
indentation facilities.
  - Because the `function' keyword has a special indentation (to handle
    case matchs) use the `fun' keyword when no case match is done.
  - Prefer the `or' keyword to `||' (they are semantically equivalent),
    it avoids some unwanted electric indentations.
  - In OCaml, `;;' is no longer necessary for correct indentation,
    except before top level phrases not introduced by `type', `val', `let'
    etc. (i.e. phrases used for their side-effects or to be executed
    in a top level.)
  - If you keep the default value for `tuareg-indent-leading-comments',
    never write comments at the beginning of a line followed by some code,
    since the line will not be automatically indented. This recommendation
    does not hold if `tuareg-indent-leading-comments' is true.
  - Long sequences of `and's may slow down indentation slightly, since
    some computations (few actually) require to go back to the
    beginning of the sequence. Some very long nested blocks may also lead
    to slow processing of `end's, `else's, `done's...
  - Multiline strings are handled properly, but the string concatenation `^'
    is preferred to break long strings (the C-j keystroke can help in some
    cases).

Known bugs:
  - With Emacs: Text highlighting may be incorrect in some tricky examples
    (nested comments and strings), but there exists no proper solution
    to this without rewritting Font-Lock algorithms. This has been done
    with the improved XEmacs Font-Lock interface: Use XEmacs!
  - When writting a line with mixed code and comments, avoid putting
    comments at the beginning or middle of the text. More precisely, 
    writing comments immediately after `=' or parentheses then writing
    some more code on the line leads to indentation errors: You may write
    `let x (* blah *) = blah' but should avoid `let x = (* blah *) blah'.
    I'll try to fix this, even if I did not get any bug report about
    it so far...

Special keys for Tuareg mode:\\{tuareg-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq case-fold-search nil)
  (setq major-mode 'tuareg-mode)
  (setq mode-name "Tuareg")
  (use-local-map tuareg-mode-map)
  (set-syntax-table tuareg-mode-syntax-table)
  (setq local-abbrev-table tuareg-mode-abbrev-table)

  (if tuareg-window-system (tuareg-build-menu))

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|\\*)$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]*")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tuareg-indent-command)
  (make-variable-buffer-local 'before-change-functions)
  (add-hook 'before-change-functions 'tuareg-before-change-function)
  (make-variable-buffer-local 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'tuareg-auto-fill-function)

  ;; hooks for tuareg-mode, use them for tuareg-mode configuration
  (run-hooks 'tuareg-mode-hook)

  (tuareg-install-font-lock)

  (if tuareg-use-abbrev-mode (abbrev-mode 1))
  (message (concat "Major mode for Caml programs, " tuareg-mode-version ".")))

(defun tuareg-install-font-lock ()
  (if (and (featurep 'font-lock)
	   (or tuareg-window-system tuareg-with-xemacs))
      (progn
	(tuareg-set-font-lock-faces)
	(if (featurep 'sym-lock) ; needed AFTER tuareg-set-font-lock-faces
	    (progn
	      (setq sym-lock-color
		    (face-foreground 'tuareg-font-lock-operator-face))
	      (if (not sym-lock-keywords)
		  (sym-lock tuareg-sym-lock-keywords))))
	(add-hook 'font-lock-mode-hook 'tuareg-font-lock-hook)
	(make-variable-buffer-local 'font-lock-defaults)
	(setq font-lock-defaults
	      (list 'tuareg-font-lock-keywords t nil tuareg-font-lock-syntax))
	(font-lock-set-defaults)
	(if (not (or tuareg-with-xemacs font-lock-mode)) (font-lock-mode 1))
	'font-lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In Emacs 19, the regexps in compilation-error-regexp-alist do not
;; match the error messages when the language is not English.
;; Hence we add a regexp.

(defconst tuareg-error-regexp
  "^[A-\377]+ \"\\([^\"\n]+\\)\", [A-\377]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by (o)camlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc tuareg-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list tuareg-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info.

(defconst tuareg-error-chars-regexp
  ".*, .*, [A-\377]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by (o)camlc.")

;; Wrapper around next-error.

;; itz 04-21-96 somebody didn't get the documentation for next-error
;; right. When the optional argument is a number n, it should move
;; forward n errors, not reparse.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer  

(defadvice next-error (after tuareg-next-error activate)
 "Reads the extra positional information provided by the Caml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."
 (if (eq major-mode 'tuareg-mode)
     (let ((beg nil) (end nil))
       (save-excursion
	 (set-buffer compilation-last-buffer)
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer))))
	   (if (looking-at tuareg-error-chars-regexp)
	       (setq beg (string-to-int (match-string 1))
		     end (string-to-int (match-string 2))))))
       (beginning-of-line)
       (if beg
	   (progn
	     (setq beg (+ (point) beg) end (+ (point) end))
	     (goto-char beg) (push-mark end t t))))))

(defvar tuareg-interactive-error-regexp
  (concat "\\(\\("
	  "Toplevel input:"
	  "\\|Entr.e interactive:"
	  "\\|Characters [0-9-]*:"
	  "\\|The global value [^ ]* is referenced before being defined."
	  "\\|La valeur globale [^ ]* est utilis.e avant d'.tre d.finie."
	  "\\|Reference to undefined global"
	  "\\|The C primitive \"[^\"]*\" is not available."
	  "\\|La primitive C \"[^\"]*\" est inconnue."
	  "\\|Cannot find \\(the compiled interface \\)?file"
	  "\\|L'interface compil.e [^ ]* est introuvable."
	  "\\|Le fichier [^ ]* est introuvable."
	  "\\|Exception non rattrap.e:"
	  "\\|Uncaught exception:"
	  "\\)[^#]*\\)" )
  "Regular expression matching the error messages produced by Caml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

(defun tuareg-auto-fill-function ()
  (if (not (tuareg-in-literal-p))
      (do-auto-fill)))

(defun tuareg-forward-char (&optional step)
  (if step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun tuareg-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun tuareg-in-indentation-p ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defvar tuareg-cache-stop (point-min))
(make-variable-buffer-local 'tuareg-cache-stop)
(defvar tuareg-cache nil)
(make-variable-buffer-local 'tuareg-cache)
(defvar tuareg-cache-local nil)
(make-variable-buffer-local 'tuareg-cache-local)
(defvar tuareg-cache-last-local nil)
(make-variable-buffer-local 'tuareg-cache-last-local)
(defvar tuareg-last-loc (cons nil nil))

(defun tuareg-before-change-function (begin end)
;;  (message (concat (number-to-string begin) "-" (number-to-string end)))
  (setq tuareg-cache-stop (min tuareg-cache-stop (1- begin))))

(defun tuareg-in-literal-p ()
  "Returns non-nil if point is inside a Caml literal."
  (car (tuareg-in-literal-or-comment)))
(defun tuareg-in-comment-p ()
  "Returns non-nil if point is inside a Caml comment."
  (cdr (tuareg-in-literal-or-comment)))
(defun tuareg-in-literal-or-comment-p ()
  "Returns non-nil if point is inside a Caml literal or comment."
  (tuareg-in-literal-or-comment)
  (or (car tuareg-last-loc) (cdr tuareg-last-loc)))
(defun tuareg-in-literal-or-comment ()
  "Returns the pair `((tuareg-in-literal-p) . (tuareg-in-comment-p))'."
  (if (and (<= (point) tuareg-cache-stop) tuareg-cache)
      (progn
	(if (or (not tuareg-cache-local) (not tuareg-cache-last-local)
		(and (>= (point) (caar tuareg-cache-last-local))))
	    (setq tuareg-cache-local tuareg-cache))
	(while (and tuareg-cache-local (< (point) (caar tuareg-cache-local)))
	  (setq tuareg-cache-last-local tuareg-cache-local
		tuareg-cache-local (cdr tuareg-cache-local)))
	(setq tuareg-last-loc
	      (if tuareg-cache-local
		  (cons (eq (cadar tuareg-cache-local) 'b)
			(> (cddar tuareg-cache-local) 0))
		(cons nil nil))))
    (let ((flag t) (op (point)) (mp (min (point) (1- (point-max))))
	  (literal nil) (balance 0) (end-of-comment nil))
      (if tuareg-cache
	  (progn
	    (while (and tuareg-cache (< tuareg-cache-stop (caar tuareg-cache)))
	      (setq tuareg-cache (cdr tuareg-cache)))
	    (if tuareg-cache
		(prog2
		    (if (eq (cadar tuareg-cache) 'b)
			(setq tuareg-cache-stop (1- (caar tuareg-cache))))
		    (setq balance (cddar tuareg-cache))))))
      (goto-char tuareg-cache-stop)
      (if (char-equal ?* (preceding-char)) (backward-char))
      (while flag
	(setq literal nil)
	(if end-of-comment (setq balance 0 end-of-comment nil))
	(skip-chars-forward "^\\\\'`\"(\\*")
	(cond
	 ((looking-at "\\\\")
	  (tuareg-forward-char 2))
	 ((looking-at "'\\([^\n']\\|\\\\..?.?\\)'")
	  (tuareg-forward-char)
	  (setq tuareg-cache (cons (cons (point) (cons 'b balance))
				   tuareg-cache))
	  (skip-chars-forward "^'") (tuareg-forward-char)
	  (setq tuareg-cache (cons (cons (point) (cons 'e balance))
				   tuareg-cache))
	  (setq literal t))
	 ((looking-at "`\\([^\n']\\|\\\\..?.?\\)`")
	  (tuareg-forward-char)
	  (setq tuareg-cache (cons (cons (point) (cons 'b balance))
				   tuareg-cache))
	  (skip-chars-forward "^`") (tuareg-forward-char)
	  (setq tuareg-cache (cons (cons (point) (cons 'e balance))
				   tuareg-cache))
	  (setq literal t))
	 ((looking-at "\"")
	  (tuareg-forward-char)
	  (setq tuareg-cache (cons (cons (point) (cons 'b balance))
				   tuareg-cache))
	  (skip-chars-forward "^\\\\\"")
	  (while (looking-at "\\\\")
	    (tuareg-forward-char 2) (skip-chars-forward "^\\\\\""))
	  (tuareg-forward-char)
	  (setq tuareg-cache (cons (cons (point) (cons 'e balance))
				   tuareg-cache))
	  (setq literal t))
	 ((looking-at "(\\*")
	  (setq balance (1+ balance))
	  (setq tuareg-cache (cons (cons (point) (cons nil balance))
				   tuareg-cache))
	  (tuareg-forward-char 2))
	 ((looking-at "\\*)")
	  (tuareg-forward-char 2)
	  (if (> balance 1)
	      (prog2
		  (setq balance (1- balance))
		  (setq tuareg-cache (cons (cons (point) (cons nil balance))
					   tuareg-cache)))
	    (setq end-of-comment t)
	    (setq tuareg-cache (cons (cons (point) (cons nil 0))
				     tuareg-cache))))
	 (t (tuareg-forward-char)))
	(setq flag (<= (point) mp)))
      (setq tuareg-cache-local tuareg-cache
	    tuareg-cache-stop (point))
      (goto-char op)
      (if tuareg-cache (tuareg-in-literal-or-comment) (cons nil nil)))))

(defun tuareg-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (if (tuareg-in-literal-or-comment-p)
      (tuareg-beginning-of-literal-or-comment-fast)))

(defun tuareg-beginning-of-literal-or-comment-fast ()
  (while (and tuareg-cache-local
	      (or (eq 'b (cadar tuareg-cache-local))
		  (> (cddar tuareg-cache-local) 0)))
    (setq tuareg-cache-last-local tuareg-cache-local
	  tuareg-cache-local (cdr tuareg-cache-local)))
  (goto-char (caar tuareg-cache-last-local))
  (if (eq 'b (cadar tuareg-cache-last-local)) (tuareg-backward-char)))

(defun tuareg-false-=-p ()
  "Is the underlying `=' the second letter of an operator?"
  (or (char-equal ?: (preceding-char))
      (char-equal ?> (preceding-char))
      (char-equal ?< (preceding-char))
      (char-equal ?= (preceding-char))))

(defun tuareg-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
		(char-equal ?\; (char-after (1+ (point)))))
	   (char-equal ?\; (preceding-char)))))

(defun tuareg-backward-up-list ()
  "Safe up-list regarding comments, literals and errors."
  (let ((balance 1) (op (point)) (oc nil))
    (tuareg-in-literal-or-comment)
    (while (and (> (point) (point-min)) (> balance 0))
      (setq oc (if tuareg-cache-local (caar tuareg-cache-local) (point-min)))
      (condition-case nil (up-list -1) (error (goto-char (point-min))))
      (if (>= (point) oc) (setq balance (1- balance))
	(goto-char op)
	(skip-chars-backward "^[]{}()") (tuareg-backward-char)
	(if (not (tuareg-in-literal-or-comment-p))
	    (cond
	     ((looking-at "[[{(]")
	      (setq balance (1- balance)))
	     ((looking-at "[]})]")
	      (setq balance (1+ balance))))
	  (tuareg-beginning-of-literal-or-comment-fast)))
      (setq op (point)))))

(defun tuareg-assoc-indent (kwop &optional looking-let)
  "Returns relative indentation of the keyword given in argument."
  (let ((ind (symbol-value (cdr (assoc kwop tuareg-keyword-alist)))))
    (if (string-match "\\<\\(with\\|function\\|parser?\\)\\>" kwop)
	(+ (if (and tuareg-let-always-indent
		    looking-let (< ind tuareg-default-indent))
	       tuareg-let-indent ind) tuareg-|-extra-unindent) ind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation stuff

(defconst tuareg-keyword-regexp "\\<\\(object\\|initializer\\|and\\|c\\(onstraint\\|lass\\)\\|m\\(atch\\|odule\\|ethod\\|utable\\)\\|s\\(ig\\|truct\\)\\|begin\\|e\\(lse\\|x\\(ception\\|ternal\\)\\)\\|t\\(o\\|hen\\|ry\\|ype\\)\\|v\\(irtual\\|al\\)\\|w\\(h\\(ile\\|en\\)\\|ith\\)\\|i\\(f\\|n\\(herit\\)?\\)\\|f\\(or\\|un\\(ct\\(or\\|ion\\)\\)?\\)\\|let\\|do\\(wnto\\)?\\|parser?\\|rule\\|of\\)\\>\\|->\\|[;,|]"
  "Regexp for all recognized keywords.")

(defconst tuareg-match-|-keyword-regexp
  "\\<\\(and\\|function\\|type\\|with\\|parser?\\)\\>\\|[|=]"
  "Regexp for keywords supporting case match.")

(defconst tuareg-operator-regexp "[---+*/=<>@^:&|]\\|\\<\\(or\\|l\\(and\\|x?or\\|s[lr]\\)\\|as[lr]\\|mod\\)\\>"
  "Regexp for all operators.")

(defconst tuareg-kwop-regexp (concat tuareg-keyword-regexp "\\|=")
  "Regexp for all keywords, and the = operator which is generally
considered as a special keyword.")

(defconst tuareg-matching-keyword-regexp
  "\\<\\(and\\|do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|with\\|\\(down\\)?to\\)\\>"
  "Regexp matching Caml keywords which act as end block delimiters.")

(defconst tuareg-leading-kwop-regexp
  (concat tuareg-matching-keyword-regexp "\\|\\<and\\>\\|[|>]?\\]\\|>?}\\|[|)#%]\\|;;")
  "Regexp matching Caml keywords which need special indentation.")

(defconst tuareg-governing-phrase-regexp
  "\\<\\(v\\(al\\|irtual\\)\\|type\\|m\\(ethod\\|odule\\)\\|c\\(onstraint\\|lass\\)\\|in\\(herit\\|itializer\\)\\|ex\\(ternal\\|ception\\)\\|open\\|let\\|object\\)\\>"
  "Regexp matching tuareg phrase delimitors.")

(defconst tuareg-keyword-alist
  '(("module" . tuareg-default-indent)
    ("class" . tuareg-class-indent)
    ("sig" . tuareg-sig-struct-indent)
    ("struct" . tuareg-sig-struct-indent)
    ("method" . tuareg-method-indent)
    ("object" . tuareg-begin-indent)
    ("begin" . tuareg-begin-indent)
    ("for" . tuareg-for-while-indent)
    ("while" . tuareg-for-while-indent)
    ("do" . tuareg-do-indent)
    ("type" . tuareg-type-indent) ; in some cases, `type' acts like a match
    ("val" . tuareg-val-indent)
    ("fun" . tuareg-fun-indent)
    ("if" . tuareg-if-then-else-indent)
    ("then" . tuareg-if-then-else-indent)
    ("else" . tuareg-if-then-else-indent)
    ("let" . tuareg-let-indent)
    ("match" . tuareg-match-indent)
    ("try" . tuareg-try-indent)
    ("rule" . tuareg-rule-indent)

    ;; case match keywords
    ("function" . tuareg-function-indent)
    ("with" . tuareg-with-indent)
    ("parse" . tuareg-parse-indent)
    ("parser" . tuareg-parser-indent)

    ;; default indentation keywords
    ("when" . tuareg-default-indent)
    ("functor" . tuareg-default-indent)
    ("exception" . tuareg-default-indent)
    ("inherit" . tuareg-default-indent)
    ("initializer" . tuareg-default-indent)
    ("constraint" . tuareg-default-indent)
    ("virtual" . tuareg-default-indent)
    ("mutable" . tuareg-default-indent)
    ("external" . tuareg-default-indent)
    ("in" . tuareg-in-indent)
    ("of" . tuareg-default-indent)
    ("to" . tuareg-default-indent)
    ("downto" . tuareg-default-indent)
    ("->" . tuareg-default-indent)
    ("|" . tuareg-default-indent))
"Association list of indentation values based on governing keywords.")

(defconst tuareg-leading-kwop-alist
  '(("|" . tuareg-find-|-match)
    ("}" . tuareg-find-match)
    (">}" . tuareg-find-match)
    (")" . tuareg-find-match)
    ("]" . tuareg-find-match)
    ("|]" . tuareg-find-match)
    (">]" . tuareg-find-match)
    ("end" . tuareg-find-match)
    ("done" . tuareg-find-done-match)
    ("in"  . tuareg-find-in-match)
    ("with" . tuareg-find-with-match)
    ("else" . tuareg-find-else-match)
    ("then" . tuareg-find-match)
    ("do" . tuareg-find-do-match)
    ("to" . tuareg-find-match)
    ("downto" . tuareg-find-match)
    ("and" . tuareg-find-and-match))
  "Association list used in Tuareg mode for skipping back over nested blocks.")

(defun tuareg-find-meaningful-word ()
  "Look back for a word, skipping comments and blanks.
Returns the actual text of the word, if found."
  (let ((found nil) (kwop nil))
    (while (and (not found)
		(re-search-backward
		 "[^ \t\n'_A-Za-z\277-\377]\\|\\<\\w+\\>\\|\\*)"
		 (point-min) t))
      (setq kwop (match-string 0))
      (if kwop
	  (if (tuareg-in-comment-p)
	      (tuareg-beginning-of-literal-or-comment-fast)
	    (setq found t))
	(setq found t)))
    (if found kwop (goto-char (point-min)) nil)))

(defconst tuareg-find-kwop-regexp
  (concat tuareg-matching-keyword-regexp "\\|\\<\\(for\\|while\\|do\\|if\\|try\\|match\\|begin\\|s\\(ig\\|truct\\)\\|class\\|object\\)\\>\\|[][(){}]\\|\\*)"))
(defun tuareg-make-find-kwop-regexp (kwop-regexp)
  (concat tuareg-find-kwop-regexp "\\|" kwop-regexp))

(defun tuareg-find-kwop (kwop-regexp &optional do-not-skip-regexp)
  "Look back for a Caml keyword or operator matching KWOP-REGEXP.
Skips blocks etc...

Ignore occurences inside literals and comments.
If found, return the actual text of the keyword or operator."
  (let ((found nil) (kwop nil))
    (while (and (not found)
		(re-search-backward kwop-regexp (point-min) t)
		(setq kwop (match-string 0)))
      (cond
       ((tuareg-in-literal-or-comment-p)
	(tuareg-beginning-of-literal-or-comment-fast))
       ((looking-at "[]})]")
	(tuareg-backward-up-list))
       ((tuareg-at-phrase-break-p)
	(setq found t))
       ((and do-not-skip-regexp (looking-at do-not-skip-regexp))
	(if (and (string= kwop "|") (char-equal ?| (preceding-char)))
	    (backward-char)
	  (setq found t)))
       ((looking-at tuareg-matching-keyword-regexp)
	(funcall (cdr (assoc (match-string 0)
			     tuareg-leading-kwop-alist))))
       (t (setq found t))))
    (if found kwop (goto-char (point-min)) nil)))

(defun tuareg-find-match ()
  (tuareg-find-kwop tuareg-find-kwop-regexp))

(defconst tuareg-find-with-match-regexp
  (tuareg-make-find-kwop-regexp "\\<\\(begin\\|module\\)\\>\\|[({[]"))
(defun tuareg-find-with-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-with-match-regexp)))
    (cond
     ((looking-at "([ \t]*\\((\\*\\|$\\)") "!with")
     ((looking-at "[[{(]") "!with")
     ((looking-at "\\<begin\\>") "!with")
     ((looking-at "\\<module\\>") "!with")
     (t kwop))))

(defconst tuareg-find-in-match-regexp
  (tuareg-make-find-kwop-regexp "\\<let\\>"))
(defun tuareg-find-in-match ()
  (tuareg-find-kwop tuareg-find-in-match-regexp "\\<and\\>"))

(defconst tuareg-find-else-match-regexp
  (tuareg-make-find-kwop-regexp ";"))
(defun tuareg-find-else-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-else-match-regexp
				     "\\<then\\>")))
    (cond ((string= kwop "then")
	   (tuareg-find-match) kwop)
	  ((string= kwop ";")
	   (tuareg-find-semi-colon-match)
	   (tuareg-find-else-match) kwop))))

(defun tuareg-find-do-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-kwop-regexp
				   "\\<\\(down\\)?to\\>")))
    (if (or (string= kwop "to") (string= kwop "downto"))
	(tuareg-find-match) kwop)))

(defun tuareg-find-done-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-kwop-regexp "\\<do\\>")))
    (if (string= kwop "do")
	(tuareg-find-do-match) kwop)))

(defconst tuareg-find-and-match-regexp
  "\\<\\(do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|with\\|\\(down\\)?to\\)\\>\\|\\<\\(for\\|while\\|do\\|if\\|try\\|match\\|begin\\|s\\(ig\\|truct\\)\\|class\\)\\>\\|[][(){}]\\|\\*)\\|\\<\\(rule\\|exception\\|let\\|in\\|type\\|val\\|module\\)\\>")
(defconst tuareg-find-and-match-regexp-dnr
  (concat tuareg-find-and-match-regexp "\\|\\<and\\>"))
(defun tuareg-find-and-match (&optional do-not-recurse)
  (let* ((kwop (tuareg-find-kwop (if do-not-recurse
					  tuareg-find-and-match-regexp-dnr
					  tuareg-find-and-match-regexp)
				      "\\<and\\>"))
	 (old-point (point)))
    (cond ((or (string= kwop "type") (string= kwop "module"))
	   (let ((kwop2 (tuareg-find-meaningful-word)))
	     (cond ((string= kwop2 "with") kwop2)
		   ((string= kwop2 "and") (tuareg-find-and-match))
		   ((string= kwop "module") (string= kwop2 "let") kwop2)
		   (t (goto-char old-point) kwop))))
	  (t kwop))))

(defconst tuareg-find-=-match-regexp
  (tuareg-make-find-kwop-regexp "\\<\\(let\\|m\\(ethod\\|odule\\)\\|type\\|class\\|when\\|i[fn]\\)\\>"))
(defun tuareg-find-=-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-=-match-regexp
				     "\\<and\\|in\\>")))
    (if (string= kwop "and") (tuareg-find-and-match) kwop)))

(defconst tuareg-find-|-match-regexp
  (tuareg-make-find-kwop-regexp
   "\\<\\(function\\|type\\|parser?\\)\\>\\|[=|]"))
(defun tuareg-find-|-match ()
  (let* ((kwop (tuareg-find-kwop tuareg-find-|-match-regexp
				    "\\<\\(and\\|with\\)\\>\\||"))
	 (old-point (point)))
    (cond ((string= kwop "and")
	   (setq old-point (point))
	   (setq kwop (tuareg-find-and-match))
	   (goto-char old-point)
	   kwop)
	  ((and (string= kwop "|")
		(looking-at "|[^|]")
		(tuareg-in-indentation-p)) kwop)
	  ((string= kwop "|") (tuareg-find-|-match))
	  ((and (string= kwop "=") (or (looking-at "=[ \t]*\\((\\*\\|$\\)")
				       (tuareg-false-=-p)
				       (not (string= (save-excursion
						       (tuareg-find-=-match))
						     "type"))))
	   (tuareg-find-|-match))
	  ((string= kwop "parse")
	   (if (and (string-match "\\.mll" (buffer-name))
		    (save-excursion
		      (string= (tuareg-find-meaningful-word) "=")))
	       kwop (tuareg-find-|-match)))
	  (t kwop))))

(defconst tuareg-find-->-match-regexp
  (tuareg-make-find-kwop-regexp "\\<\\(with\\|fun\\(ction\\)?\\|parser\\)\\>\\|[|;]"))
(defun tuareg-find-->-match ()
  (tuareg-find-kwop tuareg-find-->-match-regexp "\\<with\\>"))

(defconst tuareg-find-semi-colon-match-regexp
  (tuareg-make-find-kwop-regexp ";[ \t]*\\((\\*\\|$\\)\\|->\\|\\<\\(let\\|method\\)\\>"))
(defun tuareg-find-semi-colon-match ()
  (tuareg-find-kwop tuareg-find-semi-colon-match-regexp
			 "\\<\\(in\\|and\\|do\\|with\\)\\>")
  ;; we don't need to find the keyword matching `and' since we know it's `let'!
  (cond
   ((looking-at ";[ \t]*\\((\\*\\|$\\)")
    (forward-line 1)
    (while (or (tuareg-in-comment-p)
	       (looking-at "^[ \t]*\\((\\*\\|$\\)"))
      (forward-line 1))
    (back-to-indentation)
    (current-column))
   ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
    (tuareg-back-to-paren-or-indentation t)
    (+ (current-column) tuareg-default-indent))
   ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
    (tuareg-search-forward-paren)
    (current-column))
   ((looking-at "\\<method\\>[ \t]*\\((\\*\\|$\\)")
    (tuareg-back-to-paren-or-indentation)
    (+ (current-column) tuareg-method-indent))
   ((looking-at "\\<begin\\>[ \t]*\\((\\*\\|$\\)")
    (tuareg-back-to-paren-or-indentation t)
    (+ (current-column) tuareg-begin-indent))
   ((looking-at "->")
    (if (save-excursion
	  (tuareg-find-->-match)
	  (looking-at "\\<\\(with\\|fun\\(ction\\)?\\|parser\\)\\>\\||"))
	(prog2
	    (tuareg-back-to-paren-or-indentation)
	    (+ (current-column) tuareg-default-indent))
      (tuareg-find-semi-colon-match)))
   ((looking-at "\\<in\\>")
    (tuareg-find-in-match)
    (tuareg-back-to-paren-or-indentation)
    (+ (current-column) tuareg-in-indent))
   (t (tuareg-back-to-paren-or-indentation t)
      (+ (current-column) tuareg-default-indent))))

(defconst tuareg-find-phrase-indentation-regexp
  (tuareg-make-find-kwop-regexp (concat tuareg-governing-phrase-regexp
					"\\|\\<and\\>")))
(defconst tuareg-find-phrase-indentation-regexp-pb
  (concat tuareg-find-phrase-indentation-regexp "\\|;;"))
(defconst tuareg-find-phrase-indentation-class-regexp
  (concat tuareg-matching-keyword-regexp "\\|\\<class\\>"))
(defun tuareg-find-phrase-indentation (&optional phrase-break)
  (if (and (looking-at "\\<\\(type\\|module\\)\\>") (> (point) (point-min))
	   (save-excursion
	     (tuareg-find-meaningful-word)
	     (looking-at "module\\|with\\|and\\|let")))
      (prog2
	  (tuareg-find-meaningful-word)
	  (+ (current-column) tuareg-default-indent))
    (let ((looking-at-and (looking-at "\\<and\\>"))
	  (kwop (tuareg-find-kwop
		 (if phrase-break
		     tuareg-find-phrase-indentation-regexp-pb
		   tuareg-find-phrase-indentation-regexp)
		 "\\<\\(end\\|and\\|with\\|in\\)\\>"))
	  (tmpkwop nil) (curr nil)
	  (old-point (save-excursion (beginning-of-line) (point))))
      (if (and kwop (string= kwop "and"))
	  (setq kwop (tuareg-find-and-match)))
      (if (not kwop) (current-column)
	(cond
	 ((string= kwop "end")
	  (if (not (save-excursion
		     (setq tmpkwop (tuareg-find-match))
		     (setq curr (point))
		     (string= tmpkwop "object")))
	      (prog2
		  (tuareg-find-match)
		  (tuareg-find-phrase-indentation phrase-break))
	    (tuareg-find-kwop tuareg-find-phrase-indentation-class-regexp)
	    (current-column)))
	 ((and (string= kwop "with")
	       (not (save-excursion
		      (setq tmpkwop (tuareg-find-with-match))
		      (setq curr (point))
		      (and (string= tmpkwop "!with")
			   (looking-at "\\<module\\>")))))
	  (goto-char curr)
	  (tuareg-find-phrase-indentation phrase-break))
	 ((and (string= kwop "in")
	       (not (save-excursion
		      (setq tmpkwop (tuareg-find-in-match))
		      (if (string= tmpkwop "and")
			  (setq tmpkwop (tuareg-find-and-match)))
		      (setq curr (point))
		      (and (string= tmpkwop "let")
			   (not (tuareg-looking-at-expression-let))))))
	  (goto-char curr)
	  (tuareg-find-phrase-indentation phrase-break))
	 ((tuareg-at-phrase-break-p)
	  (end-of-line)
	  (tuareg-skip-blank-and-comments)
	  (current-column))
	 ((string= kwop "let")
	  (if (tuareg-looking-at-expression-let)
	      (tuareg-find-phrase-indentation phrase-break)
	    (current-column)))
	 ((string= kwop "with")
	  (current-column))
	 ((string= kwop "end")
	  (current-column))
	 ((string= kwop "in")
	  (tuareg-find-in-match)
	  (current-column))
	 ((looking-at "\\<\\(class\\|object\\|s\\(ig\\|truct\\)\\)\\>")
	  (tuareg-back-to-paren-or-indentation)
	  (+ (tuareg-assoc-indent kwop) (current-column)))
	 ((string= kwop "virtual")
	  (if (save-excursion
		(setq kwop (tuareg-find-meaningful-word))
		(setq curr (current-column))
		(setq old-point (point))
		(string= kwop "class"))
	      (prog2 (goto-char old-point) (+ curr tuareg-class-indent))
	    (current-column)))
	 ((or (string= kwop "type") (string= kwop "module"))
	  (if (or (tuareg-looking-at-false-type)
		  (tuareg-looking-at-false-module))
	      (if looking-at-and (current-column)
		(tuareg-find-meaningful-word)
		(if (looking-at "\\<and\\>")
		    (prog2
			(tuareg-find-and-match)
			(tuareg-find-phrase-indentation phrase-break))
		  (current-column)))
	    (current-column)))
	 ((looking-at
	   "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
	  (tuareg-back-to-paren-or-indentation)
	  (+ (current-column) tuareg-default-indent))
	 ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
	  (tuareg-search-forward-paren)
	  (current-column))
	 ((string= kwop "open") ; compatibility with camllight `#open'
	  (tuareg-back-to-paren-or-indentation) (current-column))
	 (t (current-column)))))))

(defconst tuareg-back-to-paren-or-indentation-regexp
  "[][(){}]\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")
(defconst tuareg-back-to-paren-or-indentation-in-regexp
  (concat "\\<in\\>\\|" tuareg-back-to-paren-or-indentation-regexp))
(defconst tuareg-back-to-paren-or-indentation-lazy-regexp
  "[])}]\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")
(defconst tuareg-back-to-paren-or-indentation-lazy-in-regexp
  (concat "\\<in\\>\\|" tuareg-back-to-paren-or-indentation-regexp))
(defun tuareg-back-to-paren-or-indentation (&optional forward-in)
  "Searches backwards for the first open paren in line, or skip to indentation.
Returns t iff skipped to indentation."
  (if (or (bolp) (tuareg-in-indentation-p)) (prog2 (back-to-indentation) t)
    (let ((kwop (tuareg-find-kwop
		 (if tuareg-lazy-paren
		     (if forward-in
			 tuareg-back-to-paren-or-indentation-lazy-in-regexp
		       tuareg-back-to-paren-or-indentation-lazy-regexp)
		   (if forward-in
		       tuareg-back-to-paren-or-indentation-in-regexp
		     tuareg-back-to-paren-or-indentation-regexp))
		 "\\<and\\|with\\|in\\>"))
	  (retval))
      (if (string= kwop "with")
	  (let ((with-point (point)))
	    (setq kwop (tuareg-find-with-match))
	    (if (or (string= kwop "match") (string= kwop "try"))
		(tuareg-find-kwop
		 tuareg-back-to-paren-or-indentation-regexp
		 "\\<and\\>")
	      (setq kwop "with") (goto-char with-point))))
      (setq retval
	    (cond
	     ((string= kwop "with") nil)
	     ((string= kwop "in") (tuareg-in-indentation-p))
	     ((looking-at "[[{(]")
	      (prog2 (tuareg-search-forward-paren) nil))
	     (t (back-to-indentation) t)))
      (cond
       ((looking-at "|[^|]")
	(prog2 (re-search-forward "|[^|][ \t]*") nil))
       ((and forward-in (string= kwop "in"))
	(tuareg-find-in-match)
	(tuareg-back-to-paren-or-indentation forward-in)
	(if (looking-at "\\<\\(let\\|and\\)\\>")
	    (forward-char tuareg-let-indent)) nil)
       (t retval)))))

(defun tuareg-search-forward-paren ()
  (if tuareg-lazy-paren (tuareg-back-to-paren-or-indentation)
    (re-search-forward "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*")))

(defun tuareg-add-default-indent (leading-operator)
  (if leading-operator 0 tuareg-default-indent))

(defconst tuareg-compute-argument-indent-regexp
  (tuareg-make-find-kwop-regexp tuareg-kwop-regexp))
(defun tuareg-compute-argument-indent (leading-operator)
  (let ((old-point (save-excursion (beginning-of-line) (point)))
	(match-end-point) (kwop))
    (setq kwop (tuareg-find-kwop tuareg-compute-argument-indent-regexp
				 tuareg-keyword-regexp))
    (setq match-end-point (+ (point) (length kwop))) ; match-end is invalid !
    (if (and (if tuareg-lazy-= (looking-at "[[{(]") (looking-at "[[{(=]"))
	     (not (looking-at "=[ \t]*\\((\\*.*\\)?$")))
	(prog2
	    (if (and (not tuareg-lazy-=)
		     (looking-at "=")) (re-search-forward "=[ \t]*")
	      (tuareg-search-forward-paren))
	    (+ (tuareg-add-default-indent leading-operator)
	       (current-column)))
      (if (<= old-point (point))
	  (+ (tuareg-add-default-indent leading-operator) (current-column))
	(forward-line 1)
	(beginning-of-line)
	(while (or (tuareg-in-comment-p)
		   (looking-at "^[ \t]*\\((\\*.*\\)?$"))
	  (forward-line 1))
	(tuareg-back-to-paren-or-indentation)
	(if (save-excursion (goto-char match-end-point)
			    (looking-at "[ \t]*\\((\\*.*\\)?$"))
	    (+ (tuareg-add-default-indent leading-operator)
	       (current-column))
	  (current-column))))))

(defconst tuareg-compute-indent-regexp
  (concat tuareg-compute-argument-indent-regexp "\\|^[ \t]*[^ \t\n]"))
(defun tuareg-compute-indent ()
  (let ((leading-operator (looking-at tuareg-operator-regexp)))
    (beginning-of-line)
    (save-excursion
      (tuareg-find-meaningful-word)
      (if (looking-at tuareg-operator-regexp) (setq leading-operator t)))
    (save-excursion
      (let ((kwop (tuareg-find-kwop (if leading-operator
					tuareg-compute-argument-indent-regexp
				      tuareg-compute-indent-regexp)
				    tuareg-keyword-regexp))
	    (leading-special 0))
	(if (string= kwop "and") (setq kwop (tuareg-find-and-match)))
	(while (and (string= kwop "=") (tuareg-false-=-p))
	  (setq kwop (tuareg-find-kwop tuareg-compute-indent-regexp
				       tuareg-keyword-regexp))
	  (if (string= kwop "and") (setq kwop (tuareg-find-and-match))))
	(if (not kwop) (current-column)
	  (cond
	   ((tuareg-at-phrase-break-p)
	    (tuareg-find-phrase-indentation t))
	   ((and (string= kwop "|") (not  (char-equal ?\[ (preceding-char))))
	    (tuareg-backward-char)
	    (tuareg-back-to-paren-or-indentation)
	    (+ (current-column) tuareg-default-indent
	       (tuareg-add-default-indent leading-operator)))
	   ((or (looking-at "[[{(]")
		(and (looking-at "[<|]")
		     (char-equal ?\[ (preceding-char))
		     (prog2 (tuareg-backward-char) t))
		(and (looking-at "<")
		     (char-equal ?\{ (preceding-char))
		     (prog2 (tuareg-backward-char) t)))
	    (if (looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
		(prog2
		    (tuareg-back-to-paren-or-indentation t)
		    (+ tuareg-default-indent
		       (current-column))) ; parens do not operate
	      (tuareg-search-forward-paren)
	      (+ (tuareg-add-default-indent leading-operator)
		 (current-column))))
	   ((looking-at tuareg-keyword-regexp)
	    (cond ((or (string= kwop ";") (string= kwop ","))
		   (if (looking-at ";[ \t]*\\((\\*\\|$\\)")
		       (tuareg-find-semi-colon-match)
		     (if (looking-at ",[ \t]*\\((\\*\\|$\\)")
			 (progn
			   (tuareg-back-to-paren-or-indentation t)
			   (current-column))
		       (tuareg-back-to-paren-or-indentation t)
		       (+ (current-column) tuareg-default-indent))))
		  ((and (looking-at "\\<\\(in\\|begin\\|do\\)\\>\\|->")
			(not (looking-at
			      "\\([a-z]+\\|->\\)[ \t]*\\((\\*\\|$\\)")))
		   (if (string= kwop "in")
		       (re-search-forward "\\<in\\>[ \t]*")
		     (tuareg-back-to-paren-or-indentation t))
		   (+ (current-column)
		      (tuareg-add-default-indent leading-operator)
		      (if (string= kwop "in") 0 ; aligned, do not indent
			(tuareg-assoc-indent kwop))))
		  ((string= kwop "with")
		   (if (string= (save-excursion (tuareg-find-with-match))
				"!with")
		       (prog2
			   (tuareg-back-to-paren-or-indentation)
			   (+ (current-column) tuareg-default-indent))
		     (prog2
			 (tuareg-back-to-paren-or-indentation)
			 (+ (current-column)
			    (tuareg-assoc-indent kwop
						 (looking-at "\\<let\\>"))))))
		  ((string= kwop "in")
		   (tuareg-find-in-match)
		   (tuareg-back-to-paren-or-indentation)
		   (+ (current-column) tuareg-in-indent))
		  (t (tuareg-back-to-paren-or-indentation)
		     (+ (current-column)
			(tuareg-assoc-indent kwop
					     (looking-at "\\<let\\>"))))))
	   ((looking-at "=")
	    (if (or tuareg-lazy-=
		    (looking-at "=[ \t]*\\((\\*\\|$\\)")) ; not perfect...
		(let ((current-column-module-type nil))
		  (+
		   (progn
		     (tuareg-find-=-match)
		     (save-excursion
		       (if (looking-at "\\<and\\>") (tuareg-find-and-match))
		       (cond
			((looking-at "\\<type\\>")
			 (tuareg-find-meaningful-word)
			 (if (looking-at "\\<module\\>")
			     (progn
			       (setq current-column-module-type
				     (current-column))
			       tuareg-default-indent)
			   (if (looking-at "\\<\\(with\\|and\\)\\>")
			       (progn
				 (setq current-column-module-type
				       (current-column))
				 tuareg-default-indent)
			     (re-search-forward "\\<type\\>")
			     (beginning-of-line)
			     (+ tuareg-type-indent
				tuareg-|-extra-unindent))))
			((looking-at
			  "\\<\\(let\\|m\\(ethod\\|odule\\)\\|class\\|when\\|\\|for\\|if\\)\\>")
			 (let ((matched-string (match-string 0)))
			   (tuareg-back-to-paren-or-indentation t)
			   (setq current-column-module-type (current-column))
			   (tuareg-assoc-indent matched-string)))
			(t (tuareg-search-forward-paren)
			   (setq current-column-module-type (current-column))
			   tuareg-default-indent))))
		   (if current-column-module-type
		       current-column-module-type
		     (current-column))))
	      (+ (tuareg-add-default-indent leading-operator)
		 (prog2
		     (re-search-forward "=[ \t]*")
		     (current-column)))))
	   (nil 0)
	   (t (tuareg-compute-argument-indent leading-operator))))))))

(defun tuareg-looking-at-expression-let ()
  (save-excursion
    (and (tuareg-find-meaningful-word)
	 (not (tuareg-at-phrase-break-p))
	 (or (looking-at "[[({;=]\\|\\<\\(begin\\|i[fn]\\|do\\|t\\(ry\\|hen\\)\\|else\\|match\\|wh\\(ile\\|en\\)\\)\\>")
	     (looking-at tuareg-operator-regexp)))))

(defun tuareg-looking-at-false-module ()
  (save-excursion (tuareg-find-meaningful-word)
		  (looking-at "\\<\\(let\\|with\\|and\\)\\>")))

(defun tuareg-looking-at-false-sig-struct ()
  (save-excursion (tuareg-find-module)
		  (looking-at "\\<module\\>")))

(defun tuareg-looking-at-false-type ()
  (save-excursion (tuareg-find-meaningful-word)
		  (looking-at "\\<\\(with\\|module\\|and\\)\\>")))

(defun tuareg-looking-at-in-let ()
  (save-excursion (string= (tuareg-find-meaningful-word) "in")))

(defconst tuareg-find-module-regexp
  (tuareg-make-find-kwop-regexp "\\<module\\>"))
(defun tuareg-find-module ()
  (tuareg-find-kwop tuareg-find-module-regexp))

(defun tuareg-indent-command ()
  "Indent the current line in Tuareg mode.

Compute new indentation based on Caml syntax."
  (interactive "*")
  (save-excursion
    (setq case-fold-search nil)
    (back-to-indentation)
    (indent-line-to
     (save-excursion
       (cond
	((and (tuareg-in-comment-p) (not (and (looking-at "(\\*")
					      tuareg-indent-leading-comments)))
	 (cond
	  ((looking-at "(\\*") (current-column))
	  ((looking-at "\\*\\**)")
	   (tuareg-beginning-of-literal-or-comment-fast)
	   (if tuareg-support-leading-star-comments
	       (+ (current-column)
		  (if (save-excursion
			(forward-line 1)
			(back-to-indentation)
			(looking-at "*")) 1
		    tuareg-comment-end-extra-indent))
	     (+ (current-column) tuareg-comment-end-extra-indent)))
	  (tuareg-indent-comments
	   (let ((star (and tuareg-support-leading-star-comments
			    (looking-at "\\*"))))
	     (tuareg-beginning-of-literal-or-comment-fast)
	     (if star (re-search-forward "(") (re-search-forward "(\\*[ \t]*"))
	     (current-column)))))
	((tuareg-in-literal-p)
	 (current-column))
	((looking-at "\\<let\\>")
	 (if (tuareg-looking-at-expression-let)
	     (if (tuareg-looking-at-in-let)
		 (progn
		   (tuareg-find-meaningful-word)
		   (tuareg-find-in-match)
		   (if (tuareg-looking-at-in-let)
		       (prog2 (tuareg-back-to-paren-or-indentation)
			   (current-column))
		     (current-column)))
	       (tuareg-compute-indent))
	   (tuareg-find-phrase-indentation)))
	((looking-at tuareg-governing-phrase-regexp)
	 (tuareg-find-phrase-indentation))
	((and tuareg-sig-struct-align (looking-at "\\<\\(sig\\|struct\\)\\>"))
	 (if (string= (tuareg-find-module) "module") (current-column)
	   (tuareg-back-to-paren-or-indentation)
	   (+ tuareg-default-indent (current-column))))
	((looking-at tuareg-leading-kwop-regexp)
	 (let ((kwop (match-string 0)))
	   (if (looking-at "[#%]\\|;;") 0
	     (let* ((old-point (point))
		    (need-not-back-kwop (string= kwop "and"))
		    (real-| (looking-at "|\\([^|]\\|$\\)"))
		    (matching-kwop
		     (if (string= kwop "and") (tuareg-find-and-match t)
		       (funcall (cdr (assoc kwop tuareg-leading-kwop-alist)))))
		    (match-|-keyword-p
		     (looking-at tuareg-match-|-keyword-regexp)))
	       (if (string= matching-kwop "!with")
		   (prog2
		       (cond
			((looking-at "[[{(]")
			 (tuareg-search-forward-paren))
			((looking-at "\\<begin\\>")
			 (forward-char tuareg-begin-indent))
			((looking-at "\\<module\\>")
			 (forward-char tuareg-default-indent)))
		       (current-column))
		 (if (and (looking-at "\\(\\[|?\\|{<?\\|(\\)[ \t]*[^ \t\n]")
			  (not (looking-at
				"[[{(][|<]?[ \t]*\\((\\*\\|$\\)")))
		     (prog2
			 (if tuareg-lazy-paren
			     (tuareg-back-to-paren-or-indentation))
			 (current-column))
		   (if (string= kwop "|")
		       (if real-|
			   (cond
			    ((string= matching-kwop "|")
			     (if (not need-not-back-kwop)
				 (tuareg-back-to-paren-or-indentation))
			     (current-column))
			    ((and (string= matching-kwop "=")
				  (not (tuareg-false-=-p)))
			     (re-search-forward "=[ \t]*")
			     (current-column))
			    (match-|-keyword-p
			     (if (not need-not-back-kwop)
				 (tuareg-back-to-paren-or-indentation))
			     (- (+ (tuareg-assoc-indent
				    matching-kwop (looking-at "\\<let\\>"))
				   (current-column))
				(if (string= matching-kwop "type") 0
				  tuareg-|-extra-unindent)))
			    (t (goto-char old-point)
			       (tuareg-compute-indent)))
			 (goto-char old-point)
			 (tuareg-compute-indent))
		     (if (not need-not-back-kwop)
			 (tuareg-back-to-paren-or-indentation
			  (not (string= kwop "in"))))
		     (current-column))))))))
	(t (tuareg-compute-indent))))))
  (if (tuareg-in-indentation-p) (back-to-indentation)))

(defun tuareg-split-string ()
  "Called whenever a line is broken inside a Caml string literal."
  (insert-before-markers "\" ^\"")
  (tuareg-backward-char))

(defadvice newline-and-indent (around
			       tuareg-newline-and-indent
			       activate)
  "Handle multi-line strings in Tuareg mode."
    (let ((hooked (and (eq major-mode 'tuareg-mode) (tuareg-in-literal-p)))
	  (split-mark))
      (if (not hooked) nil
	(setq split-mark (set-marker (make-marker) (point)))
	(tuareg-split-string))
      ad-do-it
      (if (not hooked) nil
	(goto-char split-mark)
	(set-marker split-mark nil))))

(defun tuareg-electric ()
  "If inserting a |, ) operator at beginning of line, reindent the line."
  (interactive "*")
  (let ((electric (and tuareg-electric-indent
		       (tuareg-in-indentation-p)
		       (not (tuareg-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if (and electric
	     (not (and (char-equal ?| (preceding-char))
		       (save-excursion
			 (tuareg-backward-char)
			 (tuareg-find-|-match)
			 (not (looking-at tuareg-match-|-keyword-regexp))))))
	(tuareg-indent-command))))

(defun tuareg-electric-rc ()
  "If inserting a } operator at beginning of line, reindent the line.

Reindent also if } is inserted after a > operator at beginning of line.
Also, if the matching { is followed by a < and this } is not preceded
by >, insert one >."
  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-bra (and tuareg-electric-close-vector
			(not (tuareg-in-literal-or-comment-p))
			(not (char-equal ?> prec))))
	 (electric (and tuareg-electric-indent
			(or (tuareg-in-indentation-p)
			    (and (char-equal ?> prec)
				 (save-excursion (tuareg-backward-char)
						 (tuareg-in-indentation-p))))
			(not (tuareg-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if look-bra
	(save-excursion
	  (let ((inserted-char
		 (save-excursion
		   (tuareg-backward-char)
		   (tuareg-backward-up-list)
		   (cond ((looking-at "{<") ">")
			 (t "")))))
	    (tuareg-backward-char)
	    (insert inserted-char))))
    (if electric (tuareg-indent-command))))

(defun tuareg-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Reindent also if ] is inserted after a | or > operator at beginning of line.
Also, if the matching [ is followed by a | (resp. <) and this ] is not
preceded by | (resp. >), insert one | (resp. >)."
  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-|-or-bra (and tuareg-electric-close-vector
			     (not (tuareg-in-literal-or-comment-p))
			     (not (or (and (char-equal ?| prec)
					   (not (char-equal
						 (save-excursion
						   (tuareg-backward-char)
						   (preceding-char)) ?\[)))
				      (char-equal ?> prec)))))
	 (electric (and tuareg-electric-indent
			(or (tuareg-in-indentation-p)
			    (and (or (char-equal ?| prec) (char-equal ?> prec))
				 (save-excursion (tuareg-backward-char)
						 (tuareg-in-indentation-p))))
			(not (tuareg-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if look-|-or-bra
	(save-excursion
	  (let ((inserted-char
		 (save-excursion
		   (tuareg-backward-char)
		   (tuareg-backward-up-list)
		   (cond ((looking-at "\\[|") "|")
			 ((looking-at "\\[<") ">")
			 (t "")))))
	    (tuareg-backward-char)
	    (insert inserted-char))))
    (if electric (tuareg-indent-command))))

(defun tuareg-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  (if (not (tuareg-in-literal-or-comment-p))
      (let* ((bol (save-excursion (beginning-of-line) (point)))
	     (kw (save-excursion
		   (and (re-search-backward "^[ \t]*\\(\\w+\\)\\=" bol t)
			(match-string 1)))))
	(if kw (progn
		   (insert " ")
		   (tuareg-indent-command)
		   (backward-delete-char-untabify 1))))))

(defun tuareg-skip-to-end-of-phrase ()
  (let ((old-point (point)))
    (if (and (string= (tuareg-find-meaningful-word) ";")
	     (char-equal (preceding-char) ?\;))
	(setq old-point (1- (point))))
    (goto-char old-point)
    (let ((kwop (tuareg-find-meaningful-word)))
      (goto-char (+ (point) (length kwop))))))

(defun tuareg-skip-blank-and-comments ()
  (skip-chars-forward " \t\n")
  (while (and (< (point) (point-max)) (tuareg-in-comment-p))
    (re-search-forward "\\*)") (skip-chars-forward " \t\n")))

(defun tuareg-skip-back-blank-and-comments ()
  (skip-chars-backward " \t\n")
  (while (save-excursion (tuareg-backward-char)
			 (and (> (point) (point-min)) (tuareg-in-comment-p)))
    (tuareg-backward-char)
    (tuareg-beginning-of-literal-or-comment) (skip-chars-backward " \t\n")))

(defconst tuareg-beginning-phrase-regexp
  "^#[ \t]*[a-z][_a-z]*\\>\\|\\<\\(end\\|val\\|value\\|type\\|module\\|sig\\|struct\\|class\\|exception\\|open\\|let\\)\\>\\|;;"
  "Regexp matching tuareg phrase delimitors.")
(defun tuareg-find-phrase-beginning ()
  "Find `real' phrase beginning and returns point."
  (beginning-of-line)
  (tuareg-skip-blank-and-comments)
  (end-of-line)
  (tuareg-skip-to-end-of-phrase)
  (let ((old-point (point)))
    (tuareg-find-kwop tuareg-beginning-phrase-regexp)
    (while (and (> (point) (point-min)) (< (point) old-point)
		(or (not (looking-at tuareg-beginning-phrase-regexp))
		    (and (looking-at "\\<let\\>")
			 (tuareg-looking-at-expression-let))
		    (and (looking-at "\\<module\\>")
			 (tuareg-looking-at-false-module))
		    (and (looking-at "\\<\\(sig\\|struct\\)\\>")
			 (tuareg-looking-at-false-sig-struct))
		    (and (looking-at "\\<type\\>")
			 (tuareg-looking-at-false-type))))
      (if (looking-at "\\<end\\>")
	  (tuareg-find-match)
	(if (not (bolp)) (tuareg-backward-char))
	(setq old-point (point))
	(tuareg-find-kwop tuareg-beginning-phrase-regexp)))
    (if (tuareg-at-phrase-break-p)
	(prog2 (end-of-line) (tuareg-skip-blank-and-comments)))
    (back-to-indentation)
    (point)))

(defun tuareg-search-forward-end ()
  (re-search-forward "\\<\\(end\\|begin\\)\\>" (point-max) t)
  (if (string= (match-string 0) "\\<begin\\>") (tuareg-search-forward-end)))

(defconst tuareg-inside-block-regexp
  (concat tuareg-matching-keyword-regexp
	  "\\|\\<\\(class\\|sig\\|struct\\)\\>"))
(defun tuareg-inside-block-find-kwop ()
  (let ((kwop (tuareg-find-kwop tuareg-inside-block-regexp
				"\\<\\(and\\|end\\)\\>")))
    (if (string= kwop "and") (setq kwop (tuareg-find-and-match)))
    (if (string= kwop "end")
	(progn
	  (tuareg-find-match)
	  (tuareg-find-kwop tuareg-inside-block-regexp)
	  (tuareg-inside-block-find-kwop))
      kwop)))
(defun tuareg-inside-block-p ()
  (let ((begin) (end) (and-end) (kwop t))
    (save-excursion
      (if (looking-at "\\<and\\>")
	  (tuareg-find-and-match))
      (if (not (looking-at "\\<\\(class\\|sig\\|struct\\)\\>"))
	  (while (and (setq kwop (tuareg-inside-block-find-kwop))
		      (not (looking-at "\\<\\(class\\|sig\\|struct\\)\\>")))))
      (if (not kwop) ()
	(setq begin (point))
	(while (and (tuareg-search-forward-end)
		    (save-excursion
		      (tuareg-backward-char 3)
		      (tuareg-find-match)
		      (if (looking-at "\\<object\\>")
			  (tuareg-inside-block-find-kwop))
		      (> (point) begin))))
	(tuareg-backward-char 3)
	(if (not (looking-at "\\<end\\>")) ()
	  (tuareg-forward-char 3)
	  (setq end (point))
	  (setq and-end (point))
	  (tuareg-skip-blank-and-comments)
	  (while (looking-at "\\<and\\>")
	    (setq and-end (point))
	    (while (and (tuareg-search-forward-end)
			(save-excursion
			  (tuareg-backward-char 3)
			  (tuareg-find-match)
			  (if (looking-at "\\<object\\>")
			      (tuareg-inside-block-find-kwop))
			  (> (point) and-end))))
	    (tuareg-backward-char 3)
	    (if (not (looking-at "\\<end\\>")) ()
	      (tuareg-forward-char 3)
	      (setq and-end (point))
	      (tuareg-skip-blank-and-comments)))
	  (list begin end and-end))))))
	    
(defun tuareg-discover-phrase (&optional quiet)
  (end-of-line)
  (let ((end (point)))
    (tuareg-find-phrase-beginning)
    (if (> (point) end) (setq end (point)))
    (save-excursion
      (let ((begin (point)) (cpt 0) (lines-left 0) (stop)
	    (inside-block (tuareg-inside-block-p))
	    (looking-block (looking-at "\\<class\\>")))
	(if (and looking-block inside-block)
	    (progn
	      (setq begin (nth 0 inside-block))
	      (setq end (nth 2 inside-block))
	      (goto-char end))
	  (if inside-block
	      (progn
		(setq stop (save-excursion (goto-char (nth 1 inside-block))
					   (beginning-of-line) (point)))
		(if (< stop end) (setq stop (point-max))))
	    (setq stop (point-max)))
	  (save-restriction
	    (goto-char end)
	    (while (and (= lines-left 0)
			(or (not inside-block) (< (point) stop))
			(<= (save-excursion
			      (tuareg-find-phrase-beginning)) end))
	      (if (not quiet)
		  (prog2
		      (setq cpt (1+ cpt))
		      (if (= 8 cpt)
			  (message "Looking for enclosing phrase..."))))
	      (setq end (point))
	      (tuareg-skip-to-end-of-phrase)
	      (beginning-of-line)
	      (narrow-to-region (point) (point-max))
	      (goto-char end)
	      (setq lines-left (forward-line 1)))))
	(if (>= cpt 8) (message "Looking for enclosing phrase... done."))
	(save-excursion (tuareg-skip-blank-and-comments) (setq end (point)))
	(tuareg-skip-back-blank-and-comments)
	(list begin (point) end)))))

(defun tuareg-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.
The Caml phrase is the phrase just before the point."
  (interactive)
  (let ((pair (tuareg-discover-phrase)))
    (goto-char (nth 1 pair)) (push-mark (nth 0 pair) t t)))

(defun tuareg-next-phrase (&optional quiet)
  "Skip to the beginning of the next phrase."
  (interactive "i")
  (goto-char (save-excursion (nth 2 (tuareg-discover-phrase quiet))))
  (if (looking-at "\\<end\\>") (tuareg-next-phrase quiet)))

(defun tuareg-previous-phrase ()
  "Skip to the beginning of the previous phrase."
  (interactive)
  (beginning-of-line)
  (tuareg-skip-to-end-of-phrase)
  (tuareg-discover-phrase))

(defun tuareg-indent-phrase ()
  "If inside a comment call `fill-paragraph-or-region'; Otherwise
indent all lines in the current phrase."
  (interactive)
  (save-excursion
    (if (and (tuareg-in-comment-p)
	     (save-excursion (beginning-of-line) (tuareg-in-comment-p)))
	(let ((begpoint (save-excursion
			  (tuareg-beginning-of-literal-or-comment) (point)))
	      (endpoint (save-excursion
			  (while (tuareg-in-comment-p)
			    (re-search-forward "*)")) (point))))
	  (fill-region begpoint endpoint))
      (let ((pair (tuareg-discover-phrase)))
	(indent-region (nth 0 pair) (nth 1 pair) nil)))))

;; Auxiliary function (from J. Garrigue)
(defun tuareg-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name (buffer-file-name)))
    (if (string-match "^\\(.*\\)\\.\\(ml\\|mli\\)$" name)
	(find-file (concat (match-string 1 name)
			   (if (string= "ml" (match-string 2 name))
			       ".mli" ".ml"))))))

(defun tuareg-insert-begin-form ()
  "Inserts a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "begin\n\nend\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (tuareg-indent-command)))

(defun tuareg-insert-for-form ()
  "Inserts a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "for  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (tuareg-indent-command)
    (beginning-of-line 1)
    (backward-char 4)))

(defun tuareg-insert-while-form ()
  "Inserts a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "while  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (tuareg-indent-command)
    (beginning-of-line 1)
    (backward-char 4)))

(defun tuareg-insert-if-form ()
  "Inserts a nicely formatted if-then-else form, leaving a mark after else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "if\n\nthen\n\nelse\n")
    (end-of-line)
    (indent-region old (point) nil)
    (tuareg-indent-command)
    (push-mark)
    (forward-line -2)
    (tuareg-indent-command)
    (forward-line -2)
    (tuareg-indent-command)))

(defun tuareg-insert-match-form ()
  "Inserts a nicely formatted math-with form, leaving a mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "match\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (tuareg-indent-command)
    (push-mark)
    (forward-line -2)
    (tuareg-indent-command)))

(defun tuareg-insert-let-form ()
  "Inserts a nicely formatted let-in form, leaving a mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "let  in\n")
    (end-of-line)
    (indent-region old (point) nil)
    (tuareg-indent-command)
    (push-mark)
    (beginning-of-line)
    (backward-char 4)
    (tuareg-indent-command)))

(defun tuareg-insert-try-form ()
  "Inserts a nicely formatted try-with form, leaving a mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "try\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (tuareg-indent-command)
    (push-mark)
    (forward-line -2)
    (tuareg-indent-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Tuareg interactive mode

;; Augment Tuareg mode, so you can process Caml code in the source files.

(require 'comint)

(defvar tuareg-interactive-mode-map nil)
(if tuareg-interactive-mode-map nil
  (setq tuareg-interactive-mode-map
        (copy-keymap comint-mode-map)))
(define-key tuareg-interactive-mode-map "#" 'tuareg-electric)
(define-key tuareg-interactive-mode-map "%" 'tuareg-electric)
(define-key tuareg-interactive-mode-map "|" 'tuareg-electric)
(define-key tuareg-interactive-mode-map ")" 'tuareg-electric)
(define-key tuareg-interactive-mode-map "}" 'tuareg-electric-rc)
(define-key tuareg-interactive-mode-map "]" 'tuareg-electric-rb)
(define-key tuareg-interactive-mode-map "\t" 'tuareg-indent-command)
(define-key tuareg-interactive-mode-map "\C-c\C-i" 'tuareg-interrupt-caml)
(define-key tuareg-interactive-mode-map "\C-c\C-k" 'tuareg-kill-caml)
(define-key tuareg-interactive-mode-map "\C-m"
  'tuareg-interactive-send-input)
(define-key tuareg-interactive-mode-map "\C-j"
  'tuareg-interactive-send-input-or-indent)
(define-key tuareg-interactive-mode-map "\M-\C-m"
  'comint-send-input)
(if (functionp 'read-kbd-macro)
    (define-key tuareg-interactive-mode-map (read-kbd-macro "<kp-enter>")
      'comint-send-input))

(defconst tuareg-interactive-buffer-name "*caml-toplevel*")

(defconst tuareg-interactive-toplevel-error-regexp
  "Characters \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by ocaml toplevel.")
(defvar tuareg-interactive-last-phrase-pos-in-source 0)
(defvar tuareg-interactive-last-phrase-pos-in-toplevel 0)

(defun tuareg-interactive-filter (text)
  (if (< comint-last-input-end comint-last-input-start) ()
    (if (and tuareg-with-xemacs tuareg-interactive-read-only-input)
	(add-text-properties comint-last-input-start comint-last-input-end
			     (list 'read-only t)))
    (if (and (or tuareg-window-system tuareg-with-xemacs)
	     (featurep 'font-lock)
	     tuareg-interactive-input-font-lock)
	(progn
	  (font-lock-fontify-region comint-last-input-start
				    comint-last-input-end)
	  (if (featurep 'sym-lock)
	      (sym-lock-make-symbols-atomic comint-last-input-start
					    comint-last-input-end)))))
  (if tuareg-interactive-output-font-lock
      (save-excursion
	(add-text-properties
	 comint-last-input-end (point-max)
	 '(face tuareg-font-lock-interactive-output-face))))
  (if tuareg-interactive-error-font-lock
      (save-excursion
	(goto-char comint-last-input-end)
	(while (re-search-forward tuareg-interactive-error-regexp nil t)
	  (add-text-properties
	   (match-beginning 1) (match-end 1)
	   '(face tuareg-font-lock-interactive-error-face))))))

(defun tuareg-interactive-mode ()
  "Major mode for interacting with a Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

Special keys for Tuareg interactive mode:\\{tuareg-interactive-mode-map}"
  (interactive)
  (comint-mode)
  (if (not (eq (tuareg-install-font-lock) 'font-lock))
      ()
    (add-hook 'comint-output-filter-functions 'tuareg-interactive-filter)
    (if (not (boundp 'after-change-functions))
	()
      (make-local-hook 'after-change-functions)
      (put 'after-change-functions 'permanent-local t)
      (remove-hook 'after-change-functions
		   'font-lock-after-change-function t))
    (if (not (boundp 'pre-idle-hook))
	()
      (make-local-hook 'pre-idle-hook)
      (put 'pre-idle-hook 'permanent-local t)
      (remove-hook 'pre-idle-hook
		   'font-lock-pre-idle-hook t)))
  (setq comint-prompt-regexp "^#")
  (setq major-mode 'tuareg-interactive-mode)
  (setq mode-name "Tuareg-Interactive")
  (setq comint-scroll-to-bottom-on-output t)
  (use-local-map tuareg-interactive-mode-map)
  (set-syntax-table tuareg-mode-syntax-table)
  (setq local-abbrev-table tuareg-mode-abbrev-table)

  (if tuareg-window-system (tuareg-interactive-build-menu))

  ;; hooks for tuareg-interactive-mode
  (run-hooks 'tuareg-interactive-mode-hook))

(defun tuareg-run-caml (&optional cmd)
  "Run a Caml toplevel process. I/O via buffer `*caml-toplevel*'."
  (interactive
   (list (if (not (comint-check-proc tuareg-interactive-buffer-name))
	     (read-from-minibuffer "Caml toplevel to run: "
				   tuareg-interactive-program))))
  (tuareg-run-process-if-needed cmd)
  (switch-to-buffer-other-window tuareg-interactive-buffer-name))

(defun tuareg-run-process-if-needed (&optional cmd)
  (if (not cmd)
      (if (comint-check-proc tuareg-interactive-buffer-name)
	  (setq cmd tuareg-interactive-program)
	(setq cmd (read-from-minibuffer "Caml toplevel to run: "
					tuareg-interactive-program))))
  (setq tuareg-interactive-program cmd)
  (if (not (comint-check-proc tuareg-interactive-buffer-name))
      (let ((cmdlist (tuareg-args-to-list cmd))
            (process-connection-type nil))
	(set-buffer (apply (function make-comint) "caml-toplevel"
			   (car cmdlist) nil (cdr cmdlist)))
	(tuareg-interactive-mode)
	(sleep-for 1))))

(defun tuareg-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (tuareg-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (tuareg-args-to-list (substring string pos
						 (length string)))))))))

(defun tuareg-interactive-end-of-phrase ()
  (save-excursion
    (end-of-line)
    (tuareg-find-meaningful-word)
    (tuareg-find-meaningful-word)
    (looking-at ";;")))

(defun tuareg-interactive-send-input ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline."
  (interactive)
  (if (tuareg-interactive-end-of-phrase)
      (comint-send-input)
    (insert "\n")
    (message "Phrase must end with `;;' to be processed by Caml toplevel...")))

(defun tuareg-interactive-send-input-or-indent ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline and indent."
  (interactive)
  (if (tuareg-interactive-end-of-phrase)
      (comint-send-input)
    (insert "\n")
    (tuareg-indent-command)
    (message "Phrase must end with `;;' to be processed by Caml toplevel...")))

(defun tuareg-eval-region (start end)
  "Eval the current region in the Caml toplevel."
  (interactive "r")
  (save-excursion (tuareg-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (setq tuareg-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (let ((text (buffer-substring-no-properties start end)))
      (goto-char end)
      (if (progn (tuareg-find-meaningful-word) (tuareg-find-meaningful-word)
		 (looking-at ";;")) ()
	(setq text (concat text ";;")))
      (if (string= text "")
	  (message "Cannot send empty commands to Caml toplevel!")
	(set-buffer tuareg-interactive-buffer-name)
	(goto-char (point-max))
	(setq tuareg-interactive-last-phrase-pos-in-toplevel (point))
	(if tuareg-interactive-echo-phrase (insert text)
	  (comint-send-string tuareg-interactive-buffer-name text)
	  (comint-send-string tuareg-interactive-buffer-name "\n"))
	(comint-send-input)))
    (display-buffer tuareg-interactive-buffer-name t)))

(defun tuareg-eval-phrase ()
  "Eval the surrounding Caml phrase in the Caml toplevel."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (tuareg-discover-phrase)))
	(setq end (nth 2 pair))
	(tuareg-eval-region (nth 0 pair) (nth 1 pair))))
    (if tuareg-skip-after-eval-phrase
	(goto-char end))))

(defun tuareg-eval-buffer ()
  "Send the buffer to the inferior Caml process."
  (interactive)
  (tuareg-eval-region (point-min) (point-max)))

(defun tuareg-interactive-next-error ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (set-buffer tuareg-interactive-buffer-name)
      (goto-char tuareg-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
	    (re-search-forward tuareg-interactive-toplevel-error-regexp
			       (point-max) t))
      (if error-pos
	  (setq beg (string-to-int (match-string 1))
		end (string-to-int (match-string 2)))))
    (if (not error-pos)
	(message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-source beg)
	    end (+ tuareg-interactive-last-phrase-pos-in-source end))
      (goto-char beg) (push-mark end t t))))

(defun tuareg-interrupt-caml ()
  (interactive)
  (if (comint-check-proc tuareg-interactive-buffer-name)
      (save-excursion
	(set-buffer tuareg-interactive-buffer-name)
	(comint-interrupt-subjob))))

(defun tuareg-kill-caml ()
  (interactive)
  (if (comint-check-proc tuareg-interactive-buffer-name)
      (save-excursion
	(set-buffer tuareg-interactive-buffer-name)
	(comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun tuareg-about () (interactive)
  (describe-variable 'tuareg-mode-version))
(defun tuareg-help () (interactive)
  (describe-function 'tuareg-mode))
(defun tuareg-interactive-help () (interactive)
  (describe-function 'tuareg-interactive-mode))

(defvar tuareg-definitions-menu-last-buffer nil)
(defvar tuareg-definitions-keymaps nil)

(defun tuareg-build-menu ()
  (if (condition-case nil (prog2 (require 'easymenu) nil) (error t))
      ()
    (easy-menu-define
     tuareg-mode-menu (list tuareg-mode-map)
     "Tuareg Mode Menu."
     '("Tuareg"
       ("Interactive Mode"
	["Run Caml Toplevel" tuareg-run-caml t]
	["Interrupt Caml Toplevel" tuareg-interrupt-caml
	 :active (comint-check-proc tuareg-interactive-buffer-name)]
	["Kill Caml Toplevel" tuareg-kill-caml
	 :active (comint-check-proc tuareg-interactive-buffer-name)]
	["Evaluate Region" tuareg-eval-region :active (region-active-p)]
	["Evaluate Phrase" tuareg-eval-phrase t]
	["Evaluate Buffer" tuareg-eval-buffer t])
       ("Caml Forms"
	 ["try .. with .." tuareg-insert-try-form t]
	 ["match .. with .." tuareg-insert-match-form t]
	 ["let .. in .." tuareg-insert-let-form t]
	 ["if .. then .. else .." tuareg-insert-if-form t]
	 ["while .. do .. done" tuareg-insert-while-form t]
	 ["for .. do .. done" tuareg-insert-for-form t]
	 ["begin .. end" tuareg-insert-begin-form t])
       ["Switch .ml/.mli" tuareg-find-alternate-file t]
       "---"
       ["Compile..." compile t]
       ["Reference manual..." tuareg-browse-manual t]
       ["Caml library..." tuareg-browse-library t]
       ("Definitions"
	:filter tuareg-update-definitions-menu
	["Scan..." tuareg-list-definitions t])
       "---"
       ("Tuareg Options" ["Dummy" nil t])
       ("Tuareg Interactive Options" ["Dummy" nil t])
       "---"
       ["About" tuareg-about t]
       ["Help" tuareg-help t]))
    (easy-menu-add tuareg-mode-menu)
    (tuareg-update-options-menu)
    ;; save and update definitions menu
    (make-local-variable 'tuareg-definitions-menu)
    (if (or tuareg-with-xemacs
	    (not (functionp 'easy-menu-create-keymaps))) ()
      ;; patch for Emacs 20.2
      (add-hook 'menu-bar-update-hook
		'tuareg-with-emacs-update-definitions-menu)
      (make-local-variable 'tuareg-definitions-keymaps)
      (setq tuareg-definitions-keymaps
	    (cdr (easy-menu-create-keymaps
		  "Definitions" tuareg-definitions-menu)))
      (setq tuareg-definitions-menu-last-buffer nil))))

(defun tuareg-interactive-build-menu ()
  (if (condition-case nil (prog2 (require 'easymenu) nil) (error t))
      ()
    (easy-menu-define
     tuareg-interactive-mode-menu (list tuareg-interactive-mode-map)
     "Tuareg Interactive Mode Menu."
     '("Tuareg"
       ("Interactive Mode"
	["Run Caml Toplevel" tuareg-run-caml t]
	["Interrupt Caml Toplevel" tuareg-interrupt-caml
	 :active (comint-check-proc tuareg-interactive-buffer-name)]
	["Kill Caml Toplevel" tuareg-kill-caml
	 :active (comint-check-proc tuareg-interactive-buffer-name)]
	["Evaluate Region" tuareg-eval-region :active (region-active-p)]
	["Evaluate Phrase" tuareg-eval-phrase t]
	["Evaluate Buffer" tuareg-eval-buffer t])
       "---"
       ("Tuareg Options" ["Dummy" nil t])
       ("Tuareg Interactive Options" ["Dummy" nil t])
       "---"
       ["Help" tuareg-interactive-help t]))
    (easy-menu-add tuareg-interactive-mode-menu)
    (tuareg-update-options-menu)))

(defun tuareg-update-definitions-menu (list)
   tuareg-definitions-menu)

(defun tuareg-with-emacs-update-definitions-menu ()
  (if (current-local-map)
      (let ((keymap
	     (lookup-key (current-local-map) [menu-bar Tuareg Definitions])))
	(if (and
	     (keymapp keymap)
	     (not (eq tuareg-definitions-menu-last-buffer (current-buffer))))
	    (setcdr keymap tuareg-definitions-keymaps)
	  (setq tuareg-definitions-menu-last-buffer (current-buffer))))))

(defun tuareg-toggle-option (symbol)
  (interactive)
  (cond
   ((eq symbol 'tuareg-labl-support) (tuareg-do-support-labl))
   (t (set symbol (not (symbol-value symbol)))))
  (if tuareg-with-xemacs nil (tuareg-update-options-menu)))

(defun tuareg-update-options-menu ()
  (easy-menu-change
   '("Tuareg") "Tuareg Options"
   (mapcar (lambda (pair)
	     (if (consp pair)
		 (vector (car pair)
			 (list 'tuareg-toggle-option (cdr pair))
			 :style 'toggle
			 :selected (nth 1 (cdr pair))
			 :active t)
	       pair)) tuareg-options-list))
  (easy-menu-change
   '("Tuareg") "Tuareg Interactive Options"
   (mapcar (lambda (pair)
	     (if (consp pair)
		 (vector (car pair)
			 (list 'tuareg-toggle-option (cdr pair))
			 :style 'toggle
			 :selected (nth 1 (cdr pair))
			 :active t)
	       pair)) tuareg-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun tuareg-browse-manual ()
  "*Browse Caml reference manual."
  (interactive)
  (setq tuareg-manual-url (read-from-minibuffer "URL: " tuareg-manual-url))
  (funcall tuareg-browser tuareg-manual-url))

(defun tuareg-xemacs-w3-manual (url)
  "*Browse Caml reference manual."
  (w3-fetch-other-frame url))

(defun tuareg-netscape-manual (url)
  "*Browse Caml reference manual."
  (start-process-shell-command
   "netscape" nil
   (concat "netscape -remote 'openURL ("
	   url ", newwindow)' || netscape " url)))

(defun tuareg-mmm-manual (url)
  "*Browse Caml reference manual."
  (start-process-shell-command
   "mmm" nil
   (concat "mmm_remote " url " || mmm -external " url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defun tuareg-browse-library()
  "Browse the Caml library."
  (interactive)
  (let ((buf-name "*caml-library*") (opoint)
	(dir (read-from-minibuffer "Library path: " tuareg-library-path)))
    (if (and (file-directory-p dir) (file-readable-p dir))
	(progn
	  (setq tuareg-library-path dir)
	  ;; list *.ml and *.mli files
	  (with-output-to-temp-buffer buf-name
	    (buffer-disable-undo standard-output)
	    (save-excursion
	      (set-buffer buf-name)
	      (kill-all-local-variables)
	      (make-local-variable 'tuareg-library-path)
	      (setq tuareg-library-path dir)
	      ;; help
	      (insert "Directory \"" dir "\".\n") 
	      (insert "Select a file with middle mouse button or RETURN.\n\n")
	      (insert "Interface files (.mli):\n\n")
	      (insert-directory (concat dir "/*.mli") "-C" t nil)
	      (insert "\n\nImplementation files (.ml):\n\n")
	      (insert-directory (concat dir "/*.ml") "-C" t nil)
	      ;; '.', '-' and '_' are now letters
	      (modify-syntax-entry ?. "w")
	      (modify-syntax-entry ?- "w")
	      (modify-syntax-entry ?_ "w")
	      ;; every file name is now mouse-sensitive
	      (goto-char (point-min))
	      (while (< (point) (point-max))
		(re-search-forward "\\.ml.?\\>")
		(setq opoint (point))
		(re-search-backward "\\<" (point-min) 1)
		(put-text-property (point) opoint 'mouse-face 'highlight)
		(goto-char (+ 1 opoint)))
	      ;; activate tuareg-library mode
	      (setq major-mode 'tuareg-library-mode)
	      (setq mode-name "tuareg-library")
	      (use-local-map tuareg-library-mode-map)
	      (setq buffer-read-only t)))))))
  
(setq tuareg-library-mode-map (make-keymap))
(suppress-keymap tuareg-library-mode-map)
(define-key tuareg-library-mode-map [return] 'tuareg-library-find-file)
(define-key tuareg-library-mode-map [mouse-2] 'tuareg-library-mouse-find-file)
  
(defun tuareg-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (save-excursion
    (if (text-properties-at (point))
	(progn
	  (re-search-backward "\\<") (setq beg (point))
	  (re-search-forward "\\>")
	  (find-file-read-only (concat tuareg-library-path "/"
				       (buffer-substring-no-properties
					beg (point))))))))

(defun tuareg-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (tuareg-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Definitions List

;; Designed from original code by M. Quercia

(defconst tuareg-definitions-regexp
  "\\<\\(and\\|val\\|value\\|type\\|module\\|class\\|exception\\|let\\)\\>"
  "Regexp matching definition phrases.")

(defconst tuareg-definitions-bind-skip-regexp
  "\\<\\(rec\\|type\\|virtual\\)\\>\\|'[A-Za-z\300-\377][0-9_'A-Za-z\300-\377]*\\|('.*)"
  "Regexp matching stuff to ignore after a binding keyword.")

(defvar tuareg-definitions-menu (list ["Scan..." tuareg-list-definitions t])
  "Initial content of the definitions menu.")

(defun tuareg-list-definitions()
  "Parses the buffer and gathers toplevel definitions for quick
jump via the definitions menu'."
  (interactive)
  (message "Searching definitions...")
  (save-excursion
    (let ((cpt 0) (kw) (menu)
	  (value-list) (type-list) (module-list) (class-list) (misc-list))
      (goto-char (point-min))
      (tuareg-skip-blank-and-comments)
      (while (< (point) (point-max))
	(if (looking-at tuareg-definitions-regexp)
	    (progn
	      (setq kw (match-string 0))
	      (if (string= kw "and")
		  (setq kw (save-match-data
			     (save-excursion (tuareg-find-and-match)))))
	      (if (or (string= kw "exception")
		      (string= kw "val")
		      (string= kw "value")) (setq kw "let"))
	      ;; skip optional elements
	      (goto-char (match-end 0))
	      (tuareg-skip-blank-and-comments)
	      (if (looking-at tuareg-definitions-bind-skip-regexp)
		  (goto-char (match-end 0)))
	      (tuareg-skip-blank-and-comments)
	      (if (looking-at "\\<[A-Za-z\300-\377][0-9_'A-Za-z\300-\377]*\\>")
		  ;; menu item : [name (goto-char ...) t]
		  (let* ((p (make-marker))
			 (ref (vector (match-string 0)
				      (list 'tuareg-goto p) t)))
		    (setq cpt (1+ cpt))
		    (message (concat "Searching definitions... ("
				     (number-to-string cpt) ")"))
		    (set-marker p (point))
		    (cond
		     ((string= kw "let")
		      (setq value-list (cons ref value-list)))
		     ((string= kw "type")
		      (setq type-list (cons ref type-list)))
		     ((string= kw "module")
		      (setq module-list (cons ref module-list)))
		     ((string= kw "class")
		      (setq class-list (cons ref class-list)))
		     (t (setq misc-list (cons ref misc-list))))))))
	;; skip to next phrase or next top-level `and'
	(tuareg-forward-char)
	(let ((old-point (point)) (last-and))
	  (tuareg-next-phrase t)
	  (setq last-and (point))
	  (save-excursion
	    (while (and (re-search-backward "\\<and\\>" old-point t)
			(not (tuareg-in-literal-or-comment-p))
			(save-excursion (tuareg-find-and-match)
					(>= old-point (point))))
	      (setq last-and (point))))
	  (goto-char last-and)))
      ;; sort and build lists
      (mapcar (lambda (pair)
		(if (cdr pair)
		    (setq menu
			  (append (tuareg-split-long-list
			    (car pair) (tuareg-sort-definitions (cdr pair)))
				  menu))))
	      (list (cons "Miscellaneous" misc-list)
		    (cons "Values" value-list)
		    (cons "Classes" class-list)
		    (cons "Types" type-list)
		    (cons "Modules" module-list)))
      ;; update definitions menu
      (setq tuareg-definitions-menu
	    (append menu (list "---" ["Rescan..." tuareg-list-definitions t])))
      (if (or tuareg-with-xemacs
	      (not (functionp 'easy-menu-create-keymaps))) ()
	;; patch for Emacs 20.2
	(setq tuareg-definitions-keymaps
	      (cdr (easy-menu-create-keymaps 
		    "Definitions" tuareg-definitions-menu)))
	(setq tuareg-definitions-menu-last-buffer nil))
      (message "Searching definitions... done"))))

(defun tuareg-goto (pos)
  (goto-char pos)
  (recenter))

(defun tuareg-sort-definitions (list)
  (let* ((last "") (cpt 1)
	 (list (sort (nreverse list)
		     (lambda (p q) (string< (elt p 0) (elt q 0)))))
	 (tail list))
    (while tail
      (if (string= (elt (car tail) 0) last)
	  (prog2
	      (setq cpt (1+ cpt))
	      (aset (car tail) 0 (format "%s (%d)" last cpt)))
	(setq cpt 1)
	(setq last (elt (car tail) 0)))
      (setq tail (cdr tail)))
    list))

;; look for the (n-1)th or last element of a list
(defun tuareg-nth (n list)
  (if (or (<= n 1) (null list) (null (cdr list))) list
    (tuareg-nth (1- n) (cdr list))))
    
;; split a definition list if it is too long
(defun tuareg-split-long-list (title list)
  (let ((tail (tuareg-nth tuareg-definitions-max-items list)))
    (if (or (null tail) (null (cdr tail)))
        ;; list not too long, cons the title
        (list (cons title list))
      ;; list too long, split and add initials to the title
      (let (lists)
        (while list
          (let ((beg (substring (elt (car list) 0) 0 1))
                (end (substring (elt (car tail) 0) 0 1)))
            (setq lists (cons
                         (cons (format "%s %s-%s" title beg end) list)
                         lists))
            (setq list (cdr tail))
            (setcdr tail nil)
            (setq tail (tuareg-nth tuareg-definitions-max-items list))))
        (nreverse lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(defvar tuareg-load-hook nil
  "This hook is run when Tuareg is loaded in. It is a good place to put
key-bindings or hack Font-Lock keywords...")

(run-hooks 'tuareg-load-hook)

(provide 'caml) ;; for compatibility with caml support modes
                ;; you may also build a link from caml.el to tuareg.el
(provide 'tuareg)
