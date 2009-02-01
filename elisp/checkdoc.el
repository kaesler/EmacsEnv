Greetings:

  After collecting all of the great comments and suggestions from
everyone here is another revision of checkdoc.  Checkdoc is a
specialized emacs lisp style checker for document strings.

  General changes include:

1) More style checks, file comment checks (using lm-verify and other
   comment suggestions), and some generic white space nukers.
2) Emacs lisp reference manual quoted into the code in relevant places.
3) Autofixer for some problems with controlled user interaction.
4) More keybindings for checkdoc minor mode.

  Checkdoc now has a small homepage of my main web page:

http://www.ultranet.com/~zappo/checkdoc.shtml

  For more details see the comments and code.

-Eric
-------------------------------------------------------------------
;;; checkdoc --- Check documentation strings for style requirements

;;; Copyright (C) 1997  Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.2
;; Keywords: docs, maint, lisp
;; X-RCS: $Id: checkdoc.el,v 1.15 1997/04/04 01:52:29 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;
;; Future versions of checkdoc will appear at:
;;   ftp://ftp.ultranet.com/pub/zappo/checkdoc-*.el
;;

;;; Commentary:
;;
;;   The emacs lisp manual has a nice chapter on how to write
;; documentation strings.  Many stylistic suggestions are fairly
;; deterministic and easy to check for programatically, but also easy
;; to forget.  The main checkdoc engine will perform the stylistic
;; checks needed to make sure these styles are remembered.
;;
;;   There are two ways to use checkdoc:
;;   1) Periodically use `checkdoc-current-buffer' and
;;      `checkdoc-defun' to check your documentation.
;;   2) Use `checkdoc-minor-mode' to automatically check your
;;      documentation whenever you evaluate lisp code with C-M-x
;;      or [menu-bar emacs-lisp eval-buffer]
;;        (require 'checkdoc)
;;        (add-hook 'emacs-lisp-mode-hook
;;	             '(lambda () (checkdoc-minor-mode 1)))
;;
;; Autofixing:
;;
;;   There are three classifications of style errors in terms of how
;; easy they are to programatically fix.  That is simple, complex, and
;; impossible.  (Impossible really means that checkdoc does not have a
;; fixing routine yet.)  Typically white-space errors are classified
;; as simple, and are auto-fixed by default.  Typographic changes are
;; considered complex, and the user is asked if they want the problem
;; fixed before checkdoc makes the change.
;;   The variable `checkdoc-autofix-flag' controls how these types of
;; errors are fixed.
;;
;; This file requires lisp-mnt (lisp maintenance routines) for the
;; comment checkers.

;;; Change log:
;; 0.1   Initial revision
;; 0.2   Fixed comments in this file to match the emacs lisp standards.
;;       Added new doc checks for: variable-flags, function arguments
;;       Added autofix functionality for white-space, and quoted variables.
;;       Unquoted symbols are allowed after ( character. (Sample code)
;;       Check for use of `? ' at end of line and warn.
;;       Check for spaces at end of lines for whole file, or one defun.
;;       Check for comments standards, including headinds like Code:
;;         and use of triple semicolons versus double semicolons
;;       Check that interactive functions have a doc string.  Optionally
;;         set `checkdoc-force-docstrings-flag' to non-nil to make all
;;         definitions have a doc string.

;;; TO DO:
;;   Hook into the byte compiler on a defun/defver level to generate
;;     warnings in the byte-compiler's warning/error buffer.
;;   Allow a 'checkdoc' comment section in a lisp file specifically
;;     listing known errors an author doesn't wish to fix.
;;   Better ways to override more typical `eval' functions.  Advice
;;     might be good but hard to turn on/off as a minor mode.
;;
;;; Maybe Do:
;;   Code sweep checks for "forbidden functions", proper use of hooks,
;;     proper keybindings, and other items from the manual that are
;;     not specifically docstring related.  Would this even be useful?

;;; Code:
(defvar checkdoc-autofix-flag 'semiautomatic
  "*Non-nil means attempt auto-fixing of doc-strings.
If this value is the symbol 'query, then the user is queried before
any change is made. If the value is 'automatic, then all changes are
made without asking.  If the value is 'semiautomatic, or any other
value, then simple fixes are made without asking, and complex changes
are made by asking the user first.")

(defvar checkdoc-bouncy-flag t
  "*Non-nil means to 'bounce' to autofix locations.
Setting this to nil will silently make fixes that require no user
interaction.  See `checkdoc-autofix-flag' for autofixing details.")

(defvar checkdoc-force-docstrings-flag t
  "*Non-nil means that all checkable definitions should have documentation.
Style guide dictates that interactive functions MUST have documentation,
and that its good but not required practice to make non user visible items
have doc strings.")

(defvar checkdoc-max-keyref-before-warn 10
  "*The number of \\ [command-to-keystroke] tokens allowed in a doc string.
Any more than this and a warning is generated suggesting that the construct
\\ {keymap} be used instead.")

(defvar checkdoc-diagnostic-buffer "*Document Warnings*"
  "Name of the buffer where checkdoc stores warning messagtes.")

(defvar checkdoc-defun-regexp
  "^(def\\(un\\|var\\|macro\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]+"
  "Regular expression used to identify a defun.
A search leaves the cursor in front of the paramter list.")

;;; User level commands
;;
;;;###autoload
(defun checkdoc-eval-current-buffer ()
  "Evaluate and check documentation for the current buffer.
Evaluation is done first because good documentation for something that
doesn't work is just not useful.  Comments, docstrings, and rogue
spacing are all verified."
  (interactive)
  (eval-current-buffer nil)
  (checkdoc-current-buffer t))

;;;###autoload
(defun checkdoc-current-buffer (&optional take-notes)
  "Check the current buffer for doc style, comment style, and rogue spaces.
Optional argument TAKE-NOTES non-nil will store all found errors in a
warnings buffer, otherwise it stops after the first error."
  (interactive "P")
  (if (interactive-p) (message "Checking buffer for style..."))
  ;; every test is responsible for returning the cursor.
  (or (checkdoc-comments take-notes)
      (checkdoc take-notes)
      (checkdoc-rogue-spaces take-notes)
      (not (interactive-p))
      (message "Checking buffer for style...Done.")))

;;;###autoload
(defun checkdoc (&optional take-notes)
  "Find the first doc string in the current buffer which is stylisticly poor.
Prefix argument TAKE-NOTES means to continue through the whole buffer and
save warnings in a separate buffer."
  (interactive "P")
  (let ((wrong nil) (msg nil) (errors nil))
    (save-excursion
      ;; If we are taking notes, encompass the whole buffer, otherwise
      ;; the user is navigating down through the buffer.
      (if take-notes (checkdoc-start-section "checkdoc"))
      (goto-char (point-min))
      (while (and (not wrong) (re-search-forward checkdoc-defun-regexp nil t))
	;; search drops us after the identifier.  The next sexp is either
	;; the argument list or the value of the variable.  skip it.
	(forward-sexp 1)
	(skip-chars-forward " \n\t")
	(if (/= (following-char) ?\")
	    ;; No doc string...
	    nil
	  ;; OK, lets look at the doc string.
	  (setq msg (checkdoc-this-string-valid))
	  (if msg
	      ;; Oops
	      (if take-notes
		  (progn
		    (checkdoc-error (point) msg)
		    (setq errors t))
		(setq wrong (point)))))))
    (if wrong
	(progn
	  (goto-char wrong)
	  (error msg)))
    (if (and take-notes errors)
	(checkdoc-show-diagnostics))
    ))

;;; ###autoload
(defun checkdoc-comments (&optional take-notes)
  "Find missing comment sections in the current emacs lisp file.
Prefix argument TAKE-NOTES non-nil means to save warnings in a
separate buffer.  Otherwise print a message.  This returns the error
if there is one."
  (interactive "P")
  (if take-notes (checkdoc-start-section "checkdoc-comments"))
  (let ((e (checkdoc-file-comments-engine)))
    (if e
	(if take-notes
	    (checkdoc-error nil e)
	  (error e)))
    (if (and e take-notes)
	(checkdoc-show-diagnostics))
    e))

(defun checkdoc-rogue-spaces (&optional take-notes)
  "Find extra spaces at the end of lines in the current file.
Prefix argument TAKE-NOTES non-nil means to save warnings in a
separate buffer.  Otherwise print a message.  This returns the error
if there is one."
  (interactive "P")
  (if take-notes (checkdoc-start-section "checkdoc-rogue-spaces"))
  (let ((e (checkdoc-rogue-space-check-engine)))
    (if e
	(if take-notes
	    (checkdoc-error nil e)
	  (message e)))
    (if (and e take-notes)
	(checkdoc-show-diagnostics))
    e))

;;;###autoload
(defun checkdoc-eval-defun ()
  "Evaluate the current form with `eval-defun' and check it's documentaion.
Evaluation is done first so the form will be read before the
documentation is checked.  If there is a doc error, then the display
of what was evaluated will be overwritten by the diagnostic message."
  (interactive)
  (eval-defun nil)
  (checkdoc-defun))

;;;###autoload
(defun checkdoc-defun (&optional no-error)
  "Examine the doc string of the function or variable under point.
Calls `error' if the doc string produces diagnostics.  If NO-ERROR is
non-nil, then do not call error, but call `message' instead.
If the document check passes, then check the function for rogue white
space at the end of each line."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (if (not (looking-at checkdoc-defun-regexp))
	(if (not no-error) (message "Cannot check this defun's doc string."))
      ;; search drops us after the identifier.  The next sexp is either
      ;; the argument list or the value of the variable.  skip it.
      (goto-char (match-end 0))
      (forward-sexp 1)
      (skip-chars-forward " \n\t")
      (let ((msg (checkdoc-this-string-valid)))
	(if msg (if no-error (message msg) (error msg))
	  (setq msg (checkdoc-rogue-space-check-engine
		     (save-excursion (beginning-of-defun) (point))
		     (save-excursion (end-of-defun) (point))))
	  (if msg (if no-error (message msg) (error msg))))))))

;;; Minor Mode specification
;;
(defvar checkdoc-minor-mode nil
  "Non-nil in `emacs-lisp-mode' for automatic documentation checking.")
(make-variable-buffer-local 'checkdoc-minor-mode)

(add-to-list 'minor-mode-alist '(checkdoc-minor-mode " CDoc"))

(defvar checkdoc-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    ;; Override some bindings
    (define-key map "\C-\M-x" 'checkdoc-eval-defun)
    (if (not (string-match "XEmacs" emacs-version))
	(progn
	  (define-key map [menu-bar emacs-lisp eval-buffer]
	    'checkdoc-eval-current-buffer)))
    (define-key pmap "x" 'checkdoc-defun)
    (define-key pmap "b" 'checkdoc-current-buffer)
    (define-key pmap "e" 'checkdoc-eval-current-buffer)
    (define-key pmap "c" 'checkdoc-comments)

    ;; bind our submap into map
    (define-key map "\C-c?" pmap)
    map)
  "Keymap used to override evaluation keybindings for documentation checking.")

;; Allow re-insertion of a new keymap
(let ((a (assoc 'checkdoc-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a checkdoc-minor-keymap)
    (add-to-list 'minor-mode-map-alist (cons 'checkdoc-minor-mode
					     checkdoc-minor-keymap))))

;;;###autoload
(defun checkdoc-minor-mode (&optional arg)
  "Toggle Checkdoc minor mode.  A mode for checking lisp doc strings.
With prefix ARG, turn Checkdoc minor mode on iff arg is positive.

In checkdoc minor mode, the usual bindings for `eval-defun' which is
bound to \\<checkdoc-minor-keymap> \\[checkdoc-eval-defun] and `eval-current-buffer' are overridden to include
checking of documentation strings.

\\{checkdoc-minor-keymap}"
  (interactive "P")
  (setq checkdoc-minor-mode
	(not (or (and (null arg) checkdoc-minor-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (force-mode-line-update))

;;; Checking engines
;;
(defun checkdoc-this-string-valid ()
  "Return a message string if the current docstring is invalid.
Check for style only, such as the first line always being a complete
sentence, whitespace restrictions, and making sure there are no
hard-coded keycodes such as C-[char] or mouse-[number] in the comment.
See the style guide in the Emacs Lisp manual for more details."

  (let ((case-fold-search nil)
	(e (save-excursion (forward-sexp 1) (point)))
	(fp (checkdoc-defun-info)))
    (or
     ;;   * Every command, function, or variable intended for users to know
     ;;     about should have a documentation string.
     ;;
     ;;   * An internal variable or subroutine of a Lisp program might as well
     ;;     have a documentation string.  In earlier Emacs versions, you could
     ;;     save space by using a comment instead of a documentation string,
     ;;     but that is no longer the case.
     (if (and (not (nth 1 fp))		; not a variable
	      (or (nth 2 fp)		; is interactive
		  checkdoc-force-docstrings-flag) ;or we always complain
	      (/= (following-char) ?\")); no doc string
	 (if (nth 2 fp)
	     "All interactive functions should have documentation"
	   "All variables and subroutines might as well have a documentation string"))
     ;;   * The first line of the documentation string should consist of one
     ;;     or two complete sentences that stand on their own as a summary.
     ;;     `M-x apropos' displays just the first line, and if it doesn't
     ;;     stand on its own, the result looks bad.  In particular, start the
     ;;     first line with a capital letter and end with a period.
     (save-excursion
       (end-of-line)
       (skip-chars-backward " \t\n")
       (forward-char -1)
       ;; 59 = ; which messes up formatting.
       ;; 41 = close paren which also messes up formatting.
       (if (not (member (following-char) '(?. 59 ?: ?\" 41)))
	   "First line is not a complete sentence"))
     ;;   * Don't write key sequences directly in documentation strings.
     ;;     Instead, use the `\\[...]' construct to stand for them.
     (save-excursion
       (if (re-search-forward
	    "\\<\\([CMA]-[a-zA-Z]\\|\\(\\([CMA]-\\)?mouse-[0-3]\\)\\)\\>"
	    e t)
	   "Keycodes embedded in doc-string.  Use \\<keymap> & \\[function] instead"))
     ;;     It is not practical to use `\\[...]' very many times, because
     ;;     display of the documentation string will become slow.  So use this
     ;;     to describe the most important commands in your major mode, and
     ;;     then use `\\{...}' to display the rest of the mode's keymap.
     (save-excursion
       (if (re-search-forward "\\\\\\\\\\[\\w+" e t
			      (1+ checkdoc-max-keyref-before-warn))
	   "Too many occurrences of \\[function].  Use \\{keymap} instead"))
     ;;   * *Do not* indent subsequent lines of a documentation string so that
     ;;     the text is lined up in the source code with the text of the first
     ;;     line.  This looks nice in the source code, but looks bizarre when
     ;;     users view the documentation.  Remember that the indentation
     ;;     before the starting double-quote is not part of the string!
     (save-excursion
       (forward-line 1)
       (beginning-of-line)
       (if (and (< (point) e)
		(looking-at "\\([ \t]+\\)[^ \t\n]"))
	   (if (checkdoc-autofix-ask-replace (match-beginning 1)
					     (match-end 1)
					     "Remove this whitespace?"
					     "")
	       nil
	     "Second line should not have indentation")))
     ;;  * Do not start or end a documentation string with whitespace.
     (let (start end)
       (if (or (if (looking-at "\"\\([ \t\n]+\\)")
		   (setq start (match-beginning 1)
			 end (match-end 1)))
	       (save-excursion
		 (forward-sexp 1)
		 (forward-char -1)
		 (if (/= (skip-chars-backward " \t\n") 0)
		     (setq start (point)
			   end (1- e)))))
	   (if (checkdoc-autofix-ask-replace
		start end "Remove this whitespace?" "")
	       nil
	     "Documentation strings should not start or end with whitespace")))
     ;;   * Format the documentation string so that it fits in an Emacs window
     ;;     on an 80-column screen.  It is a good idea for most lines to be no
     ;;     wider than 60 characters.  The first line can be wider if
     ;;     necessary to fit the information that ought to be there.
     (save-excursion
       (while (and (< (point) e)
		   (or (progn (end-of-line) (< (current-column) 80))
		       (progn (beginning-of-line)
			      (re-search-forward "\\\\\\\\[[<{]"
						 (save-excursion
						   (end-of-line)
						   (point)) t))))
	 (forward-line 1))
       (end-of-line)
       (if (and (< (point) e) (> (current-column) 80))
	   "Some lines are over 80 columns wide"))
     ;;  * When a documentation string refers to a Lisp symbol, write it as it
     ;;    would be printed (which usually means in lower case), with
     ;;    single-quotes around it.  For example: `lambda'.  There are two
     ;;    exceptions: write t and nil without single-quotes.  (In this
     ;;    manual, we normally do use single-quotes for those symbols.)
     (save-excursion
       (let ((found nil))
	 (while (and (not found)
		     (re-search-forward
		      "[^([`]\\(\\w+-\\(\\w\\|\\s_\\)+\\)[^]']" e t))
	   (setq found
		 (and (setq found (intern-soft (match-string 1)))
		      (or (boundp found) (fboundp found)))))
	 (if found
	     (let ((msg (format "Lisp symbol %s should appear in `quotes'"
				(match-string 1))))
	       (if (checkdoc-autofix-ask-replace
		    (match-beginning 1) (match-end 1) msg
		    (concat "`" (match-string 1) "'") t)
		   nil
		 msg)))))
     ;; t and nil case
     (save-excursion
       (if (re-search-forward "\\(`\\(t\\|nil\\)'\\)" e t)
	   (if (checkdoc-autofix-ask-replace
		(match-beginning 1) (match-end 1)
		(format "%s should not appear in quotes. Remove?"
			(match-string 2))
		(match-string 2) t)
	       nil
	     "Symbols t and nil should not appear in `quotes'")))
     ;; Here we deviate to tests based on a variable or function.
     (cond ((eq (nth 1 fp) t)
	    ;; This is if we are in a variable
	    (or
	     ;; * The documentation string for a variable that is a
	     ;;   yes-or-no flag should start with words such as "Non-nil
	     ;;   means...", to make it clear that all non-`nil' values are
	     ;;   equivalent and indicate explicitly what `nil' and non-`nil'
	     ;;   mean.

	     ;; If the variable has -flag in the name, make sure
	     (if (and (string-match "-flag$" (car fp))
		      (not (looking-at "\"\\*?Non-nil\\s-+means\\s-+")))
		 "Flag variable doc strings should start: Non-nil means")
	     ;; If the doc string starts with "Non-nil means"
	     (if (and (looking-at "\"\\*?Non-nil\\s-+means\\s-+")
		      (not (string-match "-flag$" (car fp))))
		 "Flag variables should end in: -flag")
	     ;; Done with variables
	     ))
	   (t
	    ;; This if we are in a function definition
	    (or
	     ;; * When a function's documentation string mentions the value
	     ;;   of an argument of the function, use the argument name in
	     ;;   capital letters as if it were a name for that value.  Thus,
	     ;;   the documentation string of the function `/' refers to its
	     ;;   second argument as `DIVISOR', because the actual argument
	     ;;   name is `divisor'.

	     ;;   Addendum:  Make sure they appear in the doc in the same
	     ;;              order that they are found in the arg list.
	     (let ((args (cdr (cdr (cdr fp))))
		   (last-pos 0)
		   (found 1))
	       (while (and args found (> found last-pos))
		 (if (member (car args) '("&optional" "&rest"))
		     (setq args (cdr args))
		   (setq last-pos found
			 found (save-excursion
				 (re-search-forward (upcase (car args)) e t)))
		   (if found (setq args (cdr args)))))
	       (if (not found)
		   (format
		    "Argument %s should appear as %s in the doc string"
		    (car args) (upcase (car args)))
		 (if (< found last-pos)
		     "Arguments occur in the doc string out of order")))
	     ;; Done with functions
	     )))
     ;; Done!
     )))

(defun checkdoc-defun-info ()
  "Return a list of details about the current sexp.
It is a list of the form '( NAME VARIABLE INTERACTIVE PARAMETERS ... )
where NAME is the name, VARIABLE is t if this is a `defvar',
INTERACTIVE is nil if this is not an interactive function, otherwise
it is the position of the `interactive' call, and PARAMTERS is a
string which is the name of each variable in the function's argument
list."
  (save-excursion
    (beginning-of-defun)
    (let ((defun (looking-at "(defun"))
	  (ret nil))
      (forward-char 1)
      (forward-sexp 1)
      (skip-chars-forward " \n\t")
      (setq ret
	    (list (buffer-substring-no-properties
		   (point) (progn (forward-sexp 1) (point)))))
      (if (not defun)
	  (setq ret (cons t ret))
	;; The variable spot
	(setq ret (cons nil ret))
	;; Interactive
	(save-excursion
	  (setq ret (cons
		     (re-search-forward "(interactive"
					(save-excursion (end-of-defun) (point))
					t)
		     ret)))
	(skip-chars-forward " \t\n(")
	(while (not (looking-at "\\s-*)"))
	  (setq ret
		;; Add in the paramters
		(cons (buffer-substring-no-properties
		       (point) (progn (forward-sexp 1) (point)))
		      ret))
	  (skip-chars-forward " \t\n")))
      (nreverse ret))))

;;; Rogue space checking engine
;;
(defun checkdoc-rogue-space-check-engine (&optional start end)
  "Return a message string if there is a line with white space at the end.
If `checkdoc-autofix-flag' permits, delete that whitespace instead.
If optional arguments START and END are non nil, bound the check to
this region."
  (let ((p (point))
	(msg nil))
    (or
     ;; Checkfor and error if `? ' is used at the end of a line.
     ;; (It's dangerous)
     (progn
       (goto-char (point-min))
       (if (re-search-forward "\\?\\\\?[ \t][ \t]*$" nil t)
	   (setq msg
		 "Don't use `? ' at the end of a line. \
Some editors & news agents may remove it")))
     ;; Check for, and pottentially remove whitespace appearing at the
     ;; end of different lines.
     (progn
       (goto-char (or start (point-min)))
       ;; There is no documentation in the elisp manual about this check,
       ;; it is intended to help clean up messy code and reduce the file size.
       (while (and (not msg) (re-search-forward "[^ \t]\\([ \t]+\\)$" end t))
	 ;; This is not a complex activity
	 (if (checkdoc-autofix-ask-replace
	      (match-beginning 1) (match-end 1)
	      "White space at end of line. Remove?" "")
	     nil
	   (setq msg "White space found at end of line.")))))
    ;; Return an error and leave the cursor at that spot, or restore
    ;; the cursor.
    (if msg
	msg
      (goto-char p)
      nil)))

;;; Comment checking engine
;;
(eval-when-compile (require 'lisp-mnt))

(defun checkdoc-file-comments-engine ()
  "Return a message string if this file does not match the emacs standard.
This checks for style only, such as the first line, Commentary:,
Code:, and others referenced in the style guide."
  (require 'lisp-mnt)
  (save-excursion
    (let ((fn (file-name-sans-extension
	       (file-name-nondirectory (buffer-file-name)))))
      (goto-char (point-min))
      (or
       (lm-verify)
       ;; Ok, now lets look for multiple occurances of ;;;, and offer
       ;; to remove the extra ";" if applicable.  This pre-supposes
       ;; that the user has semiautomatic fixing on to be useful.
       (let ((msg nil))
	 (goto-char (point-min))
	 (while (and (not msg) (re-search-forward "^;;;[^;]" nil t))
	   ;; We found a triple, lets check all following lines.
	   (beginning-of-line)
	   (forward-line 1)
	   (let ((complex-replace t))
	     (while (looking-at ";;\\(;\\)[^;]")
	       (if (checkdoc-autofix-ask-replace
		    (match-beginning 1) (match-end 1)
		    "Multiple occurances of ;;; found. Use ;; instead?" ""
		    complex-replace)
		   ;; Learn that, yea, the user did want to do this a
		   ;; whole bunch of times.
		   (setq complex-replace nil))
	       (beginning-of-line)
	       (forward-line 1)))))
       ;; Done with full file comment checks
       ))))

;;; Auto-fix helper functions
;;
(defun checkdoc-autofix-ask-replace (start end question replacewith
					   &optional complex)
  "Highlights between START and END and queries the user with QUESTION.
If the user says yes, or if `checkdoc-autofix-flag' permits, replace
the region marked by START and END with REPLACEWITH.  If optional flag
COMPLEX is non-nil, then we may ask the user a question.  See the
documentation for `checkdoc-autofix-flag' for details.

If a section is autoreplaced without asking the user, this function
will pause near the fixed code so the user will briefly see what
happened.

This function returns non-nil if the text was replaced."
  (if checkdoc-autofix-flag
      (let ((o (make-overlay start end))
	    (ret nil))
	(unwind-protect
	    (progn
	      (overlay-put o 'face 'highlight)
	      (if (or (eq checkdoc-autofix-flag 'automatic)
		      (and (eq checkdoc-autofix-flag 'semiautomatic)
			   (not complex))
		      (and (or (eq checkdoc-autofix-flag 'query) complex)
			   (y-or-n-p question)))
		  (save-excursion
		    (goto-char start)
		    ;; On the off chance this is automatic, display
		    ;; the question anyway so the user knows whats
		    ;; going on.
		    (if checkdoc-bouncy-flag (message question))
		    (delete-region start end)
		    (insert replacewith)
		    (if checkdoc-bouncy-flag (sit-for 0))
		    (setq ret t)))
	      (delete-overlay o))
	  (delete-overlay o))
	ret)))

;;; Warning management
;;
(defvar checkdoc-output-font-lock-keywords
  '(("\\(\\w+\\.el\\):" 1 font-lock-function-name-face)
    ("style check: \\(\\w+\\)" 1 font-lock-comment-face)
    ("^\\([0-9]+\\):" 1 font-lock-reference-face))
  "Keywords used to highlight a checkdoc diagnostic buffer.")

(defvar checkdoc-output-mode-map nil
  "Keymap used in `checkdoc-output-mode'")

(if checkdoc-output-mode-map
    nil
  (setq checkdoc-output-mode-map (make-sparse-keymap))
  (if (not (string-match "XEmacs" emacs-version))
      (define-key checkdoc-output-mode-map [mouse-2]
	'checkdoc-find-error-mouse))
  (define-key checkdoc-output-mode-map "\C-c\C-c" 'checkdoc-find-error))

(defun checkdoc-output-mode ()
  "Create and setup the buffer used to maintain checkdoc warnings.
\\<checkdoc-output-mode-map>\\[checkdoc-find-error]  - Go to this error location
\\[checkdoc-find-error-mouse] - Goto the error clicked on."
  (if (get-buffer checkdoc-diagnostic-buffer)
      (get-buffer checkdoc-diagnostic-buffer)
    (save-excursion
      (set-buffer (get-buffer-create checkdoc-diagnostic-buffer))
      (kill-all-local-variables)
      (setq mode-name "Checkdoc"
	    major-mode 'checkdoc-output-mode)
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults '((checkdoc-output-font-lock-keywords)
				 t t ((?- . "w") (?_ . "w"))))
      (use-local-map checkdoc-output-mode-map)
      (run-hooks 'checkdoc-output-mode-hook)
      (current-buffer))))

(defun checkdoc-find-error-mouse (e)
  "Call `checkdoc-find-error' where the user clicks the mouse.
Must be bound to mouse click event E"
  (interactive "e")
  (mouse-set-point e)
  (checkdoc-find-error))

(defun checkdoc-find-error ()
  "In a checkdoc diagnostic buffer, find the error under point."
  (interactive)
  (beginning-of-line)
  (if (looking-at "[0-9]+")
      (let ((l (string-to-int (match-string 0)))
	    (f (save-excursion
		 (re-search-backward " \\(\\(\\w+\\|\\s_\\)+\\.el\\):")
		 (match-string 1))))
	(if (not (get-buffer f))
	    (error "Can't find buffer %s" f))
	(switch-to-buffer-other-window (get-buffer f))
	(goto-line l))))

(defun checkdoc-start-section (check-type)
  "Initialize the checkdoc diagnostic buffer for a pass.
Create the header so that the string CHECK-TYPE is displayed as the
function called to create the messages."
  (checkdoc-output-to-error-buffer
   "\n\n*** " (current-time-string) " "
   (file-name-nondirectory (buffer-file-name)) ": style check: " check-type))

(defun checkdoc-error (point msg)
  "Store POINT and MSG as errors in the checkdoc diagnostic buffer."
  (checkdoc-output-to-error-buffer
   "\n" (int-to-string (count-lines (point-min) (or point 1))) ": "
   msg))

(defun checkdoc-output-to-error-buffer (&rest text)
  "Place TEXT into the checkdoc diagnostic buffer."
  (save-excursion
    (set-buffer (checkdoc-output-mode))
    (goto-char (point-max))
    (apply 'insert text)))

(defun checkdoc-show-diagnostics ()
  "Display the checkdoc diagnostic buffer in a temporary window."
  (let ((b (get-buffer checkdoc-diagnostic-buffer)))
    (if b (progn (pop-to-buffer b)
		 (beginning-of-line)))
    (other-window -1)))

(provide 'checkdoc)
;;; checkdoc ends here
