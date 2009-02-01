;;!emacs
;;
;; FILE:         br-c-ft.el
;; SUMMARY:      OO-Browser C construct handling.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc.
;;
;; ORIG-DATE:     3-May-95 at 16:47:05
;; LAST-MOD:     21-Oct-95 at 04:30:51 by Bob Weiner
;;
;; Copyright (C) 1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar   c-default-classes
  '("[constant]" "[enumeration]" "[function]" "[macro]"
    "[structure]" "[type]" "[union]")
  "*List of default class names of C constructs handled by the OO-Browser.

If you add a class to this list, you also need to add appropriate filtering
code for features of the class to \"br-c-tags\".")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun c-add-default-classes ()
  ;; Add to 'system' class table.
  (mapcar
   (function
    (lambda (class)
      (br-add-class class br-null-path nil)))
   c-default-classes))

(defun c-build-element-tags ()
  "Create C constructs tags file for the current Environment.
This excludes functions.  Call this after building the language-specific
feature tags file."
  ;; If c-tags have already been added to feature tags, then the feature tags
  ;; buffer ends with ^L.
  (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
  (if (or (not (stringp br-tags-file))
	  (progn (goto-char (point-max))
		 (skip-chars-backward "\n")
		 (if (/= (point) (point-min))
		     (backward-char 1))
		 (= (following-char) ?\^L)))
      nil
    (message "Building C construct index...")
;    For debugging.
;    (message "%s %s %s"
;	     (expand-file-name "br-c-tags" br-directory)
;	     br-tags-file
;	     (mapcar 'expand-file-name
;		     (delq nil (append br-sys-search-dirs
;				       br-lib-search-dirs))))
    (apply 'call-process (expand-file-name "br-c-tags" br-directory)
	   nil nil nil
	   ;; If no etags program in exec-directory, use one in user's $PATH.
	   (let ((etags (expand-file-name "etags" exec-directory)))
	     (if (file-executable-p etags) etags "etags"))
	   br-tags-file
	   (mapcar 'expand-file-name
		   (delq nil (append br-sys-search-dirs br-lib-search-dirs))))
    (goto-char (point-max))
    (let ((c-tags-start (point)))
      (insert-file-contents br-tags-file)
      (goto-char (point-max))
      (insert "\^L\n") ;; To mark end of C tags insertion.
      (delete-file br-tags-file)
      (goto-char c-tags-start)
      ;; Remove tag files which have no entries.
      (while (re-search-forward "^\^L\n.*\n\^L\n" nil t)
	(replace-match "\^L\n")
	(forward-line -1)))
    (message "Building C construct index...Done")))

(defun c-within-comment-p ()
  "Return non-nil if point is within a multi-line C comment."
  ;; Generally don't have to check whether patterns are matched on single line
  ;; comments  ( // ...) since the regexps to match to will preclude this.
  ;; Ignore comments of the form //***, which look like C comments when
  ;; searching backward but are actually single line comments.
  (save-excursion
    (and (re-search-backward "\\(^\\|[^/]\\)/\\*\\|\\*/" nil t)
	 (not (looking-at "\\*/")))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'br-c-ft)
