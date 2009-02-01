;; nbi-ps-hooks -- Smart news/mail setup inspired by sample code in ps-print.el

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Ulrik Dickow <dickow@nbi.dk> (/Jim Thompson <thompson@wg2.waii.com>)
;; Revisions:
;;   1.01 19950718 UD -- modified comments (esp. for RFC-822 parsing).
;;   1.0  19950717 UD -- made GNUS+RMAIL use buffer local variables;
;;                       use gnus-article-mode-hook instead of -prepare-hook.
;;        19950703 UD -- created as separate file.
;; Keywords: print, PostScript, news, GNUS, mail, RMAIL

;; This file is not part of GNU Emacs. Nonetheless, it is released
;; in accordance with the terms of the GNU General Public License as
;; published by the Free Software Foundation.

;;; Commentary:

;; Purpose: Make a special header setup for news (GNUS) and mail (RMAIL).
;;   ps-print 2.8 has sample code for VM, but not for RMAIL.
;; Requirements: ps-print 2.0 or later (2.8 is part of Emacs 19.29).
;;   If you use lazy-lock, then it should be version 1.11.02 or later;
;;   otherwise printing from an RMAIL summary buffer may sometimes fail.
;;   Works with good old GNUS as well as (ding) gnus.

;; Installation: In your ~/.emacs you can do
;;   (require 'nbi-ps-hooks)    ; Define utility functions & variables
;; and optionally this, for example (if you don't have a [print] key):
;;   (setq nbi-ps-prsc [f22]) ; Define other spool buffer key than the default.
;;   (setq nbi-ps-s-prsc [S-f22]) ; ........ spool region key
;;   (setq nbi-ps-c-prsc [C-f22]) ; ........ despool key
;; and finally
;;   (nbi-ps-special-key-setup) ; Define keys, add hooks for GNUS & RMAIL.
;; If your keyboard has a PrintScreen key, but Emacs & X11 don't respond to it,
;; then you may want to define it in ~/.Xmodmap (or /etc/X11/xinit/...), e.g.:
;;   keycode 111 = Print
;; This is the case for Linux with XFree86 3.1.  See xmodmap(1) for details.

;; Future: This code may have the "nbi-" prefix changed or stripped,
;;   and it may then be merged back into ps-print.el.

;;; Code:

(defvar nbi-ps-prsc [print] "*Key for spooling buffer with faces")
(defvar nbi-ps-s-prsc [S-print] "*Key for spooling region with faces")
(defvar nbi-ps-c-prsc [C-print] "*Key for despooling ps-print output")

;; ps-article-subject is OK in ps-print 2.0+2.8.  Just copy & rename it.
;;   Look in an article or mail message for the Subject: line.
;;   To be placed in `ps-left-headers'.
(defun nbi-ps-article-subject ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Subject:[ \t]+\\(.*\\)$")
	(buffer-substring (match-beginning 1) (match-end 1))
      "Subject ???")))

;; ps-article-author is needlessly complex; we'll use what's built-in to Emacs.
(defun nbi-ps-article-author ()
  "Look in an article or mail message for the From: line.  Use one of the
built-in RFC-822 address parsers of Emacs and pull out the real name where
it's provided.  To be placed in `ps-left-headers'."
  (save-excursion
    (goto-char (point-min))
    (or (and (re-search-forward "^From:[ \t]+\\(.+\\)")
	     (let ((x (mail-extract-address-components
		       (buffer-substring (match-beginning 1) (match-end 1)))))
	       ;; x is a list like `("Ulrik Dickow" "dickow@nbi.dk")'.
	       ;; mail-... is jwz's powerful one, autoloaded from mail-extr.el.
	       (or (car x) ;; Found a real name.
		   (nth 1 x)))) ;; Couldn't find one, then try the address.
	"From ???"))) ;; Didn't find a parsable, non-empty From: field.

(defun nbi-ps-gnus-article-mode-hook ()
  "A hook to add to `gnus-article-prepare-hook'.
This will set the `ps-left-headers' specially for GNUS articles."
  (set (make-local-variable 'ps-header-lines) 3)
  (set (make-local-variable 'ps-left-header)
	;; The left headers will display the article's subject, its
	;; author, and the newsgroup it was in.
	(list 'nbi-ps-article-subject 'nbi-ps-article-author
	      'gnus-newsgroup-name)))

;; ps-gnus-print-article-from-summary is OK.  Copy & rename.
(defun nbi-ps-gnus-print-article-from-summary ()
  "Print the current GNUS *Article* buffer (if any),
temporarily switching to that buffer.  Usually used from a summary buffer."
  (interactive)
  (if (get-buffer "*Article*")
      (save-excursion
	(set-buffer "*Article*")
	(ps-spool-buffer-with-faces))))

(defun nbi-ps-rmail-mode-hook ()
  "A hook to add to rmail-mode-hook.
This will set the ps-left-headers specially for mail messages."
  (set (make-local-variable 'ps-header-lines) 3)
  (set (make-local-variable 'ps-left-header)
	;; The left headers will display the message's subject, its
	;; author, and the name of the folder it was in.
	(list 'nbi-ps-article-subject 'nbi-ps-article-author 'buffer-name)))

(defun nbi-ps-rmail-print-message-from-summary ()
  "See `ps-gnus-print-article-from-summary'.
This function does the same thing for RMAIL."
  (interactive)
  (if (boundp 'rmail-buffer)
      (save-excursion
	(set-buffer rmail-buffer)
	(ps-spool-buffer-with-faces))
    (error "You must be in an RMAIL summary buffer in order to use this!")))

(defun nbi-ps-gnus-summary-setup ()
  "A hook to add to `gnus-summary-mode-hook' to locally bind `nbi-ps-prsc'."
  (local-set-key nbi-ps-prsc 'nbi-ps-gnus-print-article-from-summary))

(defun nbi-ps-rmail-summary-setup ()
  "A hook to add to `rmail-summary-mode-hook' to locally bind `nbi-ps-prsc'."
  (local-set-key nbi-ps-prsc 'nbi-ps-rmail-print-message-from-summary))

(defun nbi-ps-special-key-setup ()
  "Bind the keys defined by `nbi-ps-prsc' etc.,
using a special setup for GNUS and RMAIL."
  (interactive)
  (global-set-key nbi-ps-prsc 'ps-spool-buffer-with-faces)
  (global-set-key nbi-ps-s-prsc 'ps-spool-region-with-faces)
  (global-set-key nbi-ps-c-prsc 'ps-despool)
  (add-hook 'gnus-article-mode-hook 'nbi-ps-gnus-article-mode-hook)
  (add-hook 'gnus-summary-mode-hook 'nbi-ps-gnus-summary-setup)
  (add-hook 'rmail-mode-hook 'nbi-ps-rmail-mode-hook)
  (add-hook 'rmail-summary-mode-hook 'nbi-ps-rmail-summary-setup))

(provide 'nbi-ps-hooks)
