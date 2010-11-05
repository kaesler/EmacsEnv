;;!emacs
;;
;; FILE:         hvar.el
;; SUMMARY:      Variable manipulation routines for Hyperbole.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:     1-Oct-91 at 14:00:24
;; LAST-MOD:     14-Apr-95 at 16:13:46 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'set)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun var:append (var-symbol-name list-to-add)
  "Appends to value held by VAR-SYMBOL-NAME, LIST-TO-ADD.  Returns new value.
If VAR-SYMBOL-NAME is unbound, it is set to LIST-TO-ADD.
Often used to append to 'hook' variables."
  (let ((val))
    (if (and (boundp var-symbol-name)
	     (setq val (symbol-value var-symbol-name))
	     (or (if (symbolp val) (setq val (cons val nil)))
		 (listp val)))
	;; Don't add if list elts are already there.
	(if (memq nil (mapcar (function
				(lambda (elt) (set:member elt val)))
			      list-to-add))
	    (set-variable var-symbol-name
			  (if (eq (car val) 'lambda)
			      (apply 'list val list-to-add)
			    (append val list-to-add)))
	  val)
      (set-variable var-symbol-name list-to-add))))

(provide 'hvar)

