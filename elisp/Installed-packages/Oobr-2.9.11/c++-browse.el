;;!emacs
;;
;; FILE:         c++-browse.el
;; SUMMARY:      C++ source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     20-Sep-95 at 14:18:40 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'c++-browse' to invoke the C++ OO-Browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(mapcar 'require '(br-start br br-c++-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun c++-browse (&optional env-file no-ui)
  "Invoke the C++ OO-Browser.
This allows browsing through C++ library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix c++-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix c++-lang-prefix
	    *br-save-wconfig* nil))
    (setq same-env (or (equal c++-env-file env-file)
		       (and (null env-file)
			    (or c++-lib-search-dirs c++-sys-search-dirs))))
    (cond
     ;; Continue browsing an Environment
     ((and same-env same-lang))
     ((and same-env (not same-lang))
      (c++-browse-setup) (br-env-copy t))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p c++-env-file)
	    (br-env-create c++-env-file c++-lang-prefix))
	(or env-file (setq env-file c++-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(c++-browse-setup)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  c++-env-file load-succeeded
		  c++-sys-search-dirs br-sys-search-dirs
		  c++-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (br-init)
	   (or no-ui (br-browse)))
	  (no-ui nil)
	  (t (message "(c++-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(fset 'c++-class-list-filter 'identity)

(defun c++-mode-setup ()
  "Load best available C++ major mode and set 'br-lang-mode' to the function that invokes it."
  (fset 'br-lang-mode
	(cond ((or (featurep 'cc-mode) (featurep 'c++-mode))
	       'c++-mode)
	      ((load "cc-mode" 'missing-ok 'nomessage)
	       (provide 'c++-mode))
	      ((load "c++-mode" 'missing-ok 'nomessage)
	       (provide 'c++-mode))
	      ((featurep 'c-mode) 'c-mode)
	      ((load "c-mode" nil 'nomessage)
	       (provide 'c-mode)))))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun c++-browse-setup ()
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  ;; Use this until an info function is implemented for the language.
  (fmakunbound 'br-insert-class-info)
  (fset 'br-store-class-info 'c++-store-class-info)
  (c++-mode-setup)
  (br-setup-constants)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'c++-add-default-classes)
    (setq br-after-build-sys-hook '(c++-add-default-classes))))

(provide 'c++-browse)
