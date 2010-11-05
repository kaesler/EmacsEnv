;;!emacs
;;
;; FILE:         eif-browse.el
;; SUMMARY:      Eiffel source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     20-Sep-95 at 14:24:36 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'eif-browse' to invoke the Eiffel browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-start br br-eif br-eif-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun eif-browse (&optional env-file no-ui)
  "Invoke the Eiffel OO-Browser.
This allows browsing through Eiffel library and system class hierarchies.
With an optional prefix arg ENV-FILE equal to t, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix eif-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix eif-lang-prefix
	    *br-save-wconfig* nil))
    (setq same-env (or (equal eif-env-file env-file)
		       (and (null env-file)
			    (or eif-lib-search-dirs eif-sys-search-dirs))))
    (cond
     ;; Continue browsing an Environment
     ((and same-env same-lang))
     ((and same-env (not same-lang))
      (eif-browse-setup) (br-env-copy t))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p eif-env-file)
	    (br-env-create eif-env-file eif-lang-prefix))
	(or env-file (setq env-file eif-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(eif-browse-setup)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  eif-env-file load-succeeded
		  eif-sys-search-dirs br-sys-search-dirs
		  eif-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (br-init)
	   (or no-ui (br-browse)))
	  (no-ui nil)
	  (t (message "(eif-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(fset 'eif-class-list-filter 'identity)

(defun eif-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat eif-class-name-before (regexp-quote class)
	  eif-class-name-after))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun eif-browse-setup ()
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (fset 'br-store-class-info 'eif-store-class-info)
  (fset 'br-lang-mode
	(cond ((or (featurep 'eiffel3) (featurep 'eiffel-mode))
	       'eiffel-mode)
	      ((load "eiffel3" 'missing-ok 'nomessage)
	       (provide 'eiffel-mode))
	      ((load "eiffel" 'missing-ok 'nomessage)
	       (provide 'eiffel-mode))
	      (t 'fundamental-mode)))
  (br-setup-constants))

(provide 'eif-browse)
