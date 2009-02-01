;;!emacs
;;
;; FILE:         objc-brows.el
;; SUMMARY:      Objective-C source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     20-Sep-95 at 14:31:42 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'objc-browse' to invoke the Objective-C OO-Browser.  Prefix arg
;;    prompts for name of Environment file.
;;
;; DESCRIP-END.

(mapcar 'require '(br-start br br-objc-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Cases
;; 
;; env-file = nil
;;   Use default environment, objc-env-file
;;   if objc not loaded, load it
;;   if objc-env-file != br-env-file
;;      switch to objc
;; 
;; env-file = t
;;   Prompt for env
;;   if env != objc
;;      load it
;;   else if env != br-env
;;      switch to env
;; 
;; env-file = filename
;;   if env != objc-env
;;      
;; 
;; objc-env-file = br-env-file

;;;###autoload
(defun objc-browse (&optional env-file no-ui)
  "Invoke the Objective-C OO-Browser.
This allows browsing through Objective-C library and system class
hierarchies.  With an optional non-nil prefix argument ENV-FILE, prompt for
Environment file to use.  Alternatively, a string value of ENV-FILE is used
as the Environment file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix objc-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix objc-lang-prefix
	    *br-save-wconfig* nil))
    (setq same-env (or (equal objc-env-file env-file)
		       (and (null env-file)
			    (or objc-lib-search-dirs objc-sys-search-dirs))))
    (cond
     ;; Continue browsing an Environment
     ((and same-env same-lang))
     ((and same-env (not same-lang))
      (objc-browse-setup) (br-env-copy t))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p objc-env-file)
	    (br-env-create objc-env-file objc-lang-prefix))
	(or env-file (setq env-file objc-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(objc-browse-setup)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  objc-env-file load-succeeded
		  objc-sys-search-dirs br-sys-search-dirs
		  objc-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (br-init)
	   (or no-ui (br-browse)))
	  (no-ui nil)
	  (t (message "(objc-browse): You must build the Environment to browse it.")))))

(defun objc-class-list-filter (class-list)
  "Return CLASS-LIST sans any protocol or class category entries.
Used when Environment classes are listed."
  (delq nil (mapcar (function (lambda (class)
				(if (string-match "[\(\<]" class)
				    nil
				  class)))
		    class-list)))

(defun objc-mode-setup ()
  "Load best available Objective-C major mode and set 'br-lang-mode' to the function that invokes it."
  (fset 'br-lang-mode
	(cond ((or (fboundp 'objc-mode) (featurep 'objc-mode)) 'objc-mode)
	      ((load "objc-mode" t 'nomessage) 'objc-mode)
	      ((featurep 'c-mode) 'c-mode)
	      ((load "cc-mode" 'missing-ok 'nomessage)
	       (if (fboundp 'objc-mode) 'objc-mode 'c-mode))
	      ((load "c-mode" nil 'nomessage)
	       (provide 'c-mode)))))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun objc-browse-setup ()
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  ;; Use this until an info function is implemented for the language.
  (fmakunbound 'br-insert-class-info)
  (fset 'br-store-class-info 'objc-store-class-info)
  (objc-mode-setup)
  (br-setup-constants)
  ;; Setup to add default classes ([category] and [protocol]) to system class
  ;; table after building it.  This must come after br-setup-constants call
  ;; since it clears these hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'objc-add-default-classes)
    (setq br-after-build-sys-hook '(objc-add-default-classes))))


(provide 'objc-brows)
