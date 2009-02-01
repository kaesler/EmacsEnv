;;!emacs
;;
;; FILE:         hact.el
;; SUMMARY:      Hyperbole button action handling.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:    18-Sep-91 at 02:57:09
;; LAST-MOD:     14-Apr-95 at 15:57:11 by Bob Weiner
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

(require 'hhist)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hrule:action 'actype:act
  "Value is a function of any number of arguments that executes actions.
Variable is used to vary actual effect of evaluating a Hyperbole action,
e.g. to inhibit actions.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; ========================================================================
;;; action class
;;; ========================================================================

(defun action:commandp (function)
  "Return interactive calling form if FUNCTION has one, else nil."
  (let ((action
	 (cond ((null function) nil)
	       ((symbolp function)
		(and (fboundp function)
		     (hypb:indirect-function function)))
	       ((and (listp function)
		     (eq (car function) 'autoload))
		(error "(action:commandp): Autoload not supported: %s" function))
	       (t function))))
    (if (hypb:v19-byte-code-p action)
	(if (commandp action)
	    (list 'interactive (aref action 5)))
      (commandp action))))

(defun action:create (param-list body)
  "Create an action defined by PARAM-LIST and BODY, a list of Lisp forms."
  (if (symbolp body)
      body
    (list 'function (cons 'lambda (cons param-list body)))))

(defun action:kbd-macro (macro &optional repeat-count)
  "Returns Hyperbole action that executes a keyboard MACRO REPEAT-COUNT times."
  (list 'execute-kbd-macro macro repeat-count))

(defun action:params (action)
  "Returns unmodified ACTION parameter list."
  (cond ((null action) nil)
	((symbolp action)
	 (car (cdr
	       (and (fboundp action) (hypb:indirect-function action)))))
	((listp action)
	 (if (eq (car action) 'autoload)
	     (error "(action:params): Autoload not supported: %s" action)
	   (car (cdr action))))
	((hypb:v19-byte-code-p action)
	 ;; Turn into a list for extraction
	 (car (cdr (cons nil (append action nil)))))))

(defun action:param-list (action)
  "Returns list of actual ACTION parameters (removes '&' special forms)."
  (delq nil (mapcar
	      (function
		(lambda (param)
		  (if (= (aref (symbol-name param)
			       0) ?&)
		      nil param)))
	      (action:params action))))

(defun action:path-args-abs (args-list &optional default-dirs)
  "Return any paths in ARGS-LIST made absolute.
Uses optional DEFAULT-DIRS or 'default-directory'.
Other arguments are returned unchanged."
  (mapcar (function (lambda (arg) (hpath:absolute-to arg default-dirs)))
	  args-list))

(defun action:path-args-rel (args-list)
  "Return any paths in ARGS-LIST below current directory made relative.
Other paths are simply expanded.  Non-path arguments are returned unchanged."
  (let ((dir (hattr:get 'hbut:current 'dir)))
    (mapcar (function (lambda (arg) (hpath:relative-to arg dir)))
	    args-list)))


;;; ========================================================================
;;; actype class
;;; ========================================================================

(defmacro hact (&rest args)
  "Performs action formed from rest of ARGS.
First arg may be a symbol or symbol name for either an action type or a
function.  Runs 'action:act-hook' before performing action."
  (eval (` (cons 'funcall (cons 'hrule:action (quote (, args)))))))

(defun    actype:act (actype &rest args)
  "Performs action formed from ACTYPE and rest of ARGS and returns value.
If value is nil, however, t is returned instead, to ensure that implicit button
types register the performance of the action.  ACTYPE may be a symbol or symbol
name for either an action type or a function.  Runs 'action:act-hook' before
performing ACTION."
  ;; Needed so relative paths are expanded properly.
  (setq args (action:path-args-abs args))
  (let ((prefix-arg current-prefix-arg)
	(action (actype:action actype))
	(act '(apply action args)))
    (if (null action)
	(error "(actype:act): Null action for: '%s'" actype)
      (let ((hist-elt (hhist:element)))
	(run-hooks 'action:act-hook)
	(prog1 (or (cond ((or (symbolp action) (listp action)
			      (hypb:v19-byte-code-p action))
			  (eval act))
			 ((and (stringp action)
			       (let ((func (key-binding action)))
				 (if (not (integerp action))
				     (setq action func))))
			  (eval act))
			 (t (eval action)))
		   t)
	  (hhist:add hist-elt))
	))))

(defun    actype:action (actype)
  "Returns action part of ACTYPE (a symbol or symbol name).
ACTYPE may be a Hyperbole actype or Emacs Lisp function."
  (let (actname)
    (if (stringp actype)
	(setq actname actype
	      actype (intern actype))
      (setq actname (symbol-name actype)))
    (cond ((htype:body (if (string-match "^actypes::" actname)
			   actype
			 (intern-soft (concat "actypes::" actname)))))
	  ((fboundp actype) actype)
	  )))

(defmacro actype:create (type params doc &rest default-action)
  "Creates an action TYPE (an unquoted symbol) with PARAMS, described by DOC.
The type uses PARAMS to perform DEFAULT-ACTION (list of the rest of the
arguments).  A call to this function is syntactically the same as for
'defun',  but a doc string is required.
Returns symbol created when successful, else nil."
 (list 'htype:create type 'actypes doc params default-action nil))

(fset    'defact 'actype:create)
(put     'actype:create 'lisp-indent-function 'defun)

(defun    actype:delete (type)
  "Deletes an action TYPE (a symbol).  Returns TYPE's symbol if it existed."
  (htype:delete type 'actypes))

(defun    actype:doc (hbut &optional full)
  "Returns first line of act doc for HBUT (a Hyperbole button symbol).
With optional FULL, returns full documentation string.
Returns nil when no documentation."
  (let* ((act (and (hbut:is-p hbut) (or (hattr:get hbut 'action)
					(hattr:get hbut 'actype))))
	 (but-type (hattr:get hbut 'categ))
	 (sym-p (and act (symbolp act)))
	 (end-line) (doc))
    (cond ((and but-type (fboundp but-type)
		(setq doc (htype:doc but-type)))
	   ;; Is an implicit button, so use its doc string if any.
	   )
	  (sym-p
	   (setq doc (htype:doc act))))
    (if (null doc)
	nil
      (setq doc (substitute-command-keys doc))
      (or full (setq end-line (string-match "[\n]" doc)
		     doc (substring doc 0 end-line))))
    doc))

(defun    actype:identity (&rest args)
  "Returns list of ARGS unchanged or if no ARGS, returns t.
Used as the setting of 'hrule:action' to inhibit action evaluation."
  (or args t))

(defun    actype:interact (actype)
  "Interactively calls default action for ACTYPE.
ACTYPE is a symbol that was previously defined with 'defact'.
Returns nil only when no action is found or the action has no interactive
calling form." 
  (let ((action (htype:body
		 (intern-soft (concat "actypes::" (symbol-name actype))))))
    (and action (action:commandp action) (or (call-interactively action) t))))

(defun    actype:params (actype)
  "Returns list of ACTYPE's parameters."
  (action:params (actype:action actype)))

(provide 'hact)
