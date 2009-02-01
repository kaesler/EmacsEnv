;;!emacs
;;
;; FILE:         br-tree.el
;; SUMMARY:      Interface between textual and graphical OO-Browsers.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    12-Oct-90
;; LAST-MOD:      5-Jun-95 at 12:00:10 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   Requires the X Window system Version 11 or NEXTSTEP.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-lib)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar *br-tree-prog-name*
  (if (or (eq window-system 'x) (null window-system))
      "xoobr"
    "./TreeView.app/TreeView")
  "Program to run for hierarchical display of classes.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-tree (&optional arg)
  "Start the appropriate tree application with descendency tree of current class.
With optional prefix ARG, a descendency tree for each class in current buffer."
  (interactive "P")
  (let* ((classes (if arg
		    (br-this-level-classes)
		  (list (br-find-class-name))))
	 (ch (delq nil (mapcar (function (lambda (c) (br-get-children c)))
			      classes))))
    (if (or ch br-show-features)
	(br-tree-load classes)
      (beep)
      (message "No descendants to display."))))

(defun br-tree-graph ()
  "Start the appropriate tree application with the tree from current listing buffer."
  (interactive)
  (let* ((tree) (indent) (entry) (min-indent 8000) (min-count 0)
	 (feature-match (format "^%s " br-feature-type-regexp)))
    (save-excursion
      (goto-char (point-max))
      (while (and (= (forward-line -1) 0)
		  (looking-at "\\([ \t]*\\)\\(.+\\)"))
	(setq indent (buffer-substring (match-beginning 1) (match-end 1))
	      entry (length indent)
	      min-indent (cond ((= entry min-indent)
				(setq min-count (1+ min-count))
				entry)
			       ((< entry min-indent)
				(setq min-count 1)
				entry)
			       (min-indent))
	      entry (buffer-substring (match-beginning 2) (match-end 2))
	      entry (if (string-match feature-match entry)
			(concat (char-to-string (aref entry 0))
				(substring entry 2)
				"^^" (br-feature-get-signature))
		      entry)
	      tree (cons (concat indent entry "\n") tree))))
    (or (= min-count 1)
	(setq tree (cons (concat *br-tree-root-name* "\n")
			 (mapcar (function
				  (lambda (node) (concat "  " node))) tree))))
    (br-tree-load tree t)))

(defun br-tree-do-cmd (lang env cmd node)
  ;; Load necessary Environment
  (if (not (equal env br-env-file))
      (let ((br (intern-soft
		  (concat lang "browse"))))
	(if (br-in-browser) (funcall br env) (funcall br env t))))
  ;; Do command
  (cond ((and (not (eq (symbol-function 'br-feature-tree-command-p)
		       'br-undefined-function))
	      (br-feature-tree-command-p node)))
	((string-equal cmd "br-view")
	 (br-view nil nil node))
	((string-equal cmd "br-edit")
	 (br-view nil t node))
	(t (beep)
	   (message
	    (format "(OO-Browser):  Illegal command: %s" cmd)))))

(defun br-tree-features-toggle ()
  "Toggle between showing and hiding features when 'br-tree' is invoked to display descendants graphically."
  (interactive)
  (setq br-show-features (not br-show-features))
  (message "New graphical OO-Browsers will %sshow features."
	   (if br-show-features "" "not ")))

(defun br-tree-kill ()
  "Kill all current 'Tree' sub-processes."
  (interactive)
  (if (br-kill-process-group br-tree-name br-tree-num
			     "Tree displays")
      (setq br-tree-num 0)))

(defun br-tree-load (classes-or-tree &optional tree-p)
  "Start the appropriate tree application using trees from CLASSES-OR-TREE.
Optional TREE-P non-nil means CLASSES-OR-TREE is a tree ready for display."
  (interactive (list "sClass to show descendency graph of: "))
  (if (and br-env-file (not br-env-spec))
      (let ((obuf (current-buffer))
	    (tree-file (concat "/tmp/br-" (user-real-login-name)
			       (int-to-string
				(setq br-tree-num (1+ br-tree-num)))
			       ".tree")))
	(if classes-or-tree
	    (progn (find-file tree-file)
		   (widen)
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   ;; Start file with Envir file name
		   (insert "^^" br-lang-prefix "^^" br-env-file "\n")
		   (if tree-p
		       (mapcar 'insert classes-or-tree)
		     (br-tree-build classes-or-tree))
		   (untabify 1 (point-max))
		   (save-buffer)
		   (kill-buffer (current-buffer))
		   (switch-to-buffer obuf)
		   (if (eq window-system 'x)
		       (br-tree-x-load-tree-file tree-file)
		     (br-tree-nx-load-tree-file tree-file)))))))

(defun br-tree-nx-load-tree-file (tree-file)
  "Load a pre-written TREE-FILE and display it in an X OO-Browser."
  (setq delete-exited-processes t)
  (let ((proc (get-process br-tree-name)))
    (if (and proc (eq (process-status proc) 'run))  ;; existing tree browser
	;; Send it an open file command.
	(call-process "open" nil 0 nil "-a"
		      (file-name-nondirectory *br-tree-prog-name*)
		      tree-file)
      (let ((default-directory (file-name-as-directory
				 (expand-file-name "tree-nx" br-directory))))
	(setq proc (start-process
		     br-tree-name nil *br-tree-prog-name*
		     tree-file))
	(if proc
	    (progn (set-process-filter proc 'br-tree-filter)
		   (process-kill-without-query proc)
		   ))))))

(defun br-tree-x-load-tree-file (tree-file)
  "Load a pre-written TREE-FILE and display it in an X OO-Browser."
  (setq delete-exited-processes t)
  (let ((proc))
    (setq proc (start-process 
		(concat br-tree-name (int-to-string br-tree-num))
		nil
		*br-tree-prog-name*
		tree-file))
    (if proc
	(progn (set-process-filter proc 'br-tree-filter)
	       (process-kill-without-query proc)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defconst *br-tree-root-name* "NO-ROOT" 
  "Name to give root tree node when graph with no root is used as input.")

(defun br-tree-build (class-list &optional indent offset)
  "Insert descendant trees starting with classes from CLASS-LIST.
Indent each class in CLASS-LIST by optional INDENT spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2)."
  (or indent (setq indent 0))
  (or offset (setq offset 2))
  (let ((prev-expansion-str " ...")
	ch expand-subtree)
    (if (= indent 0)
	(progn (setq br-tmp-class-set nil)
	       (if (= (length class-list) 1)
		   nil
		 (insert *br-tree-root-name* "\n")
		 (setq indent offset))))
    (if class-list
	(progn 
	  (indent-to indent)
	  (mapcar (function (lambda (c)
		     (setq expand-subtree (br-set-cons br-tmp-class-set c)
			   ch (if expand-subtree (br-get-children c)))
		     (indent-to indent)
		     (insert c)
		     (and (not expand-subtree)
			  (br-has-children-p c)
			  (insert prev-expansion-str))
		     (insert "\n")
		     (if (and br-show-features
			      (br-tree-build-features
			       c expand-subtree (+ indent offset) offset))
			 nil
		       (if ch
			   (br-tree-build ch (+ indent offset) offset)))))
		  class-list))))
  (if (= indent 0) (setq br-tmp-class-set nil)))

(defun br-tree-build-features (c expand-subtree indent offset)
  "Each language under which this function is called must define its own
version of 'br-list-features' and 'br-feature-signature-to-name'."
  (let ((features) (ch))
    (and expand-subtree
	 (setq features
		 (mapcar
		  (function
		   (lambda (feature-tag)
		     (concat (br-feature-signature-to-name feature-tag nil t)
			     "^^" feature-tag)))
		  (br-list-features c)))
	 (progn
	   (mapcar
	     (function
	       (lambda (feature)
		 (indent-to indent)
		 (insert feature "\n")))
	     features)
	   (if (setq ch (if expand-subtree (br-get-children c)))
	       (br-tree-build ch indent offset))
	   t))))

(defun br-tree-filter (process output-str)
  (let ((br-lang-px)
	(br-env-nm)
	(br-cmd-nm)
	(br-node-nm))
    (if (not (string-match "\n" output-str))
	(setq br-cmd-str (concat br-cmd-str output-str))
      (setq br-cmd-str (concat br-cmd-str
			       (substring output-str 0 (match-beginning 0))))
      (if (and (> (length br-cmd-str) 9)
	       (equal (substring br-cmd-str -4)
		      " ..."))
	  (setq br-cmd-str (substring br-cmd-str 0 -4)))
      ;; Is a command only if starts with ^^
      (if (and (> (length br-cmd-str) 1)
	       (equal (substring br-cmd-str 0 2) "^^")
	       (string-match
		"^^^\\(.+\\)^^\\(.+\\)^^\\(.+\\)^^\\(.+\\)"
		br-cmd-str))
	  (progn
	    (setq br-lang-px (substring br-cmd-str
					(+ (match-beginning 1) 2)
					(match-end 1))
		  br-env-nm (substring br-cmd-str
				       (match-beginning 2)
				       (match-end 2))
		  br-cmd-nm (substring br-cmd-str
				       (match-beginning 3)
				       (match-end 3))
		  br-node-nm (substring br-cmd-str
					(match-beginning 4)
					(match-end 4))
		  br-cmd-str nil)
	    (br-tree-do-cmd br-lang-px br-env-nm
			    br-cmd-nm br-node-nm))
	(beep)
	(message "'%s': invalid command from graphical browser"
		 br-cmd-str)
	(setq br-cmd-str nil)))))


;;; ************************************************************************
;;; Private functions
;;; ************************************************************************


(defvar br-cmd-str nil
  "Command string sent from graphical OO-Browser to the textual OO-Browser.")

(defvar br-show-features nil
  "Non-nil means add features as child nodes in each graphical descendancy view.")

(defvar br-tree-num 0)
(defvar br-tree-name "Tree")

(provide 'br-tree)
