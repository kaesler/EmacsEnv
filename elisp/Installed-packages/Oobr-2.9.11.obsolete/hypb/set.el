;;!emacs
;;
;; FILE:         set.el
;; SUMMARY:      Provide general mathematical operators on unordered sets.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:    26-Sep-91 at 19:24:19
;; LAST-MOD:     14-Apr-95 at 16:17:03 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   All set operations herein work with sets of arbitrary Lisp objects,
;;   including strings.  By default, they use 'equal' for comparisons
;;   but this may be overidden by changing the function bound to
;;   the 'set:equal-op' variable.
;;
;; DESCRIP-END.

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

;; ************************************************************************
;; Public variables
;; ************************************************************************

(defvar set:equal-op 'equal
  "Comparison function used by set operators.
It must be a function of two arguments which returns non-nil only when
the arguments are equivalent.")

;; ************************************************************************
;; Public functions
;; ************************************************************************

(defmacro set:add (elt set)
  "Adds element ELT to SET and then returns SET.
Uses 'set:equal-op' for comparison.
Use (setq set (set:add elt set)) to assure set is always properly modified."
  (` (cond ((set:member (, elt) (, set)) (, set))
	   ((, set) (setq (, set) (cons (, elt) (, set))))
	   (t (list (, elt))))))

(defun set:combinations (set &optional arity)
  "Returns all possible combinations (subsets) of SET.
Assumes SET is a valid set.  With optional ARITY, returns only subsets with
ARITY members."
  (cond ((null arity) 
	 (setq arity 0)
	 (cons nil (apply 'nconc (mapcar (function
					   (lambda (elt)
					     (setq arity (1+ arity))
					     (set:combinations set arity)))
					 set))))
	((= arity 1) set)
	((<= arity 0) '(nil))
	(t (let ((rest) (ctr 1))
	     (apply
	       'nconc
	       (mapcar (function
			 (lambda (first)
			   (setq rest (nthcdr ctr set)
				 ctr (1+ ctr))
			   (mapcar (function
				     (lambda (elt)
				       (if (listp elt) (cons first elt)
					 (list first elt))))
				   (set:combinations rest (1- arity)))))
		       set))))))

(defun set:create (&rest elements)
  "Returns a new set created from any number of ELEMENTS or a list of ELEMENTS.
Uses 'set:equal-op' for comparison."
  (let ((set))
    (mapcar (function
	      (lambda (elt) (or (set:member elt set)
				(setq set (cons elt set)))))
	    (if (or (null (car elements)) (not (listp (car elements))))
		elements
	      (car elements)))
    set))

(fset 'set:delete 'set:remove)
(defun set:difference (&rest sets)
  "Returns difference of any number of SETS.
Difference is the set of elements in the first set that are not in any of the
other sets.  Uses 'set:equal-op' for comparison."
  (let ((rtn-set (set:members (car sets))))
    (mapcar
      (function
	(lambda (set)
	  (mapcar (function
		    (lambda (elt) (set:remove elt rtn-set)))
		  set)))
      (cdr sets))
    rtn-set))

(defun set:equal (set1 set2)
  "Returns t iff SET1 contains the same members as SET2.  Both must be sets.
Uses 'set:equal-op' for comparison."
  (and (listp set1) (listp set2)
       (= (set:size set1) (set:size set2))
       (set:subset set1 set2)))

(defun set:get (key set)
  "Returns the value associated with KEY in SET or nil.
Elements of SET should be of the form (key . value)."
  (cdr (car (let ((set:equal-op
		   (function (lambda (key elt)
			       (equal key (car elt))))))
	      (set:member key set)))))

(defun set:intersection (&rest sets)
  "Returns intersection of all SETS given as arguments.
Uses 'set:equal-op' for comparison."
  (let ((rtn-set))
    (mapcar
      (function
	(lambda (elt)
	  (or (memq nil (mapcar (function
				  (lambda (set) (set:member elt set)))
				(cdr sets)))
	      (setq rtn-set (cons elt rtn-set)))))
      (car sets))
    rtn-set))

(defun set:is (obj)
  "Returns t if OBJ is a set (a list with no repeated elements).
Uses 'set:equal-op' for comparison."
  (and (listp obj)
       (let ((lst obj))
	 (while (and (not (set:member (car lst) (cdr lst)))
		     (setq lst (cdr lst))))
	 (null lst))))

(fset 'set:map 'mapcar)

(defun set:member (elt set)
  "Returns non-nil if ELT is an element of SET.
The value is actually the tail of SET whose car is ELT.
Uses 'set:equal-op' for comparison."
  (while (and set (not (funcall set:equal-op elt (car set))))
    (setq set (cdr set)))
  set)

(defun set:members (list)
  "Returns set of unique elements of LIST.
Uses 'set:equal-op' for comparison.  See also 'set:create'."
  (let ((set))
    (mapcar (function
	      (lambda (elt) (or (set:member elt set) (setq set (cons elt set)))))
	    list)
    set))

(defmacro set:remove (elt set)
  "Removes element ELT from SET and returns new set.
Assumes SET is a valid set.  Uses 'set:equal-op' for comparison.
Use (setq set (set:remove elt set)) to assure set is always properly modified."
  (` (let ((rest (set:member (, elt) (, set)))
	   (rtn (, set)))
       (if rest
	   (cond ((= (length rtn) 1) (setq rtn nil))
		 ((= (length rest) 1)
		  (setcdr (nthcdr (- (length rtn) 2) rtn) nil))
		 (t (setcar rest (car (cdr rest)))
		    (setcdr rest (cdr (cdr rest))))))
       rtn)))

(defun set:replace (key value set)
  "Replaces or adds element whose car matches KEY with element (KEY . VALUE) in SET.
Returns set if modified, else nil.
Use (setq set (set:replace elt set)) to assure set is always properly modified.

Uses 'set:equal-op' to match against KEY.  Assumes each element in the set
has a car and a cdr."
  (let ((elt-set (set:member key set)))
    (if elt-set
	;; replace element
	(progn (setcar elt-set (cons key value))
	       set)
      ;; add new element
      (cons (cons key value) set))))

(fset 'set:size 'length)

(defun set:subset (sub set)
  "Returns t iff set SUB is a subset of SET.
Uses 'set:equal-op' for comparison."
  (let ((is t))
    (mapcar (function (lambda (elt) (if is (setq is (set:member elt set))))) sub)
    (and is t)))

(defun set:union (&rest sets)
  "Returns union of all SETS given as arguments.
Uses 'set:equal-op' for comparison."
  (let ((rtn-set))
    (mapcar
      (function
	(lambda (set) (mapcar (function
				(lambda (elt)
				  (setq rtn-set (set:add elt rtn-set))))
			      set)))
      sets)
    rtn-set))

;; ************************************************************************
;; Private variables
;; ************************************************************************

(provide 'set)
