;;!emacs
;;
;; FILE:         hasht.el
;; SUMMARY:      Create hash tables from lists and operate on them.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    16-Mar-90 at 03:38:48
;; LAST-MOD:     27-Sep-95 at 21:31:20 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   Featureful set of hash table operators for use in personal programs.
;;
;;   'hash-make' creates a hash table from an association list, 'hash-add'
;;   adds a value-key pair to a hash table, and 'hash-lookup' finds the value
;;   associated with a given key in a hash table, if any.
;;
;;   'hash-map' does the same thing as 'mapcar' but operates on hash tables
;;   instead.
;;
;;   For a list of 300 items, these hash tables improve lookup times by a
;;   factor of between 8 and 10 to 1 over those for an unsorted list.
;;
;;   Public and private function names are alphabetized for easy location.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hash-merge-values-function 'hash-merge-values
  "*Function to call in hash-merge to merge the values from 2 hash tables that contain the same key.
It is sent the two values as arguments.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hash-add (value key hash-table)
  "Add VALUE, any lisp object, referenced by KEY, a string, to HASH-TABLE.
Replaces any VALUE previously referenced by KEY."
  (if (hashp hash-table)
      (let* ((obarray (hash-obarray hash-table))
	     (sym (intern key obarray)))
	(if sym (set sym value)))))

(defun hash-copy (hash-table)
  "Return a copy of HASH-TABLE, list and vector elements are shared across both tables."
  (if (not (hashp hash-table))
      (error "(hash-copy): Invalid hash-table: '%s'" hash-table))
  (let ((htable-copy (hash-make (length (hash-obarray hash-table)))))
    (hash-map
     (function (lambda (elt) (hash-add (car elt) (cdr elt) htable-copy)))
     hash-table)
    htable-copy))

(defun hash-count (hash-table)
  "Return number of elements stored in HASH-TABLE or nil if not a valid hash table."
  (if (hashp hash-table)
      (let ((obarray (hash-obarray hash-table))
	    (count 0))
	(mapatoms (function
		    (lambda (sym)
		      (and (boundp sym) sym (setq count (1+ count)))))
		  obarray)
	count)))

(defun hash-delete (key hash-table)
  "Delete element referenced by KEY, a string, from HASH-TABLE.
Return nil if KEY is not in HASH-TABLE or non-nil otherwise."
  (if (hashp hash-table)
      (let* ((obarray (hash-obarray hash-table))
	     (sym (intern-soft key obarray)))
	(and sym (boundp sym) (makunbound sym)))))

(defun hash-deep-copy (obj)
  "Return a copy of OBJ with new copies of all elements, except symbols."
  (cond ((null obj) nil)
	((stringp obj)
	 (copy-sequence obj))
	((hashp obj)
	 (let ((htable-copy (hash-make (length (hash-obarray obj)))))
	   (mapcar
	    (function
	     (lambda (elt) (hash-add (car elt) (cdr elt) htable-copy)))
	    (hash-map 'hash-deep-copy obj))
	   htable-copy))
	((vectorp obj)
	 ;; convert to list for mapping
	 (setq obj (append obj nil))
	 ;; Return as a vector
	 (vconcat (mapcar 'hash-deep-copy obj)))
	((atom obj) obj)
	((nlistp obj)
	 (error "(hash-deep-copy): Invalid type, '%s'" obj))
	(t ;; list
	 (cons (hash-deep-copy (car obj)) (hash-deep-copy (cdr obj))))))

(fset  'hash-get  'hash-lookup)
(defun hash-key-p (key hash-table)
  "Return non-nil iff KEY is in HASH-TABLE.  KEY's hash table symbol is returned."
  (if (hashp hash-table)
      (let* ((obarray (hash-obarray hash-table))
	     (sym (intern-soft key obarray)))
	 (if (boundp sym) sym))))

(defun hash-lookup (key hash-table)
  "Lookup KEY in HASH-TABLE and return associated value.
If value is nil, this function does not tell you whether or not KEY is in the
hash table.  Use 'hash-key-p' instead for that function."
  (if (hashp hash-table)
      (let* ((obarray (hash-obarray hash-table))
	     (sym (intern-soft key obarray)))
	 (if (boundp sym) (symbol-value sym)))))

(defun hash-make (initializer &optional reverse)
  "Create a hash table from INITIALIZER.
INITIALIZER may be an alist with elements of the form (<value> . <key>) from
which the hash table is built.  Alternatively, it may be a non-negative
integer which is used as the minimum size of a new, empty hash table.
Optional non-nil second argument REVERSE means INITIALIZER has elements of
form (<key> . <value>)."
  (if (integerp initializer)
      (if (>= initializer 0)
	  (cons 'hasht (make-vector (hash-next-prime initializer) 0))
	(error "(hash-make): Initializer must be >= 0, not '%s'"
	       initializer))
    (let* ((vlen (hash-next-prime (length initializer)))
	   (obarray (make-vector vlen 0))
	   sym)
      (mapcar
       (function
	(lambda (cns)
	  (let ((key) (value))
	    (if (consp cns)
		(if reverse
		    (setq key (car cns) value (cdr cns))
		  (setq key (cdr cns) value (car cns))))
	    (if (setq sym (intern key))
		(set sym value)))))
       initializer)
      (cons 'hasht obarray))))

(defun hash-map (func hash-table)
  "Return result of application of FUNC to each (<value> . <key>) element of HASH-TABLE."
  (if (not (hashp hash-table))
      (error "(hash-map): Invalid hash-table: '%s'" hash-table))
  (let ((result))
    (mapatoms (function
		(lambda (sym)
		  (and (boundp sym)
		       sym
		       (setq result (cons (funcall
					   func
					   (cons (symbol-value sym)
						 (symbol-name sym)))
					  result)))))
	      (hash-obarray hash-table))
    result))

(defun hash-merge (&rest hash-tables)
  "Merge any number of HASH-TABLES.  Return resultant hash table.
A single argument consisting of a list of hash tables may also be given.
Return an empty hash table if any argument from the merge list is other
than nil or a hash table."
  (let ((empty-ht (hash-make 1)))
    (and (not (hashp (car hash-tables)))
	 (listp (car hash-tables))
	 ;; Handle situation where a list of hash-tables is passed in as a
	 ;; single argument, rather than as multiple arguments.
	 (setq hash-tables (car hash-tables)))
    (if (memq nil (mapcar (function (lambda (ht) (or (null ht) (hashp ht))))
			  hash-tables))
	empty-ht
      (setq hash-tables
	    (delq nil (mapcar (function (lambda (ht)
					  (if (equal ht empty-ht)
					      nil ht)))
			      hash-tables)))
      (let ((len (length hash-tables)))
	(cond ((= len 0) empty-ht)
	      ((= len 1) (car hash-tables))
	      (t (let ((htable (hash-make
				(apply '+ (mapcar 'hash-count hash-tables))))
		       key value)
		   (mapcar
		     (function
		       (lambda (ht)
			 (hash-map (function
				     (lambda (val-key-cons)
				       (setq value (car val-key-cons)
					     key (cdr val-key-cons))
				       (if (not (hash-key-p key htable))
					   (hash-add value key htable)
					 ;; Merge values
					 (hash-add
					  (funcall hash-merge-values-function
						   (hash-get key htable)
						   value)
					  key htable))))
				   ht)))
		     hash-tables)
		   htable)))))))

(defun hash-merge-values (value1 value2)
  "Return a list from merging VALUE1 and VALUE2 or creating a new list.
Nil values are thrown away.  If both arguments are lists, their elements are
assumed to be strings and the result is a set of ordered strings."
  (cond ((and (listp value1) (listp value2))
	 ;; Assume desired result is a set of strings.
	 (br-set-of-strings (sort (append value1 value2) 'string<)))
	((null value1)
	 value2)
	((null value2)
	 value1)
	((listp value1)
	 (cons value2 value1))
	((listp value2)
	 (cons value1 value2))
	(t (list value1 value2))))

(make-obsolete 'hash-new 'hash-make)
(defun hash-new (size)
  "Return a new hash table of SIZE elements.
This is obsolete.  Use 'hash-make' instead."
  (hash-make size))

(defun hash-prin1 (hash-table &optional stream)
  "Output the printed representation of HASH-TABLE.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output'."
  (if (not (hashp hash-table))
      (progn (prin1 hash-table stream)
	     (princ "\n" stream))
    (princ "\(\n" stream)
    (hash-map
      (function (lambda (val-key-cons)
		  (prin1 val-key-cons stream)
		  (princ "\n" stream)))
      hash-table)
    (princ "\)\n" stream)))

(defun hash-replace (value key hash-table)
  "Replace VALUE referenced by KEY, a string, in HASH-TABLE.
An error will occur if KEY is not found in HASH-TABLE."
  (if (hashp hash-table)
       (let* ((obarray (hash-obarray hash-table))
	      (sym (intern-soft key obarray)))
	 (if (and (boundp sym) sym)
	     (set sym value)
	   (error "(hash-replace): '%s' key not found in hash table." key)))))

(defun hash-resize (hash-table new-size)
  "Resize HASH-TABLE to NEW-SIZE without losing any elements and return new table.
NEW-SIZE must be greater than 0.  Hashing works best if NEW-SIZE is a prime
number.  See also 'hash-next-prime'."
  (if (< new-size 1)
      (error "(hash-resize): Cannot resize hash table to size %d" new-size))
  (let ((htable (hash-make new-size)))
    (hash-map (function
		(lambda (elt)
		  (hash-add (car elt) (cdr elt) htable)))
	      hash-table)
    htable))

(defun hash-resize-p (hash-table)
  "Resizes HASH-TABLE to 1.5 times its size if above 80% full.
Returns new hash table when resized, else nil."
  (if (hashp hash-table)
      (let ((count (hash-count hash-table))
	    (size (length (hash-obarray hash-table))))
	(if (> (* count (/ count 5)) size)
	    (hash-resize hash-table (hash-next-prime (+ size (/ size 2))))))))

(defun hash-size (hash-table)
  "Return size of HASH-TABLE which is >= number of elements in the table.
Return nil if not a valid hash table."
  (if (hashp hash-table)
      (length (hash-obarray hash-table))))
(fset 'hash-length 'hash-size)

(defun hashp (object)
  "Return non-nil if OBJECT is a hash-table."
  (and (listp object) (eq (car object) 'hasht)
       (vectorp (cdr object))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hash-next-prime (n)
  "Return next prime number >= N."
  (if (<= n 2)
      2
    (and (= (% n 2) 0) (setq n (1+ n)))
    (while (not (hash-prime-p n))
      (setq n (+ n 2)))
    n))

(defun hash-obarray (hash-table)
  "Return symbol table (object array) portion of HASH-TABLE."
  (cdr hash-table))

(defun hash-prime-p (n)
  "Return non-nil iff N is prime."
  (if (< n 0) (setq n (- n)))
  (let ((small-primes '(1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67
			  71 73 79 83 89)))
    (cond ((< n 91) (memq n small-primes))
	  ((< n 7921)  ;; 89, max small-prime, squared
	   (let ((prime t)
		 (pr-list small-primes))
	     (while (and (setq pr-list (cdr pr-list))
			 (setq prime (/= (% n (car pr-list)) 0))))
	     prime))
	  ((or (= (% n 3) 0) (= (% n 2) 0)) nil)
	  ((let ((factor1 5)
		 (factor2 7)
		 (is-prime))
	     (while (and (<= (* factor1 factor1) n)
			 (setq is-prime (and (/= (% n factor1) 0)
					     (/= (% n factor2) 0))))
	       (setq factor1 (+ factor1 6)
		     factor2 (+ factor2 6)))
	     is-prime)))))

(provide 'hasht)
