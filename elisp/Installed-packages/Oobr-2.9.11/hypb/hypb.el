;;!emacs
;;
;; FILE:         hypb.el
;; SUMMARY:      Miscellaneous Hyperbole support features.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:     6-Oct-91 at 03:42:38
;; LAST-MOD:     30-Oct-95 at 21:23:19 by Bob Weiner
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

(mapcar 'require '(hversion hact))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hypb:help-buf-suffix " Hypb Help*"
  "Suffix attached to all native Hyperbole help buffer names.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hypb:call-process-p (program infile &optional predicate &rest args)
  "Calls an external PROGRAM with INFILE for input.
If PREDICATE is given, it is evaluated in a buffer with the PROGRAM's
output and the result returned.  If PREDICATE is nil, returns t iff
program has no output or just a 0-valued output.
Rest of ARGS are passed as arguments to PROGRAM."
  (let ((buf (get-buffer-create "*test-output*"))
	(found))
    (save-excursion
      (set-buffer buf) (setq buffer-read-only nil) (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq found 
	    (if predicate
		(eval predicate)
	      (or (= (point-max) 1) ;; No output, consider cmd a success.
		  (and (< (point-max) 4)
		       (string= (buffer-substring 1 2) "0")))))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    found))


(defun hypb:chmod (op octal-permissions file)
  "Uses OP and OCTAL-PERMISSIONS integer to set FILE permissions.
OP may be +, -, xor, or default =."
  (let ((func (cond ((eq op '+)   (function logior))
		    ((eq op '-)   (function
				   (lambda (p1 p2) (logand (lognot p1) p2))))
		    ((eq op 'xor) (function logxor))
		    (t            (function (lambda (p1 p2) p1))))))
    (set-file-modes file (funcall func (hypb:oct-to-int octal-permissions)
				  (file-modes file)))))

(defun hypb:cmd-key-string (cmd-sym &optional keymap)
  "Returns a single pretty printed key sequence string bound to CMD-SYM.
Global keymap is used unless optional KEYMAP is given."
  (if (and cmd-sym (symbolp cmd-sym) (fboundp cmd-sym))
  (let* ((get-keys (function
		    (lambda (cmd-sym keymap)
		      (key-description (where-is-internal
					cmd-sym keymap 'first)))))
	 (keys (funcall get-keys cmd-sym keymap)))
    (concat "{"
	    (if (string= keys "")
		(concat (funcall get-keys 'execute-extended-command nil)
			" " (symbol-name cmd-sym) " RTN")
	      keys)
	    "}"))
  (error "(hypb:cmd-key-string): Invalid cmd-sym arg: %s." cmd-sym)))

;;;###autoload
(defun hypb:configuration (&optional out-buf)
  "Insert Emacs configuration information at the end of optional OUT-BUF or the current buffer."
  (save-excursion
    (and out-buf (set-buffer out-buf))
    (goto-char (point-max))
    (delete-blank-lines) (delete-blank-lines)
    (let ((start (point)))
      (insert (format "I use:\tEditor:      %s\n\tHyperbole:   %s\n"
                      (if (boundp 'epoch::version)
                          epoch::version
                        (hypb:replace-match-string
			 " of .+" (emacs-version) "" t))
                      hyperb:version))
      (if (and (boundp 'system-configuration) (stringp system-configuration))
	  (insert (format "\tSys Type:    %s\n" system-configuration)))
      (insert (format "\tOS Type:     %s\n\tWindow Sys:  %s\n"
                      system-type (or window-system hyperb:window-system
				      "None")))
      (if (and (boundp 'hmail:reader) hmail:reader)
          (insert (format "\tMailer:      %s\n"
                          (cond ((eq hmail:reader 'rmail-mode) "RMAIL")
                                ((eq hmail:reader 'vm-mode)
                                 (concat "VM " vm-version))
                                ((and (eq hmail:reader 'mh-show-mode)
                                      (string-match "v ?\\([0-9]+.[0-9]+\\)"
                                          mh-e-RCS-id))
                                 (concat "MH-e "
                                         (substring mh-e-RCS-id
                                                    (match-beginning 1)
                                                    (match-end 1))))
                                ((eq hmail:reader 'pm-fdr-mode)
                                 (concat "PIEmail " pm-version))
                                ))))
      (if (and (boundp 'hnews:reader) (boundp 'gnus-version) hnews:reader)
          (insert (format "\tNews Rdr:    %s\n" gnus-version)))
      (if (and (boundp 'br-version) (stringp br-version))
	  (insert (format "\tOO-Browser:  %s\n" br-version)))
      (untabify start (point)))))

(if (fboundp 'copy-tree)
    (fset 'hypb:copy-sublists 'copy-tree)
  ;;
  ;; This function is derived from a copylefted function.
  ;; Define hypb:copy-sublists if not a builtin.  This version 
  ;; is a Lisp translation of the C version in Lemacs 19.8.
  ;; Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.
  ;; Available for use and distribution under the GPL.
  ;;
  (defun hypb:copy-sublists (obj &optional vector-p)
    "Return a copy of a list and substructures.
The argument is copied, and any lists contained within it are copied
recursively.  Circularities and shared substructures are not preserved.
Second arg VECP causes vectors to be copied, too.  Strings are not copied."
    (cond ((consp obj)
	   (let (rest)
	     (setq obj (copy-sequence obj)
		   rest obj)
	     (while (consp rest)
	       (let ((elt (car rest)))
		 (if quit-flag (top-level))
		 (if (or (consp elt) (vectorp elt))
		     (setcar rest (hypb:copy-sublists elt vector-p)))
		 (if (vectorp (cdr rest))
		     (setcdr rest (hypb:copy-sublists (cdr rest) vector-p)))
		 (setq rest (cdr rest))))))
	  ((and (vectorp obj) obj)
	   (let ((i (length obj))
		 (j 0)
		 elt)
	     (setq obj (copy-sequence obj))
	     (while (< j i)
	       (setq elt (aref obj j))
	       (if quit-flag (top-level))
	       (if (or (consp elt) (vectorp elt))
		   (aset obj j (hypb:copy-sublists elt vector-p)))
	       (setq j (1+ j))))))
    obj))

(defun hypb:debug ()
  "Loads Hyperbole hbut.el source file and sets debugging traceback flag."
  (interactive)
  (or (featurep 'hinit) (load "hsite"))
  (or (and (featurep 'hbut)
	   (let ((func (hypb:indirect-function 'ebut:create)))
	     (not (or (hypb:v19-byte-code-p func)
		      (eq 'byte-code
			  (car (car (nthcdr 3 (hypb:indirect-function
					       'ebut:create)))))))))
      (load "hbut.el"))
  (setq debug-on-error t))

(defun hypb:domain-name ()
  "Returns current Internet domain name with '@' prepended or nil if none."
  (let* ((dname-cmd (or (file-exists-p "/usr/bin/domainname")
			(file-exists-p "/bin/domainname")))
	 (dname (or (getenv "DOMAINNAME")
		    (if dname-cmd
			(hypb:call-process-p
			 "domainname" nil 
			 '(substring (buffer-string) 0 -1))))))
    (if (or (and dname (string-match "\\." dname))
	    (let* ((src "/etc/resolv.conf")
		   (src-buf-exists-p (get-file-buffer src)))
	      (and (file-exists-p src) (file-readable-p src)
		   (save-excursion
		     (set-buffer (find-file-noselect src))
		     (goto-char (point-min))
		     (if (re-search-forward  "^domain[ \t]+\\([^ \t\n]+\\)"
					     nil t)
			 (setq dname (buffer-substring (match-beginning 1)
						       (match-end 1))))
		     (or src-buf-exists-p (kill-buffer nil))
		     dname))))
	(concat "@" dname))))

(defun hypb:error (&rest args)
  "Signals an error typically to be caught by 'hui:menu'."
  (let ((msg (apply 'format args)))
    (put 'error 'error-message msg)
    (error msg)))

(defun hypb:functionp (obj)
"Returns t if OBJ is a function, nil otherwise."
  (cond
    ((symbolp obj) (fboundp obj))
    ((subrp obj))
    ((hypb:v19-byte-code-p obj))
    ((consp obj)
     (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
    (t nil)))

(defun hypb:function-copy (func-symbol)
  "Copies FUNC-SYMBOL's body for overloading.  Returns copy of body."
  (if (fboundp func-symbol)
      (let ((func (hypb:indirect-function func-symbol)))
	(cond ((listp func) (copy-sequence func))
	      ((subrp func) (error "(hypb:function-copy): `%s' is a primitive; can't copy body."
				   func-symbol))
	      ((and (hypb:v19-byte-code-p func) (fboundp 'make-byte-code))
	       (let ((new-code (append func nil))) ; turn it into a list
		 (apply 'make-byte-code new-code)))
	      (t (error "(hypb:function-copy): Can't copy function body: %s" func))
	      ))
    (error "(hypb:function-copy): `%s' symbol is not bound to a function."
	   func-symbol)))

(defun hypb:function-overload (func-sym prepend &rest new-forms)
  "Redefine function named FUNC-SYM by either PREPENDing (or appending if nil) rest of quoted NEW-FORMS."
  (let ((old-func-sym (intern
			(concat "*hypb-old-"
				(symbol-name func-sym)
				"*"))))
    (or (fboundp old-func-sym)
	(fset old-func-sym (hypb:function-copy func-sym)))
    (let* ((old-func (hypb:indirect-function old-func-sym))
	   (old-param-list (action:params old-func))
	   (param-list (action:param-list old-func))
	   (old-func-call
	     (list (if (memq '&rest old-param-list)
		       ;; Have to account for extra list wrapper from &rest.
		       (cons 'apply
			     (cons (list 'quote old-func-sym) param-list))
		     (cons old-func-sym param-list)))))
      (eval (append
	      (list 'defun func-sym old-param-list)
	      (delq nil
		    (list
		      (documentation old-func-sym)
		      (action:commandp old-func-sym)))
	      (if prepend
		  (append new-forms old-func-call)
		(append old-func-call new-forms)))))))

(defun hypb:function-symbol-replace (func-sym sym-to-replace replace-with-sym)
  "Replaces in body of FUNC-SYM SYM-TO-REPLACE with REPLACE-WITH-SYM.
All occurrences within lists are replaced.  Returns body of modified FUNC-SYM."
  (let ((body (hypb:indirect-function func-sym))
	(arg-vector) (arg))
    (if (listp body)
	;; assume V18 byte compiler
	(setq arg-vector
	      (car (delq nil (mapcar
			       (function
				 (lambda (elt)
				   (and (listp elt)
					(vectorp (setq arg-vector (nth 2 elt)))
					arg-vector)))
			       body))))
      ;; assume V19 byte compiler   (eq (compiled-function-p body) t)
      (setq arg (aref body 2)
	    arg-vector (if (vectorp arg) arg))
      )
    (if arg-vector
	;; Code is byte-compiled.
	(let ((i (1- (length arg-vector))))
	  (setq arg nil)
	  (while (and (not arg) (>= i 0))
	    (if (eq (setq arg (aref arg-vector i)) sym-to-replace)
		(aset arg-vector i replace-with-sym)
	      (setq arg nil i (1- i)))))
      ;; Code is not byte-compiled.
      ;; Only replaces occurrence of symbol as an element of a list.
      (hypb:map-sublists
	(function
	  (lambda (atom list) (if (eq atom sym-to-replace)
				  (let ((again t))
				    (while (and again list)
				      (if (eq (car list) atom)
					  (progn (setcar list replace-with-sym)
						 (setq again nil))
					(setq list (cdr list))))))))
	body)
      )
    body))

(defun hypb:help-buf-name (&optional prefix)
  "Returns a Hyperbole help buffer name for current buffer.
With optional PREFIX string, uses it rather than buffer name."
  (let ((bn (or prefix (buffer-name))))
    (if (string-match " Hypb " bn)
	(buffer-name (generate-new-buffer bn))
      (concat "*" bn hypb:help-buf-suffix))))

(defun hypb:indirect-function (obj)
  "Return the function at the end of OBJ's function chain.
Resolves autoloadable function symbols properly."
  (let ((func
	 (if (fboundp 'indirect-function)
	     (indirect-function obj)
	   (while (symbolp obj)
	     (setq obj (symbol-function obj)))
	   obj)))
    ;; Handle functions with autoload bodies.
    (if (and (symbolp obj) (listp func) (eq (car func) 'autoload))
	(let ((load-file (car (cdr func))))
	  (load load-file)
	  ;; Prevent infinite recursion
	  (if (equal func (symbol-function obj))
	      (error "(hypb:indirect-function): Autoload of '%s' failed" obj)
	    (hypb:indirect-function obj)))
      func)))

(defun hypb:insert-region (buffer start end invisible-flag)
  "Insert into BUFFER the contents of a region from START to END in the current buffer.
INVISIBLE-FLAG, if non-nil, means invisible text in an outline region is
copied, otherwise, it is omitted."
  (let ((from-koutline (eq major-mode 'kotl-mode)))
  (append-to-buffer buffer start end)
  (save-excursion
    (set-buffer buffer)
    (let ((first (- (point) (- end start)))
	  (last (point)))
      ;; Remove from buffer any copied text that was hidden if invisible-flag
      ;; is nil.
      (if invisible-flag
	  ;; Show all hidden text within the copy.
	  (subst-char-in-region first last ?\r ?\n t)
	;; Remove hidden text.
	(goto-char first)
	(while (search-forward "\r" last t)
	  (delete-region (1- (point)) (progn (end-of-line) (point)))))
      ;;
      ;; If region came from a koutline, remove any characters with an
      ;; invisible property which separate cells.
      (if from-koutline
	  (kproperty:map
	   (function (lambda (prop) (delete-char 1))) 'invisible t))))))
	
(if (or hyperb:lemacs-p hyperb:emacs19-p)
    (fset 'hypb:mark 'mark)
  (defun hypb:mark (inactive-p)
    "Return this buffer's mark value as integer, or nil if no mark.
INACTIVE-P non-nil means return value of mark even if region is not active
under Emacs version 19.
If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
    (mark))
  )
(if hyperb:lemacs-p
    (fset 'hypb:mark-marker 'mark-marker)
  (defun hypb:mark-marker (inactive-p)
    "Return this buffer's mark as a marker object, or nil if no mark.
INACTIVE-P is unused, it is for compatibility with Lucid Emacs'
version of mark-marker."
    (mark-marker))
  )

(defun hypb:map-sublists (func list)
  "Applies FUNC to every atom found at any level of LIST.
FUNC must take two arguments, an atom and a list in which the atom is found.
Returns values from applications of FUNC as a list with the same
structure as LIST.  FUNC is therefore normally used just for its side-effects."
  (mapcar (function
	    (lambda (elt) (if (atom elt)
			      (funcall func elt list)
			    (hypb:map-sublists func elt)))
	    list)))

(defun hypb:map-vector (func object)
  "Returns list of results of application of FUNC to each element of OBJECT.
OBJECT should be a vector or byte-code object."
  (if (not (or (vectorp object) (hypb:v19-byte-code-p object)))
      (error "(hypb:map-vector): Second argument must be a vector or byte-code object."))
  (let ((end (length object))
	(i 0)
	(result))
    (while (< i end)
      (setq result (cons (funcall func (aref object i)) result)
	    i (1+ i)))
    (nreverse result)))

(defun hypb:mouse-help-file ()
  "Return the full path to the Hyperbole mouse key help file."
  (let* ((hypb-man (expand-file-name "man/" hyperb:dir))
	 (help-file (expand-file-name "hypb-mouse.txt" hypb-man)))
    (if (or (file-exists-p help-file)
	    (file-exists-p
	     (setq help-file (expand-file-name
			      "hypb-mouse.txt" data-directory))))
	help-file
      (error "(hypb:mouse-help-file): Non-existent file: \"%s\"" help-file))))

(if (or hyperb:lemacs-p hyperb:emacs19-p)
    (fset 'hypb:push-mark 'push-mark)
  (defun hypb:push-mark (&optional location nomsg activate-region)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.
Optional third arg ACTIVATE-REGION is ignored.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
    (push-mark location nomsg))
  )

(defun hypb:replace-match-string (regexp str newtext &optional literal)
  "Replaces all matches for REGEXP in STR with NEWTEXT string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\.
NEWTEXT may instead be a function of one argument, the string to replace in,
that returns a replacement string."
  (if (not (stringp str))
      (error "(hypb:replace-match-string): 2nd arg must be a string: %s" str))
  (if (or (stringp newtext) (hypb:functionp newtext))
      nil
    (error "(hypb:replace-match-string): 3rd arg must be a string or function: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond ((hypb:functionp newtext) (funcall newtext str))
		    (literal newtext)
		    (t (mapconcat
			 (function
			   (lambda (c)
			     (if special
				 (progn
				   (setq special nil)
				   (cond ((eq c ?\\) "\\")
					 ((eq c ?&)
					  (substring str
						     (match-beginning 0)
						     (match-end 0)))
					 ((and (>= c ?0) (<= c ?9))
					  (if (> c (+ ?0 (length
							   (match-data))))
					      ;; Invalid match num
					      (error "(hypb:replace-match-string) Invalid match num: %c" c)
					    (setq c (- c ?0))
					    (substring str
						       (match-beginning c)
						       (match-end c))))
					 (t (char-to-string c))))
			       (if (eq c ?\\) (progn (setq special t) nil)
				 (char-to-string c)))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))

(defun hypb:supercite-p ()
  "Returns non-nil iff the Emacs add-on supercite package is in use."
  (let (hook-val)
    (if (memq t (mapcar
		 (function
		  (lambda (hook-var)
		    (and (boundp hook-var)
			 (progn (setq hook-val (symbol-value hook-var))
				(cond ((listp hook-val)
				       (if (memq 'sc-cite-original hook-val)
					   t))
				      ((eq hook-val 'sc-cite-original)))))))
		 '(mail-citation-hook mail-yank-hooks)))
	t)))

;;; Next function is copied from a copylefted function:
;;; Copyright (C) 1987, 1988 Kyle E. Jones
(if (or hyperb:lemacs-p hyperb:emacs19-p)
    (defun hypb:window-list-all-frames (&optional mini)
      "Returns a list of Lisp window objects for all Emacs windows in all frames.
Optional first arg MINI t means include the minibuffer window
in the list, even if it is not active.  If MINI is neither t
nor nil it means to not count the minibuffer window even if it is active."
      (let* ((first-window (next-window
			    (previous-window (selected-window) nil t t)
			    mini t t))
	     (windows (cons first-window nil))
	     (current-cons windows)
	     (w (next-window first-window mini t t)))
	(while (not (eq w first-window))
	  (setq current-cons (setcdr current-cons (cons w nil)))
	  (setq w (next-window w mini t t)))
	windows)))

;;; Next function is copied from a copylefted function:
;;; Copyright (C) 1987, 1988 Kyle E. Jones
(defun hypb:window-list (&optional mini)
  "Returns a list of Lisp window objects for all Emacs windows in selected frame.
Optional first arg MINI t means include the minibuffer window
in the list, even if it is not active.  If MINI is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (let* ((first-window (next-window
			(previous-window (selected-window)) mini))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window mini)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w mini)))
    windows))

(defun hypb:v19-byte-code-p (obj)
  "Return non-nil iff OBJ is an Emacs V19 byte compiled object."
  (or (and (fboundp 'compiled-function-p) (compiled-function-p obj))
      (and (fboundp 'byte-code-function-p) (byte-code-function-p obj))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hypb:oct-to-int (oct-num)
  "Returns octal integer OCTAL-NUM converted to a decimal integer."
  (let ((oct-str (int-to-string oct-num))
	(dec-num 0))
    (and (string-match "[^0-7]" oct-str)
	 (error (format "(hypb:oct-to-int): Bad octal number: %s" oct-str)))
    (mapconcat (function
		(lambda (o)
		  (setq dec-num (+ (* dec-num 8)
				   (if (and (>= o ?0) (<= o ?7))
				       (- o ?0))))))
	       oct-str "")
    dec-num))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hypb)
