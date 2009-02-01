;;!emacs
;;
;; FILE:         eif-ise-er.el
;; SUMMARY:      Parses ISE's Eiffel error messages; compiles Eiffel classes.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Dec-89 at 00:17:18
;; LAST-MOD:     17-Apr-95 at 12:39:18 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   'eif-ec' compiles an Eiffel class.
;;   'eif-es' compiles an Eiffel system.
;;
;;   Load this library and then invoke error parsing via {C-x `}.
;;   See the GNU Emacs Manual for an explanation of error parsing.
;;
;;   'eif-ise-next-error' bound to {C-x `} parses ISE Eiffel compiler
;;   error messages.  As in: 
;;
;;   "my_class", 16: syntax error : Keyword 'expanded' may not be used as identifier
;;
;;   Only handles compilation lines of the following form:
;;
;;      <compiler> [<option> ... <option>] <pathname>
;;
;;   Requires the 'br-class-path', 'br-build-sys-paths-htable', and
;;   'br-build-paths-htable' functions from the OO-Browser 'br-lib' package.
;;   This is used to determine the full pathname for the source code of each
;;   class since ISE does not include any pathname information in its error
;;   messages.
;;
;;
;;   To reset the {C-x `} key to parse non-Eiffel error messages, use:
;;
;;           {M-x load-lib RTN compile RTN}
;;
;; DESCRIP-END.

(require 'br-lib)
(require 'br-eif)
(require 'compile)

(global-set-key "\C-x`" 'eif-ise-next-error)
(and (boundp 'eiffel-mode-map) (define-key eiffel-mode-map "\C-c!" 'eif-ec))

(setq compilation-error-regexp "\"\\([^ \t]+\\)\", \\([0-9]+\\):.*")

(defconst eif-compile-dir nil
  "Default directory in which to invoke an Eiffel compile command.")

(defconst eif-compile-cmd "ec"
  "Default command name with which to invoke the Eiffel compiler.")

(defun eif-ise-next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  (interactive "P")
  (if (or (eq compilation-error-list t)
	  argp)
      (progn (compilation-forget-errors)
	     (setq compilation-parsing-end 1)))
  (if compilation-error-list
      nil
    (save-excursion
      (switch-to-buffer "*compilation*")
      (set-buffer-modified-p nil)
      (eif-ise-compilation-parse-errors)))
  (let ((next-error (car compilation-error-list)))
    (if (null next-error)
	(error (concat compilation-error-message
		       (if (and compilation-process
				(eq (process-status compilation-process)
				    'run))
			   " yet" ""))))
    (setq compilation-error-list (cdr compilation-error-list))
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      (set-marker (car (cdr next-error)) nil))
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))
    (set-marker (car next-error) nil)))

(defun eif-ise-compilation-filename ()
  "Return a string which is the last filename from the compilation command.
Ignore quotes around it.  Return nil if no filename was given."
  ;; First arg of compile cmd should be filename
  (if (string-match "^.*[ \t]+\\([^ \t\"]+\\)" compile-command)
      (substring compile-command (match-beginning 1) (match-end 1))))

(defun eif-ise-compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, compilation-error-list.  For each
error line-number in the buffer, the source file is read in, and the text
location is saved in compilation-error-list.  The function next-error,
assigned to \\[next-error], takes the next error off the list and visits its
location."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
	last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (let (class-name case-fold-search linenum filename error-marker text-marker)
      (while (re-search-forward compilation-error-regexp nil t)
	;; Extract line number from error message.
	(setq linenum (string-to-int (buffer-substring
				       (match-beginning 2)
				       (match-end 2))))
	;; Extract class name from error message and convert to the full
	;; pathname of the class' source file.
	(setq class-name (downcase (buffer-substring (match-beginning 1) (match-end 1)))
	      filename (br-class-path class-name))
	(if (null filename) ; No matching class name in lookup table.
	    (progn 
	      (message "Rebuilding Eiffel system class locations table...")
	      (sit-for 2)
	      (call-interactively 'br-build-sys-classes-htable) ; Typically pretty fast
	      (message "Rebuilding Eiffel system class locations table...Done")
	      (setq filename (br-class-path class-name))
	      (if (null filename)
		  (error (format "'%s' not in lookup table, use {M-x br-build-paths-htable RTN} to update."
				 class-name)))))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (beginning-of-line 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq text-buffer
		    (and (file-exists-p (setq last-filename filename))
			 (if (boundp 'br-find-file-noselect-function)
			     (set-buffer
			       (funcall br-find-file-noselect-function
					filename))
			   (find-file-noselect filename)))
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		(forward-line (- linenum last-linenum))
		(setq last-linenum linenum)
		(setq text-marker (point-marker))
		(setq compilation-error-list
		      (cons (list error-marker text-marker)
			    compilation-error-list)))))
	(forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))


;;; The following version of 'eif-ec' courtesy of:
;;; Heinz W. Schmidt                                     hws@icsi.berkeley.edu
;;; International Computer Science Institute             (415) 643-9153   x175
;;; 1947 Center Street, Ste. 600                    /\/\|;; CLOS saves time and
;;; Berkeley, CA 94704                              \/\/|-- Eiffel is faster
;;; 2/11/90
;;; With a number of Bob Weiner's modifications

(defun str2argv (STR)
  (if (string-match "[^ ]" STR)
      (let ((arg1 (read-from-string STR)))
        (cons (prin1-to-string (car arg1))
              (str2argv (substring STR (cdr arg1)))))))

(defvar eif-ec-args "" "Default arguments to send to the Eiffel ec class compiler.")

(defun eif-ec (ARG &optional CMD DIR CLASS-NAME)
  "Calls Eiffel compiler.  Compile with optional CMD, 'eif-compile-cmd' or \"ec\".
By default, the compiler is called on the file associated with the current
buffer.  With numeric argument 0 prompts for explicit command line arguments.
Other numeric arguments allow you to insert options or further class names."
  (interactive "P")
  (setq CLASS-NAME (or CLASS-NAME
		       (let ((fn (file-name-nondirectory buffer-file-name)))
			 (substring fn 0 (- (length fn) 2))))
	ec-dir (or DIR eif-compile-dir (file-name-directory buffer-file-name)))
  (let* ((ec-output (get-buffer-create "*compilation*"))
         (ec-process (get-buffer-process ec-output))
	 (curr-buffer (current-buffer)))
    (if ec-process
        (if (y-or-n-p "Kill current Eiffel compilation process? ")
            (delete-process ec-process)
          (error "Can't ec concurrently.")))
    (if (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " buffer-file-name)))
        (progn (save-buffer) (message "")))
    ;; Maybe prompt for args and dispatch according to numeric ARG.
    (setq eif-ec-args (if ARG (read-string "ec args: " eif-ec-args) ""))
    ;; Switch to shell buffer and run ec.
    (set-buffer ec-output)
    (erase-buffer)
    ;; Move to directory and trim classname so ec works in situations
    ;; like: ec -t class1 <CLASS-NAME>
    (cd ec-dir)
    (insert (or CMD eif-compile-cmd "ec")
	    (if ARG (format " %s" eif-ec-args) "")
	    (format " %s" (if (not (and ARG (zerop ARG))) CLASS-NAME ""))
            "\n")
    (set-buffer curr-buffer)
    (display-buffer ec-output)
    (eval   
     (append '(start-process "ec" ec-output (or CMD eif-compile-cmd "ec"))
             (str2argv eif-ec-args)
             (if (not (and ARG (zerop ARG))) (list CLASS-NAME)))))) 

(defun eif-es (&optional dir)
  "Compile Eiffel system with es."
  (interactive)
  (eif-ec nil "es" dir ""))


(provide 'eif-ise-er)
