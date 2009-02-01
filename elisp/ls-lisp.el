;;; ls-lisp.el --- emulate insert-directory completely in Emacs Lisp

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.
;; Copyright (C) 2000 Francis J. Wright

;; Author:		Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer:		FSF
;; Time-stamp:		<02 December 2000>
;; Modified by:		Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; URL:			http://centaur.maths.qmw.ac.uk/Emacs/
;; Keywords:		unix, dired

;; $Id: ls-lisp.el,v 1.22 2000-12-03 22:38:45+00 fjw Exp $

;; This file is (not yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; INSTALLATION ======================================================

;; Put this file into your load-path and byte-compile it.  (NB: Ignore
;; any compiler complaints about the number of arguments to
;; `ls-lisp-insert-directory' and `ls-lisp-format'.  They are due to
;; internal interface changes from the version distributed with Emacs
;; 20/21.)  To use it, load it with (load "ls-lisp").  If the standard
;; version of this library is compiled into the dumped image (as on
;; Microsoft platforms) and you want to load this as a revised version
;; then it must be forced to load (`require' does not work).  Use this
;; in your .emacs:

;; (add-hook 'dired-load-hook
;; 	     (lambda () (load-library "ls-lisp")))

;; OVERVIEW ==========================================================

;; This file overloads the function `insert-directory' to implement it
;; directly from Emacs lisp, without running ls in a subprocess.  It
;; is useful if you cannot afford to fork Emacs on a real memory UNIX,
;; under VMS, if you don't have the ls program, or if you want a
;; different format from what ls offers.

;; This function can use regexps instead of shell wildcards.  If you
;; enter regexps remember to double each $ sign.  For example, to
;; include files *.el, enter `.*\.el$$', resulting in the regexp
;; `.*\.el$'.

;; RESTRICTIONS ======================================================

;; * A few obscure ls switches are still ignored: see the docstring of
;; `insert-directory'.

;; * Generally only numeric uid/gid.

;; TO DO =============================================================

;; Complete handling of F switch (if/when possible).

;; FJW: May be able to sort much faster by consing the sort key onto
;; the front of each list element, sorting and then stripping the key
;; off again!

;;; History:

;; Written originally by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Revised by Andrew Innes and Geoff Volker (and maybe others).

;; Modified by Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>, mainly
;; to support many more ls options, "platform emulation", symbolic
;; links and more robust sorting.


;;; Code:

;; FJW: Temporary Emacs 20 ELisp emulation of an Emacs 21 built-in function.
;; Based on previous code in Emacs 20 `ls-lisp.el'.
(eval-and-compile
  (or
   (fboundp 'directory-files-and-attributes)
   (defun directory-files-and-attributes (directory &optional full match nosort)
     "Return a list of names of files and their attributes in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself."
     (mapcar
      (lambda (x)
	;; file-attributes("~bogus") bombs
	(cons x (file-attributes (expand-file-name x))))
      (directory-files directory full match nosort)))
   ))

;;;###autoload
(defgroup ls-lisp nil
  "Emulate the ls program completely in Emacs Lisp."
  :group 'dired)

(defcustom ls-lisp-emulation
  (cond ((memq system-type '(windows-nt ms-dos emx)) 'Microsoft)
	;; FJW: not sure about how to handle emx!
	((memq system-type
	       '(hpux dgux usg-unix-v unisoft-unix rtu irix berkeley-unix))
	 'UNIX))			; very similar to GNU
  ;; linux gnu/linux and anything else defaults to GNU.
  ;; What about vax-vms axp-vms -- does anyone still use VMS?
  "*Platform to emulate: GNU (default), Microsoft, UNIX.
Sets default values for: `ls-lisp-ignore-case', `ls-lisp-dirs-first',
`ls-lisp-verbosity'.  Need not match actual platform.  Changing this
variable will have no effect until you restart Emacs."
  :type '(choice (const :tag "GNU" nil)
		 (const Microsoft)
		 (const UNIX))
  :group 'ls-lisp)

(defcustom ls-lisp-ignore-case
  ;; Name change for consistency with other option names.
  (or (eq ls-lisp-emulation 'Microsoft)
      (and (boundp 'ls-lisp-dired-ignore-case) ls-lisp-dired-ignore-case))
  "*Non-nil causes ls-lisp alphabetic sorting to ignore case."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-dirs-first (eq ls-lisp-emulation 'Microsoft)
  "*Non-nil causes ls-lisp to sort directories first in any ordering.
\(Or last if it is reversed.)  Follows Microsoft Windows Explorer."
  ;; Functionality suggested by Chris McMahan <cmcmahan@one.net>
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-verbosity
  (cond ((eq ls-lisp-emulation 'Microsoft)
	 (if (getenv "SystemRoot") '(links))) ; distinguish NT/2K from 9x
	((eq ls-lisp-emulation 'UNIX) '(links uid)) ; UNIX ls
	(t '(links uid gid)))		; GNU ls
  "*A list of optional file attributes that ls-lisp should display.
It should contain none or more of the symbols: links, uid, gid.
Nil (or an empty list) means display none of them.
Defaults to `(links)' on Microsoft Windows NT/2K, nil on
MS-DOS/Windows 9x and `(links uid gid)' on other systems."
  ;; Functionality suggested by Howard Melman <howard@silverstream.com>
  :type '(set (const :tag "Link Count" links)
	      (const :tag "User" uid)
	      (const :tag "Group" gid))
  :group 'ls-lisp)

(defcustom ls-lisp-parse-symlinks
  (and (eq system-type 'windows-nt) '(shortcut))
  "*A list of symbolic link types that ls-lisp should parse.
It should contain none or more of the symbols: shortcut, cygwin.
They indicate Microsoft Windows shortcut (.lnk) and Cygwin-style symlinks.
Parsing Cygwin-style symlinks is slow, so turn it on only if you need it.
Should be nil on UNIX etc."
  :type '(set (const :tag "Microsoft shortcuts" shortcut)
	      (const :tag "Cygwin-style symlinks" cygwin))
  :group 'ls-lisp)

(defcustom ls-lisp-follow-symlinks
  (if (eq system-type 'windows-nt)
      '((	     ; Simple commands:
	 dired-advertised-find-file
	 dired-display-file
	 dired-do-shell-command
	 dired-diff
	 dired-do-byte-compile
	 dired-do-copy
	 dired-do-chgrp
	 dired-do-hardlink
	 dired-do-load
	 dired-do-chmod
	 dired-do-chown
	 dired-do-print
	 dired-do-symlink
	 dired-do-shell-command
	 dired-do-compress
	 dired-find-file
	 dired-find-file-other-window
	 dired-view-file
	 dired-mouse-find-file
	 dired-mouse-find-file-other-window
	 dired-do-symlink-regexp
	 dired-do-hardlink-regexp
	 dired-do-copy-regexp
	 dired-backup-diff
	 woman-dired-find-file
	 )
	.
	(	    ; Complex commands:
	 dired-do-search
	 dired-do-query-replace
	 )))
  "Cons of lists of `dired-mode' commands that need target of a symlink.
*** THIS VARIABLE SHOULD NOT BE CHANGED BY THE CASUAL USER! ***
The `car' consists of simple commands and the `cdr' of complex commands.
Complex commands are those that go into the variable `command-history'.
All other `dired-mode' commands receive the symlink itself, as per default.
Do not include w32-shellex commands.  Should be nil on UNIX etc."
  :type '(cons (repeat function) (repeat function))
  :group 'ls-lisp)

(defcustom ls-lisp-use-insert-directory-program nil
  "*Non-nil causes ls-lisp to revert back to using `insert-directory-program'.
This is useful on platforms where ls-lisp is dumped into Emacs, such as
Microsoft Windows, but you would still like to use a program to list
the contents of a directory."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-support-shell-wildcards t
  "*Non-nil means ls-lisp treats file patterns as shell wildcards.
Otherwise they are treated as Emacs regexps (for backward compatibility)."
  :type 'boolean
  :group 'ls-lisp)

;; Remember the original insert-directory function
(or (featurep 'ls-lisp)  ; FJW: unless this file is being reloaded!
    (fset 'original-insert-directory (symbol-function 'insert-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
If the value of `ls-lisp-use-insert-directory-program' is non-nil then
it works exactly like the version from `files.el' and runs a directory
listing program whose name is in the variable
`insert-directory-program'; if also WILDCARD is non-nil then it runs
the shell specified by `shell-file-name'.  If the value of
`ls-lisp-use-insert-directory-program' is nil then it runs a Lisp
emulation.

The Lisp emulation does not run any external programs or shells.  It
supports ordinary shell wildcards if `ls-lisp-support-shell-wildcards'
is non-nil; otherwise, it interprets wildcards as regular expressions
to match file names.  It does not support all `ls' switches -- those
that work are: A a c i r S s t u U X g G B C R and F partly."
  (if ls-lisp-use-insert-directory-program
      (original-insert-directory file switches wildcard full-directory-p)
    ;; We need the directory in order to find the right handler.
    (let ((handler (find-file-name-handler (expand-file-name file)
					   'insert-directory)))
      (if handler
	  (funcall handler 'insert-directory file switches
		   wildcard full-directory-p)
	;; Convert SWITCHES to a list of characters.
	(setq switches (delete ?- (append switches nil)))
	(if wildcard
	    (setq wildcard
		  (if ls-lisp-support-shell-wildcards
		      (wildcard-to-regexp (file-name-nondirectory file))
		    (file-name-nondirectory file))
		  file (file-name-directory file))
	  (if (memq ?B switches) (setq wildcard "[^~]\\'")))
	(ls-lisp-insert-directory
	 file switches (ls-lisp-time-index switches) wildcard full-directory-p)))))

(defun ls-lisp-insert-directory
  (file switches time-index wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
  ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
  ;; `ls' don't mind, we certainly do, because it makes us think
  ;; there is no wildcard, only a directory name.
  (if (and ls-lisp-support-shell-wildcards
	   (string-match "[[?*]" file))
      (progn
	(or (not (eq (aref file (1- (length file))) ?/))
	    (setq file (substring file 0 (1- (length file)))))
	(setq wildcard t)))
  (if (or wildcard full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir)	; so that file-attributes works
	     (file-alist
	      (directory-files-and-attributes dir nil wildcard t))
	     (now (current-time))
	     (sum 0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size fil attr)
	(cond ((memq ?A switches)
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\." file-alist))))
	(setq file-alist
	      (ls-lisp-handle-switches file-alist switches))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist)
	  (setq total-line (cons (point) (car-safe file-alist)))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 (setq sum (+ file-size
			      ;; Even if neither SUM nor file's size
			      ;; overflow, their sum could.
			      (if (or (< sum (- 134217727 file-size))
				      (floatp sum)
				      (floatp file-size))
				  sum
				(float sum))))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index now))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 (not (string-match "\\`\\.\\.?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file)))
      (if fattr
	  (insert (ls-lisp-format file fattr (nth 7 fattr)
				  switches time-index (current-time)))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))		; to show user the message!

(defun ls-lisp-column-format (file-alist)
  "Insert the file names (only) in FILE-ALIST into the current buffer.
Format in columns, sorted vertically, following GNU ls -C.
Responds to the window width as ls should but may not!"
  (let (files fmt ncols collen (nfiles 0) (colwid 0))
    ;; Count number of files as `nfiles', build list of filenames as
    ;; `files', and find maximum filename length as `colwid':
    (let (file len)
      (while file-alist
	(setq nfiles (1+ nfiles)
	      file (caar file-alist)
	      files (cons file files)
	      file-alist (cdr file-alist)
	      len (length file))
	(if (> len colwid) (setq colwid len))))
    (setq files (nreverse files)
	  colwid (+ 2 colwid)		; 2 character column gap
	  fmt (format "%%-%ds" colwid)	; print format
	  ncols (/ (window-width) colwid) ; no of columns
	  collen (/ nfiles ncols))	; floor of column length
    (if (> nfiles (* collen ncols)) (setq collen (1+ collen)))
    ;; Output the file names in columns, sorted vertically:
    (let ((i 0) j)
      (while (< i collen)
	(setq j i)
	(while (< j nfiles)
	  (insert (format fmt (nth j files)))
	  (setq j (+ j collen)))
	;; FJW: This is completely unnecessary, but I don't like
	;; trailing white space...
	(delete-region (point) (progn (skip-chars-backward " \t") (point)))
	(insert ?\n)
	(setq i (1+ i))))))

(defun ls-lisp-delete-matching (regexp list)
  "Delete all elements matching REGEXP from LIST, return new list."
  ;; Should perhaps use setcdr for efficiency.
  (let (result)
    (while list
      (or (string-match regexp (caar list))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    result))

(defsubst ls-lisp-string-lessp (s1 s2)
  "Return t if string S1 is less than string S2 in lexicographic order.
Case is significant if `ls-lisp-ignore-case' is nil.
Unibyte strings are converted to multibyte for comparison."
  (let ((u (compare-strings s1 0 nil s2 0 nil ls-lisp-ignore-case)))
    (and (numberp u) (< u 0))))

(defun ls-lisp-handle-switches (file-alist switches)
  "Return new FILE-ALIST sorted according to SWITCHES.
SWITCHES is a list of characters.  Default sorting is alphabetic."
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  (or (memq ?U switches)		; unsorted
      ;; Catch and ignore unexpected sorting errors
      (condition-case err
	  (setq file-alist
		(let (index)
		  ;; Copy file-alist in case of error
		  (sort (copy-sequence file-alist) ; modifies its argument!
			(cond ((memq ?S switches)
			       (lambda (x y) ; sorted on size
				 ;; 7th file attribute is file size
				 ;; Make largest file come first
				 (< (nth 7 (cdr y))
				    (nth 7 (cdr x)))))
			      ((setq index (ls-lisp-time-index switches))
			       (lambda (x y) ; sorted on time
				 (ls-lisp-time-lessp (nth index (cdr y))
						     (nth index (cdr x)))))
			      ((memq ?X switches)
			       (lambda (x y) ; sorted on extension
				 (ls-lisp-string-lessp
				  (ls-lisp-extension (car x))
				  (ls-lisp-extension (car y)))))
			      (t
			       (lambda (x y) ; sorted alphabetically
				 (ls-lisp-string-lessp (car x) (car y))))))))
	(error (message "Unsorted (ls-lisp sorting error) - %s"
			(error-message-string err))
	       (ding) (sit-for 2))))	; to show user the message!
  (if (memq ?F switches)		; classify switch
      (setq file-alist (mapcar 'ls-lisp-classify file-alist)))
  (if ls-lisp-dirs-first
  ;; Re-sort directories first, without otherwise changing the
  ;; ordering, and reverse whole list.  cadr of each element of
  ;; `file-alist' is t for directory, string (name linked to) for
  ;; symbolic link, or nil.
      (let (el dirs files)
	(while file-alist
	  (if (eq (cadr (setq el (car file-alist))) t) ; directory
	      (setq dirs (cons el dirs))
	    (setq files (cons el files)))
	  (setq file-alist (cdr file-alist)))
	(setq file-alist
	      (if (memq ?U switches)	; unsorted order is reversed
		  (nconc dirs files)
		(nconc files dirs)
		))))
  ;; Finally reverse file alist if necessary.
  ;; (eq below MUST compare `(not (memq ...))' to force comparison of
  ;; `t' or `nil', rather than list tails!)
  (if (eq (eq (not (memq ?U switches))	; unsorted order is reversed
	      (not (memq ?r switches)))	; reversed sort order requested
	  ls-lisp-dirs-first)		; already reversed
      (nreverse file-alist)
    file-alist))

(defun ls-lisp-parse-symlinks (file-name)
  "Optionally parse FILE-NAME as a MS Windows symlink file, if possible."
  (and
   ls-lisp-parse-symlinks
   (condition-case nil
       (or (and (memq 'shortcut ls-lisp-parse-symlinks)
		(string-match "\\.lnk\\'" file-name)
		(ls-lisp-parse-w32-lnk file-name))
	   (and (memq 'cygwin ls-lisp-parse-symlinks)
		(ls-lisp-parse-Cygwin-symlink file-name)))
     (error nil))))

(defun ls-lisp-classify (filedata)
  "Append a character to each file name indicating the file type.
Also, for regular files that are executable, append `*'.
The file type indicators are `/' for directories, `@' for symbolic
links, `|' for FIFOs, `=' for sockets, and nothing for regular files.
\[But FIFOs and sockets are not recognised.]
FILEDATA has the form (filename . `file-attributes').  Its `cadr' is t
for directory, string (name linked to) for symbolic link, or nil."
  (let ((dir (cadr filedata)) (file-name (car filedata)))
    (cond ((or dir
	       ;; Parsing .lnk files here is perhaps overkill!
	       (setq dir (ls-lisp-parse-symlinks file-name)))
	   (cons
	    (concat file-name (if (eq dir t) "/" "@"))
	    (cdr filedata)))
	  ((string-match "x" (nth 9 filedata))
	   (cons
	    (concat file-name "*")
	    (cdr filedata)))
	  (t filedata))))

(defun ls-lisp-extension (filename)
  "Return extension of FILENAME (ignoring any version extension)
FOLLOWED by null and full filename, SOLELY for full alpha sort."
  ;; Force extension sort order: `no ext' then `null ext' then `ext'
  ;; to agree with GNU ls.
  (concat
   (let* ((i (length filename)) end)
     (if (= (aref filename (1- i)) ?.) ; null extension
	 "\0"
       (while (and (>= (setq i (1- i)) 0)
		   (/= (aref filename i) ?.)))
       (if (< i 0) "\0\0"		; no extension
	 (if (/= (aref filename (1+ i)) ?~)
	     (substring filename (1+ i))
	   ;; version extension found -- ignore it
	   (setq end i)
	   (while (and (>= (setq i (1- i)) 0)
		       (/= (aref filename i) ?.)))
	   (if (< i 0) "\0\0"	; no extension
	     (substring filename (1+ i) end))))
       )) "\0" filename))

;; From Roland McGrath.  Can use this to sort on time.
(defun ls-lisp-time-lessp (time0 time1)
  "Return t if time TIME0 is earlier than time TIME1."
  (let ((hi0 (car time0)) (hi1 (car time1)))
    (or (< hi0 hi1)
	(and (= hi0 hi1)
	     (< (cadr time0) (cadr time1))))))

(defun ls-lisp-format (file-name file-attr file-size switches time-index now)
  "Format one line of long ls output for file FILE-NAME.
FILE-ATTR and FILE-SIZE give the file's attributes and size.
SWITCHES, TIME-INDEX and NOW give the full switch list and time data."
  (let ((file-type (or (nth 0 file-attr)
		       ;; t for directory, string (name linked to)
		       ;; for symbolic link, or nil.
		       ;; If no kernel support for symlinks then...
		       (ls-lisp-parse-symlinks file-name))))
    (concat (if (memq ?i switches)	; inode number
		(format " %6d" (nth 10 file-attr)))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format " %4.0f" (fceiling (/ file-size 1024.0))))
	    (if ls-lisp-parse-symlinks ; nil if platform is UNIX
		;; If no kernel support for symlinks then...
		(let ((perms (nth 8 file-attr))) ; permission bits
		  (if (stringp file-type) ; symbolic link
		      (aset perms 0 ?l))
		  perms)
	      (nth 8 file-attr))	; permission bits
	    (if (memq 'links ls-lisp-verbosity)
		(format " %3d" (nth 1 file-attr))) ; no. of links
	    ;; Numeric uid/gid are more confusing than helpful;
	    ;; Emacs should be able to make strings of them.
	    ;; user-login-name and user-full-name could take an
	    ;; optional arg.  (For now optionally hide them.)
	    (if (memq 'uid ls-lisp-verbosity)
		(if (= (user-uid) (nth 2 file-attr))
		    (format " %-8s" (user-login-name))
		  (format " %-8d" (nth 2 file-attr)))) ; uid
	    (if (not (memq ?G switches)) ; GNU ls -- no GID
		(if (or (memq ?g switches) ; UNIX ls -- show GID
			(memq 'gid ls-lisp-verbosity))
		    (if (memq system-type '(windows-nt ms-dos))
			"everyone"	; cf. GNU ls
		      (format " %-8d" (nth 3 file-attr))))) ; gid
	    (format (if (floatp file-size) " %8.0f" " %8d") file-size)
	    " "
	    (ls-lisp-format-time file-attr time-index now)
	    " "
	    file-name
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type))
	    "\n"
	    )))

(defun ls-lisp-time-index (switches)
  "Return time index into file-attributes according to ls SWITCHES list.
Return nil if no time switch found."
  ;; FJW: Default of nil is IMPORTANT and used in `ls-lisp-handle-switches'!
  (cond ((memq ?c switches) 6)		; last mode change
	((memq ?t switches) 5)		; last modtime
	((memq ?u switches) 4)))	; last access

(defun ls-lisp-format-time (file-attr time-index now)
  "Format time for file with attributes FILE-ATTR according to TIME-INDEX.
Use the same method as ls to decide whether to show time-of-day or year,
depending on distance between file date and NOW.
All ls time options, namely c, t and u, are handled."
  (let* ((time (nth (or time-index 5) file-attr)) ; default is last modtime
	 (diff16 (- (car time) (car now)))
	 (diff (+ (ash diff16 16) (- (car (cdr time)) (car (cdr now)))))
	 (past-cutoff (- (* 6 30 24 60 60)))	; 6 30-day months
	 (future-cutoff (* 60 60)))		; 1 hour
    (condition-case nil
	(format-time-string
	 (if (and
	      (<= past-cutoff diff) (<= diff future-cutoff)
	      ;; Sanity check in case `diff' computation overflowed.
	      (<= (1- (ash past-cutoff -16)) diff16)
	      (<= diff16 (1+ (ash future-cutoff -16))))
	     "%b %e %H:%M"
	   "%b %e  %Y")
	 time)
      (error "Unk  0  0000"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Experimental code to parse Microsoft Windows shortcut (.lnk) files,
;; parse Cygwin symbolic link files, and make dired follow symlinks
;; when appropriate.

;; Author: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>

;; REMARKS.  Parsing w32 symbolic links is relatively slow, because
;; each link file must be opened and read.  Cygwin symbolic link files
;; have the system property set, but currently it is not accessible
;; from ELisp.  Could try caching symlinks to speed up directory
;; re-reading.  Could re-use temp buffer for all symlinks in one
;; directory.

(defun ls-lisp-buffer-substring-as-int (start length)
  "Return contents of part of the current buffer as an unsigned integer.
START is a character position\; LENGTH specifies the length of the
integer in bytes and should be 1, 2 or 4.
Assumes byte order is low to high."
  (let ((idx (+ start length))
	(int 0))
    ;; Base (radix) using unsigned char digits is 2^8 = 256.
    (while (>= (setq idx (1- idx)) start)
      (setq int (+ (* 256 int) (char-after idx))))
    int))

(defun ls-lisp-parse-w32-lnk (file)
  "Return file or directory referenced by MS Windows shortcut (.lnk) FILE.
Return nil if the file cannot be parsed."
  ;;  Based on \"The Windows Shortcut File Format\" as
  ;;  reverse-engineered by Jesse Hager <jessehager@iname.com>
  ;;  available from http://www.wotsit.org/download.asp?f=shortcut.
  (with-temp-buffer
    (set-buffer-multibyte nil)		; need to force unibyte mode here!
    (insert-file-contents file)
    (and
     ;; Parse the File Header Table.
     (looking-at "L\0\0\0")		; otherwise not a shortcut file
     ;; Get the main flags dword at offset 14h.
     (let ((flags (ls-lisp-buffer-substring-as-int (+ (point) ?\x14) 4)))
       ;; Bit 1 set means shortcut to file or directory:
       (when (= (logand flags 2) 2)
	 ;; Skip to end of Header:
	 (forward-char ?\x4C)
	 ;; Skip Shell Item Id List.
	 ;; It is present if flags bit 0 is set, in which case the list
	 ;; length is the first word, which must also be skipped:
	 (if (= (logand flags 1) 1)
	     (forward-char
	      (+ 2 (ls-lisp-buffer-substring-as-int (point) 2))))
	 ;; Parse the File Location Info Table.
	 ;; The full file pathname is (generally) stored in two
	 ;; pieces: a head depending on whether the file is on a local
	 ;; or network volume and a remaining pathname tail.
	 ;; Get and check the volume flags dword at offset 8h:
	 (setq flags (ls-lisp-buffer-substring-as-int (+ (point) ?\x8) 4))
	 (if (/= (logand flags 3) 0)	; Must have bit 0 or 1 set.
	     (let ((head		; Get local or network
		    (save-excursion	; pathname head.
		      ;; If bit 0 then local else network:
		      (if (setq flags (= (logand flags 1) 1))
			  ;; Go to the base pathname on the local system at
			  ;; the offset specified as a dword at offset 10h:
			  (forward-char
			   (ls-lisp-buffer-substring-as-int (+ (point) ?\x10) 4))
			;; Go to the network volume table at the offset
			;; specified as a dword at offset 14h:
			(forward-char
			 (ls-lisp-buffer-substring-as-int (+ (point) ?\x14) 4))
			;; Go to the network share name at offset 14h:
			(forward-char ?\x14))
		      (buffer-substring (point) (1- (search-forward "\0")))))
		   (tail		; Get the remaining pathname tail
		    (progn		; specified as a dword at
		      (forward-char	; offset 18h.
		       (ls-lisp-buffer-substring-as-int (+ (point) ?\x18) 4))
		      (buffer-substring (point) (1- (search-forward "\0"))))))
	       (expand-file-name	; Convert \ to /, etc.
		(concat head
			;; Network share name needs trailing \ added:
			(unless (or flags (string= tail "")) "\\")
			tail))))
	 )))))

(defun ls-lisp-parse-Cygwin-symlink (file)
  "Return file or directory referenced by Cygwin symbolic link FILE.
Return nil if the file cannot be parsed.
Convert any leading drive specifier `//x/' to `x:'."
  (with-temp-buffer
    (insert-file-contents file nil 0 127) ; read first 128 bytes only
    (when (looking-at "!<symlink>\\(.+\\)\0")
      (setq file (match-string-no-properties 1))
      (cond ((string-match "\\`//./" file)  ; //x/ -> x:/
	     (setq file (substring file 1)) ; //x/ -> /x/
	     (aset file 0 (aref file 1))    ;  /x/ -> xx/
	     (aset file 1 ?:)))		    ;  xx/ -> x:/
      file)))

(defadvice dired-get-filename (around dired-get-filename-advice)
  "On non-UNIX platforms, return source or target of symlink as appropriate.
Return target for dired commands in `ls-lisp-follow-symlinks'.
On MS Windows automatically return shortcut directly if called by shellex."
  ;; `dired-get-filename' always returns the symlink itself
  ;; but most Windows commands cannot follow symlinks!
  (if (and (save-excursion		; symbolic link?
	     (beginning-of-line)
	     (looking-at "\\(\\(.+\\.lnk\\)\\|.+\\) -> \\(.+\\)"))
	   ;; Symbolic link:
	   (or
	    ;; Always apply these simple commands to the target file:
	    (memq this-command (car ls-lisp-follow-symlinks))
	    ;; Always apply these complex commands to the target file:
	    (memq (caar command-history) (cdr ls-lisp-follow-symlinks))
	    (and
	     ;; But shellex handles MS Windows shortcuts directly:
	     (eq this-command 'w32-shellex-dired-on-objects)
	     (not (match-beginning 2)))
	    ))
      (setq ad-return-value
	    (match-string-no-properties 3))
    ;; Not symbolic link:
    ad-do-it))

;; Activate the above advice on platforms that do not have native
;; support for symbolic links.  This currently includes w32, but what
;; else?
(if (eq system-type 'windows-nt)
    (ad-activate 'dired-get-filename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Experimental code to make symlinks on non-UNIX platforms.

;; Author: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>

;; REMARKS.  The following function does not exist in NTEmacs.  It is
;; implemented in the kernel on other platforms which provide suitable
;; system calls.  Instead, I use either an external `ln' program or,
;; if that fails, an ELisp emulation.

(eval-when-compile
  (require 'dired-aux))

(eval-and-compile
  (or
   (fboundp 'make-symbolic-link)
   (defun make-symbolic-link (file newname &optional ok-if-already-exists)
     "Give FILE symbolic link NEWNAME.  Both args strings.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Use external ln program if available, otherwise emulate Cygwin-style symlink
that will not have system attribute set and can only be parsed by ls-lisp."
     ;; Modelled on `add-name-to-file'
     (interactive "fMake symbolic link to file: \nFName for link to %s: \np")
     (if (or (not (file-exists-p newname))
	     (if (numberp ok-if-already-exists)
		 (yes-or-no-p
		  (format
		   "File %s already exists; make it a symlink anyway? "
		   newname))
	       ok-if-already-exists)
	     (signal 'file-already-exists
		     (list "File already exists" newname)))
	 (condition-case nil
	     ;; ln -s FILE NEWNAME
	     ;; (call-process "ln" nil nil nil "-s" file newname)
	     (dired-check-process
	      "Making symlink" "ln" "-s" "-f" file newname)
	   (error
	    ;; Assume no `ln' program available.
	    ;; This works, but cannot set system attribute:
	    (with-temp-file newname
	      (insert "!<symlink>" file "\0"))))))
   ))

(provide 'ls-lisp)

;;; ls-lisp.el ends here
