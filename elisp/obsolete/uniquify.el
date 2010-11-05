;;; uniquify.el --- unique buffer names dependent on pathname

;; Copyright (c) 1989, 1995 Free Software Foundation, Inc.

;; Author: Dick King <king@kestrel>
;; Maintainer: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Created: 15 May 86
;; Time-stamp: <95/06/13 07:22:22 mernst>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 1 as
;; published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Emacs's standard method for making buffer names unique involves adding
;; <2>, <3>, etc to the end of (all but one of) the buffers.  This file
;; replaces that behavior, for buffers visiting files and dired buffers,
;; with a uniquification that adds parts of the pathname until the buffer
;; names are unique.  For instance, buffers visiting /u/mernst/tmp/Makefile
;; and /usr/projects/zaphod/Makefile would be named Makefile|tmp/ and
;; Makefile|zaphod/, respectively (instead of Makefile and Makefile<2>).

;; To use this file, just load it.
;; To disable it after loading, set variable mnemonic-buffer-names to nil.
;; For other options, see "User-visible-variables", below.

;; Doesn't correctly handle buffer names created by M-x write-file in Emacs 18.

;;; Change Log:

;; Originally by Dick King <king@reasoning.com> 15 May 86
;; Converted for Emacs 18 by Stephen Gildea <gildea@lcs.mit.edu>
;; Make uniquify-min-dir-content 0 truly non-invasive. gildea 23 May 89
;; Some cleanup.  uniquify-min-dir-content default 0.  gildea 01 Jun 89
;; Don't rename to "".  Michael Ernst <mernst@microsoft.com> 15 Jun 94
;; Add kill-buffer-hook.  Kenneth Manheimer <ken.manheimer@nist.gov> 09 May 95
;; Add advice for rename-buffer and create-file-buffer, handle dired buffers,
;;  kill-buffer-rationalize-buffer-names-p, documentation.  mernst 24 May 95
;; Remove free variables, fix typos.  mernst 5 Jun 95
;; Efficiently support Emacs 19.27 & earlier.  ken.manheimer, mernst 10 Jun 95
;; Rename user options to "uniquify-...", add uniquify-reverse-dir-content-p,
;;  add uniquify-ask-about-buffer-names-p.  king, mernst 13 Jun 95

;;; Code:

(provide 'uniquify)

;;; User-visible variables

(defvar mnemonic-buffer-names t
  "*If non-nil, uniquifies buffer names with parts of directory name.")

(defvar uniquify-after-kill-buffer-p nil
  "*If non-nil, rerationalize buffer names after a buffer has been killed.
This can be dangerous if Emacs Lisp code is keeping track of buffers by their
names (rather than keeping pointers to the buffers themselves).")

;; Thanks to gyro@reasoning.com for this suggestion.
(defvar uniquify-reverse-dir-content-p nil
  "*If non-nil, put directory components in buffer name in reverse order.
For instance, files /foo/bar/mumble/baz and /foo/goo/mumble/baz would be
in buffers baz\mumble\bar and baz\mumble\goo
instead of baz|bar/mumble/ and baz|goo/mumble/.")

(defconst uniquify-ask-about-buffer-names-p nil
  "*If non-nil, permit user to choose names for buffers with same base file.
If the user chooses to name a buffer, uniquification is preempted and no
other buffer names are changed.")

(defvar uniquify-separator "|"
  "*String separating base file name from directory part in buffer names.
Ignored if `uniquify-reverse-dir-content-p is non-nil.")

(defvar uniquify-min-dir-content 0
  "*Minimum parts of directory pathname included in buffer name.")

;;; Utilities

(defmacro uniquify-push (item list)
  (` (setq (, list) (cons (, item) (, list)))))

(defmacro fix-list-base (a)
  (` (car (, a))))

(defmacro fix-list-filename (a)
  (` (car (cdr (, a)))))

(defmacro fix-list-buffer (a)
  (` (car (cdr (cdr (, a))))))

(defmacro uniquify-cadddr (a)
  (` (car (cdr (cdr (cdr (, a)))))))

;; Internal variables used free
(defvar uniquify-non-file-buffer-names nil)
(defvar uniquify-possibly-resolvable nil)

;;; Main entry point.

(defun rationalize-file-buffer-names (&optional newbuffile newbuf)
  "Makes file buffer names unique by adding segments from pathname.
If `uniquify-min-dir-content' > 0, always pulls that many
pathname elements.  Arguments cause only a subset of buffers to be renamed."
  (interactive)
  (let (fix-list
	uniquify-non-file-buffer-names
	(depth uniquify-min-dir-content))
    (let ((buffers (buffer-list)))
      (while buffers
	(let* ((buffer (car buffers))
	       (bfn (if (eq buffer newbuf)
			(expand-file-name newbuffile)
		      (buffer-file-name-for-uniquify buffer)))
	       (rawname (and bfn (file-name-nondirectory bfn)))
	       (deserving (and rawname
			       (or (not newbuffile)
				   (equal rawname
					  (file-name-nondirectory newbuffile))))))
	  (if deserving
	      (uniquify-push (list rawname bfn buffer nil) fix-list)
	    (uniquify-push (list (buffer-name buffer))
			   uniquify-non-file-buffer-names)))
	(setq buffers (cdr buffers))))
    ;; selects buffers whose names may need changing, and others that
    ;; may conflict.
    (setq fix-list
	  (sort fix-list 'backward-filename-string-lessp-fix-list-filename))
    ;; bringing conflicting names together
    (rationalize-a-list fix-list depth)
    (mapcar 'do-the-buffers-you-couldnt-rationalize fix-list)))

(defun buffer-file-name-for-uniquify (buffer)
  "Return name of file BUFFER is visiting, or nil if none.
Works on dired buffers as well as ordinary file-visiting buffers."
  (or (buffer-file-name buffer)
      ;; This should perhaps use list-buffers-directory instead.
      (and (boundp 'dired-directory)	; equivalently, (featurep 'dired)
	   (save-excursion
	     (set-buffer buffer)
	     (and dired-directory
		  (expand-file-name
		   (directory-file-name
		    (if (consp dired-directory)
			(car dired-directory)
		      dired-directory))))))))

(defun backward-filename-string-lessp-fix-list-filename (s1 s2)
  (backward-filename-string-lessp
   (fix-list-filename s1) (fix-list-filename s2)))

(defun backward-filename-string-lessp (s1 s2)
  (let ((s1f (file-name-nondirectory s1))
	(s2f (file-name-nondirectory s2)))
    (and (not (equal s2f ""))
	 (or (string-lessp s1f s2f)
	     (and (equal s1f s2f)
		  (let ((s1d (file-name-directory s1))
			(s2d (file-name-directory s2)))
		    (and (not (<= (length s2d) 1))
			 (or (<= (length s1d) 1)
			     (backward-filename-string-lessp
			      (substring s1d 0 -1)
			      (substring s2d 0 -1))))))))))

(defun do-the-buffers-you-couldnt-rationalize (item)
  (or (uniquify-cadddr item) nil))	;maybe better in the future

(defun rationalize-a-list (fix-list depth)
  (let (conflicting-sublist
	(old-name "")
	proposed-name uniquify-possibly-resolvable)
    (let ((fix-list-items fix-list))
      (while fix-list-items
	(let ((item (car fix-list-items)))
	  (setq proposed-name (get-proposed-name item depth))
	  (if (not (equal proposed-name old-name))
	      (progn
		(process-conflicting-sublist conflicting-sublist old-name depth)
		(setq conflicting-sublist nil)))
	  (uniquify-push item conflicting-sublist)
	  (setq old-name proposed-name))
	(setq fix-list-items (cdr fix-list-items))))
    (process-conflicting-sublist conflicting-sublist old-name depth)))

(defun get-proposed-name (item depth)
  (let (index
	(extra-string "") (n depth)
	(base (fix-list-base item)) (fn (fix-list-filename item)))
    (while (and (> n 0)
		(setq index (string-match
			     (concat "\\(^\\|/[^/]*\\)/"
				     (regexp-quote extra-string)
				     (regexp-quote base)
				     "\\'")
			     fn)))
      (setq extra-string (substring fn
				    (if (zerop index) 0 (1+ index))
				    ;; (- (length base)) fails for base = "".
				    ;; Equivalently, we could have used
				    ;; (apply 'substring ...
				    ;;        (and (not (string= "" base))
				    ;;             (list (- (length base)))))
				    (- (length fn) (length base)))
	    n (1- n)))
    (if (zerop n) (setq uniquify-possibly-resolvable t))
    (if uniquify-reverse-dir-content-p
	(setq extra-string (uniquify-reverse-components extra-string)))
    (cond ((string-equal extra-string "")
	   base)
	  ((string-equal base "")
	   extra-string)
	  (t
	   (concat base uniquify-separator extra-string)))))

;; Deal with conflicting-sublist, which is set by rationalize-a-list.
;; This is only called by rationalize-a-list.
(defun process-conflicting-sublist (conflicting-sublist old-name depth)
  (or (null conflicting-sublist)
      (and (null (cdr conflicting-sublist))
	   (not (assoc old-name uniquify-non-file-buffer-names))
	   (or (and (not (string= old-name ""))
		    (rename-the-buffer (car conflicting-sublist) old-name))
	       t))
      (if uniquify-possibly-resolvable
	  (rationalize-a-list conflicting-sublist (1+ depth)))))

(defun rename-the-buffer (item newname)
  (let ((buffer (fix-list-buffer item)))
    (if (not (equal newname (buffer-name buffer)))
	(let ((unset (current-buffer))
	      ;; avoid hooks on rename-buffer
	      (mnemonic-buffer-names nil))
	  (set-buffer buffer)
	  (rename-buffer newname)
	  (set-buffer unset))))
  (rplaca (nthcdr 3 item) t))

(defun uniquify-reverse-components (instring)
  (let ((sofar ()) (cursor 0) (len (length instring)))
    (while (< cursor len)
      (if (= (aref instring cursor) ?/)
          (setq sofar (cons "\\" sofar)
                cursor (1+ cursor))
        (let ((first-slash (or (string-match "/" instring cursor) len)))
          (setq sofar (cons (substring instring cursor first-slash) sofar)
                cursor first-slash))))
    (apply (function concat) sofar)))


;;; Hooks from the rest of Emacs

(cond
 ((string-match "^19" emacs-version)
  ;; Emacs 19

  ;; The logical place to put all this code is in generate-new-buffer-name.
  ;; It's written in C, so we would add a generate-new-buffer-name-function
  ;; which, if non-nil, would be called instead of the C.  One problem with
  ;; that is that generate-new-buffer-name takes a potential buffer name as
  ;; its argument -- not other information, such as what file the buffer will
  ;; visit.

  ;; The below solution works because generate-new-buffer-name is called
  ;; only by rename-buffer (which, as of 19.29, is never called from C) and
  ;; generate-new-buffer, which is called only by Lisp functions
  ;; create-file-buffer and rename-uniquely.  Rename-uniquely generally
  ;; isn't used for buffers visiting files, so it's sufficient to hook
  ;; rename-buffer and create-file-buffer.  (Setting find-file-hooks isn't
  ;; sufficient.)

  (defadvice rename-buffer (after rename-buffer-uniquify activate)
    "Uniquify buffer names with parts of directory name."
    (if (and mnemonic-buffer-names
	     ;; UNIQUE argument
	     (ad-get-arg 1))
	(progn
	  (if uniquify-after-kill-buffer-p
	      ;; call with no argument; rationalize vs. old name as well as new
	      (rationalize-file-buffer-names)
	    ;; call with argument: rationalize vs. new name only
	    (rationalize-file-buffer-names
	     (buffer-file-name-for-uniquify (current-buffer)) (current-buffer)))
	  (setq ad-return-value (buffer-name (current-buffer))))))

  (defadvice create-file-buffer (after create-file-buffer-uniquify activate)
    "Uniquify buffer names with parts of directory name."
    (if mnemonic-buffer-names
	(rationalize-file-buffer-names (ad-get-arg 0) ad-return-value)))

  ;; Buffer deletion
  ;; Rerationalize after a buffer is killed, to reduce coinciding buffer names.
  ;; This mechanism uses `kill-buffer-hook', which runs *before* deletion.
  ;; That means that the kill-buffer-hook function cannot just delete the
  ;; buffer -- it has to set something to do the rationalization *later*.
  ;; It actually puts another function on `post-command-hook'.  This other
  ;; function runs the rationalization and then removes itself from the hook.
  ;; Is there a better way to accomplish this?
  ;; (This ought to set some global variables so the work is done only for
  ;; buffers with names similar to the deleted buffer.  -MDE)

  (cond
   ((not (string-lessp emacs-version "19.28"))
    ;; Emacs 19.28 or later
    (defun delay-rationalize-file-buffer-names ()
      "Add `delayed-rationalize-file-buffer-names' to `post-command-hook'.
For use on, eg, `kill-buffer-hook', to rationalize *after* buffer deletion."
      (if (and mnemonic-buffer-names
	       uniquify-after-kill-buffer-p)
	  (add-hook 'post-command-hook 'delayed-rationalize-file-buffer-names)))
    (defun delayed-rationalize-file-buffer-names ()
      "Rerationalize buffer names and remove self from `post-command-hook'.
See also `delay-rationalize-file-buffer-names' for hook setter."
      (rationalize-file-buffer-names)
      (remove-hook 'post-command-hook 'delayed-rationalize-file-buffer-names))

    (add-hook 'kill-buffer-hook 'delay-rationalize-file-buffer-names))
   (t
    ;; Emacs 19.01 through 19.27
    ;; Before version 19.28, {pre,post}-command-hook was unable to set itself.

    (defvar post-command-do-rationalize-p nil
      "Set to trigger re-rationalization of buffer names by function on
post-command-hook.  Used by kill-buffer-rationalization mechanism.")

    (defun prime-post-command-rerationalization ()
      "Set variable so buffer names may be rationalized by post-command-hook.

See variables `post-command-do-rationalize-p', `mnemonic-buffer-names', and
`uniquify-after-kill-buffer-p'."
      (if (and mnemonic-buffer-names
	       uniquify-after-kill-buffer-p)
	  (setq post-command-do-rationalize-p
		;; Note that we set the buffer name, so, once the
		;; delimiter character is parameterized, we could
		;; selectively rationalize just related buffer names... klm.
		(cons (buffer-name) post-command-do-rationalize-p))))
    (defun rationalize-after-buffer-kill ()
      "Via post-command-hook, rerationalize buffer names after kill-buffer.

Checks `post-command-do-rationalize-p', which should be set by
`post-command-rationalize-buffer-names' function on kill-buffer-hook."
      (if post-command-do-rationalize-p
	  (progn (if (and mnemonic-buffer-names
			  uniquify-after-kill-buffer-p)
		     (rationalize-file-buffer-names))
		 (setq post-command-do-rationalize-p nil))))

    (add-hook 'kill-buffer-hook 'prime-post-command-rerationalization)
    (add-hook 'post-command-hook 'rationalize-after-buffer-kill))
  ))
 (t
  ;; Emacs 18: redefine create-file-buffer and dired-find-buffer.

  ;; Since advice.el can run in Emacs 18 as well as Emacs 19, we could use
  ;; advice here, too, if it is available; but it's not worth it, since
  ;; Emacs 18 is obsolescent anyway.

  (defun create-file-buffer (filename)	;from files.el
    "Create a suitably named buffer for visiting FILENAME, and return it."
    (let ((base (file-name-nondirectory filename)))
      (if (string= base "")
	  (setq base filename))
      (if (and (get-buffer base)
	       uniquify-ask-about-buffer-names-p)
	  (get-buffer-create
	   (let ((tem (read-string (format
				    "Buffer name \"%s\" is in use; type a new name, or Return to clobber: "
                                    base))))
	     (if (equal tem "") base tem)))
	(let ((buf (generate-new-buffer base)))
	  (if mnemonic-buffer-names
	      (rationalize-file-buffer-names filename buf))
	  buf))))

  (defun dired-find-buffer (dirname)	;from dired.el
    (let ((blist (buffer-list))
	  found)
      (while blist
	(save-excursion
	  (set-buffer (car blist))
	  (if (and (eq major-mode 'dired-mode)
		   (equal dired-directory dirname))
	      (setq found (car blist)
		    blist nil)
	    (setq blist (cdr blist)))))
      (or found
	  (progn (if (string-match "/$" dirname)
		     (setq dirname (substring dirname 0 -1)))
		 (create-file-buffer (if mnemonic-buffer-names
					 dirname
				       (file-name-nondirectory dirname)))))))))

;;; uniquify.el ends here
