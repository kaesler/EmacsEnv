;; nc.el --- emulate famos ms-dog file browser
;;; Copyright (C) 1996 Stefan Hegny

;; Author: Stefan Hegny (hegny@fzi.de)
;; Version: 1.0alpha5, Mar 11,1996
;; Maintained: not after final release
;; Keywords: file browser, directory, shell
;; Available: soon from  ftp://ftp.math.ohio-state.edu/pub/users/ilya/emacs
;;;                      (thanks Ilya Zakharevich <ilya@math.ohio-state.edu>)

;; This file is not part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Installation:
;;; put nc.el/nc.elc in your load-path
;;; add (autoload 'nc "nc" "Emulate MS-DOG file shell" t) to _emacs/.emacs
;;; type `M-x nc'

;;;This package emulates the file browser of a famous ms-dog application.
;;;Look-and-feel is supposed to be close to the original, and there exist the
;;;following functions on the following keys:
;;;(marked [*] means: in NC-directory buffers and in
;;;buffers opened from there; marked [+] means only in
;;;buffers opened from there; unmarked: only in 
;;;NC-directory buffers)

;;;          [*] M-x nc:   Invoke file browser
;;;  up,down,left,right:   Move around 
;;;home,end,pgup,pgdown:   Goto first/last item
;;;              return:   If dir, change into dir
;;;                        If executable, execute
;;;                 ins:   Select/deselect file under cursor
;;;                   +:   Select files of pattern
;;;                   -:   Deselect files of pattern
;;;                 tab:   Goto other of the two nc-buffers
;;;                  f1:   Short help
;;;                  f2:   Change to a new directory or update current
;;;                        (re-read from disk)
;;;                C-f2:   Update view of current dirs (dont re-read)
;;;                  f3:   View file under cursor
;;;                  f4:   Edit file under cursor
;;;                  f5:   Copy selected files, 
;;;                        if none selected, use file the cursor is on
;;;                C-f5:   like f5, default target is dir in same buffer
;;;                  f6:   Move/Rename selected files, 
;;;                        if none selected, use file the cursor is on
;;;                C-f6:   like f6, default target is dir in same buffer
;;;                  f7:   Make Directory, prompt for name
;;;                  f8:   Delete selected files, 
;;;                        if none selected, use file or dir the cursor is on
;;;             [*] f10:   Quit file browser, killing all its buffers
;;;             ESC ESC:   Get back to two column nc-windows
;;;         [+] ESC ESC:   Get back to two column nc-windows,
;;;                        killing current buffer
;;;  [msdog only]  a..s:   Change disk drive
;;;      [caps]     N,T:   Sort by [N]ame or [T]ime
;;;                   B:   Byte compile file under cursor
;;;                   Q:   Quit file browser, killing all its buffers

;;;Nc can be configured to work under other os' like UN*X.
;;;If you dont like the defaults, change them (see below)!

;;;Names of Directorys are displayed as CAPS on ms-dog, by
;;;appending "/" on other os.

;;;The displayed length of names and the number of lines in a column
;;;are well estimated out of (frame-width) and (frame-height).
;;;To adjust display to a resized frame or after changing
;;;the sorting criteria by hand, type `C-f2' (In this case, the files in 
;;;the directory are NOT reloaded). To reload directory,
;;;type `f2 RET'.

;;;Things that can be changed reasonably include the following variables:

;;;*COLORS*
;;;nc-highlight-background-color
;;;nc-highlight-foreground-color
;;;nc-select-background-color
;;;nc-select-foreground-color

;;;*CHARS*
;;;nc-horiz-line-char
;;;nc-verti-line-char
;;;nc-name-too-long-char

;;;*SPECIALS*
;;;nc-suffix-length      if not 0, display that much positions of the
;;;                      filename to the right of the reserved space for
;;;                      the first dot in the filename (except dot at pos. 0)
;;;                      if =0, display all filenames flushed left without
;;;                      separating name and suffix
;;;                      Longer filenames or parts thereof are indicated
;;;                      with nc-name-too-long-char
;;;nc-time-format        string used to format file time
;;;nc-sort-by-name       sort criterium for displayed list of files

;;;Things that could be done but havent been:
;;; - directory tree
;;; - undo function

;;; Change Log:
;;;
;;; 1.0alpha5: corrected switching between the nc-buffers.
;;;            the side-by-side-view will now  be restored
;;;            every time. 
;;;            Text-cursor will now always be on the file
;;;            where the file cursor would be.
;;;
;;; 1.0alpha4: corrected malfunction for ESC-ESC
;;;            added comments for variables and removed unused ones
;;;
;;; 1.0alpha3: Byte compiling on linked files (I needed this)
;;;
;;; 1.0alpha2:  removed bug from nc-update-all-modifications
;;;             now also renaming in the same directory is displayed corr.

;;; Code:

;(require 'rect)
 
;;; Customazible variables:

;;;###autoload
(defvar nc-highlight-background-color "DarkSlateGray"
  "Background of Cursor and Title")
;;;###autoload
(defvar nc-highlight-foreground-color "Yellow"
  "Foreground of Cursor and Title")
;;;###autoload
(defvar nc-select-foreground-color "DarkSlateGray"
  "Background of selection mark")
;;;###autoload
(defvar nc-select-background-color "Green"
  "foreground of selection mark")

;;;###autoload
(defvar nc-time-format "%d.%m.%y, %H:%M"
  "Format String to display times")

;;;###autoload
(defvar nc-sort-by-name t
  "Sort files by name instead of time")

;;;###autoload
(defvar nc-horiz-line-char (if (eql system-type 'ms-dos)
			       205
			     ?=)
  "Character number for horizontal lines")

;;;###autoload
(defvar nc-verti-line-char (if (eql system-type 'ms-dos)
			       179
			     ?|)
  "Character number for vertical lines")

;;;###autoload
(defvar nc-name-too-long-char
  (if (eql system-type 'ms-dos)
      175
    ?>)
  "Character to designate longer filenames")

;;;###autoload
(defvar nc-suffix-length
  (cond ((eql system-type 'ms-dos)
	 3)
	(t 0))
  "Length to be displayed behind dot in filename.")

;;; Callable Functions:

;;;###autoload
(defun nc-mode ()
  "Major mode for File Browser in GNU emacs.

Normally displays two adjacent buffers with
directory contents. (This state can always 
be restored typing `M-x nc'). "
  (interactive)
  (let ((assbuff nc-associated-nc-buffer))
    (nc-set-keys)
    (nc-switch-to-buffers)
    (cond ((or (not (boundp 'nc-files))
	       (not nc-files))
	   (let ((defdir (expand-file-name (getenv "HOME"))))
	     (if (not (nc-valid-dirname-p defdir))
		 (setq defdir (format "%s/" defdir)))
	     (setq nc-files (list defdir)
		   default-directory defdir))
	   (nc-display-buffer 0)
	   (nc-setup-cursor)))
    (nc-deselect-buffer)
    (other-window 1)
    (cond ((or (not (boundp 'nc-files))
	       (not nc-files))
	   (if (not (nc-valid-dirname-p default-directory))
	       (setq default-directory (format "%s/" default-directory)))
	   (setq nc-files (list (expand-file-name default-directory)))
	   (nc-display-buffer 0)
	   (nc-setup-cursor)))
    (nc-deselect-buffer)
    (cond ((and assbuff
		(not (eql assbuff (current-buffer))))
	   (nc-deselect-buffer)
	   (other-window 1)
	   (nc-select-buffer))
	  (assbuff
	   (other-window 1)
	   (nc-deselect-buffer)
	   (other-window 1)
	   (nc-select-buffer))
	  (t (nc-select-buffer)))))

;;;###autoload
(defalias 'nc 'nc-mode)


;;; Internal Stuff:

(defvar nc-buffers nil
  "List of Buffers associated to File Browser
Normally contains exactly two entries")

(make-face 'nc-highlight-face)
(set-face-background 'nc-highlight-face nc-highlight-background-color)
(set-face-foreground 'nc-highlight-face nc-highlight-foreground-color)
(make-face 'nc-select-face)
(set-face-foreground 'nc-select-face nc-select-foreground-color)
(set-face-background 'nc-select-face nc-select-background-color)

(defvar nc-mode-map (make-sparse-keymap)
  "Keymap for File Browser")

(defvar nc-local-esc-map (copy-keymap esc-map)
  "Keymap for Files openend from File Browser")

(defvar nc-local-map nil
  "local keymap for file browser")

(defvar nc-cl
    (if (eql system-type 'ms-dos)
	12
      (- (/ (- (frame-width) 5) 6) 1))
  "Width of one column")

(defvar nc-cursor-col nil
  "Cursor column in file browser")
(defvar nc-cursor-lin nil
  "Cursor line in file browser")
(defvar nc-files nil
  "Files in file browsers directory")
(defvar nc-first-column nil
  "index of leftmost visible column")
(defvar nc-format nil
  "formatted file lists for file browser columns")
(defvar nc-format-align-right 0
  "offsett of displayed files in file browser")
(defvar nc-title-start nil
  "start column of title in file browser")
(defvar nc-title-end nil
  "end column of title in file browser")
(defvar nc-associated-nc-buffer nil
  "Buffer associated in file browser")
(defvar nc-selected-files nil
  "List of selected files in file browsers directory")
(defvar nc-file-mod nil
  "List of modified files during file browser operation")

(defvar nc-nl
    (if (eql system-type 'ms-dos)
	18
      (- (frame-height) 7))
  "Number of lines in browser")

(define-key nc-local-esc-map [27] 'nc-bufferkill-or-nc)

(defun nc-absolute-from-relative-pos (col lin)
  "Compute the position of leftmost character
of filename in line LIN and column COL as point"
  (+ (* 6 (+ 1 nc-cl))
     (* (* 3 (+ 1 nc-cl)) lin)
     (* (+ nc-cl 1) (- col nc-first-column))))

(defun nc-relative-from-absolute-pos (pos)
  "Compute the list (column line) from POS in buffer"
  (setq pos (- pos (* 6 (+ 1 nc-cl))))
  (if (> pos 0)
      (let ((line (/ pos (* 3 (+ 1 nc-cl)))))
	(if (< line nc-nl)
	    (list (+ (/ (- pos  (* line (* 3 (+ 1 nc-cl))))
			(+ nc-cl 1))
		     nc-first-column)
		  line)
	  '(-1 -1)))
    '(-1 -1)))

(defun nc-filename-norm (name &optional dirp)
  "Format the NAME of the file for display in File Browser buffer.
Iff DIRP is t, letters will be capitals in ms-dog,
name will be suffixed `/' otherwise."
  (let ((name (file-name-nondirectory name))
	(name-length (length name))
	(dotpos  0)
	(prelength 0)
	(prestring nil)
	(pos 0)
	(postlength 0))
    (cond((and dirp
	       (eql system-type 'ms-dos))
	  (setq name (upcase name)))
	 (dirp
	  (setq name (format "%s/" name)
		name-length (+ 1 name-length))))
    (while (and (< pos name-length)
		(= 0 dotpos))
      (cond ((and (= 0 dotpos)
		  (= 46 (aref name pos))
		  (>  pos 0))
	     (setq dotpos prelength))
	    ((> dotpos 0)
	     (setq postlength (+ 1 postlength)))
	    (t
	     (setq prelength (+ 1 prelength))))
      (setq pos (+ 1 pos)))
    (if (or (string= name "..")
	    (string= name "../")
	    (= 0 nc-suffix-length))
	(setq prelength (length name) dotpos 0))
    (setq postlength (- name-length prelength 1))
    (cond ((and (= 0 dotpos)
		(> name-length nc-cl))
	   (format "%s%c" (substring name 0 (- nc-cl 1))
		   nc-name-too-long-char))
	  ((= 0 dotpos)
	   (format "%s%s" name
		   (make-string (- nc-cl name-length) 32)))
	  (t (cond ((> prelength (- nc-cl (+ 1 nc-suffix-length)))
		    (setq prestring
			  (format "%s%c"
				  (substring name 0
					     (- nc-cl
						(+ 2 nc-suffix-length)))
				  nc-name-too-long-char)))
		   (t (setq prestring
			    (format "%s%s" (substring name 0 prelength)
				(make-string
				 (- nc-cl (+ 1 nc-suffix-length) prelength)
				 32)))))
	     (cond ((> postlength nc-suffix-length)
		    (setq postlength
			  (format "%s%c"
				  (substring name (+ 1 prelength)
					     (+ nc-suffix-length
						prelength))
				  nc-name-too-long-char)))
		   (t (setq postlength
			    (format "%s%s"
				    (substring name (+ 1 prelength))
				    (make-string
				     (- nc-suffix-length postlength) 32)))))
	     (format "%s %s" prestring postlength)))))

(defun nc-sort-dired-files (files)
  (if nc-sort-by-name
      (sort files '(lambda (x y)
		     (string< (car x) (car y))))
    (sort files '(lambda (x y)
		   (let ((ga (car (elt x 3)))
			 (ka (elt (elt x 3) 1))
			 (gb (car (elt y 3)))
			 (kb (elt (elt y 3) 1)))
		     (or (< ga gb)
			 (and (= ga gb)
			      (< ka kb))))))))  

(defun nc-file-attributes-list (name)
  (let ((attr (file-attributes name)))
    (list (nc-filename-norm            ;;; 0:the name 
	   (file-name-nondirectory name)
	   (file-directory-p name))
	  (file-directory-p name)
	  ;(and (car attr)	       ;;; 1:directory-p
	  ;(= ?d (elt (elt attr 8) 0)))
	  (elt attr 7)		       ;;; 2:size
	  (elt attr 5)		       ;;; 3:date
	  (elt attr 8)                 ;;; 4:attributes
	  (file-name-nondirectory name)   ;;; 5:realname
	  nil                          ;;; 6: place for selected
	  (file-symlink-p name)        ;;; 7: extension
	  )))

(defun nc-dired-to-list (name)
  "Generate a list of files and attributes for
the given directory"
  (let ((files
	 (delete
	  nil
	  (mapcar '(lambda (x)
		     (if (string= (file-name-nondirectory x) ".")
			 nil
		       (nc-file-attributes-list x)))
		  (directory-files name t)))))
    (nc-sort-dired-files files)))

(defun nc-adjust-and-show (&optional soft screen)
  (nc-adjust-screen soft)
  (if screen (nc-display-buffer))
  (nc-write-modeline)
  (nc-show-cursor nc-cursor-col nc-cursor-lin))

(defun nc-adjust-screen (&optional soft)
  (if (>= nc-cursor-col (length nc-format))
      (setq nc-cursor-col (- (length nc-format) 1)
	    nc-cursor-lin nc-nl))
  (if (>= nc-cursor-lin
  	  (length (elt nc-format nc-cursor-col)))
      (setq nc-cursor-lin (- (length (elt nc-format nc-cursor-col)) 1)))
  (cond ((> nc-cursor-col (+ 2 nc-first-column))
	 (cond
	  ;;(soft (setq nc-first-column (+ 1 nc-first-column))
	  ;;      (setq buffer-read-only nil)
	  ;;	  (kill-rectangle 78 753)
	  ;;	  (goto-char 104)
	  ;;	  (insert-rectangle killed-rectangle)
	  ;;	  (kill-rectangle 78 742)
	  ;;	  (goto-char 103)
	  ;;	  (insert-rectangle killed-rectangle)
	  ;;	  (nc-display-column (list (+ 2 nc-first-column)))
	  ;;	  (nc-show-selected-files))
	  (soft (setq nc-first-column (+ nc-first-column 1)))
	  (t (setq nc-first-column
		   (max 0 (- (length nc-format) 3)))))
	 (if (= (+ 3 nc-first-column)
		(length nc-format))
	     (setq nc-format-align-right
		   (- (- nc-nl 1) (% (- (length (cdr nc-files)) 1)
			    nc-nl))
		   nc-cursor-lin (- nc-nl 1))
	   (setq nc-format-align-right 0))
	 (nc-display-buffer)
	 )
	((< nc-cursor-col nc-first-column)
	 (cond ((not soft)
		(setq nc-first-column 0
		      nc-cursor-col 0
		      nc-cursor-lin 0))
	       (t (setq nc-first-column (- nc-first-column 1))))
	 (if (= 0 nc-first-column)
	     (setq nc-format-align-right 0))
	 (nc-display-buffer)
	 )))

(defun nc-cursor-left ()
  "Move file cursor left one column"
  (interactive)
  (cond ((> nc-cursor-col
	    0)
	 (nc-delete-cursor nc-cursor-col nc-cursor-lin )
	 (setq nc-cursor-col (- nc-cursor-col 1))
	 (nc-adjust-and-show t))))
	  

    
(defun nc-cursor-up ()
  "Move file cursor up one line"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (cond ((>  nc-cursor-lin
	     0)
	 (setq nc-cursor-lin (- nc-cursor-lin 1)))
	((>  nc-cursor-col
	     0)
	 (setq nc-cursor-col (- nc-cursor-col 1)
	       nc-cursor-lin (- nc-nl 1))))
  (nc-adjust-and-show t))

(defun nc-cursor-right ()
  "Move file cursor right one column"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (cond ((< (+ 1 nc-cursor-col)
	    (length nc-format))
	 (setq nc-cursor-col (+ nc-cursor-col 1))
	 (if (< nc-cursor-lin
		(length
		 (elt nc-format
		      nc-cursor-col)))
	     nil
	   (setq nc-cursor-lin
		 (- (length (elt nc-format
				 nc-cursor-col)) 1)))))
  (nc-adjust-and-show t))

(defun nc-cursor-home ()
  "Move file cursor to first position"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (setq nc-cursor-col 0
	nc-cursor-lin 0)
  (nc-adjust-and-show))

(defun nc-cursor-end ()
  "Move file cursor to last position"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (setq nc-cursor-col (- (length nc-format) 1)
	nc-cursor-lin (if (> (length nc-format) 3)
			  (- nc-nl 1)
			(- (length
			    (elt nc-format (- (length nc-format) 1)))
			   1)))
  (nc-adjust-and-show))

(defun nc-cursor-down ()
  "Move file cursor down one line"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
    (cond ((< (+ 1 nc-cursor-lin)
	      (length (elt nc-format
			   nc-cursor-col)))
	   (setq nc-cursor-lin (+ 1 nc-cursor-lin)) )
	  ((< (+ 1 nc-cursor-col)
	      (length nc-format))
	   (setq nc-cursor-col (+ 1 nc-cursor-col)
		 nc-cursor-lin 0)))
    (nc-adjust-and-show t))

(defun nc-set-cursor ()
  "Set cursor position by mouse in File Browser"
  (interactive)
  (nc-deselect-buffer)
  (mouse-set-point last-nonmenu-event)
  (nc-select-buffer)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (let ((newpos (nc-relative-from-absolute-pos
		 (elt (elt last-nonmenu-event 1) 1))))
    (cond ((>= (car newpos) 0)
	   (cond ((or (< (car newpos) (- (length nc-format) 1))
		      (and (= (car newpos) (- (length nc-format) 1))
			   (< (car (cdr newpos))
			      (length (elt nc-format
					   (- (length nc-format) 1))))))
		  (setq nc-cursor-col (car newpos)
			nc-cursor-lin (car (cdr newpos))))
		 (t (setq nc-cursor-col (- (length nc-format) 1)
			  nc-cursor-lin (- (length
					    (elt nc-format
						 (- (length nc-format) 1)))
					   1))))))
    (nc-show-cursor nc-cursor-col nc-cursor-lin)
    (nc-write-modeline)))

(defun nc-other-buffer ()
  "Select other Browser buffer"
  (interactive)
  (nc-mode)
  (nc-deselect-buffer)
  (other-window 1)
  (nc-select-buffer))

(defun nc-valid-dirname-p (name)
  (memq (aref name
	      (- (length name) 1))
	'(92 47)))  ;; should the backslash be in here?

(defun nc-display-new-dir (&optional dir)
  "Display directory dir in buffer"
  (interactive "DChange to directory: ")
  (if (and dir
	   (not (nc-valid-dirname-p dir)))
      (setq dir (format "%s/" dir)))
  (if dir (setq dir (expand-file-name dir)))
  (cond (dir
	 (setq nc-files (list dir)
	       default-directory dir)))
  (setq nc-cursor-col 0
	nc-cursor-lin 0
	nc-selected-files nil)
  (nc-adjust-screen)
  (nc-display-buffer 1)
  (nc-setup-cursor))

(defun nc-change-dir ()
  "Goto the directory in line"
  (interactive)
  (setq buffer-read-only nil)
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 (filename (elt file 5)))
    (cond ((string= filename "..")
	   (let ((leng (- (length (car nc-files)) 2))
		 (stri (car nc-files)))
	     (while (and (> leng 0)
			 (not (= (aref stri leng) 92))
			 (not (= (aref stri leng) 47)))
	       (setq leng (- leng 1)))
	     (cond ((>= leng 0)
		    (nc-display-new-dir (substring stri 0 (+ 1 leng)))
		    ))))
	  ((elt file 1)
	   (let ((newname (if (nc-valid-dirname-p (car nc-files))
			      (format "%s%s/" (car nc-files) filename)
			    (format "%s/%s/"  (car nc-files) filename))))
	     (nc-display-new-dir newname)))
	  ((or (and (eql system-type 'ms-dos)
		    (or (string= (substring
				  filename
				  (- (length filename) 4))
				 ".exe")
			(string= (substring filename
					    (- (length filename) 4))
				 ".bat")))
	       (and (not (eql system-type 'ms-dos))
		    (= ?x (elt (elt file 4) 3))))
	   (shell-command filename)))))

(defun nc-bufferkill-or-nc ()
  "If current buffer invoked from nc, kill and go back.
If current buffer part of nc, restore nc view."
  (interactive)
  (if (or (eql (current-buffer) (car nc-buffers))
	  (eql (current-buffer) (car (cdr nc-buffers))))
      (nc)
    (nc-bufferkill)))

(defun nc-bufferkill ()
  "Kill current-buffer going back to nc"
  (interactive)
  (if (and (buffer-modified-p)
	   (y-or-n-p
	    (format "Save modified buffer %s before killing? "
		    (buffer-name (current-buffer)))))
      (save-buffer))
  (kill-buffer (current-buffer))
  (nc))

(defun nc-bufferswitch ()
  "Switch to other buffer out of nc"
  (interactive)
  (call-interactively 'switch-to-buffer)
  (cond ((not (memq (current-buffer)
		    nc-buffers))
	 (delete-other-windows)
	 )
	((and (not (eql (current-buffer)
			(car nc-buffers)))
	      (not (eql (current-buffer)
			(car (cdr nc-buffers)))))
	 (delete-other-windows)
	 )
	(t (nc))))

(defun nc-set-local-keys ()
  (make-local-variable 'nc-local-map)
  (if (current-local-map)
      (setq nc-local-map (copy-keymap (current-local-map)))
    (setq nc-local-map (make-sparse-keymap)))
  (make-local-hook 'after-save-hook)
  (add-hook 'after-save-hook
	    `(lambda () (nc-update-current-file ,(current-buffer))))
  (define-key nc-local-map [27] nc-local-esc-map)
  (use-local-map nc-local-map)
  ;(local-set-key [27 27] 'nc-bufferkill)
  (local-set-key [\f10] 'nc-quit)
  (local-set-key "\C-xb" 'nc-bufferswitch)
  (local-set-key "\C-xk" 'nc-bufferkill)
  (message
   "Type `ESC ESC' in this buffer or `M-x nc RET' to get back"))

(defun nc-view ()
  "View the file from Browser"
  (interactive)
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 (ncbuf (current-buffer))
	 (filename (elt file 5)))
    (cond ((null (elt file 1))
	   (find-file-read-only filename)
	   (delete-other-windows)
	   (make-local-variable 'nc-associated-nc-buffer)
	   (setq nc-associated-nc-buffer ncbuf)
	   (if (not (memq (current-buffer)
			  nc-buffers))
	       (setq nc-buffers (append nc-buffers
					(list (current-buffer)))))
	   (nc-set-local-keys)))))

(defun nc-edit ()
  "edit the file"
  (interactive)
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 (ncbuf (current-buffer))
	 (filename (elt file 5)))
    (cond ((null (elt file 1))
	   (find-file filename)
	   (delete-other-windows)
	   (make-local-variable 'nc-associated-nc-buffer)
	   (setq nc-associated-nc-buffer ncbuf)
	   (if (not (memq (current-buffer)
			  nc-buffers))
	       (setq nc-buffers (append nc-buffers
					(list (current-buffer)))))
	   (nc-set-local-keys)))))

(defun nc-num-from-col-lin (col lin)
  (+ (* nc-nl col)  lin
     (- nc-format-align-right)))

(defun nc-col-from-num (num)
  (/ (+ num nc-format-align-right) nc-nl))

(defun nc-lin-from-num (num &optional col)
  (- num (- nc-format-align-right)
     (* nc-nl (if col
	       col
	     (/ (+ num nc-format-align-right) nc-nl)))))
     
(defun nc-delete-selected-files ()
  (let ((oldsel nc-selected-files))
    (while oldsel
      (nc-delete-select
       (nc-col-from-num (car oldsel))
       (nc-lin-from-num (car oldsel)))
      (setq oldsel (cdr oldsel)))))
      
(defun nc-convert-to-regexp (pattern)
  "Convert a file-name pattern to an emacs-regexp"
  (let ((len (- (length pattern) 1))
	(new ""))
    (while (>= len 0)
      (setq new
	    (cond ((and (= (elt pattern len) ?*);; an asterisk at beginn
			(= 0 len))
		   (format "[^.].*%s" new))
		  ((= (elt pattern  len) ?*);; an asterisk
		   (format ".*%s" new))
		  ((and (= (elt pattern len) ??);; a question-mark at beginn
			(= 0 len))
		   (format "[^.]%s" new))
		  ((= (elt pattern len) ??);; a question-mark
		   (format ".%s" new))
		  ((memq (elt pattern len) '(?. ?+));; dot or plus
		   (format "[%c]%s" (elt pattern len) new))
		  (t
		   (format "%c%s" (elt pattern len) new))))
      (setq len (- len 1)))
    new))

(defun nc-deselect-pattern (pat)
  "deselect files according to filename pattern"
  (interactive "sDeselect Files Pattern:")
  (setq buffer-read-only nil)
  (let ((oldsel nc-selected-files)
	(files (- (length nc-files) 1))
	(newpat (nc-convert-to-regexp pat)))
    (nc-delete-selected-files)
    (while (> files 0)
      (let* ((beg (string-match newpat (elt (elt nc-files files) 5)))
	     (end (match-end 0)))
	(cond ((and beg
		    (= 0 beg)
		    (= (length (elt (elt nc-files files) 5))
		       end)
		    (not (elt (elt nc-files files) 1))
		    (memq (- files 1) nc-selected-files))
	       (setq nc-selected-files
		     (delete (- files 1) nc-selected-files)))))
      (setq files (-  files 1))))
  (nc-show-selected-files)
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-modeline)
  (setq buffer-read-only t))

(defun nc-select-pattern (pat)
  "select files according to filename pattern"
  (interactive "sSelect Files Pattern:")
  (setq buffer-read-only nil)
  (let ((oldsel nc-selected-files)
	(files (- (length nc-files) 1))
	(newpat (nc-convert-to-regexp pat)))
    (while (> files 0)
      (let* ((beg (string-match newpat (elt (elt nc-files files) 5)))
	     (end (match-end 0)))
	(cond ((and beg
		    (= 0 beg)
		    (= (length (elt (elt nc-files files) 5))
		       end)
		    (not (elt (elt nc-files files) 1))
		    (not (memq (- files 1) nc-selected-files)))
	       (setq nc-selected-files
		     (cons (- files 1) nc-selected-files)))))
      (setq files (-  files 1))))
  (nc-show-selected-files)
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-modeline)
  (setq buffer-read-only t))
    
(defun nc-select ()
  "Toggle select/deselect of current file"
  (interactive)
  (setq buffer-read-only nil)
  (cond ((memq (nc-num-from-col-lin nc-cursor-col
				    nc-cursor-lin)
	       nc-selected-files) ;; it is drin
	 (nc-delete-select nc-cursor-col nc-cursor-lin)
	 (setq nc-selected-files
	       (delete (nc-num-from-col-lin nc-cursor-col
					    nc-cursor-lin)
		       nc-selected-files))
	 (nc-show-cursor nc-cursor-col nc-cursor-lin))
	(t
	 ;(nc-delete-selected-files)
	 (cond ((null (elt (elt nc-files    ;; it mustn't be a DIR
				(+ (nc-num-from-col-lin
				    nc-cursor-col
				    nc-cursor-lin) 1)) 1))
		(setq nc-selected-files
		      (cons (nc-num-from-col-lin nc-cursor-col
						 nc-cursor-lin)
			    nc-selected-files))
		(nc-show-select nc-cursor-col nc-cursor-lin)))))
  (nc-cursor-down)
  (nc-write-modeline)
  (setq buffer-read-only t))

(defun nc-change-drive ()
  "Select a new drive in File Browser"
  (interactive)
  (let* ((newdrive last-nonmenu-event)
	 (newdir (format "%c:/" newdrive))
	 (files (directory-files newdir)))
    (cond (files
	   (setq buffer-read-only nil)
	   (setq nc-files (list newdir))
	   (setq default-directory (expand-file-name newdir))
	   (setq nc-cursor-col 0
		 nc-cursor-lin 0)
	   (nc-adjust-screen)
	   (setq nc-selected-files nil)
	   (nc-display-buffer 0)
	   (nc-setup-cursor)
	   (setq buffer-read-only t)))))
	   
(defun nc-mkdir (name)
  "Create a directory from File Browser"
  (interactive "FDirectory Name:")
  (if (nc-valid-dirname-p name)
      (setq name (substring name 0 (- (length name) 1))))
  (setq name (expand-file-name name))
  (cond ((file-directory-p name)
	 (message "The directory %s already exists!" name))
	((string= (file-name-directory  name)
		  default-directory)
	 (make-directory name)
	 (nc-update-all-modifications
	  (list (cons nil
		      name))
	  nil t)
;	 (setq nc-files
;	       (cons (car nc-files)
;		     (nc-sort-dired-files
;		      (cons
;		       (nc-file-attributes-list name)
;		       (cdr nc-files)))))
;	 (nc-display-buffer)
	 (nc-setup-cursor))
	(t (make-directory name))))

(defun nc-save-selections ()
  (let ((le (- (length nc-selected-files) 1)))
    (while (>= le 0)
      (rplaca (nthcdr 6 (elt (cdr nc-files)
			     (elt nc-selected-files le)))
	      t)
      (setq le (- le 1))))
  (setq nc-selected-files nil))

(defun nc-restore-selections ()
  (setq nc-selected-files nil)
  (let ((le (- (length nc-files) 1)))
    (while (> le 0)
      (if (elt (elt nc-files le) 6)
	  (setq nc-selected-files
		(cons (- le 1) nc-selected-files)))
      (rplaca (nthcdr 6 (elt nc-files le))
	      nil)
      (setq le (- le 1)))))
      
(defun nc-update-current-file (buff)
  "Update the display of the file in the buffer BUFF.
Called in the local after-save-hook to show new version"
  (interactive)
  (let ((buf (buffer-file-name buff)))
    (save-excursion
      (nc)
      (if buf
	  (nc-update-all-modifications
	   (list
	    (cons nil
		  buf))
	   nil t)))))

(defun nc-update-all-modifications (modi source-p dest-p)
  "update modifications for both buffers"
  ;; Problems: both buffers might be associated with
  ;; dirs that have diffenent names (via links etc)
  (nc-mode)
  (setq nc-selected-files nil)
  (nc-update-modifications modi source-p dest-p)
  (other-window 1)
  (nc-update-modifications modi source-p dest-p)
  (nc-deselect-buffer)
  (other-window 1)
  (nc-select-buffer))

(defun nc-update-modifications (modi source-p dest-p)
  (nc-save-selections)
  (if (and source-p;; remove source files from dir (moved or deleted)
	   (or (null (file-name-directory (car (car modi))))
	       (string= (file-truename
			 (expand-file-name
			  (file-name-directory (car (car modi)))))
			(file-truename (car nc-files)))))
      (let ((newfiles nil)
	    (oldfiles (cdr nc-files)))
	(while oldfiles
	  (let ((modis modi))
	    (while (and modis
			(not (string=
			      (file-name-nondirectory (car (car modis)))
			      (elt (car oldfiles) 5))))
	      (setq modis (cdr modis)))
	    (if (and modis (string= (file-name-nondirectory
				     (car (car modis)))
				    (elt (car oldfiles) 5)))
		nil
	      (setq newfiles (cons (car oldfiles) newfiles))))
	  (setq oldfiles (cdr oldfiles)))
	(setq nc-files (cons (car nc-files)
			     (reverse newfiles)))))
  (if (and dest-p;; add dest files if nonexistent
	   (or (null (file-name-directory (cdr (car modi))))
	       (string= (file-truename
			 (expand-file-name
			  (file-name-directory (cdr (car modi)))))
			(file-truename (car nc-files)))))
      (let ((newfiles nil)
	    (oldfiles (cdr nc-files)))
	(while oldfiles;; 1 check for existing and replace
	  (let ((modis modi))
	    (while (and modis
			(not (string=
			      (file-name-nondirectory (cdr (car modis)))
			      (elt (car oldfiles) 5))))
	      (setq modis (cdr modis)))
	    (if (and modis
		     (string= (file-name-nondirectory (cdr (car modis)))
			      (elt (car oldfiles) 5)))
		(setq newfiles (cons (nc-file-attributes-list
				      (cdr (car modis)))
				     newfiles))
	      (setq newfiles (cons (car oldfiles) newfiles))))
	  (setq oldfiles (cdr oldfiles)))
	(let ((modis modi))
	  (while modis
	    (let ((oldfiles newfiles))
	      (while (and oldfiles;; 2 add nonexisting
			  (not (string= (file-name-nondirectory
					 (cdr (car modis)))
					(elt (car oldfiles) 5))))
		(setq oldfiles (cdr oldfiles)))
	      (if (and oldfiles
		       (string= (file-name-nondirectory
				 (cdr (car modis)))
				(elt (car oldfiles) 5)))
		  nil
		(setq newfiles
		      (cons  (nc-file-attributes-list
			      (cdr (car  modis)))
			     newfiles))))
	    (setq modis (cdr modis))))
	(setq nc-files
	      (cons (car nc-files)
		    (nc-sort-dired-files newfiles)))))
  (setq nc-format (nc-format-files-for-buffer nc-files))
  (nc-restore-selections)
  (nc-adjust-and-show nil t))

(defun nc-get-other-default-dir ()
  "Get the default-directory of the other nc-buffer.
Used as target for copy, move operations"
  (nc-mode)
  (save-excursion
    (other-window 1)
    (let ((erg default-directory))
      (other-window 1)
      erg)))

(defun nc-get-target-file (command &optional default-dir)
  "Prompt for filename to operate on one file"
  (if (null default-dir)
      (setq default-dir (expand-file-name (nc-get-other-default-dir)))
    (setq default-dir (expand-file-name default-dir)))
  (expand-file-name (read-file-name
		     command default-dir nil nil)))

(defun nc-get-target-dir (command &optional default-dir)
  "Prompt for target of copy or move for multiple files."
  (if (null default-dir)
      (setq default-dir (nc-get-other-default-dir)))
  (let ((erg
	 (expand-file-name
	  (read-file-name command default-dir nil t))))
    (while (not (file-directory-p erg))
      (setq erg (expand-file-name
		 (read-file-name
		  (format "Please select a DIRECTORY for %s"
			  command default-dir)))))
    erg))

(defun nc-get-target-dir-or-file (command &optional nodest target-dir)
  "Returns the target dir-or file and the source file(s)
for copy and move commands."
  (let ((source
	 (cond ((null nc-selected-files)
		(let* ((abspos (nc-num-from-col-lin nc-cursor-col
						    nc-cursor-lin))
		       (file (elt (cdr nc-files) abspos))
		       (filename (elt file 5)))
		  (list filename)))
	       ((= 1 (length nc-selected-files))
		(let* ((col (nc-col-from-num (car nc-selected-files)))
		       (lin (nc-lin-from-num (car nc-selected-files) col))
		       (abspos (nc-num-from-col-lin col lin))
		       (file (elt (cdr nc-files) ;;abspos
				  (car nc-selected-files)
				  ))
		       (filename (elt file 5)))
		  (list filename)))
	       (t (mapcar '(lambda (x)
			     (let* ((col (nc-col-from-num x))
				    (lin (nc-lin-from-num x col))
				    (abspos (+ (* nc-nl col) lin))
				    (file (elt (cdr nc-files) ;;abspos
					       x
					       ))
				    (filename (elt file 5)))
			       filename))
			  nc-selected-files)))))
    (cons
     (if nodest
	 (car source)
       (cond ((= 1 (length source))
	      (nc-get-target-file
	       (format "%s file %s to: " command (car source))
	       target-dir))
	     (t   (nc-get-target-dir
		   (format "%s %s files to: " command
			   (length source))
		   target-dir))))
     source)))

(defun nc-check-file-op (destname operation ovwrt)
  "If necessary, interact with user to find out
if file shall be operated on"
  (cond ((or (and (file-exists-p destname)
		  (not (eql ovwrt 'none))
		  (not (eql ovwrt 'all)))
	     (eql ovwrt 'always))
	 (message "%s file %s? [y]es [n]o [a]ll [N]one) "
		  operation destname)
	 (let ((answ (read-char)))
	   (while (not (memq answ '(?y ?Y ?n ?a ?A ?N)))
	     (message
	      "y/Y[es this file], n[ot this file], a/A[ll files], N[o files]")
	     (setq answ (read-char)))
	   (list (if (memq answ '(?y ?Y ?a ?A))
		     t
		   nil)
		 (cond ((memq answ '(?a ?A))
			'all)
		       ((eql answ ?N)
			'none)
		       (t ovwrt)))))
	((and (file-exists-p destname)
	      (eql ovwrt 'none))
	 (list nil 'none))
	((and (file-exists-p destname)
	      (eql ovwrt 'all))
	 (list t 'all))
	(t (list t ovwrt))))
	
(defun nc-copy-file (sourcename destname ovwrt)
  "Copy sourcename to destname.
Handle overwrite mode.
Return new status of ovwrt-flag"
  (let ((test (nc-check-file-op destname "Overwrite existing" ovwrt)))
    (cond ((car test)
	   (copy-file sourcename destname
		      t)
	   (setq nc-file-mod
		 (cons (cons (expand-file-name sourcename) destname)
				   nc-file-mod))))
    (car (cdr test))))

(defun nc-move-file (sourcename destname ovwrt)
  "Move/Rename sourcename to destname."
  (let ((test (nc-check-file-op destname "While moving, overwrite" ovwrt)))
    (cond ((car test)
	   (rename-file sourcename destname
		      t)
	   (setq nc-file-mod (cons (cons (expand-file-name sourcename) destname)
				   nc-file-mod))))
    (car (cdr test))))

(defun nc-delete-file (sourcename destname ovwrt)
  "delete sourcefile"
  (let ((test (nc-check-file-op sourcename "Delete" ovwrt)))
    (cond ((car test)
	   (if (file-directory-p sourcename)
	       (delete-directory sourcename)
	     (delete-file sourcename))
	   (setq nc-file-mod (cons (cons (expand-file-name sourcename) nil)
				   nc-file-mod))))
    (car (cdr test))))

(defun nc-operate-on-files (sourcdest operation &optional ask)
  (setq nc-file-mod nil)
  (let ((source (cdr sourcdest))
	(target (car sourcdest))
	(dest nil)
	(ovwrt-flag nil))
    (if (and (file-directory-p target)
	     (not (nc-valid-dirname-p target)))
	(setq target (format "%s/" target)))
    (cond ((file-directory-p target)
	   (setq dest
		 (mapcar '(lambda (x)
			    (format "%s%s"
				    target x))
			 source)))
	  (t (setq dest (list target))))
    (let ((len (- (length source) 1))
	  (ovwrt ask))
      (while (>= len 0)
	(setq ovwrt
	      (funcall operation
		       (elt source len) (elt dest len) ovwrt))
	(setq len (- len 1))))))

(defun nc-copy (target)
  "Copy one or more files"
  (interactive
   (list (nc-get-target-dir-or-file "Copy"
				    nil
				    (if (memq 'control
					      (event-modifiers
					       last-nonmenu-event))
					default-directory))))
  (nc-operate-on-files target 'nc-copy-file)
  (if nc-file-mod (nc-update-all-modifications nc-file-mod nil t)))

(defun nc-move (target)
  "Move or rename one or more files"
  (interactive
   (list (nc-get-target-dir-or-file "Move/Rename"
				    nil
				    (if (memq 'control
					      (event-modifiers
					       last-nonmenu-event))
					default-directory))))
  (nc-operate-on-files target 'nc-move-file)
  (if nc-file-mod (nc-update-all-modifications nc-file-mod t t)))  

(defun nc-delete (target)
  "Delete one or more files"
  (interactive
   (list (nc-get-target-dir-or-file "Delete" t)))
  (nc-operate-on-files target 'nc-delete-file 'always)
  (if nc-file-mod (nc-update-all-modifications nc-file-mod t nil)))

(defun nc-sort-one-buffers-files ()
  (setq nc-files
	(cons (car nc-files)
	      (nc-sort-dired-files
	       (mapcar '(lambda (x)
			  (list
			   (nc-filename-norm
			    (elt x 5)
			    (elt x 1))
			   (elt x 1)
			   (elt x 2)
			   (elt x 3)
			   (elt x 4)
			   (elt x 5)
			   (elt x 6)
			   (elt x 7)))
		       (cdr nc-files))))))

(defun nc-resized ()
  (interactive)
  (nc-mode)
  (setq nc-cl
	(if (eql system-type 'ms-dos)
	    12
	  (- (/ (- (frame-width) 5) 6) 1)))
  (setq nc-suffix-length
	(cond ((eql system-type 'ms-dos)
	       3)
	      (t nc-suffix-length))
	;;  ((< nc-cl 13)
	;;   3)
	;;  ((< nc-cl 17)
	;;   4)
	;;  (t 5))
	)
  (setq nc-nl
	(if (eql system-type 'ms-dos)
	    18
	  (- (frame-height) 7)))
  (setq buffer-read-only nil)
  (kill-region (point-min) (point-max))
  (nc-sort-one-buffers-files)
  (setq nc-first-column 0
	nc-cursor-col 0
	nc-cursor-lin 0
	nc-format-align-right 0
	nc-format (nc-format-files-for-buffer (cdr nc-files)))
  (nc-adjust-screen)
  (nc-display-buffer)
  (nc-setup-cursor)
  (setq buffer-read-only t)
  (other-window 1)
  (setq buffer-read-only nil)
  (kill-region (point-min) (point-max))
  (nc-sort-one-buffers-files)
  (setq nc-first-column 0
	nc-cursor-col 0
	nc-cursor-lin 0
	nc-format-align-right 0
	nc-format (nc-format-files-for-buffer (cdr nc-files)))
  (nc-adjust-screen)
  (nc-display-buffer)
  (nc-setup-cursor)
  (nc-deselect-buffer)
  (setq buffer-read-only t)
  (other-window 1)
  (nc-select-buffer))
  
(defun nc-sort-by-name ()
  (interactive)
  (cond ((null nc-sort-by-name)
	 (setq nc-sort-by-name t)
	 (nc-resized))))

(defun nc-sort-by-time ()
  (interactive)
  (cond (nc-sort-by-name
	 (setq nc-sort-by-name nil)
	 (nc-resized))))  

(defun nc-byte-compile ()
  "Byte compile the file in File Browser"
  (interactive)
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 (ncbuf (current-buffer))
	 (filename (file-truename (elt file 5))))
    (cond ((string= (substring filename
			       (- (length filename)
				  3))
		    ".el")
	   (byte-compile-file filename)
	   (nc-update-all-modifications
	    (list (cons nil (format "%sc"
				    ;(car nc-files)
				    filename)))
	    nil t))
	  (t (message "I will only byte-compile files ending .el !")))))

(defun nc-quit (flag)
  "leave File Browser"
  (interactive
   (list (y-or-n-p "Sure you want to quit File Browsing? ")))
  (if flag
      (let ((bufs nc-buffers)
	    (buffers (buffer-list)))
	(while bufs
	  (cond ((memq (car bufs) buffers)
		 (switch-to-buffer (car bufs))
		 (setq nc-associated-nc-buffer nil)
		 (kill-buffer (car bufs))))
	  (setq bufs (cdr bufs)))
	(setq nc-buffers nil)
	(delete-other-windows)
	(message "Bye... Type `M-x nc' to invoke me again"))))

(defun nc-define-drive-keys ()
  (define-key nc-mode-map "a" 'nc-change-drive)
  (define-key nc-mode-map "b" 'nc-change-drive)
  (define-key nc-mode-map "c" 'nc-change-drive)
  (define-key nc-mode-map "d" 'nc-change-drive)
  (define-key nc-mode-map "e" 'nc-change-drive)
  (define-key nc-mode-map "a" 'nc-change-drive)
  (define-key nc-mode-map "g" 'nc-change-drive)
  (define-key nc-mode-map "h" 'nc-change-drive)
  (define-key nc-mode-map "i" 'nc-change-drive)
  (define-key nc-mode-map "j" 'nc-change-drive)
  (define-key nc-mode-map "k" 'nc-change-drive)
  (define-key nc-mode-map "l" 'nc-change-drive)
  (define-key nc-mode-map "m" 'nc-change-drive)
  (define-key nc-mode-map "n" 'nc-change-drive)
  (define-key nc-mode-map "o" 'nc-change-drive)
  (define-key nc-mode-map "p" 'nc-change-drive)
  (define-key nc-mode-map "q" 'nc-change-drive)
  (define-key nc-mode-map "r" 'nc-change-drive)
  (define-key nc-mode-map "s" 'nc-change-drive))

(defun nc-set-keys ()
  (define-key nc-mode-map [27] nc-local-esc-map)
  ;(local-set-key [27 27] 'nc)
  (define-key nc-mode-map  [14] 'nc-cursor-down)
  (define-key nc-mode-map  [16] 'nc-cursor-up)
  (define-key nc-mode-map  [2] 'nc-cursor-left)
  (define-key nc-mode-map  [6] 'nc-cursor-right)
  (define-key nc-mode-map  [\down] 'nc-cursor-down)
  (define-key nc-mode-map  [\up] 'nc-cursor-up)
  (define-key nc-mode-map  [\left] 'nc-cursor-left)
  (define-key nc-mode-map  [\right] 'nc-cursor-right)
  (define-key nc-mode-map [\down-mouse-1] 'nc-set-cursor)
  (define-key nc-mode-map [?\r] 'nc-change-dir)
  (define-key nc-mode-map [?\t] 'nc-other-buffer)
  (define-key nc-mode-map [\home] 'nc-cursor-home)
  (define-key nc-mode-map [\prior] 'nc-cursor-home)
  (define-key nc-mode-map [\end] 'nc-cursor-end)
  (define-key nc-mode-map [\next] 'nc-cursor-end)
  (define-key nc-mode-map [\insert] 'nc-select)
  (define-key nc-mode-map "+" 'nc-select-pattern)
  (define-key nc-mode-map "-" 'nc-deselect-pattern)
  (define-key nc-mode-map [\f1] 'nc-display-help-short)
  (define-key nc-mode-map [\f2] 'nc-display-new-dir)
  (define-key nc-mode-map [C-f2] 'nc-resized)
  (define-key nc-mode-map [\f3] 'nc-view)
  (define-key nc-mode-map [\f4] 'nc-edit)
  (define-key nc-mode-map [\f5] 'nc-copy)
  (define-key nc-mode-map [C-f5] 'nc-copy)
  (define-key nc-mode-map [\f6] 'nc-move)
  (define-key nc-mode-map [C-f6] 'nc-move)
  (define-key nc-mode-map [\f7] 'nc-mkdir)
  (define-key nc-mode-map [\f8] 'nc-delete)
  (define-key nc-mode-map [\f10] 'nc-quit)
  (define-key nc-mode-map "\C-xo" 'nc-other-buffer)
  (define-key nc-mode-map "T" 'nc-sort-by-time)
  (define-key nc-mode-map "N" 'nc-sort-by-name)
  (define-key nc-mode-map "B" 'nc-byte-compile)
  (define-key nc-mode-map "\C-xb" 'nc-bufferswitch)
  (define-key nc-mode-map "Q" 'nc-quit)
  (if (eql system-type 'ms-dos)
      (nc-define-drive-keys)))

(defun nc-setup-current-buffer ()
  (goto-char 0)
  (setq major-mode 'nc-mode)
  (use-local-map nc-mode-map)
  (setq mode-name "NC")
;  (make-local-variable 'stack-trace-on-error)
;  (setq stack-trace-on-error t)
  (make-local-variable 'nc-cursor-col)
  (make-local-variable 'nc-cursor-lin)
  (make-local-variable 'nc-files)
  (make-local-variable 'nc-first-column)
  (make-local-variable 'nc-format)
  (make-local-variable 'nc-format-align-right)
  (make-local-variable 'nc-title-start)
  (make-local-variable 'nc-title-end)
  (make-local-variable 'nc-selected-files)
  (make-local-variable 'nc-associated-nc-buffer)
  (make-local-variable 'nc-file-mod)
  (setq nc-associated-nc-buffer (current-buffer))
  (setq nc-first-column 0
	nc-cursor-col 0
	nc-cursor-lin 0))

(defun nc-deselect-buffer ()
  (setq buffer-read-only nil)
  (put-text-property nc-title-start nc-title-end 'face 'default)
  (setq buffer-read-only t)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin))

(defun nc-select-buffer ()
  (setq buffer-read-only nil)
  (put-text-property nc-title-start nc-title-end 'face 'nc-highlight-face)
  (setq buffer-read-only t)
  (nc-show-cursor nc-cursor-col nc-cursor-lin))

(defun nc-switch-to-buffers ()
  "Select buffers for File Browser"
  (let ((buffers (buffer-list)))
    (cond ((and nc-buffers
		(memq (car nc-buffers) buffers)
		(memq (car (cdr nc-buffers)) buffers))
	   (switch-to-buffer (car nc-buffers))
	   (goto-char 0)
	   (setq buffer-read-only t)
	   (delete-other-windows)
	   (split-window-horizontally)
	   (switch-to-buffer-other-window (car (cdr nc-buffers)))
	   (goto-char 0)
	   (setq buffer-read-only t))
	  (t
	   (switch-to-buffer "*NC 1*")
	   (nc-setup-current-buffer)
	   (setq nc-buffers (list (current-buffer)))
	   (setq buffer-read-only t)
	   (delete-other-windows)
	   (split-window-horizontally)
	   (switch-to-buffer-other-window "*NC 2*")
	   (nc-setup-current-buffer)
	   (setq nc-buffers (list (car nc-buffers) (current-buffer)))))))

(defun nc-setup-cursor ()
  (if (and (boundp 'nc-cursor-col)
	   (boundp 'nc-cursor-lin)
	   (boundp 'nc-first-column))
      nil
    (setq nc-cursor-col 0
	  nc-cursor-lin 0
	  nc-first-column 0))
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-modeline))

(defun nc-delete-cursor (col lin)
  (let ((pos (nc-absolute-from-relative-pos col lin)))
    (setq buffer-read-only nil)
    (put-text-property pos (+ pos nc-cl) 'face 'default)
    (if (memq (nc-num-from-col-lin col lin) nc-selected-files)
	(nc-show-select col lin))
    (setq buffer-read-only t)))

(defun nc-show-select (col lin)
  (let ((pos (nc-absolute-from-relative-pos col lin)))
    (put-text-property (+ pos (if (= 0 nc-suffix-length)
				  0
				(- nc-cl nc-suffix-length 1)))
		       (+ pos (if (= 0 nc-suffix-length)
				  1
				(- nc-cl nc-suffix-length)))
		       'face 'nc-select-face)))

(defun nc-delete-select (col lin)
  (let ((pos (nc-absolute-from-relative-pos col lin)))
    (put-text-property (+ pos (if (= 0 nc-suffix-length)
				  0
				(- nc-cl nc-suffix-length 1)))
		       (+ pos (if (= 0 nc-suffix-length)
				  1
				(- nc-cl nc-suffix-length)))
		       'face 'default)))

(defun nc-show-cursor (col lin)
  (let ((pos (nc-absolute-from-relative-pos col lin)))
    (setq buffer-read-only nil)
    (goto-char pos)
    (put-text-property (+ pos 1) (+ pos nc-cl) 'face 'nc-highlight-face)
    (if (memq (nc-num-from-col-lin col lin) nc-selected-files)
	(nc-show-select col lin))
    (setq buffer-read-only t)))

(defun nc-write-title (title &optional toponly)
  "write TITLE line in File Browser."
  (let* ((title-length (length title))
	 (half-rand (- (/ (- (* 3 (+ 1 nc-cl))
			     title-length) 2) 2))
	 (rand (if (> half-rand 0)
		   (make-string half-rand nc-horiz-line-char)
		 nil))
	 (count nc-nl)
	 (title-line (if rand
			 (format "%s %s %s" rand title
				 (make-string (- (- (* 3 (+ 1 nc-cl)) 4)
						 title-length
						 half-rand)
					      nc-horiz-line-char))
		       (format " %s....%s "
			       (substring title 0
					 nc-cl)
			       (substring title
					  (- title-length
					     (- (* 2 (+ 1 nc-cl)) 7))))
;		       (format " %s... "
;			       (substring title 0
;					  (- (* 3 (+ 1 nc-cl)) 7)))
		       )))
    (insert (format "%s "
		    title-line))
    (cond ((null toponly)
	   (newline)
	   (insert (format " Name:%s%c Name:%s%c Name:%s"
			   (make-string (- nc-cl 6) 32)
			   nc-verti-line-char
			   (make-string (- nc-cl 6) 32)
			   nc-verti-line-char
			   (make-string (- nc-cl 7) 32)))
	   (newline) 
	   (while (> count 0)
	     (insert (format "%s%c%s%c%s"
			     (make-string nc-cl 32)
			     nc-verti-line-char
			     (make-string nc-cl 32)
			     nc-verti-line-char
			     (make-string nc-cl 32)))
	     (newline)
	     (setq count (- count 1)))
	   (insert (make-string (- (* 3 (+ 1 nc-cl)) 2)
				nc-horiz-line-char))
	   (newline)))
    (cond ((> half-rand 0)
	   (setq nc-title-start (+ half-rand 2)
		 nc-title-end (+ half-rand title-length 2)))
	  (t
	   (setq nc-title-start 1
		 nc-title-end (- (* 3 (+ 1 nc-cl)) 2))))
    (put-text-property nc-title-start nc-title-end 'face 'nc-highlight-face)
    ))
				     
(defun nc-find-files-for-buffer (&optional re-read)
  "get the list of files to display in BUFFER.
If RE-READ or no files in cache, read directory"
  (let ((dirname (car nc-files))
	(liste nc-files))
    (if (and (cdr liste)
	     (null re-read))
	(cdr liste)
      (rplacd liste
	      (nc-dired-to-list dirname)))
    liste))

(defun nc-format-files-for-buffer (file-list)
  "Format files in FILE-LIST in appropriate
columns (lists) and lines (the entrys in list)"
  (if (not (listp (car file-list)))
      (setq file-list (cdr file-list)))
  (let ((erg nil)
	(ergaux nil)
	(count 0)
	(mylist file-list))
    (while mylist
      (setq ergaux nil
	    count 0)
      (while (and mylist
		  (< count nc-nl)
		  (or (> (length erg) 0)
		      (< count (- nc-nl nc-format-align-right))))
	(setq ergaux (cons (car (car mylist))
			   ergaux))
	(setq mylist (cdr mylist)
	      count (+ count 1)))
      (if ergaux
	  (setq erg (cons (reverse ergaux)
			  erg))))
    (reverse erg)))

(defun nc-display-column (cols)
  "display the contents of columns n"
  (let* ((columns '(0 1 2))
	 zeile spalte)
    (setq buffer-read-only nil)
    (goto-char 0)
    (kill-line)
    (nc-write-title (car nc-files) t)
    (while columns
      (cond ((memq (+ (car columns) nc-first-column)
		   cols)
	     (setq spalte (elt nc-format
			       (+ (car columns) nc-first-column)))
	     (if (< (length spalte) nc-nl)
		 (setq spalte (append (copy-sequence spalte)
				      (make-list
				       (- nc-nl (length spalte))
				       (make-string nc-cl 32)))))
	     (goto-line 3)
	     (setq zeile 0)
	     (while spalte
	       (beginning-of-line)
	       (forward-char (+ (* (car columns)
				   (+ nc-cl 1))))
	       (insert (car spalte))
	       (delete-char  nc-cl)
	       (next-line 1)
	       (setq zeile (+ 1 zeile)
		     spalte (cdr spalte))
	       )))
      (setq columns (cdr columns)))
    (goto-char 0)
    (setq buffer-read-only t)
    ))

(defun nc-show-selected-files ()
  (let ((start (- (* nc-nl nc-first-column) nc-format-align-right))
	(end (- (* nc-nl (+ nc-first-column 3)) nc-format-align-right))
	(liste nc-selected-files))
    (setq buffer-read-only nil)
    (while liste
      (cond ((and (memq (car liste)
			nc-selected-files)
		  (>= (car liste) start)
		  (< (car liste) end))
	     (nc-show-select
	      (nc-col-from-num (car liste))
	      (nc-lin-from-num (car liste)))))
      (setq liste (cdr liste)))
    (goto-char 0)
    (setq buffer-read-only t)))

(defun nc-display-buffer (&optional re-read)
  "display the contents of directory dir."
  (let* ((files (nc-find-files-for-buffer  re-read))
	 (format (nc-format-files-for-buffer files))
	 (columns '(0 1 2))
	 zeile spalte)
    (setq nc-format format) 
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (nc-write-title (car files))
    (while columns
      (setq spalte (elt format
			(+ (car columns) nc-first-column)))
      (goto-line 3)
      (setq zeile 0)
      (while spalte
	(beginning-of-line)
	(forward-char (+ (* (car columns)
			    (+ nc-cl 1))))
	(insert (car spalte))
	(delete-char  nc-cl) 
	(next-line 1)
	(setq zeile (+ 1 zeile)
	      spalte (cdr spalte)))
      (setq columns (cdr columns)))
    (nc-show-selected-files)
    (setq buffer-read-only t)
    ))

(defun nc-display-help-short ()
  (interactive)
  (message "f2: dir f3: view f4: edit f5: copy f6: move f7: mkdir f8: del f10: quit"))

(defun nc-write-modeline ()
  (setq buffer-read-only nil)
  (save-excursion
    (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	   (file (elt (cdr nc-files)
		      abspos)))
      (goto-char (+ 1
		    (* 6 (+ nc-cl 1))
		    (* nc-nl 3 (+ nc-cl 1))
		    (- (* 3 (+ 1 nc-cl)) 2)))
      (kill-region (+ 1
		      (* 6 (+ nc-cl 1))
		      (* nc-nl 3 (+ nc-cl 1))
		      (- (* 3 (+ 1 nc-cl)) 2))
		   (point-max))
      (if nc-selected-files
	  (let ((totsize 0))
	    (mapcar '(lambda (x)
		      (setq totsize
			(+ totsize
			 (elt (elt  nc-files (+ 1 x))
			  2))))
		    nc-selected-files)
	    (insert (format "%s bytes in %s files"
			    totsize (length nc-selected-files)))))
      (cond ((or (null nc-selected-files)
		 (not (eql system-type 'ms-dos)))
	     (if  nc-selected-files
		 (newline))
	     (insert (format "%s  %s  " (elt file 5)
			     (cond  ((elt file 7)
				     (format "-> %s" (elt file 7)))
				    ((elt file 1)
				     ">>DIR<<")
				    (t (elt file 2)))))
	     (insert (format-time-string
		      nc-time-format
		      (elt file 3)))
	     (insert (format ", %s"
			     (elt file 4)))))))
  (setq buffer-read-only t)
  ;(goto-char 0)
  (nc-display-help-short))
