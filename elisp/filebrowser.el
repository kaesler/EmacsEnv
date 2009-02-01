;;; filebrowser.el --- file browser mode commands for Emacs

;; $Id: filebrowser.el,v 0.9 1996/05/07 00:09:15 glenn Exp $

;; Copyright (C) 1995 Glenn Moloney

;; Author: Glenn Moloney (glenn@physics.unimelb.edu.au)

;; This file is not part of GNU emacs

;; LCD Archive Entry:
;; filebrowser|Glenn Moloney|glenn@physics.unimelb.edu.au|
;; file browser mode commands for Emacs|
;; 07-May-1996|$Revision: 0.9 $|~/misc/filebrowser.el.gz|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; This package is a major mode for file browsing. It is like any
;; common file browser - like Xtree, windows file manager, etc... But,
;; it is very basic at the moment - I expect it will get extended with
;; useful features in the fullness of time. I wrote it primarily for
;; handling large multi-file software projects. Most cases I find I
;; don't want the long listings of dired, but I do want to navigate
;; around directory trees. I really like this mode, and I've been
;; wanting something like this for emacs for some time.

;; The directory browser-mode is derived from outline-mode and also uses
;; foldout mode. This is a good example of the benefit of derived.el.

;; Requires: outline.el foldout.el derived.el easymenu.el
;; These are all part of emacs-19.28.

;; Two modes are provided: "browser-mode" and "browser-file-mode".

;; To use the file browser include something like the following in
;; your `~/.emacs' file.

;;  ;; Auto-load the file browser
;;  (autoload 'browse
;;    "filebrowser" "File and directory tree browser" t)
;;  (autoload 'browse-other-window
;;    "filebrowser" "File and directory tree browser" t)
;;  (autoload 'browse-other-frame
;;    "filebrowser" "File and directory tree browser" t)
;;  (global-set-key "\C-cb"	'browse)
;;  (global-set-key "\C-co"	'browse-other-window)
;;  (global-set-key "\C-cf"	'browse-other-frame)
;;  (global-set-key [f5]	'browse-other-frame)

;; Then just use `C-c f' and you will be prompted for a directory
;; node to scan.

;;; Customisation:

;; As of version 0.6, filebrowser uses the `custom' package to provide customisation
;; As of version 0.6, filebrowser performs colour highlighting of
;; files in the file listing buffer. To customise this use the
;; `browser-set-face-list' function.

;; The following is an example of some initialisation code to
;; customise the faces used to display files in the file listing
;; window, and to set up some dynamic file operations menus for use in
;; the listing window. You could put something like this in your
;; ~/.emacs file.

;;  ;; Inhibit browser file listing of emacs auto-save files
;;  (set-default 'browser-file-inhibit-regexp "^#.*#$\\|^\\.saves-")
;;  (set-default 'browser-inhibit-dots t)
;;
;;  (set-face-foreground (copy-face 'default 'VioletRed) "VioletRed")
;;  (set-face-foreground (copy-face 'default 'SlateBlue) "SlateBlue")
;;  (set-face-foreground (copy-face 'default 'grey40) "grey40")
;;  (set-face-foreground (copy-face 'default 'Purple) "Purple")
;;  (browser-set-face-list
;;   '(("*.[cfp] *.el *.tex" . SlateBlue)
;;     ("*.h" . Purple)
;;     ("*.gz" . VioletRed)
;;     ("*.o *.elc *.dvi *.ps *.aux *.log" . grey40)))
;;
;;  ;; This is a list of globbing wildcards and "easymenu" menus. If
;;  ;; the selected file matches the wildcard of a submenu, the
;;  ;; submenu will be added to the popup file operations menu.
;;  (browser-add-menu
;;   '(("[mM]akefile"
;;      ["Make"				compile t])
;;     ("News"
;;      ["GNUS"				gnus t])
;;     ("*.tar"
;;      ["Untar Archive"		(browser-shell "tar -xf %s") t]
;;      ["List Archive"			(browser-shell "tar -tvf %s") t])
;;     ("*.(tar.gz|tar.[zZ]|tgz)"
;;      ["UnPack Compressed Archive"	(browser-shell "tar -zxf %s") t]
;;      ["List Compressed Archive"	(browser-shell "tar -ztvf %s") t])
;;     ("Xdefaults\\|Xresources"
;;      ["Load X Resources"		(browser-shell "xrdb -merge %s") t])
;;     ("*.gif *.jpg *.xwd *.xpm)"
;;      ["View Image"			(browser-shell "xv %s &") t])
;;     ("*.ps *.eps *.epsf"
;;      ["View Postscript"		(browser-shell "ghostview %s &") t]
;;      ["Print Postscript"		(browser-shell "lpr %s &") t])
;;     ("*.dvi"
;;      ["View TeX DVI"			(browser-shell "xdvi %s &") t]
;;      ["Print TeX DVI"		(browser-shell "dvips %s &") t])
;;     ("TAGS"
;;      ["Etags"			(browser-shell "etags *.[ch] *.el") t])
;;     ("*.html"
;;      ["Browse URL"			(mapcar 'browse-url-of-file (mapcar 'expand-file-name browser-fileops-files)) t])
;;     ("*,v"
;;      ["RCS checkin"			(browser-shell "ci %s") t]
;;      ["RCS checkout"			(browser-shell "co %s") t]
;;      ["RCS checkout & lock"		(browser-shell "co -l %s") t]
;;      ["RCS info"			(browser-shell "rlog %s") t])
;;     ))

;;; NOTES:

;; The syntax table in the browser tree and file buffers are modified
;; so that all but whitespace chars are considered to have "word"
;; syntax.

;; The directory tree listing will list symlinks, which are links to
;; directories, but does not recurse into those directories. You can
;; scan the subtree by "re-scanning" that entry (ie. move point to a
;; symlink entry and press the ` ' key. Symlinks are identified by the
;; "@" symbol after the file name, or by colour highlighting or both
;; (see the documentation on the variable `browser-flag-file-type').

;; Directory entries which have been scanned, or are known to be empty
;; are listed with a trailing "/". Unscanned directories have no
;; trailing "/". This is used as a flag to the user, and to the
;; software to semi-automatically re-scan directories opened with the
;; "c" (browser-show-children), "s" (browser-show-subtree) and " "
;; (browser-do-listing) keys.

;; Mouse button 2 is used for the simple operations in the tree and
;; file listing windows. Button 2 on a file entry will cause Emacs to
;; visit that file in another frame. Button 2 on a directory entry in
;; the tree listing will re-scan that directory node if required, and
;; display the contents of the directory in the listing window.

;; Mouse button 3 is used to popup a menu of more complex file
;; operations. The menu can be configured to be context-sensitive
;; (depending on the filename clicked on). This menu can also be
;; accessed from the Operate menu in the menubar.

;; Shift-mouse-button-1-down begins a "drag&drop" operation to copy a
;; file. This is very primitive, and needs work (see TODO below). The
;; file entry is highlighted and you can drag the mouse to another
;; listing window or any directory entry in the directory tree window
;; to copy the file to that directory.

;; Visiting a file while in the file listing window (by pressing
;; button-2 or key ` ') will display the buffer via
;; `find-file-other-window'. Since the browser uses dedicated windows,
;; this will always popup in other windows or frames.

;;; HISTORY:

;; I originally used "find" to generate the recursive directory
;; listings and "ls" to generate the file listings, but I found better
;; performance using (directory-files dirname). This also gives better
;; portability. I am considering moving to using a co-process
;; (asynchronous) to generate the listings - I don't want the overhead
;; of starting up processes on each invocation.

;; I previously used hilit19 and the browser-file-display-hook to do
;; the colour highlighting of the filenames in the file listing
;; buffer. The internal method I use now is much faster for display
;; and especially redisplay. It is also more convenient as filename
;; globbing wildcards are usually easier for people than regular
;; expressions.

;;; BUGS:

;; Emacs version 19.29 has a bug with "activate-menubar-hook". The
;; hook functions are invoked inappropriately, which adds a
;; significant overhead to normal emacs operation. I have therefore
;; disabled the dynamic context sensitive "Operate" menubar item for
;; emacs-19.29.  RMS has reported that this will be fixed in
;; emacs-19.30. (It has been fixed :-). The context sensitive menu is
;; still available by pressing down-mouse-3 over a filename in the
;; file listing window.

;; Support for ange-ftp has been added, but ange-ftp does not
;; correctly implement file-executable-p, so all files appear to be
;; executable in the file listing windows. Perhaps it would be better
;; if ange-ftp interpreted the long ftp listings to infer the file
;; timestamps and attributes. Perhaps I need a work around.

;; Caching of directory listings can cause problems if a directory has
;; been previously deleted from the tree listing. It will not be
;; re-scanned into the tree listing unless a re-read is forced (`g' in
;; the file listing window).

;; Drag and drop of files does not work between frames. There appears
;; to currently be no way to implement this in Emacs. Tracking mouse
;; movements outside the current frame still provides coords relative
;; to the current frame. There is no easy way to determine which frame
;; the mouse has been dragged into - then what position the mouse was
;; dragged to in which window in that frame.

;;; TODO:

;; The drag&drop needs some sort of fancy user feedback to indicate a
;; drag&drop operation is under way. Can I change the mouse shape to
;; reflect the operation (eg. nice little folder shaped cursor being
;; dragged by mouse)? Can I drag a little x-popup around with the name
;; of the file in it? Can I perform some sort of mouse grab operation?
;; Any other ideas?

;; Dreaming: the next obvious feature is to go one step further and
;; intregrate the file browser with `shell', to get a file and program
;; interface. Hmm, is this a good idea - I'm not sure?

;;; Code:

(require 'outline)
(require 'foldout)

(defvar browser-use-custom
  (condition-case nil
      (require 'custom)
    (error nil))
  "Should we use custom.el - only if it is available.")

;; User options

(defvar browser-use-dired nil
  "*Use `dired' mode to provide the file listings.
Dired does not interact so usefully with the directory tree, but may
be preferred by those who like the `long listings' of dired.")

(defvar browser-use-dired-narrowing nil
  "*Use narrowing in `dired' mode to show only the subdir of interest.
This is useful, but does cause problems with dired as some commands in
dired fail when narrowing is in effect.")

(defvar browser-indent-string " "
  "*String to use as indentation for subdirectories in the tree listing.")
(put 'browser-indent-string 'variable-interactive "s")

(defvar browser-pre-indent-string " "
  "*String to use as pre-indentation for subdirectories in the tree listing.
The pre-indentation is the leading string for all entries in the
directory tree listing.")
(put 'browser-pre-indent-string 'variable-interactive "s")

(defvar browser-dir-depth 1
  "*Default depth to scan directory trees on each read.
A negative value causes a complete read of the entire directory tree.")
(put 'browser-dir-depth 'variable-interactive "n")

(defvar browser-dir-max-depth 50
  "*The maximum depth to recursively scan directory trees on each read.
A negative value removes the limit.")
(put 'browser-dir-max-depth 'variable-interactive "n")

(defvar browser-info-switches "-ldqF"
  "*The command line switches for generating the detailed file information.
This will be passed to the `ls' command as command line switches.")
(put 'browser-info-switches 'variable-interactive "s")

(defvar browser-tree-window-width 18
  "*Width (in columns) of the directory tree window.
The File listing windows get the remaining width in the current frame.")
(put 'browser-tree-window-width 'variable-interactive "n")

(defvar browser-pop-up-frames nil
  "*If non-nil, file browser will open files in new frames.
`pop-up-frames' is set local to each listing buffer and initialised
from `browser-pop-up-frames'. If `pop-up-frames' is set non-nil,
`display-buffer' will display any buffers in new frames.")

(defvar browser-inhibit-dots t
  "*Enable suppression of filenames which begin with a `.'.")

(defvar browser-tree-regexp nil
  "*Regular expression matching directories to be shown in the tree.
An empty string (or `nil') will match all directories, else only the
directories which match the expression will be listed.")
(put 'browser-tree-regexp 'variable-interactive "s")

(defvar browser-tree-inhibit-regexp nil
  "*Regular expression matching directories to be suppressed in the tree.
An empty string (or `nil') will match no directories, else the directories
which match the expression will be suppressed from the listing.")
(put 'browser-tree-inhibit-regexp 'variable-interactive "s")

(defvar browser-default-tree-root "~/"
  "*Default directory for the File Browser popup windows.")
(put 'browser-default-directory 'variable-interactive "s")

(defvar browser-directory-face
  (progn
    (copy-face 'default 'browser-directory-face)
    (set-face-foreground 'browser-directory-face "Black")
    'browser-directory-face)
  "*Face for displaying directories in the file listing.")

(defvar browser-symlink-face
  (progn
    (copy-face 'default 'browser-symlink-face)
    (set-face-foreground 'browser-symlink-face "FireBrick")
    'browser-symlink-face)
  "*Face for displaying symbolic links in the file listing.")

(defvar browser-executable-face
  (progn
    (copy-face 'default 'browser-executable-face)
    (set-face-foreground 'browser-executable-face "ForestGreen")
    'browser-executable-face)
  "*Face for displaying executable files in the file listing.")

(defvar browser-highlight-face
  (progn
    (copy-face 'default 'browser-highlight-face)
    (set-face-background 'browser-highlight-face "LightBlue")
    'browser-highlight-face)
  "*Face for displaying highlighted files in the browser buffers.")

;; Internal variables

(defvar browser-highlight-overlay nil
  "Highlight the directory currently in the listing buffer.")

(defvar browser-highlight-overlay-list nil
  "List of overlays for highlighting tagged files in the file listing.")

(defvar browser-buffer-list nil
  "The list of buffers associated with the browser session.
The first buffer in the list is the directory tree buffer.")

(defvar browser-tree-buffer nil
  "The buffer which holds the directory tree listing.
Returns the directory tree buffer associated with the current buffer.")

(defvar browser-mode-syntax-table nil
  "Syntax table used while in browser mode.
Default sets the syntax table so all filename chars have `word' syntax.
So, any valid filename can be considered as a `word'.
This helps for motion and word selection with the mouse.")

(if browser-mode-syntax-table
    ()
  (setq browser-mode-syntax-table (make-syntax-table))
  (let ((i 0))
    (while (< i 256)
      (modify-syntax-entry i "w" browser-mode-syntax-table)
      (setq i (1+ i))))
  (modify-syntax-entry ?\  " " browser-mode-syntax-table)
  (modify-syntax-entry ?\t " " browser-mode-syntax-table)
  (modify-syntax-entry ?\n " " browser-mode-syntax-table)
  (modify-syntax-entry ?\f " " browser-mode-syntax-table)
  (modify-syntax-entry ?\r " " browser-mode-syntax-table))

(defun browser-display-buffer ( buffer &optional not-this-window )
  (let ((pop-up-windows t)
	(pop-up-frames browser-pop-up-frames)
	(display-buffer-function nil))
    (display-buffer buffer not-this-window)))
  
;; Define the browser directory listing mode
(define-derived-mode browser-mode outline-mode "FileBrowser"
  "Major mode for file and directory tree browsing.

The FileBrowser provides a filemanager interface for Emacs.
Operation is similar to other common filebrowsers, and provides:
  - simple point-and-click navigation of a directory tree
  - complete file listings of selected directories
  - point-and-click to open a file in an Emacs buffer
  - file operations (delete, rename, copy, remove,...)
  - primitive drag-and-drop to copy or move files between directories.

The `browse' command prompts for a directory name, and accepts an
optional numeric prefix argument (eg. C-u 5 M-x browse).  The
FileBrowser will display two buffers side-by-side.

The left hand window shows the \"tree listing\" buffer. This buffer
contains a listing of the recursive sub-directory tree of the
directory specified. The subdirectory tree listing may optionally,
pre-scan the entire sub-directory tree, OR scan the subdirectory tree
dynamically as the user navigates around in the listing. A negative
numeric prefix arg tells the browser to pre-scan the subdirectory
tree, while a positive prefix arg will cause the browser to scan that
deep in the browser listing on each read. Each time the user navigates
into a subdir which has not been scanned it will be dynamically
re-scanned to this depth. The default scanning depth is `1'.

The right hand window shows the \"file listing\" buffer. A complete file
listing of any directory in the tree listing may be shown in this
buffer (use the Space bar in the tree listing window). Simple file
operations and file visiting can be performed (try `C-h m' in the file
listing window).

\\{browser-mode-map}.

Customisation:
==============

The following variables may be used to customise the behaviour of the
directory tree window:

browser-pop-up-frames
browser-tree-window-width
browser-dir-depth
browser-dir-max-depth
browser-info-switches
browser-inhibit-dots
browser-tree-regexp
browser-tree-inhibit-regexp
browser-use-dired
browser-use-dired-narrowing
browser-indent-string
browser-pre-indent-string
"
  (setq case-fold-search nil
	buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (auto-fill-mode -1)
  (if (fboundp 'make-local-hook)
      (progn
        (make-local-hook 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook 'browser-delete-buffer nil 'local))
    (add-hook 'kill-buffer-hook 'browser-delete-buffer nil))
  (set (make-local-variable 'outline-level) 'browser-outline-level)
  (set (make-local-variable 'browser-buffer-list) nil)
  (set (make-local-variable 'browser-tree-buffer) nil)
  (set (make-local-variable 'browser-highlight-overlay) nil)
  (set (make-local-variable 'pop-up-windows) t)
  (set (make-local-variable 'outline-regexp)
    (concat (regexp-quote browser-pre-indent-string)
	    "\\(" (regexp-quote browser-indent-string) "\\)*"))
  (set (make-local-variable 'display-buffer-function)
    'browser-display-buffer)
  (setq truncate-lines t)
  (clear-visited-file-modtime)
  (set-syntax-table browser-mode-syntax-table))

(defun browser-outline-level ()
  ;;"Return the depth to which a statement is nested in the outline.
  ;; Point must be at the beginning of a header line.  This is actually
  ;; the number of characters that `outline-regexp' matches divided by the
  ;; length of the browser-indent-string."
  (looking-at outline-regexp)
  (/ (- (match-end 0) (match-beginning 0) (length browser-pre-indent-string))
     (length browser-indent-string)))

(defun browser-delete-buffer ()
  ;;"Delete a filebrowser buffer and remove it from the browser buffer list."
  (let* ((buffer (current-buffer))
	 (window (get-buffer-window buffer)))
    ;; If buffer is not tree buffer - delete it from the list
    (if (not (eq buffer (car browser-buffer-list)))
	(delq buffer browser-buffer-list))
    (condition-case nil
	(delete-window window)
      (error nil))))

(defun killed-buffer-p (buffer)
  ;;"Return t if BUFFER is killed."
  (not (and buffer
	    (buffer-name buffer))))

(defun browser-check-buffer-list (buffer-list)
  "Check a list of buffers. Remove any killed buffers found.
The first buffer is a special case (the tree buffer), and is not deleted
if it has been killed."
  (interactive)
  (let ((b buffer-list))
    (while (setq b (cdr b))
      (if (killed-buffer-p (car b))
	  (delq (car b) buffer-list)))))

(defun browser-insert-tree-root (dirname)
  ;; Insert the root specification for the named directory
  (let ((buffer-read-only nil))
    (save-excursion
      (insert (abbreviate-file-name (file-name-as-directory dirname)) "\n")
      (insert browser-pre-indent-string ".\n"))))

(defun browser-goto-root ()
  ;; Go to the root of this file tree
  (forward-line 1)
  (re-search-backward "^[^ \t\r\n]" nil t))

(defun browser-end-of-tree ()
  ;; Go to the end of this file tree
  (if (re-search-forward "^[^ \t\r\n]" nil t)
      (forward-line -1)
    (goto-char (point-max))))

(defun browser-delete-tree ()
  "Delete the entire current file tree"
  (interactive)
  (browser-goto-root)
  (let ((buffer-read-only nil)
	(p (point)))
    (forward-line 1)
    (browser-end-of-tree)
    (forward-line 1)
    (delete-region p (point))))

(defun browser-set-root-name (dirname)
  ;; Set the name of the root directory for this file system
  (let ((browser-read-only nil))
    (save-excursion
      (and (browser-goto-root)
	   (looking-at "^..*$")
	   (replace-match (abbreviate-file-name
			   (file-name-as-directory
			    dirname))
			  (match-beginning 0) (match-end 0))))))

(defun browser-get-root-name ()
  ;; Return the name of the root directory of this file tree
  ;; Return `nil' if none found
  (save-excursion
    (and (browser-goto-root)
	 (looking-at "^..*$")
	 (buffer-substring (match-beginning 0) (match-end 0)))))

(defun browser-find-tree-root (dirname)
  ;; Find a file tree with the specified root directory
  ;; Returns `nil' if not found
  (let ((p (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^"
				   (regexp-quote
				    (abbreviate-file-name
				     (file-name-as-directory
				      dirname)))
				   "$") nil t)
	(progn
	  (beginning-of-line)
	  t)
      (goto-char p)
      nil)))

(defun browser-find-tree (dirname)
  ;; Find a file tree which contains the specified directory
  ;; Returns `nil' if not found
  (let ((tmpname "") found)
    (setq dirname (directory-file-name (expand-file-name dirname "/")))
    (goto-char (point-min))
    (while (and dirname
		(not (string-equal dirname tmpname))
		(not (and (browser-find-tree-root dirname)
			  (setq found t))))
      (setq tmpname dirname)
      (setq dirname (directory-file-name (file-name-directory dirname))))
    (beginning-of-line)
    found))

(defun browser-new-tree (dirname depth)
  "Insert a new directory tree in the tree buffer."
  (interactive "DBrowse (directory): \nP")
  (setq depth
	(if depth
	    (prefix-numeric-value depth)
	  browser-dir-depth))
  (widen)
  (goto-char (point-min))
  (insert "\n")
  (goto-char (point-min))
  (browser-insert-tree-root dirname)
  (forward-line 1)
  (browser-read-subdir depth))
 
(defun browser-reset-tree-root (dirname dirlist)
  ;; Modify the current directory tree to be rooted at DIRNAME.
  ;; DIRLIST is a list of the pathname directory elements of the
  ;; new DIRNAME.
  (let ((buffer-read-only nil)
	indent-string sbuf p)
    (browser-goto-root)
    (browser-set-root-name dirname)
    (if (not dirlist)
	()
      (forward-line 2)
      (setq indent-string (concat browser-pre-indent-string
				  browser-indent-string))
      (setq sbuf "")
      (while dirlist
	(insert-before-markers indent-string (car dirlist) "/\n")
	(setq indent-string (concat indent-string browser-indent-string))
	(setq sbuf (concat sbuf browser-indent-string))
	(setq dirlist (cdr dirlist)))
      (setq p (save-excursion (browser-end-of-tree) (point)))
      (while (and (looking-at outline-regexp)
		  (progn (goto-char (match-end 0))
			 (insert sbuf)
			 (forward-line 1)
			 (< (point) p)))))))

(defun browser-dir-list (dirname)
  ;; Split a full pathname into a list of subdir names
  (let (tmpname)
    (setq dirname (directory-file-name dirname))
    (let (dirlist tmpname)
      (while (and dirname
		  (not (string-equal dirname
				     (setq tmpname (directory-file-name
						    (file-name-directory
						     dirname))))))
	(setq dirlist (cons (file-name-nondirectory dirname) dirlist))
	(setq dirname tmpname))
      (setq dirlist (cons dirname dirlist)))))

(defun browser-find-dir (dirname)
  ;; "Find the location for the specified directory in the tree listing."
  (let ((buffer-read-only nil)
	rootlist dirlist indent-string end p filename)
    (setq dirlist (browser-dir-list (expand-file-name dirname "/")))
    (if (not (browser-find-tree dirname))
	(progn
	  (goto-char (point-max))
	  (insert "\n")
	  (browser-insert-tree-root (car dirlist))))
    (setq rootlist (browser-dir-list (expand-file-name (browser-get-root-name)
						       "/")))
    (setq dirname "")
    (while (and rootlist dirlist (string-equal (car rootlist)
					       (car dirlist)))
      (setq dirname (expand-file-name (car dirlist) dirname))
      (setq dirlist (cdr dirlist))
      (setq rootlist (cdr rootlist)))
    (if rootlist
	(browser-reset-tree-root dirname rootlist))
    (browser-goto-root)
    (forward-line 1)
    (setq indent-string (concat browser-pre-indent-string
				browser-indent-string))
    (setq end (browser-end-subdir))
    (while dirlist
      (setq filename (car dirlist))
      (show-children)
      (if (re-search-forward (concat "^"
				     (regexp-quote indent-string)
				     (regexp-quote filename)
				     "[@/]?[\r\n]")
			     end t)
	  (backward-char)
	;; The directory entry is not present - rescan
	(save-excursion
	  (browser-insert-dirlist (list filename)))
	(setq end (browser-end-subdir))
	(if (re-search-forward (concat "^"
				       (regexp-quote indent-string)
				       (regexp-quote filename)
				       "[@/]?[\r\n]")
			       end t)
	    (forward-line -1)
	  (error "Sub-directory %s not found in %s"
		 filename (browser-get-dir-name))))
      (setq dirlist (cdr dirlist))
      (setq end (browser-end-subdir))
      (setq indent-string (concat indent-string browser-indent-string)))
    (beginning-of-line))
  t)

(defun browser-make-dir-tree (dirname depth)
  ;; Scan a directory subtree to the specified depth.
  ;; Returns a recursive list of directory subtree lists
  ;; Each directory list is either:
  ;;   '("dirname" (subdir-list)) OR
  ;;   "dirname" if the directory is empty or not scanned.
  (if (< depth 1)
      nil
    (let ((handler (find-file-name-handler (expand-file-name dirname)
					   'directory-files))
	  filelist filename pathname dirlist dirlast)
      (setq depth (1- depth))
      (setq filelist
	    (if handler
		(funcall handler 'directory-files
			 dirname nil browser-tree-regexp)
	      (directory-files dirname nil browser-tree-regexp)))
      (setq dirname (directory-file-name dirname))
      (while filelist
	(setq filename (car filelist))
	(setq pathname (expand-file-name filename dirname))
	(if (and (file-directory-p pathname)
		 (not (or (and browser-inhibit-dots
			       (string-match "^\\." filename))
			  (string-match "^\\.\\.?$" filename)
			  (and browser-tree-inhibit-regexp
			       (string-match browser-tree-inhibit-regexp
					     filename)))))
	    (progn
	      (if (file-symlink-p pathname)
		  (progn
		    (setq filename (concat filename "@"))
		    (put-text-property 0 (length filename)
				       'face browser-symlink-face filename))
		(put-text-property 0 (length filename)
				   'face browser-directory-face filename)
		(if (> depth 1)
		    (progn		; Scan subdirectory recursively
		      (setq filename
			    (cons (concat filename "/")
				  (browser-make-dir-tree pathname depth)))
		      (if (eq (cdr filename) nil)
			  (setq filename (car filename))))))
	      (if dirlist
		  (progn
		    (setcdr dirlast (cons filename nil))
		    (setq dirlast (cdr dirlast)))
		(setq dirlist (cons filename nil)
		      dirlast dirlist))))
	(setq filelist (cdr filelist)))
      dirlist)))

(defun browser-internal-insert-dirlist (filelist indent-string)
  ;; Insert a file tree listing into the current buffer
  ;; Each element of FILELIST is either
  ;;   '("dirname" (sublist)) OR
  ;;   "dirname"
  ;; This is the recursive part of `browser-insert-dirlist
  (let (filename sublist fullname (end (make-marker)))
    (set-marker end (browser-end-subdir))
    (forward-line 1)
    (while filelist
      (setq filename (car filelist))
      (if (listp filename)
	  (setq sublist (cdr filename)
		filename (car filename)))
      (setq fullname filename)
      (if (string-match "^\\(.*\\)[@/]$" filename)
	  (setq filename (substring filename 0 (match-end 1))))
      (if (and browser-tree-inhibit-regexp
	       (string-match browser-tree-inhibit-regexp filename))
	  ()				; discard
	(while (and (< (point) end)
		    (string< (browser-get-file-name-only)
			     filename))
	  (browser-delete-subtree))
	(if (and (< (point) end)
		 (string-equal (browser-get-file-name-only)
			       filename))
	    (goto-char (browser-end-subdir))
	  (insert-before-markers indent-string fullname "\n"))
	(if sublist
	    (progn
	      (forward-line -1)
	      (browser-internal-insert-dirlist sublist
					       (concat indent-string
						       browser-indent-string))
	      (setq sublist nil))))
      (setq filelist (cdr filelist)))
    (delete-region (point) end)
    (set-marker end nil)))

(defun browser-insert-dirlist (filelist)
  ;; Prepare and insert a recursive directory tree listing after
  ;; the current entry
  (let ((buffer-read-only nil)
	indent-string)
    (beginning-of-line)
    (if (looking-at outline-regexp)
	(setq indent-string
	      (buffer-substring (match-beginning 0)
				(match-end 0))))
    (if (looking-at "[^\r\n]*[^/@\r\n]\\(@?\\)[\r\n]")
	(replace-match "/" t nil nil 1))
    (setq indent-string (concat indent-string
				browser-indent-string))
    (browser-internal-insert-dirlist filelist indent-string)))

(defun browser-recenter-dir ()
  ;; Adjust the view in the directory tree window to show the selected
  ;; directory and it's contents
  (let* ((beg (point))
	 (end (browser-end-subdir))
	 (window (get-buffer-window (current-buffer)))
	 (nlines (count-lines beg end))
	 (wlines (1- (window-height window)))
	 (wbeg (window-start window))
	 (wend (save-excursion
		 (goto-char wbeg)
		 (forward-line wlines)
		 (point))))
    (if (and window
	     (or (< beg wbeg)
		 (> end wend)))
	(save-excursion
	  (forward-line (min 0 (/ (- nlines wlines) 2)))
	  (set-window-start window (point))))
    (set-window-point window beg)))

(defun browser-highlight-dir-at-point ()
  ;; Highlight the directory in the tree at point (using
  ;; browser-highlight-face).
  (show-children)
  (browser-recenter-dir)
  (save-excursion
    (if (and (re-search-forward outline-regexp nil t)
	     (looking-at "[^\r\n]*"))
	(if browser-highlight-overlay
	    (move-overlay browser-highlight-overlay
			  (match-beginning 0) (match-end 0))
	  (overlay-put
	   (setq browser-highlight-overlay
		 (make-overlay (match-beginning 0) (match-end 0)))
	   'face 'browser-highlight-face)))))

(defun browser-highlight-dir ( dirname )
  ;; Find and highlight the specified directory in the tree buffer.
  (if browser-tree-buffer
      (save-excursion
	(set-buffer browser-tree-buffer)
	(if (not (browser-find-dir dirname))
	    (if browser-highlight-overlay
		(delete-overlay browser-highlight-overlay))
	  (browser-highlight-dir-at-point)))))

(defun browser-update-directory ( dirname filelist )
  ;; "Insert the specified subdirs in the directory tree listing."
  (if browser-tree-buffer
      (save-excursion
	(set-buffer browser-tree-buffer)
	(let ((narrow-beg (point-min-marker))
	      (narrow-end (point-max-marker))
	      (buffer-read-only nil)
	      p)
	  (widen)
	  (if (and (= (point-min-marker) narrow-beg)
		   (= (point-max-marker) narrow-end))
	      (progn
		(set-marker narrow-beg nil)
		(set-marker narrow-end nil)
		(setq narrow-beg nil)))
	  (if (not (browser-find-dir dirname))
	      (if browser-highlight-overlay
		  (delete-overlay browser-highlight-overlay))
	    (save-excursion
	      (browser-insert-dirlist filelist))
	    (browser-highlight-dir-at-point))
	  (if narrow-beg
	      (progn
		(if (not (or (< (point) narrow-beg)
			     (> (point) narrow-end)))
		    (narrow-to-region narrow-beg narrow-end))
		(set-marker narrow-beg nil)
		(set-marker narrow-end nil)))
	  (set-buffer-modified-p nil)))))

(defun browser-insert-dir ( dirname depth )
  ;; Insert a recursive listing of DIRNAME (to DEPTH) to replace the
  ;; listing at point.
  (if (= depth 0)
      (browser-delete-subdirs)
    (browser-insert-dirlist (browser-make-dir-tree dirname depth))))

(defun browser-end-subdir ()
  ;; "Return location of the end of this subdir in directory listing."
  (save-excursion
    (beginning-of-line)
    (min (save-excursion (browser-end-of-tree) (point))
	 (save-excursion (outline-end-of-subtree) (forward-char 1) (point)))))

(defun browser-delete-subtree ()
  ;; "Delete the subdirectory tree at point from the directory tree."
  (outline-back-to-heading)
  (let ((buffer-read-only nil))
    (delete-region (point) (browser-end-subdir))))

(defun browser-delete-subdirs ()
  "Delete subdirectories of directory at point."
  (interactive)
  (outline-back-to-heading)
  (let ((buffer-read-only nil)
	(p (browser-end-subdir)))
    (save-excursion
      (forward-line 1)
      (delete-region (point) p)
      (forward-line -1)
      (if (looking-at "[^\r\n]*\\(/\\)[\r\n]")
	  (replace-match "" t nil nil 1)))))

(defun browser-read-subdir ( depth )
  "Read the directory subtree for the entry under point.
The optional argument, DEPTH, indicates the depth in the recursive
directory structure to scan. If DEPTH is nil, use the default, which
is set by the variable, \"browser-dir-depth\".
browser-read-subdir( DEPTH )"
  (let ((p (point))
	indent-string
	dirname)
    (save-excursion
      (outline-back-to-heading)
      (let ((indent-string "")
	    (gc-cons-threshold (* gc-cons-threshold 10))
	    dirname)
	(setq dirname (browser-get-dir-name))
	(browser-insert-dir dirname depth)))
    (goto-char p)))

(defun browser-do-read-subdir ( depth )
  "Read the directory subtree for the entry under point.
The argument, DEPTH, indicates the depth in the recursive directory
structure to scan. If DEPTH is nil, use the default, which is set
by the variable, \"browser-dir-depth\".
browser-do-read-subdir( DEPTH )"
  (interactive "P")
  (if (null depth)
      (setq depth browser-dir-depth)
    (setq depth (prefix-numeric-value depth)))
  (browser-read-subdir depth)
  (show-children))

(defun browse-noselect (filename &optional depth)
  "Run the File browser on directory DIRNAME."
  (if (null depth)
      (setq depth browser-dir-depth)
    (setq depth (prefix-numeric-value depth)))
  (save-excursion
    (let ((dirname (file-name-as-directory
		    (abbreviate-file-name (expand-file-name filename))))
	  (buffer nil))
      (setq buffer (create-file-buffer (directory-file-name dirname)))
      (set-buffer buffer)
      (browser-mode)
      (cd filename)
      (clear-visited-file-modtime)
      (set-default 'browser-tree-buffer buffer)
      (setq browser-tree-buffer buffer)
      (setq browser-buffer-list (list buffer))
      (browser-insert-tree-root dirname)
      buffer)))

(defun browse ( &optional dirname )
  "Run the File Browser on directory DIRNAME.
Returns the buffer containing the directory listing."
  (interactive)
  (if (null dirname)
      (setq dirname default-directory))
  (save-excursion
    (if (killed-buffer-p browser-tree-buffer)
	(browse-noselect browser-default-tree-root))
    (let ((buffer browser-tree-buffer))
      (set-window-buffer (selected-window) buffer)
      (set-buffer buffer)
      (browser-file-list dirname)
      (browser-configure-windows)
      (select-window (get-buffer-window (browser-get-listing-buffer
					 browser-buffer-list))))))

(defun browse-other-window ( &optional dirname )
  "Open a file browser display in another window.
May use an existing window or create a new one.
See the `browse' function for more information on the file browser."
  (interactive)
  (if (null dirname)
      (setq dirname default-directory))
  (save-excursion
    (if (killed-buffer-p browser-tree-buffer)
	(browse-noselect browser-default-tree-root))
    (let ((buffer browser-tree-buffer)
	  (pop-up-windows t))
      (display-buffer buffer t)
      (set-buffer buffer)
      (select-window (get-buffer-window buffer))
      (browser-configure-windows)
      (browser-file-list dirname)
      (select-window (get-buffer-window (browser-get-listing-buffer
					 browser-buffer-list)))
      )))

(defun browse-other-frame ( &optional dirname )
  "Open a file browser display in a new frame.
See the `browse' function for more information on the file browser."
  (interactive)
  (let ((new-frame (make-frame)))
    (raise-frame new-frame)
    (select-frame new-frame)
    (browse dirname)))

(defun browser-new-listing-buffer ( dirname )
  (let ((buffer (browser-file-noselect dirname)))
    (setcdr browser-buffer-list (cons buffer (cdr browser-buffer-list)))
    (browser-configure-windows)
    buffer))

(defun browser-list-files ( dirname &optional no-cache )
  "Create a file listing from the named directory."
  (interactive "DOpen (directory): \nP")
  (let ((old-buffer (current-buffer))
	(buffer (browser-get-listing-buffer browser-buffer-list)))
    (if (not buffer)
	(browser-new-listing-buffer dirname)
      (if (null (get-buffer-window old-buffer))
	  (browser-configure-windows))
      (set-buffer buffer)
      (browser-file-list dirname no-cache)
      (set-buffer old-buffer))))

(defun browser-maybe-new-tree ( dirname )
  "Find or create a file tree from the named directory."
  (interactive "DOpen tree (directory): ")
  (if (not (browser-find-tree-root dirname))
      (let ((buffer-read-only nil))
	(goto-char (browser-end-of-tree))
	(insert "\n")
	(browser-insert-tree-root dirname)))
  (browser-goto-root)
  (forward-line 1)
  (browser-do-listing))

(defun browser-new-tree-at-point ()
  "Create a new file tree for the directory at point."
  (interactive)
  (browser-maybe-new-tree (browser-get-dir-name)))

(defun browser-get-file-name ()
  "Read the name of the directory on the current line in the buffer."
  (interactive)
  (let (beg end)
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (if (re-search-forward outline-regexp end t)
	  (setq beg (match-end 0)))
      (re-search-forward "@?[\n\r]" nil t))
    (and beg (buffer-substring beg (match-beginning 0)))))

(defun browser-get-file-name-only ()
  "Read the name of the directory on the current line in the buffer."
  (interactive)
  (let (beg end)
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (if (re-search-forward outline-regexp end t)
	  (setq beg (match-end 0)))
      (re-search-forward "[@/]?[\n\r]" nil t))
    (and beg (buffer-substring beg (match-beginning 0)))))

(defun browser-up-heading ( arg )
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  ;; Find the previous (or current) heading line
  (or (outline-on-heading-p)
      (re-search-backward (concat "[\n\r]\\(" outline-regexp "\\)") nil t))
  (if (> (funcall outline-level) 1)
      (while (and (> (funcall outline-level) 1)
		  (> arg 0)
		  (not (bobp)))
	(let ((present-level (funcall outline-level)))
	  ;; Skip back past all headings of this level to a higher level heading
	  (while (and (not (< (funcall outline-level) present-level))
		      (if (re-search-backward
			   (concat "[\n\r]\\(" outline-regexp "\\)")
			   nil 'move)
			  (goto-char (1+ (match-beginning 0))))))
	  (setq arg (- arg 1))))))

(defun browser-get-dir-name ()
  "Read the relative directory name of the file on the current line."
  (interactive)
  (let (dirname)
    (save-excursion
      (save-restriction
	(widen)
	(beginning-of-line)
	(setq dirname (browser-get-file-name))
	(while (and (> (funcall outline-level) 1)
		    (not (bobp)))
	  (browser-up-heading 1)
	  (setq dirname (concat (file-name-as-directory (browser-get-file-name))
				dirname)))))
    (expand-file-name dirname (browser-get-root-name))))

(defun browser-get-listing-buffer (buffer-list)
  (if (not (killed-buffer-p (nth 1 buffer-list)))
      (nth 1 buffer-list)
    (browser-check-buffer-list buffer-list)
    (nth 1 buffer-list)))

(defun browser-scanned-directory-p ()
  "Predicate to test if directory under point has already been scanned."
  (looking-at "[^\n\r]*/@?[\n\r]"))

(defun browser-do-listing ()
  "Make a listing of files in the directory under point."
  (interactive)
  (let ((p (point)))
    (show-children)
    (let ((dirname (browser-get-dir-name))
	  (buffer (browser-get-listing-buffer browser-buffer-list)))
      (if (not buffer)
	  (browser-new-listing-buffer dirname)
	(if (null (get-buffer-window buffer))
	    (browser-configure-windows))
	(set-buffer buffer)
	(browser-list-files dirname)))
    (goto-char p)))

(defun browser-show-children ()
  "Re-scan directory under point, and show children."
  (interactive)
  (if (not (browser-scanned-directory-p))
      (browser-read-subdir browser-dir-depth))
  (hide-subtree)
  (show-children))

(defun browser-show-subtree ()
  "Re-scan directory under point, and show subtree."
  (interactive)
  (if (browser-scanned-directory-p)
      (show-subtree)
    (browser-read-subdir browser-dir-depth)))

(defun browser-dir-info ()
  "Display more detailed information on file."
  (interactive)
  (let ((filename (expand-file-name (browser-get-dir-name)))
	(directory default-directory))
    (cond (filename
	   (save-excursion
	     (set-buffer (get-buffer-create "*Browser-file-info*"))
	     (widen)
	     (erase-buffer)
	     (cd directory)
	     (let ((handler (find-file-name-handler
			     filename 'insert-directory)))
	       (if handler
		   (funcall handler 'insert-directory filename
			    browser-info-switches)
		 (insert-directory filename browser-info-switches)))
	     (goto-char (point-min))
	     (end-of-line)
	     (message (buffer-substring (point-min) (point))))))))

(defun browser-new-listing (filename)
  "Create and display a new file listing buffer associated with the
current directory browser buffer."
  (set-buffer (browser-new-listing-buffer filename)))

(defun browser-mouse-do-listing ( event )
  "Do file listing on the directory name clicked on."
  (interactive "e")
  (let* ((window (posn-window (event-end event))))
    (select-window window)
    (goto-char (posn-point (event-end event)))
    (browser-do-listing)))

(defun browser-do-new-listing ()
  "Create and display a new file listing buffer associated with the
current directory browser buffer."
  (interactive)
  (show-children)
  (let ((filename (browser-get-dir-name)))
    (if filename
	(browser-new-listing filename))))

(defun browser-relist-files ()
  (interactive)
  (save-excursion
    (let ((buffer (browser-get-listing-buffer browser-buffer-list)))
      (if (not buffer)
	  (error "No current file listing to re-list")
	(if (null (get-buffer-window buffer))
	    (browser-configure-windows))
	(set-buffer buffer)
	(browser-file-relist)))))

(defun browser-mouse-do-new-listing ( event )
  "Do file listing on the directory name clicked on."
  (interactive "e")
  (save-excursion
    (let ((dir-buffer (window-buffer (posn-window (event-end event))))
	  (window (selected-window)))
      (set-buffer dir-buffer)
      (goto-char (posn-point (event-end event)))
      (browser-do-new-listing))))

(defun browser-quit ( &optional force )
  "Quit the file browser and delete all associated buffers."
  (interactive)
  (if (or force
	  (y-or-n-p "Quit the file browser (and delete buffers) ? "))
      (let (buffer-list buffer)
	(setq buffer-list browser-buffer-list)
	(while buffer-list
	  (setq buffer (car buffer-list))
	  (setq buffer-list (cdr buffer-list))
	  (if (not (killed-buffer-p buffer))
	      (kill-buffer buffer))))))

(defun browser-customise ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (customize)
  (search-forward "File Browser")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defalias 'browser-customize 'browser-customise)

(suppress-keymap browser-mode-map)

(defun browser-keymap-define ( map binding-list )
  (mapcar (lambda ( binding )
	       (define-key map (car binding) (cdr binding)))
	  binding-list))

(browser-keymap-define
 browser-mode-map
 '(
   ;; Showing and hiding sublevels
   ("a" 	. show-all)
   ("l" 	. hide-sublevels)
   ("c" 	. browser-show-children)
   ("s" 	. browser-show-subtree)
   ("h" 	. hide-subtree)
   ("x" 	. foldout-exit-fold)
   ("z" 	. foldout-zoom-subtree)
   ;; Navigating in directory tree
   ("n" 	. outline-next-visible-heading)
   ("p" 	. outline-previous-visible-heading)
   ("u" 	. outline-up-heading)
   ("b" 	. outline-backward-same-level)
   ("f" 	. outline-forward-same-level)
   ;; Editing the directory tree
   ("r" 	. browser-do-read-subdir)
   ("d"		. browser-delete-subdirs)
   ([delete]	. browser-delete-subdirs)
   ("D" 	. browser-delete-tree)
   ("I" 	. browser-new-tree-at-point)
   ("\C-c\C-i"	. browser-maybe-new-tree)
   ;; Interacting with the file listing
   (" " 	. browser-do-listing)
   ("\C-c\C-d"	. browser-list-files)
   ("." 	. browser-dir-info)
   ("g" 	. browser-relist-files)
   ("o" 	. browser-do-new-listing)
   ("q" 	. kill-buffer)
   ;; Configuration
   ("Q" 	. browser-quit)
   ("@" 	. browser-configure-windows)
   ("C" 	. browser-customise)
   ;; Mouse operations
   ([mouse-2]   . browser-mouse-do-listing)
   ([S-down-mouse-1] 	. browser-drag-copy-file)
   ([M-S-down-mouse-1]	. browser-drag-move-file)
   ;; Clear out the menubar
   ([menu-bar edit]   . undefined)
   ([menu-bar search] . undefined)))

;; Menu bar bindings
(require 'easymenu)

;; I like easymenu. This was much longer, more complicated - and it
;; looked ugly too, till I switched to easymenu :-).
;; What about easy-keymap ? ;-)

(easy-menu-define
 browser-cache-menu browser-mode-map
 "Browser Cache Listing menu"
 '("Cache"))

(easy-menu-define
 browser-tree-menu browser-mode-map
 "Browser menu"
 '("Tree"
   ["Show Children"		browser-show-children t]
   ["Show Subtree"		browser-show-subtree t]
   ["Hide Subtree"		hide-subtree t]
   ["Show All"			show-all t]
   ["Hide All"			hide-sublevels t]
   ["Zoom Subtree"		foldout-zoom-subtree t]
   ["Exit Zoom"			foldout-exit-fold t]))

(easy-menu-define
 browser-browser-menu browser-mode-map
 "Browser menu"
 '("Browser"
   ["Open Directory"		browser-do-listing t]
   ["Open Directory - new buffer" browser-do-new-listing t]
   ["Display File Info"		browser-dir-info t]
   ["Re-read Subdirectory"	browser-do-read-subdir t]
   ["Redraw Windows"		browser-configure-windows t]
   ["Kill Buffer"		kill-this-buffer t]
   ["Customise"			browser-customise t]))

(defvar browser-file-regexp nil
  "*Regular expression matching files to be shown in a file listing.
An empty string (or `nil') will match all files, else only the files
which match the expression will be listed.")
(put 'browser-file-regexp 'variable-interactive "s")

(defvar browser-file-inhibit-regexp nil
  "*Regular expression matching files to be suppressed in a file listing.
An empty string (or `nil') suppresses no files.")
(put 'browser-file-inhibit-regexp 'variable-interactive "s")

(defvar browser-sort-by-extension nil
  "*If set non-nil (On) sort the file listings by the file extensions.")

(defvar browser-flag-file-type (not window-system)
  "*If set non-nil (On) use a suffix to specify file type (like \"ls -F\").
A special character is added to the end of the filename in the file
listing window to indicate the file type. The characters used are:
	`/'  file is a directory
	`@'  file is a symbolic link
	`*'  file is executable.
These are the same indicator characters used when listing files with the
`ls -F' Unix shell command.")

(defvar browser-file-default-tag ?*
  "*The character to use as the default tag for tagged files.")
(put 'browser-file-default-tag 'variable-interactive "c")

(defvar browser-file-entry-width 0
  "The field width of each column in the file listing window.")

(defvar browser-file-num-columns 0
  "The number of filenames listed on each line of the file listing.")

(defvar browser-file-num-rows 0
  "The number of lines of filenames in the file listing.")

(defvar browser-list-of-files nil
  "A list of the filenames for this listing.")

(defvar browser-list-of-files-cache nil
  "A list of cached filename listings.")

(defvar browser-file-cache-size 10
  "*Number of directories to maintain in the directory listing cache.")

(defvar browser-start-of-listing 0
  "Specifies the buffer position of the start of the list of files.")

(defvar browser-listing-length 0
  "Specifies the number of lines in this buffers file listing display.")

(defvar browser-fileops-files nil
  "The current filename for dynamic file operations menus.")

(defvar browser-file-mode-map (make-sparse-keymap) "")

(defun browser-file-mode ()
  "Major mode for file and directory tree browsing.
The FileBrowser provides a filemanager interface for Emacs.
Operation is similar to other common filebrowsers, and provides:
  - simple point-and-click navigation of a directory tree
  - complete file listings of selected directories
  - point-and-click to open a file in an Emacs buffer
  - file operations (delete, rename, copy, remove,...)
  - primitive drag-and-drop to copy or move files between directories.

The `browse' command prompts for a directory name, and accepts an
optional numeric prefix argument (eg. C-u 5 M-x browse).  The
FileBrowser will display two buffers side-by-side.

The left hand window shows the \"tree listing\" buffer. This buffer
contains a listing of the recursive sub-directory tree of the
directory specified. The subdirectory tree listing may optionally,
pre-scan the entire sub-directory tree, OR scan the subdirectory tree
dynamically as the user navigates around in the listing. A negative
numeric prefix arg tells the browser to pre-scan the subdirectory
tree, while a positive prefix arg will cause the browser to scan that
deep in the browser listing on each read. Each time the user navigates
into a subdir which has not been scanned it will be dynamically
re-scanned to this depth. The default scanning depth is `1'.

The right hand window shows the \"file listing\" buffer. A complete file
listing of any directory in the tree listing may be shown in this
buffer (use the Space bar in the tree listing window). Simple file
operations and file visiting can be performed (try `C-h m' in the file
listing window).

\\{browser-file-mode-map}.

Customisation:
==============

The following variables may be used to customise the behaviour of the
file listing buffer:

browser-info-switches
browser-flag-file-type
browser-file-default-tag
browser-file-cache-size
browser-inhibit-dots
browser-file-regexp
browser-file-inhibit-regexp"
  (interactive)
  (kill-all-local-variables)
  (use-local-map browser-file-mode-map)
  (setq mode-name "BrowserListing")
  (setq major-mode 'browser-file-mode)
  (setq local-abbrev-table nil)
  (set-syntax-table browser-mode-syntax-table)
  (setq indent-tabs-mode nil)
  (auto-fill-mode -1)
  (buffer-disable-undo (current-buffer))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (if (fboundp 'make-local-hook)
      (progn
        (make-local-hook 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook 'browser-delete-buffer nil t))
    (add-hook 'kill-buffer-hook 'browser-delete-buffer))
  (set (make-local-variable 'browser-buffer-list) nil)
  (set (make-local-variable 'browser-tree-buffer) nil)
  (set (make-local-variable 'browser-start-of-listing) nil)
  (set (make-local-variable 'browser-listing-length) 0)
  (set (make-local-variable 'browser-file-entry-width) 0)
  (set (make-local-variable 'browser-file-num-columns) 0)
  (set (make-local-variable 'browser-file-num-rows) 0)
  (set (make-local-variable 'pop-up-windows) t)
  (set (make-local-variable 'pop-up-frames) browser-pop-up-frames)
  (set (make-local-variable 'display-buffer-function)
    'browser-display-buffer)
  (make-local-variable 'browser-fileops-files)
  ;; Emacs-19.29 has a bug with activate-menubar-hook
  (if (not (string-match "^19\\.29" emacs-version))
      (if (fboundp 'make-local-hook)
          (progn
            (make-local-hook 'activate-menubar-hook)
            (add-hook 'activate-menubar-hook
                      'browser-dynamic-fileops-menu nil t))
        (add-hook 'activate-menubar-hook 'browser-dynamic-fileops-menu)))
  (setq truncate-lines t)
  (run-hooks 'browser-file-mode-hook))

(defun browser-delete-other-browser-windows()
  "Delete other browser windows."
  (interactive)
  (if browser-buffer-list
      (let* ((this-window (selected-window))
	     (window (next-window this-window 'false))
	     (next-window nil))
	(while (not (eq this-window window))
	  (setq next-window (next-window window 'false))
	  (if (memq (window-buffer window) browser-buffer-list)
	      (delete-window window))
	  (setq window next-window)))))

(defun browser-configure-windows ()
  "Re-draw all the associated browser listing buffers.
Also checks for killed buffers in the buffer list - and deletes them."
  (interactive)
  (if browser-buffer-list
      (progn
	(browser-check-buffer-list browser-buffer-list)
	(let ((buffer-list browser-buffer-list)
	      (this-window (selected-window))
	      (buffer (current-buffer))
	      (dedicate-windows t))
	  (browser-delete-other-browser-windows)
	  (let ((margin (- (frame-width) (window-width))))
	    (if (> margin 0)
		(enlarge-window margin t)))
	  (set-window-dedicated-p (selected-window) nil)
;;	  (if (and (not browser-pop-up-frames)
;;		   (eq (next-window this-window) this-window))
;;	      (progn
;;		;; Find a non-browser window to display in the bottom window
;;		(split-window-vertically)
;;		(let ((buflist (buffer-list)))
;;		  (set-buffer (car buflist))
;;		  (while (and buflist
;;			      (or (eq (aref (buffer-name) 0) ?\ )
;;				  (eq major-mode 'browser-file-mode)
;;				  (eq major-mode 'browser-mode)))
;;		    (setq buflist (cdr buflist))
;;		    (set-buffer (car buflist)))
;;		  (set-buffer buffer)
;;		  (if buflist
;;		      (display-buffer (car buflist) t)))))
	  (if (killed-buffer-p (car buffer-list))
	      nil
	    (split-window-horizontally browser-tree-window-width)
	    (set-window-buffer (selected-window) (car buffer-list))
	    (set-window-dedicated-p (selected-window) dedicate-windows)
	    (other-window 1))
	  (setq buffer-list (reverse (cdr buffer-list)))
	  (set-window-buffer (selected-window) (car buffer-list))
	  (set-window-dedicated-p (selected-window) nil)
	  (browser-redisplay-files (selected-window))
	  (let ((maxheight (/ (window-height) (length buffer-list))))
	    (while (setq buffer-list (cdr buffer-list))
	      (split-window-vertically
	       (- (min maxheight (max window-min-height
				      (1+ browser-listing-length)))))
	      (set-window-dedicated-p (next-window) dedicate-windows)
	      (set-window-buffer (selected-window) (car buffer-list))
	      (browser-redisplay-files (selected-window))))
	  (set-window-dedicated-p (selected-window) dedicate-windows)
	  (select-window (get-buffer-window buffer))))))

(defun browser-bury-browser ()
  "Bury this display of the file browser."
  (interactive)
  (let ((window (get-buffer-window browser-tree-buffer)))
    (if window
	(progn
	  (select-window window)))
    (browser-delete-other-browser-windows)
    (delete-window)
    (mapcar 'bury-buffer browser-buffer-list)))

;; Functions to create the File Listings

(defun browser-dired-update-tree ( &optional from-cache )
  (save-excursion
    (let ((old-dir default-directory)
	  (dirname (dired-current-directory))
	  file-list)
      (let* ((dirlist (cons 'dummy nil))
	     (dirtail dirlist))
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (dired-goto-next-file)
	  (setcdr dirtail
		  (cons (file-name-nondirectory (dired-get-filename)) nil))
	  (setq dirtail (cdr dirtail))
	  (forward-line 1))
	(cd dirname)
	(browser-file-list-filter (cdr dirlist)))
	(cd old-dir))))

(defun browser-do-dired-buffer ( filename &optional no-cache )
  (save-excursion
    (let ((buffer-list browser-buffer-list)
	  (tree-buffer browser-tree-buffer)
	  (buffer (dired-noselect filename)))
      (set-buffer buffer)
      (set (make-local-variable 'browser-buffer-list) buffer-list)
      (set (make-local-variable 'browser-tree-buffer) tree-buffer)
      (set (make-local-variable 'pop-up-windows) t)
      (if (fboundp 'make-local-hook)
	  (progn
	    (make-local-hook 'kill-buffer-hook)
	    (add-hook 'kill-buffer-hook 'browser-delete-buffer nil t))
	(add-hook 'kill-buffer-hook 'browser-delete-buffer))
      ;; (make-local-hook 'dired-after-readin-hook)
      (add-hook 'dired-after-readin-hook 'browser-dired-update-tree)
      (browser-dired-update-tree (not no-cache))
      buffer)))

(defun browser-file-noselect ( filename )
  "Run the Browser file listing on directory DIRNAME."
  (if filename
      (setq filename (file-name-as-directory
		      (abbreviate-file-name (expand-file-name filename)))))
  (let ((buffer-list browser-buffer-list)
	(tree-buffer browser-tree-buffer)
	buffer)
      (if browser-use-dired
	  (setq buffer (browser-do-dired-buffer filename))
	(save-excursion
	  (setq buffer (get-buffer-create "*browser-new-listing*"))
	  (set-buffer buffer)
	  (browser-file-mode)
	  (setq browser-buffer-list buffer-list)
	  (setq browser-tree-buffer tree-buffer)
	  (if filename
	      (browser-file-list filename))))
      buffer))

;; Stolen and adapted from dired.el ;-)
(defun browser-format-columns-of-files (files width)
  (let* ((fieldwidth (apply 'max (mapcar 'length files)))
	 (maxlen (+ fieldwidth 2))
	 (columns (max 1 (/ width maxlen)))
	 (nfiles (length files))
	 (rows (+ (/ nfiles columns)
		  (if (zerop (% nfiles columns)) 0 1)))
	 (i 0)
	 (j 0))
    (setq files (nconc (copy-sequence files)
		       (make-list (- (* columns rows) nfiles) "")))
    (setcdr (nthcdr (1- (length files)) files) files) ; make circular
    (while (< j rows)
      (while (< i columns)
	(move-to-column (1+ (* i maxlen)) t)
	(insert (car files))
	(setq i (1+ i))
	(setq files (nthcdr rows files)))
      (insert "\n")
      (setq i 0
	    j (1+ j)
	    files (cdr files)))
    (setq browser-file-entry-width maxlen)
    (setq browser-file-num-columns columns)
    (setq browser-file-num-rows rows)
    (setq browser-listing-length (1+ rows))))

(defun browser-display-files ()
  "Re-format the file listing in the current buffer."
  (save-excursion
    (let ((window (get-buffer-window (current-buffer) 'visible))
	  (buffer-read-only nil))
      (widen)
      (erase-buffer)
      ;; Disable use of uniquify.el as it croaks badly when there
      ;; is no buffer-file-name (although it has since been fixed).
      (let ((mnemonic-buffer-names nil))
	(rename-buffer "*browser-temp-buffer*" t))
      (let ((filename (file-name-nondirectory
		       (directory-file-name default-directory))))
	(if (string-equal filename "")
	    (setq filename "/"))
	(let ((mnemonic-buffer-names nil))
	  (rename-buffer filename t)))
      (insert "Directory: "
	      default-directory
	      (if (file-symlink-p (directory-file-name default-directory))
		  ;; If it's a symlink - show link target name
		  (concat " -> " (abbreviate-file-name
				  (file-truename default-directory)))
		"")
	      (if browser-file-regexp
		  (concat "  (" browser-file-regexp")")
		"")
	      "\n")
      (setq browser-start-of-listing (point))
      (browser-format-columns-of-files
       browser-list-of-files (1- (if window (window-width window) (frame-width))))
      (delete-char -1)
      (goto-char browser-start-of-listing)
      (browser-file-this-entry)
      (run-hooks 'browser-file-display-hook)
      (set-buffer-modified-p nil))))

(defun browser-redisplay-files (window)
  (let ((buffer-read-only nil)
	(width (window-width window)))
    (if (or (> (* browser-file-num-columns browser-file-entry-width)
	       width)
	    (<= (* (+ browser-file-num-columns 1) browser-file-entry-width)
		width))
	(let ((columns (max 1 (/ width browser-file-entry-width)))
	      file-at-point file-at-start filename file-list)
	  (setq file-at-point (browser-file-get-entry))
	  (erase-buffer)
	  (browser-display-files)
	  (if (and file-at-point
		   (re-search-forward (concat "\\<"
					      (regexp-quote file-at-point)
					      "\\>") nil t))
	      (browser-file-this-entry))
	  (set-window-point window (point))))))

(defun browser-file-compare-ext ( fname1 fname2 )
  "Compare the filename extensions of fname1 and fname2"
  (string< 
   (if (string-match "^[^.].*\\\.\\(.+\\)$" fname1)
       (substring fname1 (match-beginning 1))
     "")
   (if (string-match "^[^.].*\\\.\\(.+\\)$" fname2)
       (substring fname2 (match-beginning 1))
     "")))

(defvar browser-file-face-list nil
  "*List of cons pairs `(REGEXP . FACE)' to set faces for filenames. 
Files in the display buffer which match a given regular expression
will be displayed in the specified face.")

(defun browser-file-list-filter ( file-list )
  "Process the file-list, adding suffixes to filenames to indicate type.
A suffix indicating file type is added to the end of each file name,
similar to \"ls -F\". Any files which match `browser-file-inhibit-regexp'
are deleted from the list. Also, sort file list by extension if
`browser-sort-by-extension' is non-nil."
  (if browser-sort-by-extension
      (setq file-list (sort file-list 'browser-file-compare-ext)))
  (let ((next-file file-list)
	filename orig-filename dirlist len)
    (while next-file
      (setq filename (car next-file))
      (if (or (and browser-file-inhibit-regexp
		   (string-match browser-file-inhibit-regexp filename))
	      (and browser-inhibit-dots
		   (not (string-equal ".." filename))
		   (string-match "^\\." filename))
	      (string-equal filename "."))
	  ;; Filename matches the "inhibit" regexp - delete from list
	  (setq file-list (delq filename file-list))
	;; Filename does not match - keep in list & do file type indicator
	(setq len (length filename))
	(set-text-properties 0 len '(mouse-face highlight
						rear-nonsticky t) filename)
	(cond ((string-equal ".." filename)
	       (if browser-flag-file-type
		   (progn
		     (setq len (1+ len))
		     (setcar next-file (setq filename (concat filename "/")))))
	       (put-text-property 0 len 'face browser-directory-face
				  filename))
	      ((file-symlink-p filename)
	       (setq orig-filename filename)
	       (if browser-flag-file-type
		   (progn
		     (setq len (1+ len))
		     (setcar next-file (setq filename (concat filename "@")))))
	       (put-text-property 0 len 'face browser-symlink-face
				  filename)
	       (if (file-directory-p orig-filename)
		   (setq dirlist (nconc dirlist (list filename)))))
	      ((file-directory-p filename)
	       (if browser-flag-file-type
		   (progn
		     (setq len (1+ len))
		     (setcar next-file (setq filename (concat filename "/")))))
	       (put-text-property 0 len 'face browser-directory-face
				  filename)
	       (setq dirlist (nconc dirlist (list filename))))
	      ((file-executable-p filename) ; is executable
	       (if browser-flag-file-type
		   (progn
		     (setq len (1+ len))
		     (setcar next-file (setq filename (concat filename "*")))))
	       (put-text-property 0 len 'face browser-executable-face
				  filename))
	      (t
	       (let ((face-list browser-file-face-list))
		 (while (and face-list
			     (if (string-match (car (car face-list)) filename)
				 (progn
				   (put-text-property 0 len 'face
						      (cdr (car face-list))
						      filename)
				   nil)
			       t))
		   (setq face-list (cdr face-list)))))))
      (setq next-file (cdr next-file)))
    (cons file-list dirlist)))

(defun browser-set-face-list (face-list)
  "Set the list of face specifications for the file listing buffer.
FACE-LIST is a list of (WILDCARD . FACE) pairs. If a filename matches
WILDCARD it will be displayed using FACE.
WILDCARD is a string containing any number of shell wildcard expressions
separated by spaces. FACE is any valid Emacs face specification.
For more information on the wildcard expressions see the documentation
for the `wildcard-to-regexp' function.
eg. (browser-set-face-list '((\"*.el *.elc\" . bold)
                             (\"*.[ch]\" . italic)))"
  (interactive "x")
  (setq browser-file-face-list
	(mapcar (function (lambda (arg)
			    (cons (wildcard-to-regexp (car arg)) (cdr arg))))
		face-list)))

(defun browser-set-face-list-regexp (face-list)
  "Set the list of face specifications for the file listing buffer.
FACE-LIST is a list of (REGEXP . FACE) pairs. If a filename matches
REGEXP it will be displayed using FACE. FACE is any valid Emacs face
specification.

eg. (browser-set-face-list-regexp '((\"\\\\.el$\\\\|\\\\.elc$\" . bold)
                                    (\"\\\\.[ch]$\" . italic)))"
  (interactive "x")
  (setq browser-file-face-list (copy-sequence face-list)))

(defvar browser-history-size 20
  "*Maximum length of the File Browser history list.
The history list is a list of the last directories visited.")
(defvar browser-history-list nil)
(make-variable-buffer-local 'browser-history-list)

(defun browser-add-to-history ( dirname )
  (save-excursion
    (setq dirname (directory-file-name (abbreviate-file-name dirname)))
    (if (and browser-history-list
	     (string-equal dirname (car browser-history-list)))
	() ;; Don't put duplicate entries in the history
      (setq browser-history-list (cons dirname browser-history-list))
      (if (> (length browser-history-list) browser-history-size)
	  (setcdr (nthcdr (1- browser-history-size) browser-history-list)
		  nil)))))

(defun browser-file-update-cache-menu ()
  (save-excursion
    (let (items)
      (setq items
	    (mapcar (function (lambda ( cache-entry )
				(vector (directory-file-name (car cache-entry))
					(list 'browser-file-list
					      (car cache-entry))
					t)))
		    browser-list-of-files-cache))
      (easy-menu-change '() "Cache" items)
      (set-buffer browser-tree-buffer)
      (easy-menu-change '() "Cache" items))))

(defun browser-delete-from-cache ( directory )
  "Delete a directory listing if it is in the cache."
  (interactive "DDelete from cache (directory): ")
  (let ((cache-entry (assoc directory browser-list-of-files-cache)))
    (if cache-entry
	(setq browser-list-of-files-cache
	      (delq cache-entry browser-list-of-files-cache))))
  (browser-file-update-cache-menu))

(defun browser-get-from-cache ( directory )
  "Return a directory listing if it is in the cache."
  (let ((cache-entry (assoc directory browser-list-of-files-cache)))
    (if cache-entry
	;; Move this cache entry to top of list
	(progn
	  (setq browser-list-of-files-cache
		(delq cache-entry browser-list-of-files-cache))
	  (set-visited-file-modtime (nth 1 cache-entry))
	  (if (browser-out-of-date-p)
	      nil
	    (setq browser-list-of-files-cache
		  (cons cache-entry browser-list-of-files-cache))
	    (browser-file-update-cache-menu)
	    (nth 2 cache-entry)))
      nil)))

(defun browser-add-to-cache ( directory file-list )
  "Add a directory listing to the cache.
If there is already an entry in the cache for this directory - update it."
  (let ((cache-entry (assoc directory browser-list-of-files-cache)))
    (if cache-entry
	;; Delete the old cache entry
	(setq browser-list-of-files-cache
	      (delq cache-entry browser-list-of-files-cache)))
    (setq cache-entry (list directory (visited-file-modtime) file-list))
    (if (> (length browser-list-of-files-cache) browser-file-cache-size)
	;; delete the last cache entry from the list
	(setcdr (nthcdr (1- browser-file-cache-size)
			browser-list-of-files-cache) nil))
    ;; put new listing at top of cache
    (setq browser-list-of-files-cache
	  (cons cache-entry browser-list-of-files-cache)))
  (browser-file-update-cache-menu)
  (browser-add-to-history directory))

(defun browser-read-files ( dirname &optional no-cache )
  "Create a file listing from the named directory"
  (let ((buffer-read-only nil)
	file-list)
    (clear-visited-file-modtime)
    (cd dirname)
    (setq browser-list-of-files (if (not no-cache)
				    (browser-get-from-cache default-directory)))
    (if (null browser-list-of-files)
	;; read the directory listing from the disk
	(progn
	  (run-hooks 'browser-file-pre-read-hook)
	  (message "Reading directory %s..." default-directory)
	  (setq browser-list-of-files
		(let ((handler (find-file-name-handler (expand-file-name
							default-directory)
						       'directory-files)))
		  (if handler
		      (funcall handler 'directory-files default-directory
			       nil browser-file-regexp)
		    (directory-files default-directory
				     nil browser-file-regexp))))
	  (setq browser-list-of-files
		(browser-file-list-filter browser-list-of-files))
	  (message "Reading directory %s...done" default-directory)
	  (let ((buffer-file-name (expand-file-name default-directory)))
	    (set-visited-file-modtime))
	  (browser-update-directory default-directory
				    (cdr browser-list-of-files))
	  (browser-add-to-cache default-directory browser-list-of-files))
      (browser-highlight-dir default-directory)
      (message "Directory %s loaded from cache" default-directory))
    (setq browser-list-of-files (car browser-list-of-files))))

(defun browser-file-list ( dirname &optional no-cache )
  "Create a file listing from the named directory"
  (interactive "DOpen (directory): \nP")
  (setq no-cache (not (null no-cache)))
  (setq dirname (expand-file-name dirname))
  (if (and (string-equal default-directory "/")
	   (string-equal (file-relative-name dirname) ".."))
      (error "Already at root directory"))
  (let ((old-buffer (current-buffer))
	new-buffer)
    (cond (browser-use-dired
	   (if (and (eq major-mode 'dired-mode)
		    (dired-in-this-tree dirname
					(expand-file-name default-directory)))
	       (progn
		 (widen)
		 (dired-maybe-insert-subdir dirname nil t)
		 (dired-goto-subdir dirname)
		 (beginning-of-line)
		 (let ((window (get-buffer-window old-buffer)))
		   (if (not window)
		       (progn
			 (browser-configure-windows)
			 (setq window (get-buffer-window old-buffer))))
		   (if browser-use-dired-narrowing
		       (let ((p (point)))
			 (if (not (dired-next-subdir 1 t))
			     (goto-char (point-max)))
			 (beginning-of-line)
			 (narrow-to-region p (point))
			 (goto-char (point-min))))
		   (if window
		       (set-window-start window (point)))))
	     (setq new-buffer (browser-do-dired-buffer dirname no-cache))))
	  ((eq major-mode 'browser-mode)
	   (browser-list-files dirname no-cache))
	  ((eq major-mode 'browser-file-mode)
	   (browser-read-files dirname no-cache)
	   (browser-display-files)
	   (goto-char browser-start-of-listing)
	   (browser-file-this-entry))
	  (t
	   (setq new-buffer (browser-file-noselect dirname))))
    (if new-buffer				; if using a new buffer...
	(let ((buflist browser-buffer-list))
	  (set-buffer new-buffer)
	  (while (and buflist
		      (not (eq (car buflist) old-buffer)))
	    (setq buflist (cdr buflist)))
	  (if buflist
	      (progn
		(save-excursion
		  (set-buffer old-buffer)
		  (if (eq major-mode 'browser-file-mode)
		      (progn
			(remove-hook 'kill-buffer-hook
				     'browser-delete-buffer t)
			(kill-buffer (current-buffer)))))
		(setcar buflist new-buffer)))
	  (browser-configure-windows)))))

(defun browser-file-relist ()
  "Re-read the current directory file listing."
  (interactive)
  (let ((file-at-point (browser-file-get-entry)))
    (browser-file-list default-directory t)
    (if (and file-at-point
	     (re-search-forward (concat "\\<"
					(regexp-quote file-at-point)
					"\\>") nil t))
	(browser-file-this-entry))))

(defun browser-file-toggle-dots ()
  "Toggle the option to display dot-files in the Files window."
  (interactive)
  (setq browser-inhibit-dots (not browser-inhibit-dots))
  (browser-file-relist))

;; Navigation and entry selection function

(defun browser-file-this-entry ()
  "Move to start of this entry in the directory listing."
  (interactive)
  (if (< (point) browser-start-of-listing)
      (goto-char browser-start-of-listing))
  (condition-case nil
      (progn
	(move-to-column (* (/ (current-column) browser-file-entry-width)
			   browser-file-entry-width))
	(forward-char 1)
	(looking-at "[^ \n]"))
    (error nil)))

(defun browser-file-next-entry ( arg )
  "Move cursor to next file entry.
With prefix ARG, move that many entries."
  (interactive "p")
  (if (< (point) browser-start-of-listing)
      (goto-char (+ browser-start-of-listing 1)))
  (if (< arg 0)
      (browser-file-prev-entry (- arg))
    (let* ((col (/ (current-column) browser-file-entry-width))
	   (column (+ (* col browser-file-entry-width) 1)))
      (while (and (not (zerop arg))
		  (or (search-forward "\n" nil t)
		      (if (< (setq col (1+ col)) browser-file-num-columns)
			  (progn
			    (goto-char browser-start-of-listing)
			    (setq column
				  (+ (* col browser-file-entry-width) 1))
			    t)
			nil))
		  (eq (move-to-column column) column))
	(setq arg (1- arg)))
      arg)))

(defun browser-file-prev-entry ( arg )
  "Move cursor to previous file entry.
With prefix ARG, move that many entries."
  (interactive "p")
  (if (< (point) browser-start-of-listing)
      (goto-char (+ browser-start-of-listing 1)))
  (if (< arg 0)
      (browser-file-next-entry (- arg))
    (let ((col (/ (current-column) browser-file-entry-width)))
      (while (and (not (zerop arg))
		  (or (search-backward "\n" browser-start-of-listing t)
		      (if (>= (setq col (1- col)) 0)
			  (progn (goto-char (point-max)) t)
			nil)))
	(setq arg (1- arg)))
      (if (zerop arg)
	  (move-to-column (+ (* col browser-file-entry-width) 1)))))
  arg)

(defun browser-file-get-entry ()
  "Get the name of the current entry."
  (interactive)
  (if (< (point) browser-start-of-listing)
      nil
    (save-excursion
      (if (browser-file-this-entry)
	  (buffer-substring (point)
			    (or (next-single-property-change (point)
							     'mouse-face)
				(point-max)))))))

(defun browser-file-get-file-name ()
  "Get the full pathname of the file in the current entry."
  (interactive)
  (let ((name (browser-file-get-entry)))
    (if name
	(abbreviate-file-name (expand-file-name name)))))

;; File tagging (marking) functions

(defun wildcard-to-regexp ( wildcard )
  "Convert the shell filename WILDCARD to an Emacs regular expression.
This function does a reasonable job for simple wildcard expressions, but
will not work correctly for all wildcards. The conversion is:
  \"*\"    ->    \".*\"  (\"[^.].*\" if at front of a wildcard subexpression)
  \".\"    ->    \"\\.\"
  \"?\"    ->    \".\"
  \"|\"    ->    \"\\|\"
  \" \"    ->    \"\\|\"
  \"(\"    ->    \"\\(\"
  \")\"    ->    \"\\)\"
  \"\\\"   ->    Insert the next character without interpreting it.
                 (Quote the char if it is a regexp character).
NOTE: Characters not interpreted will be passed directly to the regexp.
Thus, any characters which are special to regular expressions should be
escaped by a backslash - unless it is desired they be interpreted as
regexps."
  (let ((beg 0) (depth 0) (atfront t) end c
	(specials-regexp "[*\\.?|() ]")
	(regexp "^\\("))
    (while (setq end (string-match specials-regexp wildcard beg))
      (if (> end beg) (setq atfront nil))
      (setq c (aref wildcard end)
	    regexp (concat regexp
			   (substring wildcard beg end)
			   (cond ((eq c ?*) (if atfront "[^.].*" ".*"))
				 ((eq c ?.) "\\.")
				 ((eq c ??) ".")
				 ((eq c ?|) (setq atfront (= depth 0)) "\\|")
				 ((eq c ? ) (setq atfront (= depth 0)) "\\|")
				 ((eq c ?\() (setq depth (1+ depth)) "\\(")
				 ((eq c ?\)) (setq depth (1- depth)) "\\)")
				 ((eq c ?\\) (regexp-quote
					      (substring wildcard
							 (setq end (1+ end))
							 (1+ end))))
				 ))
	    atfront nil
	    beg (1+ end)))
    (concat regexp (substring wildcard beg) "\\)$")))

(defun browser-file-do-tag-file ( tag )
  "Tag the current file for future file operations."
  (save-excursion
    (if (browser-file-this-entry)
	(progn
	  (move-to-column (* (/ (current-column) browser-file-entry-width)
			     browser-file-entry-width))
	  (if (null tag)
	      (setq tag ?\ ))
	  (let ((buffer-read-only nil))
	    (delete-char 1)
	    (insert tag)
	    (set-buffer-modified-p nil))))))

(defun browser-file-tag-file ( tag )
  "Tag the current file for future file operations."
  (interactive "P")
  (if (null tag)
      (setq tag browser-file-default-tag)
    (let ((cursor-in-echo-area t))
      (message "Character to use as tag ? ")
      (setq tag (read-char-exclusive))))
  (if (eq tag 0)
      (setq tag nil))
  (browser-file-do-tag-file tag))

(defun browser-file-untag-file ()
  "Remove any tags on the current file."
  (interactive)
  (browser-file-do-tag-file nil))

(defun browser-file-get-tag ()
  ;; Return the tag for the current entry
  (save-excursion
    (if (browser-file-this-entry)
	(let ((c (char-after (1- (point)))))
	  (if (eq c ? ) nil c)))))

(defun browser-file-untag-files ( tag )
  "Remove the supplied tag from all files with this tag."
  (interactive "P")
  (save-excursion
    (if (null tag)
	(if (null (setq tag (browser-file-get-tag)))
	    (setq tag browser-file-default-tag))
      (let ((cursor-in-echo-area t))
	(message "Character to use as tag ? ")
	(setq tag (read-char-exclusive))))
    (goto-char (point-min))
    (end-of-line)
    (while (re-search-forward
	    (concat "\\<" (regexp-quote (char-to-string tag)))
	    nil t)
      (if (eq (% (current-column) browser-file-entry-width) 1)
	  (browser-file-untag-file)))))

(defun browser-file-tag-regexp ( regexp &optional tag )
  "Tag files which match the supplied regexp."
  (interactive "sRegular Expression to match files ? \nP")
  (save-excursion
    (if (null tag)
	(setq tag browser-file-default-tag)
      (let ((cursor-in-echo-area t))
	(message "Character to use as tag ? ")
	(setq tag (read-char-exclusive))))
    (goto-char browser-start-of-listing)
    (browser-file-this-entry)
    (let (filename)
      (while (and (zerop (browser-file-next-entry 1))
		  (setq filename (browser-file-get-entry)))
	(if (string-match regexp filename)
	    (browser-file-do-tag-file tag))))))

(defun browser-file-tag-wildcard ( wildcard &optional tag )
  "Tag files which match the supplied regexp."
  (interactive "sWildcard to match files ? \nP")
  (let ((regexp (wildcard-to-regexp wildcard)))
    (if regexp
	(browser-file-tag-regexp regexp tag))))

(defun browser-file-get-tagged-files( &optional tag )
  "Return a list of all files with a given tag.
Use the default tag if no tag is supplied."
  (if (null tag)
      (setq tag browser-file-default-tag))
  (let (flist)
    (save-excursion
      (goto-char browser-start-of-listing)
      (while (re-search-forward
	      (concat "\\<" (regexp-quote (char-to-string tag)))
	      nil t)
	(if (eq (% (current-column) browser-file-entry-width) 1)
	    (setq flist (nconc flist (cons (browser-file-get-entry) nil))))))
    flist))

(defun browser-file-get-selected-files ()
  "Get a list of the files selected."
  (save-excursion
    (if (browser-file-this-entry)
	(let ((tag (browser-file-get-tag)))
	  (if tag
	      (browser-file-get-tagged-files tag)
	    (let ((filename (browser-file-get-entry)))
	      (if filename
		  (list filename))))))))

;; File visiting functions

(defun browser-file-find-file (filename &optional other-window)
  "Visit the current file in the browser file listing window."
  (let* ((buffer (find-file-other-window filename))
	 (window (get-buffer-window buffer t))
	 (frame (window-frame window)))
    (raise-frame frame)
    (select-window window)))

(defun browser-file-find-file-at-point ()
  "Visit the current file in the browser file listing window."
  (interactive)
  (let ((filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (browser-file-list filename)
	  (browser-file-find-file filename))
      (error "No filename selected."))))

(defun browser-file-view-file ()
  "Visit the current file (in view-mode) in the browser file listing window."
  (interactive)
  (let ((filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (browser-file-list filename)
	  (let ((old-buf (current-buffer))
		(had-a-buf (get-file-buffer filename))
		(buf-to-view (find-file-noselect filename)))
	    (set-buffer buf-to-view)
	    (view-mode (and (not had-a-buf)
			    (not (buffer-modified-p buf-to-view))
			    'kill-buffer))
	    (select-window (display-buffer buf-to-view))))
      (error "No filename selected."))))

(defun browser-file-find-file-other-window ()
  "Visit the current file in the browser file listing in another window."
  (interactive)
  (let ((filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (browser-new-listing filename)
	  (browser-file-find-file filename t))
      (error "No filename selected."))))

(defun browser-file-mouse-find-file (event)
  "Visit the file clicked on by the mouse."
  (interactive "e")
  (let (filename buffer)
    (save-excursion
      (setq buffer (window-buffer (posn-window (event-end event))))
      (set-buffer buffer)
      (goto-char (posn-point (event-end event)))
      (setq filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (progn
	      (set-buffer buffer)
	      (browser-file-list filename))
	  (browser-file-find-file filename)))))

(defun browser-file-mouse-find-file-other-window (event)
  "Visit the file clicked on by the mouse - in another window."
  (interactive "e")
  (let ((buffer (window-buffer (posn-window (event-end event)))))
    (save-excursion
      (set-buffer buffer)
      (goto-char (posn-point (event-end event)))
      (setq filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (progn
	      (set-buffer buffer)
	      (browser-new-listing filename))
	  (browser-file-find-file filename t))
      (error "No filename selected."))))

;; Functions to perform file operations

(defun browser-file-unhighlight-entry ()
  "Turn off highlighting of entry in the browser listing."
  (interactive)
  (let (overlay)
    (while browser-highlight-overlay-list
      (setq overlay (car browser-highlight-overlay-list))
      (overlay-put overlay 'face nil)
      (overlay-put overlay 'mouse-face 'highlight)
      (setq browser-highlight-overlay-list
	    (cdr browser-highlight-overlay-list)))))
  
(defun browser-file-highlight-entry ()
  "Highlight the current entry in the browser file listing."
  (interactive)
  (if (browser-file-this-entry)
      (let ((overlay-list (overlays-at (point))))
	(while (and overlay-list
		    (null (overlay-get (car overlay-list) 'mouse-face)))
	  (setq overlay-list (cdr overlay-list)))
	(if overlay-list
	    (let ((overlay (car overlay-list)))
	      (setq browser-highlight-overlay-list
		    (nconc browser-highlight-overlay-list (list overlay)))
	      (overlay-put overlay 'face 'browser-highlight-face)
	      (overlay-put overlay 'mouse-face nil))))))

(defun browser-file-highlight-entries ( highlight-list )
  (save-excursion
    (goto-char browser-start-of-listing)
    (browser-file-this-entry)
    (while (not (zerop (browser-file-next-entry 1)))
      (if (string-equal (browser-file-get-entry) (car highlight-list))
	  (progn
	    (browser-file-highlight-entry)
	    (setq highlight-list (cdr highlight-list)))))))

(defun browser-file-info ()
  "Display more detailed information on file."
  (interactive)
  (let ((filename (browser-file-get-entry))
	(directory default-directory))
    (if filename
	(save-excursion
	  (set-buffer (get-buffer-create "*Browser-file-info*"))
	  (widen)
	  (erase-buffer)
	  (cd directory)
	  (let ((handler (find-file-name-handler
			  filename 'insert-directory)))
	    (if handler
		(funcall handler 'insert-directory filename
			 browser-info-switches)
	      (insert-directory filename browser-info-switches)))
	  (goto-char (point-min))
	  (end-of-line)
	  (message (buffer-substring (point-min) (point))))
      (error "No filename selected."))))

(defun browser-out-of-date-p ()
  "Test if the browser listing directory has been modified since last read."
  (let ((buffer-file-name (expand-file-name
			   (file-chase-links default-directory))))
    (not (verify-visited-file-modtime (current-buffer)))))

(defun browser-internal-delete-file (filename)
  (if filename
      (if (file-directory-p filename)
	  (delete-directory filename)
	(delete-file filename))
    (error "No filename selected.")))

(defun browser-file-delete-file ()
  "Delete the current file in a browser file listing."
  (interactive)
  (let ((flist (browser-file-get-selected-files)))
    (if flist
	(progn
	  (map-y-or-n-p
	   "Delete file: %s "
	   (function browser-internal-delete-file)
	   flist
	   '("file" "files" "delete"))
	  (if (browser-out-of-date-p)
	      (browser-file-relist)))
      (error "No filename selected."))))

(defun browser-file-byte-compile-file ()
  "Byte compile the current file in a browser file listing."
  (interactive)
  (let ((flist (browser-file-get-selected-files)))
    (if flist
	(progn
	  (map-y-or-n-p
	   "Byte-Compile file: %s "
	   (function byte-compile-file)
	   flist
	   '("file" "files" "Byte-Compile"))
	  (if (browser-out-of-date-p)
	      (browser-file-relist)))
      (error "No filename selected."))))

(defun browser-internal-copy-file ( filename newfilename )
  (if (and filename newfilename)
      (progn
	(if (file-directory-p newfilename)
	    (setq newfilename
		  (expand-file-name (file-name-nondirectory filename)
				    (file-name-as-directory newfilename))))
	(copy-file filename newfilename 1 t))
    (error "No filename selected.")))

(defun browser-file-copy-file ()
  "Copy the current file in the listing to another file or directory."
  (interactive)
  (let ((flist (browser-file-get-selected-files))
	(filename "files"))
    (if flist
	(progn
	  (if (eq (length flist) 1)
	      (setq filename (car flist)))
	  (let ((newfilename (read-file-name
			      (format "Copy %s to: " filename) nil "")))
	    (if (and newfilename (> (length newfilename) 0))
		(if (and (> (length flist) 1)
			 (not (file-directory-p newfilename)))
		    (error "Destination must be a directory if copying more than one file.")
		  (map-y-or-n-p
		   (concat "Copy %s to " newfilename " ")
		   (` (lambda (arg)
			(browser-internal-copy-file arg (, newfilename))))
		   flist
		   '("file" "files" "copy"))
		  (if (browser-out-of-date-p)
		      (browser-file-relist)))
	      (message "%s not copied" filename))))
      (error "No filename selected."))))

(defun browser-file-rename-file ()
  "Rename the current file in the listing."
  (interactive)
  (let ((filename (browser-file-get-entry)))
    (if filename
	(let ((newfilename (read-file-name
			    (format "Rename %s to: " filename) "" "")))
	  (if (and newfilename (> (length newfilename) 0))
	      (progn
		(rename-file filename newfilename 1)
		(if (browser-out-of-date-p)
		    (browser-file-relist)))
	    (message "File %s not renamed" filename)))
      (error "No filename selected."))))

(defun browser-internal-move-file ( filename dest-dir )
  (if (and filename dest-dir)
      (rename-file filename
		   (expand-file-name (file-name-nondirectory filename)
				     (file-name-as-directory dest-dir))
		   1)
    (error "No filename selected.")))

(defun browser-file-move-file ()
  "Move the current file in the listing."
  (interactive)
  (let ((flist (browser-file-get-selected-files))
	(filename "files"))
    (if flist
	(progn
	  (if (eq (length flist) 1)
	      (setq filename (car flist)))
	  (let ((dest-dir (read-file-name
			   (format "Move %s to directory: " filename) nil "")))
	    (if (and dest-dir (> (length dest-dir) 0))
		(if (not (file-directory-p dest-dir))
		    (error (substitute-command-keys "Destination must be a directory - use \\[browser-file-rename-file] to rename a file."))
		  (map-y-or-n-p
		   (concat "Move %s to " dest-dir " ")
		   (` (lambda (arg)
			(browser-internal-move-file arg (, dest-dir))))
		   flist
		   '("file" "files" "move"))
		  (if (browser-out-of-date-p)
		      (browser-file-relist)))
	      (message "%s not moved" filename))))
      (error "No filename selected."))))

(defun browser-file-symlink-file (flag)
  "Make a symbolic link to the current file in the listing."
  (interactive "P")
  (let ((filename (browser-file-get-entry)))
    (if filename
	(let ((newfilename (read-file-name
			    (format "Link to %s - Name of link: " filename) nil "")))
	  (if (and newfilename (> (length newfilename) 0))
	      (progn
		(setq filename (expand-file-name filename))
		(setq newfilename (expand-file-name newfilename))
		(setq filename (file-relative-name filename
						   (file-name-directory newfilename)))
		(make-symbolic-link filename newfilename 1)
		(if (browser-out-of-date-p)
		    (browser-file-relist)))
	    (message "File %s not linked" filename)))
      (error "No filename selected."))))

(defun browser-drag-and-drop-file (event action)
  "Track mouse movements till a button is released.
Returns a list containing the name of the file first clicked on, and
the directory name where the mouse button was released, and a symbol
representing the action slected by the user ('copy or 'move)."
  (mouse-minibuffer-check event)
  (let* ((posn (event-start event))
	 (window (posn-window posn))
	 (start-buffer (window-buffer window))
	 end-buffer filename flist dirname file-point frame)
    (set-buffer start-buffer)
    (save-excursion
      (mouse-set-point event)
      (setq file-point (point))
      (cond ((eq major-mode 'browser-file-mode)
	     (if (setq flist (browser-file-get-selected-files))
		 (browser-file-highlight-entries flist)))
	    ((eq major-mode 'browser-mode)
	     (setq flist (list (browser-get-dir-name))))
	    (t
	     (error "Not a File Browser window"))))
    (if flist
	(progn
	  (deactivate-mark)
	  (progn ; unwind-protect
	      (save-excursion
		(if (eq (length flist) 1)
		    (setq filename (car flist))
		  (setq filename "files"))
		(track-mouse
		  (while (progn
			   (if (char-or-string-p event)
			       (cond ((eq event ?m)
				      (setq action 'move))
				     ((eq event ?c)
				      (setq action 'copy))
				     (t
				      (beep t)))
			     (cond ((eq action 'move)
				    (message "Move %s to.. (drag and release,`m'-Move,`c'-Copy)" filename))
				   ((eq action 'copy)
				    (message "Copy %s to.. (drag and release,`m'-Move,`c'-Copy)" filename))))
			   (setq event (read-event))
			   (if (and (listp event)
				    (framep (setq frame (posn-window (event-end event)))))
			       (progn
				 (message "Selecting frame %S" frame)
				 (select-frame frame)))
			   (or (not (listp event))
			       (mouse-movement-p event)
			       (eq (car event) 'switch-frame)))))
		(if (not (consp event))
		    ()
		  (setq posn (event-end event)
			window (posn-window posn)
			end-buffer (window-buffer window))
		  (if (and (eq start-buffer end-buffer)
			   (eq major-mode 'browser-file-mode))
		      (mouse-set-point event)
		    (set-buffer end-buffer)
		    (cond ((eq major-mode 'browser-file-mode)
			   (setq dirname default-directory))
			  ((eq major-mode 'browser-mode)
			   (mouse-set-point event)
			   (setq dirname (browser-get-dir-name)))
			  (t
			   (error "Drag to a non-File Browser window"))))))
	    (save-excursion
	      (goto-char file-point)
	      (if (eq major-mode 'browser-file-mode)
		  (browser-file-unhighlight-entry))))
	  (if (and flist dirname)
	      (list flist dirname action)
	    (cond ((eq action 'copy)
		   (error "%s not copied." filename))
		  ((eq action 'move)
		   (error "%s not moved." filename)))))
      (error "No filename selected."))))

(defun browser-drag-copy-or-move-file (event action)
  "Copy or move a file on the disk, as a user \"drags\" a file to another
browser window or directory entry."
  (unwind-protect
      (let ((x-pointer-shape x-pointer-hand2))
	(set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
	(let ((from-to (browser-drag-and-drop-file event action))
	      (filename nil)
	      (flist nil)
	      (newfilename nil)
	      (action-name nil))
	  (if (not from-to)
	      ()
	    (setq flist (nth 0 from-to))
	    (setq newfilename (nth 1 from-to))
	    (setq action (nth 2 from-to))
	    (cond ((eq action 'copy)
		   (setq action-name "Copy"))
		  ((eq action 'move)
		   (setq action-name "Move")))
	    (map-y-or-n-p
	     (concat action-name " %s to " newfilename " ")
	     (cond ((eq action 'copy)
		    (` (lambda (arg)
			 (browser-internal-copy-file arg (, newfilename)))))
		   ((eq action 'move)
		    (` (lambda (arg)
			 (browser-internal-move-file arg (, newfilename))))))
	     flist
	     (list "file" "files" action-name))
	    (if (browser-out-of-date-p)
		(browser-file-relist))
	    (save-excursion
	      (let ((dirname (file-name-directory newfilename)))
		(mapcar (function
			 (lambda (buffer)
			   (set-buffer buffer)
			   (if (and (string-equal dirname
						  default-directory)
				    (browser-out-of-date-p))
			       (browser-file-relist))))
			(cdr browser-buffer-list)))))))
    (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))))

(defun browser-drag-copy-file (event)
  "Copy a file on the disk, as a user \"drags\" a file to another
browser window or directory entry."
  (interactive "e")
  (browser-drag-copy-or-move-file event 'copy))

(defun browser-drag-move-file (event)
  "Move a file on the disk, as a user \"drags\" a file to another
browser window or directory entry."
  (interactive "e")
  (browser-drag-copy-or-move-file event 'move))

(defun browser-shell ( fmt )
  (if (null browser-fileops-files)
      (setq browser-fileops-files (browser-file-get-selected-files)))
  (if browser-fileops-files
      (map-y-or-n-p (concat fmt " ")
		    (` (lambda ( arg )
			 (let ((command (format (, fmt) arg)))
			   (message "Shell command: %s..." command)
			   (shell-command command))))
		    browser-fileops-files
		    '("Command" "Commands" "Execute"))
    (error "No filename selected."))
  (setq browser-fileops-files nil)
  (if (browser-out-of-date-p)
      (browser-file-relist)))

(defun browser-shell-list ( fmt )
  (if (null browser-fileops-files)
      (setq browser-fileops-files (browser-file-get-selected-files)))
  (if browser-fileops-files
      (let ((files (apply 'concat browser-fileops-files)))
	(map-y-or-n-p (concat fmt " ")
		      (` (lambda ( arg )
			   (let ((command (format (, fmt) arg)))
			     (message "Shell command: %s..." command)
			     (shell-command command))))
		      files
		      '("Command" "Commands" "Execute"))
	(error "No filename selected.")))
  (setq browser-fileops-files nil)
  (if (browser-out-of-date-p)
      (browser-file-relist)))

(defvar browser-fileops-menu-alist
  (mapcar
   (function (lambda (arg) (if (stringp (car arg))
			       (cons (wildcard-to-regexp (car arg)) (cdr arg))
			     arg)))
   '(("*.el .emacs"
      ["Byte-Compile"		browser-file-byte-compile-file t]
      ["Load Elisp file"	(mapcar 'load-file browser-fileops-files) t])
     ((lambda (arg) (and (file-executable-p arg) (not (file-directory-p arg))))
      ["Execute"		(browser-shell "%s") t]
      ["Execute in background"	(browser-shell "%s &") t])
     (file-directory-p
      ["Dired"			(mapcar 'find-file-other-window browser-fileops-files) t])
     ((lambda (arg) (not (or (file-directory-p arg)
			     (string-match "\\.\\(gz\\|[zZ]\\)$" arg))))
      ["Gzip"			(browser-shell "gzip %s") t])
     ("*.gz"
      ["Gunzip"			(browser-shell "gunzip %s") t])
     ("*.[zZ]"
      ["Uncompress"		(browser-shell "uncompress %s") t])
     ))
  "Builtin list of context sensitive menus for file operations.")

(defconst browser-fileops-keymap-alist nil
  "List of keymaps for the file operations popup menu.
The keymaps are derived from the contents of the
`browser-fileops-menu-alist' variable.")

(defun browser-add-menu ( menu-alist )
  "Add new menus to the list of dynamic menus for file operations.
MENU-ALIST should be a list of sub-lists. Each sub-list contains a
wildcard for matching filenames (or a function to be evaluated), and
an easy-menu style menu list. If the wildcard matches the selected
filename (or the function returns non-nil) this menu will be added to
the File Operations menu-bar menu, and the File Operations popup menu.
The function should take a filename as an argument."
  (setq browser-fileops-menu-alist
	(append browser-fileops-menu-alist
		(mapcar (function (lambda (arg)
				    (if (stringp (car arg))
					(cons (wildcard-to-regexp (car arg))
					      (cdr arg))
				      arg)))
			menu-alist)))
  (setq browser-fileops-keymap-alist
	(mapcar (function
		 (lambda ( menu )
		   (cons (car menu)
			 (easy-menu-create-keymaps (car menu) (cdr menu)))))
		browser-fileops-menu-alist)))

(defun browser-add-menu-regexp ( menu-alist )
  "Add new menus to the list of dynamic menus for file operations.
MENU-ALIST should be a list of sub-lists. Each sub-list contains a
regular expression for matching filenames (or a function to be evaluated),
and an easy-menu style menu list. If the regexp matches the selected
filename (or the function returns non-nil) this menu will be added to
the File Operations menu-bar menu, and the File Operations popup menu.
The function should take a filename as an argument."
  (setq browser-fileops-menu-alist
	(append browser-fileops-menu-alist menu-alist))
  (setq browser-fileops-keymap-alist
	(mapcar (function
		 (lambda ( menu )
		   (cons (car menu)
			 (easy-menu-create-keymaps (car menu) (cdr menu)))))
		browser-fileops-menu-alist)))

(browser-add-menu nil)

(defun browser-popup-fileops-menu ( event )
  "Popup a menu of file operations for file under pointer."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    (let (menu keyseq func)
      (setq browser-fileops-files (browser-file-get-selected-files))
      (setq menu (browser-make-fileops-keymap browser-fileops-files))
      (and (setq keyseq (x-popup-menu event menu))
	   (setq func (lookup-key menu (apply 'vector keyseq)))
	   (command-execute func))))
  (setq browser-fileops-files nil))

(defun browser-dynamic-fileops-menu ()
  "Bind a dynamic file operations menu to the Operate menu in the menu-bar."
  (condition-case nil
      (if (eq major-mode 'browser-file-mode)
	  (progn
	    (setq browser-fileops-files (browser-file-get-selected-files))
	    (define-key browser-file-mode-map [menu-bar Operate]
	      (cons "Operate"
		    (browser-make-fileops-keymap browser-fileops-files)))))
    (error nil)))

;; Use Per Abrahamsen's `custom.el' to provide a user interface to the
;; various user configurable options in the filebrowser.

(if browser-use-custom
    (custom-declare
     '()
     '((tag . "File Browser")
       (doc . "\
A simple tool for file and directory browsing.
Including support for:
	- simultaneous access to multiple file trees
	- single click navigation in directory tree
	- single click loading of files
	- context sensitive file operations menu
	- `drag and drop' copying and moving of files
	- browsing remote filesystems (via ftp)")
       (type . group)
       (data ((tag . "Pop-up New Frame")
	      (name . browser-pop-up-frames)
	      (default . t)
	      (type . toggle))
	     ((tag . "Default listing directory for popup browser")
	      (name . browser-default-directory)
	      (default . "~/")
	      (type . string))
	     ((tag . "Tree Window Width")
	      (name . browser-tree-window-width)
	      (default . 18)
	      (type . integer))
	     ((tag . "Default Tree Depth")
	      (name . browser-dir-depth)
	      (default . 1)
	      (type . integer))
	     ((tag . "Maximum Tree Depth")
	      (name . browser-dir-max-depth)
	      (default . 50)
	      (type . integer))
	     ((tag . "File Information Switches")
	      (name . browser-info-switches)
	      (default . "-ldqF")
	      (type . string))
	     ((tag . "Inhibit `Dot-Files'")
	      (name . browser-inhibit-dots)
	      (default . t)
	      (type . toggle))
	     ((tag . "Files to List")
	      (name . browser-file-regexp)
	      (default . "")
	      (import . (lambda (c v) (list (or v ""))))
	      (export . (lambda (c v) (if (= (length v) 0) nil v)))
	      (type . string))
	     ((tag . "Files to Suppress")
	      (name . browser-file-inhibit-regexp)
	      (default . "")
	      (import . (lambda (c v) (list (or v ""))))
	      (export . (lambda (c v) (if (= (length v) 0) nil v)))
	      (type . string))
	     ((tag . "Directories to List")
	      (name . browser-tree-regexp)
	      (default . "")
	      (type . string)
	      (import . (lambda (c v) (list (or v ""))))
	      (export . (lambda (c v) (if (= (length v) 0) nil v))))
	     ((tag . "Directories to Suppress")
	      (name . browser-tree-inhibit-regexp)
	      (default . "")
	      (type . string)
	      (import . (lambda (c v) (list (or v ""))))
	      (export . (lambda (c v) (if (= (length v) 0) nil v))))
	     ((tag . "Sort by Extension")
	      (name . browser-sort-by-extension)
	      (default . nil)
	      (type . toggle))
	     ((tag . "Tag for Marked Files")
	      (name . browser-file-default-tag)
	      (default . ?*)
	      (type . string)
	      (width . 1)
	      (import . (lambda (c v) (list (char-to-string v))))
	      (export . (lambda (c v) (elt v 0))))
	     ((tag . "Directory Cache Size")
	      (name . browser-file-cache-size)
	      (default . 10)
	      (type . integer))
	     ((tag . "Use Dired")
	      (name . browser-use-dired)
	      (default . nil)
	      (type . toggle))
	     ((tag . "Narrowing in Dired")
	      (name . browser-use-dired-narrowing)
	      (default . nil)
	      (type . toggle))
	     ))))

(suppress-keymap browser-file-mode-map)

(browser-keymap-define
 browser-file-mode-map
 '(
   ;; Browser buffer navigation keys
   ([tab]	. browser-file-next-entry)
   ([M-tab]	. browser-file-prev-entry)
   ("p"		. previous-line)
   ("n"		. next-line)
   ;; Browser action keys
   ("\C-c\C-d"	. browser-file-list)
   (" "		. browser-file-find-file-at-point)
   ("o"		. browser-file-find-file-other-window)
   ("v"		. browser-file-view-file)
   ("g"     	. browser-file-relist)
   ("l"     	. browser-file-info)
   ("."     	. browser-file-toggle-dots)
   ("q"     	. kill-this-buffer)
   ("Q"     	. browser-quit)
   ("B"     	. browser-bury-browser)
   ("@"     	. browser-configure-windows)
   ("C"	    	. browser-customise)
   ;; File operation keys
   ("d"     	. browser-file-delete-file)
   ("b"     	. browser-file-byte-compile-file)
   ("c"     	. browser-file-copy-file)
   ("r"     	. browser-file-rename-file)
   ("m"     	. browser-file-move-file)
   ("s"     	. browser-file-symlink-file)
   ("t"     	. browser-file-tag-file)
   ("u"     	. browser-file-untag-file)
   ("U"     	. browser-file-untag-files)
   ("*"     	. browser-file-tag-wildcard)
   ("%"     	. browser-file-tag-regexp)
   ;; Mouse button bindings
   ([down-mouse-3]	. browser-popup-fileops-menu)
   ([mouse-2] 		. browser-file-mouse-find-file)
   ([S-mouse-2] 	. browser-file-mouse-find-file-other-window)
   ([S-down-mouse-1] 	. browser-drag-copy-file)
   ([M-S-down-mouse-1]	. browser-drag-move-file)

   ([menu-bar edit] 	. undefined)
   ([menu-bar search] 	. undefined)))

;; Menu bar bindings
(require 'easymenu)

(easy-menu-define
 browser-file-cache-menu browser-file-mode-map
 "Browser Cache Listing menu"
 '("Cache"))

(easy-menu-define
 browser-file-operate-menu browser-file-mode-map
 "Browser File Operations menu" 
 '("Operate"
   ["Open"			browser-file-find-file-at-point 'browser-fileops-files]
   ["Copy"			browser-file-copy-file t]
   ["Rename"			browser-file-rename-file t]
   ["Move"			browser-file-move-file t]
   ["Delete"			browser-file-delete-file t]
   ["Tag"			browser-file-tag-file t]
   ["Untag"			browser-file-untag-file t]))

(defvar browser-fileops-default-keymap nil
  "Keymap for default file operations menu.")

;; Emacs version < 19.28 does not set the keymap variable - so look it up
(setq browser-fileops-default-keymap
      (or (and (boundp 'browser-file-operate-menu)
	       browser-file-operate-menu)
	  (lookup-key browser-file-mode-map [menu-bar Operate])))

(defun browser-make-fileops-keymap ( filelist )
  "Make a dynamic filename dependant keymap for the file operations menu."
  (let ((fileopslist browser-fileops-keymap-alist)
	(filename (car-safe filelist))
	menu selector)
    (if (null filename)
	(setq menu (list 'keymap (list nil "No File Selected")))
      (if (= (length filelist) 1)
	  (setq menu (list 'keymap (list nil (concat "File: " filename))))
	(setq menu (list 'keymap
			 (list nil (concat "Files: " filename))
			 (list nil (concat "   ... " (nth (1- (length filelist))
							  filelist))))))
      (while fileopslist
	(setq selector (car (car fileopslist)))
	(if (if (stringp selector)
		(string-match selector filename)
	      (funcall selector filename))
	    (setq menu (append menu (cons '(nil "---")
					  (cdr (cdr (car fileopslist)))))))
	(setq fileopslist (cdr fileopslist))))
    (setq menu (append menu (cons '(nil "---")
				  (cdr browser-fileops-default-keymap))))))

(easy-menu-define
 browser-file-browser-menu browser-file-mode-map
 "Browser menu"
 '("Browser"
   ["Open File"			browser-file-find-file-at-point t]
   ["Open File - other window"	browser-file-find-file-other-window t]
   ["View File - read-only"	browser-file-view-file t]
   ["Display File Info"		browser-file-info t]
   ["Re-read"			browser-file-relist t]
   ["Redraw Windows"		browser-configure-windows t]
   ["Kill Buffer"		kill-this-buffer t]
   ["Customise"			browser-customise t]))

(provide 'filebrowser)
