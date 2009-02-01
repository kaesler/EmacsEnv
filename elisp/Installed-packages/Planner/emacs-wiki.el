;;; emacs-wiki.el --- Maintain a local Wiki using Emacs-friendly markup

;; Copyright (C) 2001 John Wiegley <johnw@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki.el
;; Version: 2.7
;; Keywords: hypermedia
;; Author: John Wiegley <johnw@gnu.org>
;;         Alex Schroeder <alex@gnu.org>
;; Maintainer: John Wiegley <johnw@gnu.org>
;; Description: Maintain Emacs-friendly Wikis in a local directory
;; URL: http://www.gci-net.com/~johnw/Emacs/emacs-wiki.el
;; Compatibility: Emacs20, Emacs21, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Wiki is a concept, more than a thing.  It is a way of creating
;; document pages using plain text markup and simplified hyperlinking.

;; By typing a name in MixedCase, a hyperlink is automatically created
;; to the document "MixedCase".  Pressing return on that name will
;; create the file if it doesn't exist, or visit it if it does.

;; The markup used by emacs-wiki is intended to be very friendly to
;; people familiar with Emacs.  See the documentation for the variable
;; `emacs-wiki-publishing-markup' for a full description.

;; - Startup

;; To begin using emacs-wiki, put this in your .emacs file:

;;   (load "emacs-wiki")

;; Now you can type M-x emacs-wiki-find-file, give it a WikiName (or
;; just hit return) and start typing!

;; You should also type M-x customize-group, and give the name
;; "emacs-wiki".  Change it to suite your preferences.  Each of the
;; options has its own documentation.

;; - Keystroke summary

;; Here is a summary of keystrokes available in every Wiki buffer:

;;   C-c C-a    jump to an index of all the Wiki pages
;;   C-c C-b    show all pages that reference this page
;;   C-c C-s    search for a word in your Wiki pages
;;   C-c C-f    jump to another Wiki page; prompts for the name
;;   C-c C-l    highlight/refresh the current buffer
;;   C-c C-p    publish any Wiki pages that have changed as HTML
;;   C-c C-r    reveal all hidden text in the buffer
;;   C-c =      diff this page against the last backup version
;;   TAB        move to the next Wiki reference
;;   S-TAB      move to the previous Wiki reference

;; - Using pcomplete

;; If you have pcomplete loaded, you can type M-TAB to complete Wiki
;; names.  Hitting M-TAB twice or more time in succession, will cycle
;; through all of the possibilities.  You can download pcomplete from
;; my Website:

;;   http://www.gci-net.com/~johnw/emacs.html

;; - ChangeLog support

;; If you use a ChangeLog (C-x 4 a) within one of your Wiki
;; directories, it will be used for notifying visitors to your wiki of
;; recent changes.

;; - <lisp> tricks

;; <lisp></lisp> tags can be used, not only to evaluate forms for
;; insertion at that point, but to influence the publishing process in
;; many ways.  Let's say you want to change the background image for a
;; particular page, or center its title.  The best way to do this is
;; using a custom stylesheet for that page.  And here's how to change
;; the stylesheet for a single page, in that page:

;; <lisp>
;; (ignore
;;   ;; use special.css for this Wiki page
;;   (set (make-variable-buffer-local 'emacs-wiki-style-sheet)
;;        "<link rel=\"stylesheet\" type=\"text/css\" href=\"special.css\">"))
;; </lisp>

;; The 'ignore' is needed so nothing is inserted where the <lisp> tag
;; occurred.  Also, there should be no blank lines before or after the
;; tag (to avoid empty paragraphs from being created).  The best place
;; to put this would be at the very top or bottom of the page.

;;; Thanks

;; Alex Schroeder <alex@gnu.org>, current author of "wiki.el".
;;   His latest version is here:
;;       http://www.geocities.com/kensanata/wiki/WikiMode.html
;;
;; Frank Gerhardt <Frank.Gerhardt@web.de>, author of the original wiki-mode
;;   His latest version is here:
;;       http://www.s.netic.de/fg/wiki-mode/wiki.el
;;
;; Thomas Link <t.link@gmx.at>

;;; Code:

;; The parts of this code, and work to be done:
;;
;; * setup emacs-wiki major mode
;; * generate WikiName list
;; * utility functions to extract link parts
;; * open a page
;; * navigate links in the buffer
;; * visit a link
;; * search Wiki pages for text/backlinks
;; * index generation
;; * buffer highlighting (using font-lock)
;; * HTML publishing
;;   - Allow for alternate markup tables: DocBook, xhtml, etc.
;; * HTTP serving (using httpd.el)
;;   - Diffing (look at using highlight-changes-mode and htmlify.el)
;;   - Editing (requires implementing POST method for httpd.el)

(require 'derived)

(load "pcomplete" t t)			; optional

;;; Options:

(defgroup emacs-wiki nil
  "Options controlling the behaviour of Emacs Wiki Mode.
Wiki is a concept, more than a thing.  It is a way of creating
document pages using plain text markup and simplified hyperlinking.

By typing a name in MixedCase, a hyperlink is automatically created
to the document \"MixedCase\".  Pressing return on that name will
create the file if it doesn't exist, or visit it if it does.

The markup used by emacs-wiki is intended to be very friendly to
people familiar with Emacs.  See the documentation for the variable
`emacs-wiki-publishing-markup' for a full description."
  :group 'hypermedia)

(defcustom emacs-wiki-mode-hook (unless (featurep 'httpd)
				  '(emacs-wiki-use-font-lock))
  "A hook that is run when emacs-wiki mode is entered."
  :type 'hook
  :options '(emacs-wiki-use-font-lock
	     flyspell-mode
	     footnote-mode
	     highlight-changes-mode)
  :group 'emacs-wiki)

;;;###autoload
(defcustom emacs-wiki-directories '("~/Wiki")
  "A list of directories where Wiki pages can be found."
  :require 'emacs-wiki
  :type '(repeat :tag "Wiki directories" directory)
  :group 'emacs-wiki)

(defcustom emacs-wiki-home-page "WelcomePage"
  "Name of the Wiki home page.
This is the default target for \\[emacs-wiki-find-file].
It is also linked to as \"Home\" in the default HTML footer."
  :type 'string
  :group 'emacs-wiki)

(defcustom emacs-wiki-file-ignore-regexp
  "\\`\\(\\.?#.*\\|CVS\\|.*,v\\|.*~\\|\\.\\.?\\)\\'"
  "A regexp matching files to be ignored in Wiki directories."
  :type 'regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-interwiki-names
  '(("GnuEmacs" . "http://www.gnu.org/software/emacs/emacs.html")
    ("MeatballWiki" .
     (lambda (tag)
       (concat "http://www.usemod.com/cgi-bin/mb.pl?"
	       (or tag "MeatballWiki")))))
  "A table of WikiNames that refer to external entities.
The format of this table is an alist, or series of cons cells.
Each cons cell must be of the form:

  (WIKINAME . STRING-OR-FUNCTION)

The second part of the cons cell may either be a STRING, which in most
cases should be a URL, or a FUNCTION.  If a function, it will be
called with one argument: the tag applied to the Interwiki name, or
nil if no tag was used.  If the cdr was a STRING and a tag is used,
the tag is simply appended.

Here are some examples:

  (\"JohnWiki\" . \"http://alice.dynodns.net/wiki?\")

Referring to JohnWiki#EmacsModules then really means:

  http://alice.dynodns.net/wiki?EmacsModules

If a function is used for the replacement text, you can get creative
depending on what the tag is.  Tags may contain any alphabetic
character, any number, % or _.  If you need other special characters,
use % to specify the hex code, as in %2E.  All browsers should support
this."
  :type '(repeat (cons (string :tag "WikiName")
		       (choice (string :tag "URL") function)))
  :group 'emacs-wiki)

(defvar emacs-wiki-url-or-name-regexp nil
  "Matches either a Wiki link or a URL.  This variable is auto-generated.")

(defcustom emacs-wiki-extended-link-regexp
  "\\[\\[\\([^][ \t\n]+\\)\\]\\(\\[\\([^][\n]+\\)\\]\\)?\\]"
  "Regexp used to match [[extended][links]]."
  :type 'regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-name-regexp
  (concat "\\(" emacs-wiki-extended-link-regexp "\\|"
	  "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\(#[A-Za-z0-9_%]+\\)?\\)")
  "Regexp used to match WikiNames."
  :type 'regexp
  :set (function
	(lambda (sym value)
	  (if (boundp 'emacs-wiki-url-regexp)
	      (setq emacs-wiki-url-or-name-regexp
		    (concat "\\(" value "\\|" emacs-wiki-url-regexp "\\)")))
	  (set sym value)))
  :group 'emacs-wiki)

(defcustom emacs-wiki-url-regexp
  (concat "\\<\\(https?:/?/?\\|ftp:/?/?\\|gopher://\\|"
	  "telnet://\\|wais://\\|file:/\\|s?news:\\)"
	  "[^]	\n \"'()<>[^`{}]*[^]	\n \"'()<>[^`{}.,;]+")
  "A regexp used to match URLs within a Wiki buffer."
  :type 'regexp
  :set (function
	(lambda (sym value)
	  (setq emacs-wiki-url-or-name-regexp
		(concat "\\(" emacs-wiki-name-regexp "\\|" value "\\)"))
	  (set sym value)))
  :group 'emacs-wiki)

(defcustom emacs-wiki-browse-url-function 'browse-url
  "Function to call to browse a URL."
  :type 'function
  :group 'emacs-wiki)

(defcustom emacs-wiki-grep-command
  "find %D -type f ! -name '*~' | xargs egrep -n -e \"\\<%W\\>\""
  "The name of the program to use when grepping for backlinks.
The string %D is replaced by `emacs-wiki-directiories', space-separated.
The string %W is replaced with the name of the Wiki page.

Note: I highly recommend using glimpse to search large Wikis.  To use
glimpse, install and edit a file called .glimpse_exclude in your home
directory.  Put a list of glob patterns in that file to exclude Emacs
backup files, etc.  Then, run the indexer using:

  glimpseindex -o <list of Wiki directories>

Once that's completed, customize this variable to have the following
value:

  glimpse -nyi \"%W\"

Your searches will go much, much faster, especially for very large
Wikis.  Don't forget to add a user cronjob to update the index at
intervals."
  :type 'string
  :group 'emacs-wiki)

(defvar emacs-wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?a)] 'emacs-wiki-index)
    (define-key map [(control ?c) (control ?f)] 'emacs-wiki-find-file)
    (define-key map [(control ?c) (control ?b)] 'emacs-wiki-backlink)
    (define-key map [(control ?c) (control ?s)] 'emacs-wiki-search)
    (define-key map [(control ?c) (control ?p)] 'emacs-wiki-publish)

    (define-key map [(control ?c) (control ?l)] 'font-lock-mode)

    (define-key map [(control ?c) ?=]
      (lambda ()
	(interactive)
	(diff-backup buffer-file-name)))

    (define-key map [tab] 'emacs-wiki-next-reference)
    (define-key map [(control ?i)] 'emacs-wiki-next-reference)

    (if (featurep 'xemacs)
	(define-key map [(shift tab)] 'emacs-wiki-previous-reference)
      (define-key map [(shift iso-lefttab)] 'emacs-wiki-previous-reference)
      (define-key map [(shift control ?i)] 'emacs-wiki-previous-reference))

    (when (featurep 'pcomplete)
      (define-key map [(meta tab)] 'pcomplete)
      (define-key map [(meta control ?i)] 'pcomplete))

    map)
  "Keymap used by Emacs Wiki mode.")

(defvar emacs-wiki-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'emacs-wiki-follow-name-at-point)
    (define-key map [(control ?m)] 'emacs-wiki-follow-name-at-point)
    (if (featurep 'xemacs)
	(define-key map [(button2)] 'emacs-wiki-follow-name-at-mouse)
      (define-key map [mouse-2] 'emacs-wiki-follow-name-at-mouse))
    map)
  "Local keymap used by emacs-wiki while on a WikiName.")

;; Code:

;;;###autoload
(define-derived-mode emacs-wiki-mode text-mode "Wiki"
  "An Emacs mode for maintaining a local Wiki database.

Wiki is a hypertext and a content management system: Normal users are
encouraged to enhance the hypertext by editing and refactoring existing
wikis and by adding more.  This is made easy by requiring a certain way
of writing the wikis.  It is not as complicated as a markup language
such as HTML.  The general idea is to write plain ASCII.

Words with mixed case such as ThisOne are WikiNames.  WikiNames are
links you can follow.  If a wiki with that name exists, you will be
taken there.  If such a does not exist, following the link will create a
new wiki for you to fill.  WikiNames for non-existing wikis have a `?'
appended so that you can see wether following the link will give you any
informatin or not.

In order to follow a link, hit RET when point is on the link, or use
mouse-2.

All wikis reside in the `emacs-wiki-directories'.

\\{emacs-wiki-mode-map}"
  ;; if pcomplete is available, set it up!
  (when (featurep 'pcomplete)
    (set (make-variable-buffer-local 'pcomplete-default-completion-function)
	 'emacs-wiki-completions)
    (set (make-variable-buffer-local 'pcomplete-command-completion-function)
	 'emacs-wiki-completions)
    (set (make-variable-buffer-local 'pcomplete-parse-arguments-function)
	 'emacs-wiki-current-word)))

(defun emacs-wiki-maybe (&optional check-only)
  "Maybe turn Emacs Wiki mode on for this file."
  (let ((d emacs-wiki-directories)
	(here (directory-file-name (expand-file-name default-directory)))
	yes)
    (while d
      (if (string= here (expand-file-name (car d)))
	  (setq yes t d nil)
	(setq d (cdr d))))
    (if (and yes (not check-only))
	(emacs-wiki-mode))
    yes))

;; Unless planner is loaded (which will call emacs-wiki-maybe), add
;; emacs-wiki to the find-file-hooks
(unless (memq 'planner-maybe find-file-hooks)
  (add-hook 'find-file-hooks 'emacs-wiki-maybe))

;;; Support WikiName completion using pcomplete

(defun emacs-wiki-completions ()
  "Return a list of possible completions names for this buffer."
  (mapcar
   (function
    (lambda (member)
      (car member)))
   (append (emacs-wiki-file-alist) emacs-wiki-interwiki-names)))

(defun emacs-wiki-current-word ()
  (let ((end (point)))
    (save-restriction
      (save-excursion
	(skip-chars-backward "^\\[ \t\n")
	(narrow-to-region (point) end))
      (pcomplete-parse-buffer-arguments))))

;;; Return an list of known wiki names and the files they represent.

(defvar emacs-wiki-last-update nil)
(defvar emacs-wiki-file-alist nil)

(defsubst emacs-wiki-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defsubst emacs-wiki-page-name (&optional name)
  "Return the canonical form of the Wiki page name.
All this means is that certain extensions, like .gz, are removed."
  (let ((page (file-name-nondirectory (or name buffer-file-name))))
    (if (string-match emacs-wiki-ignored-extensions-regexp page)
	(replace-match "" t t page)
      page)))

(defvar emacs-wiki-under-windows-p (memq system-type '(ms-dos windows-nt)))

(defun emacs-wiki-file-alist (&optional no-check-p)
  "Return possible Wiki filenames in `emacs-wiki-directories'.
On UNIX, this list is only updated if one of the directories' contents
have changed.  On Windows, it is always reread from disk."
  (let ((dirs emacs-wiki-directories) last-mod)
    (unless (or emacs-wiki-under-windows-p no-check-p)
      (while dirs
	(let ((mod-time (nth 5 (file-attributes (car dirs)))))
	  (if (or (null last-mod)
		  (and mod-time (emacs-wiki-time-less-p last-mod mod-time)))
	      (setq last-mod mod-time)))
	(setq dirs (cdr dirs))))
    (if (or (and no-check-p emacs-wiki-file-alist)
	    (not (or emacs-wiki-under-windows-p
		     (null emacs-wiki-last-update)
		     (null last-mod)
		     (emacs-wiki-time-less-p emacs-wiki-last-update
					     last-mod))))
	emacs-wiki-file-alist
      (setq dirs emacs-wiki-directories
	    emacs-wiki-last-update last-mod)
      (save-match-data
	(setq
	 emacs-wiki-file-alist
	 (let* ((names (list t))
		(lnames names))
	   (while dirs
	     (if (file-readable-p (car dirs))
		 (let ((files (directory-files (car dirs) t nil t)))
		   (while files
		     (unless
			 (string-match emacs-wiki-file-ignore-regexp
				       (file-name-nondirectory (car files)))
		       (setcdr lnames
			       (cons (cons (emacs-wiki-page-name (car files))
					   (car files)) nil))
		       (setq lnames (cdr lnames)))
		     (setq files (cdr files)))))
	     (setq dirs (cdr dirs)))
	   (cdr names)))))))

;; Utility functions to extract parts of a Wiki name

(defsubst emacs-wiki-published-name (name)
  "Return the externally visible NAME for a wiki page."
  (concat emacs-wiki-publishing-file-prefix name
	  emacs-wiki-publishing-file-suffix))

(defsubst emacs-wiki-published-file (&optional file)
  "Return the filename of the published file."
  (expand-file-name (emacs-wiki-published-name (emacs-wiki-page-name file))
		    emacs-wiki-publishing-directory))

(defsubst emacs-wiki-wiki-url-p (name)
  "Return non-nil if NAME is a URL."
  (save-match-data
    (string-match emacs-wiki-url-regexp name)))

(defun emacs-wiki-wiki-visible-name (wiki-name)
  "Return the visible part of a Wiki link.
This only really means something if [[extended][links]] are involved."
  (save-match-data
    (let ((name wiki-name))
      (if (string-match emacs-wiki-extended-link-regexp name)
	  (if (match-string 2 name)
	      (setq name (match-string 3 name))
	    (setq name (match-string 1 name))))
      (if (and (not (emacs-wiki-wiki-url-p name))
	       (string-match "#" name))
	  (if (= 0 (match-beginning 0))
	      (setq name (emacs-wiki-page-name))
	    (let ((base (substring name 0 (match-beginning 0))))
	      (if (assoc base emacs-wiki-interwiki-names)
		  (setq name (concat (substring name 0 (match-beginning 0))
				     ":" (substring name (match-end 0))))
		(setq name base)))))
      name)))

(defun emacs-wiki-wiki-tag (wiki-name)
  (save-match-data
    (if (string-match "#" wiki-name)
	(substring wiki-name (match-end 0)))))

(defun emacs-wiki-wiki-link-target (wiki-name)
  "Return the target of a Wiki link.  This might include anchor tags."
  (save-match-data
    (let ((name wiki-name) lookup)
      (if (string-match "^\\[\\[\\([^]]+\\)\\]" name)
	  (setq name (match-string 1 name)))
      (if (and emacs-wiki-interwiki-names
	       (string-match "\\`\\([^#]+\\)\\(#\\(.+\\)\\)?\\'" name)
	       (setq lookup (assoc (match-string 1 name)
				   emacs-wiki-interwiki-names)))
	  (let ((tag (match-string 3 name))
		(target (cdr lookup)))
	    (if (stringp target)
		(setq name (concat target tag))
	      (assert (functionp target))
	      (setq name (funcall target tag))))
	(if (eq (aref name 0) ?#)
	    (setq name (concat (emacs-wiki-page-name) name))))
      name)))

(defun emacs-wiki-wiki-base (wiki-name)
  "Find the WikiName or URL mentioned by a Wiki link.
This means without tags, in the case of a WikiName."
  (save-match-data
    (let ((file (emacs-wiki-wiki-link-target wiki-name)))
      (if (emacs-wiki-wiki-url-p file)
	  file
	(if (string-match "#" file)
	    (substring file 0 (match-beginning 0))
	  file)))))

;;; Open a Wiki page (with completion)

(defvar emacs-wiki-history-list nil)

(defun emacs-wiki-read-name ()
  "Read the name of a valid Wiki page from minibuffer, with completion."
  (if (featurep 'xemacs)
      (let ((str (completing-read
		  (format "Wiki page: (default: %s) "
			  emacs-wiki-home-page)
		  (emacs-wiki-file-alist)
		  nil nil nil 'emacs-wiki-history-list)))
	(if (or (null str) (= (length str) 0))
	    emacs-wiki-home-page
	  str))
    (completing-read
     (format "Wiki page: (default: %s) " emacs-wiki-home-page)
     (emacs-wiki-file-alist)
     nil nil nil 'emacs-wiki-history-list
     emacs-wiki-home-page)))

(defun emacs-wiki-find-file (wiki-name &optional command)
  "Open an Emacs Wiki page by name."
  (interactive (list (emacs-wiki-read-name)))
  (let* ((filename (or (cdr (assoc wiki-name emacs-wiki-file-alist))
		       (expand-file-name wiki-name
					 (unless (emacs-wiki-maybe t)
					   (car emacs-wiki-directories)))))
	 (dirname (file-name-directory filename)))
    (unless (file-exists-p dirname)
      (make-directory dirname t))
    (funcall (or command 'find-file) filename)))

;;; Navigate/visit links or URLs.  Use TAB, S-TAB and RET (or mouse-2).

(defun emacs-wiki-next-reference ()
  "Move forward to next Wiki link or URL, cycling if necessary."
  (interactive)
  (unless (eobp)
    (let ((case-fold-search nil)
	  (cycled 0) pos)
      (save-excursion
	(if (emacs-wiki-link-at-point)
	    (goto-char (match-end 0)))
	(while (< cycled 2)
	  (if (re-search-forward emacs-wiki-url-or-name-regexp nil t)
	      (setq pos (match-beginning 0)
		    cycled 2)
	    (goto-char (point-min))
	    (setq cycled (1+ cycled)))))
      (if pos
	  (goto-char pos)))))

(defun emacs-wiki-previous-reference ()
  "Move backward to the next Wiki link or URL, cycling if necessary.
This function is not entirely accurate, but it's close enough."
  (interactive)
  (unless (bobp)
    (let ((case-fold-search nil)
	  (cycled 0) pos)
      (save-excursion
	(while (< cycled 2)
	  (if (re-search-backward emacs-wiki-url-or-name-regexp nil t)
	      (setq pos (point)
		    cycled 2)
	    (goto-char (point-max))
	    (setq cycled (1+ cycled)))))
      (if pos
	  (goto-char pos)))))

(defun emacs-wiki-visit-link (link-name)
  "Visit the URL or link named by LINK-NAME."
  (let ((link (emacs-wiki-wiki-link-target link-name)))
    (if (emacs-wiki-wiki-url-p link)
	(funcall emacs-wiki-browse-url-function link)
      ;; The name list is current since the last time the buffer was
      ;; highlighted
      (let* ((base (emacs-wiki-wiki-base link-name))
	     (file (assoc base (emacs-wiki-file-alist t)))
	     (tag  (and (not (emacs-wiki-wiki-url-p link))
			(emacs-wiki-wiki-tag link))))
	(if (null file)
	    (find-file base)
	  (find-file (cdr file))
	  (when tag
	    (goto-char (point-min))
	    (re-search-forward (concat "^\\.?#" tag) nil t)))))))

(unless (fboundp 'line-end-position)
  (defsubst line-end-position (&optional N)
    (save-excursion (end-of-line N) (point))))

(unless (fboundp 'line-beginning-position)
  (defsubst line-beginning-position (&optional N)
    (save-excursion (beginning-of-line N) (point))))

(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(defsubst emacs-wiki-link-at-point (&optional pos)
  "Return non-nil if a URL or Wiki link name is at point."
  (if (and (char-after pos)
	   (not (eq (char-syntax (char-after pos)) ? )))
      (let ((case-fold-search nil)
	    (here (or pos (point))))
	(save-excursion
	  (goto-char here)
	  (skip-chars-backward "^'\"( \t\n")
	  (or (looking-at emacs-wiki-url-or-name-regexp)
	      (and (search-backward "[[" (line-beginning-position) t)
		   (looking-at emacs-wiki-name-regexp)
		   (<= here (match-end 0))))))))

(defun emacs-wiki-follow-name-at-point ()
  "Visit the link at point, or insert a newline if none."
  (interactive)
  (if (emacs-wiki-link-at-point)
      (emacs-wiki-visit-link (match-string 0))))

(defun emacs-wiki-follow-name-at-mouse (event)
  "Visit the link at point, or yank text if none."
  (interactive "e")
  (save-excursion
    (cond ((fboundp 'event-window)	; XEmacs
	   (set-buffer (window-buffer (event-window event)))
	   (and (event-point event) (goto-char (event-point event))))
	  ((fboundp 'posn-window)	; Emacs
	   (set-buffer (window-buffer (posn-window (event-start event))))
	   (goto-char (posn-point (event-start event)))))
    (if (emacs-wiki-link-at-point)
	(emacs-wiki-visit-link (match-string 0)))))

;;; Find text in Wiki pages, or pages referring to the current page

(defvar emacs-wiki-search-history nil)

(defun emacs-wiki-grep (text)
  "Grep for TEXT in the Wiki directories."
  (let ((str (concat emacs-wiki-grep-command))
	(dirs (mapconcat 'identity emacs-wiki-directories " ")))
    (while (string-match "%W" str)
      (setq str (replace-match text t t str)))
    (while (string-match "%D" str)
      (setq str (replace-match dirs t t str)))
    (require 'compile)
    (compile-internal str "No more search hits" "search"
		      nil grep-regexp-alist)))

(defun emacs-wiki-search (text)
  "Search for the given TEXT string in the Wiki directories."
  (interactive
   (list (let ((str (concat emacs-wiki-grep-command)) pos)
	   (while (string-match "%W" str)
	     (setq pos (1+ (match-beginning 0)))
	     (setq str (replace-match "" t t str)))
	   (read-from-minibuffer "Search command: "
				 (cons str pos)
				 nil nil 'emacs-wiki-search-history))))
  (while (string-match "%D" text)
    (setq text
	  (replace-match (mapconcat 'identity
				    emacs-wiki-directories " ") t t text)))
  (require 'compile)
  (compile-internal text "No more search hits" "search"
		    nil grep-regexp-alist))

(defun emacs-wiki-backlink ()
  "Grep for the current pagename in the current Wiki directory."
  (interactive)
  (emacs-wiki-grep (emacs-wiki-page-name)))

;;; Generate an index of all known Wiki pages

(defun emacs-wiki-generate-index (&optional as-list exclude-private)
  "Generate an index of all Wiki pages."
  (with-current-buffer (get-buffer-create "*Wiki Index*")
    (let ((files (setq emacs-wiki-file-alist
		       (sort (emacs-wiki-file-alist)
			     (function
			      (lambda (l r)
				(string-lessp (car l) (car r)))))))
	  file)
      (while files
	(unless (and exclude-private
		     (emacs-wiki-private-p (caar files)))
	  (insert (if as-list "- " "") "[[" (caar files) "]]\n"))
	(setq files (cdr files))))
    (current-buffer)))

(defun emacs-wiki-index ()
  "Display an index of all known Wiki pages."
  (interactive)
  (message "Generating Wiki index...")
  (pop-to-buffer (emacs-wiki-generate-index))
  (goto-char (point-min))
  (emacs-wiki-mode)
  (message "Generating Wiki index...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Highlighting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-highlight nil
  "Options controlling the behaviour of Emacs Wiki highlighting.
See `emacs-wiki-highlight-buffer' for more information."
  :group 'emacs-wiki)

(defface emacs-wiki-link-face
  '((((class color) (background light))
     (:foreground "green" :underline "green" :bold t))
    (((class color) (background dark))
     (:foreground "cyan" :underline "cyan" :bold t))
    (t (:bold t)))
  "Face for Wiki cross-references."
  :group 'emacs-wiki-highlight)

(defface emacs-wiki-bad-link-face
  '((((class color) (background light))
     (:foreground "red" :underline "red" :bold t))
    (((class color) (background dark))
     (:foreground "coral" :underline "coral" :bold t))
    (t (:bold t)))
  "Face for Wiki cross-references."
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-inline-images (and (not (featurep 'xemacs))
					 (= emacs-major-version 21)
					 window-system)
  "If non-nil, inline locally available images within Wiki pages."
  :type 'boolean
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-image-regexp
  "\\.\\(eps\\|gif\\|jp\\(e?g\\)\\|p\\(bm\\|ng\\)\\|tiff\\|x\\([bp]m\\)\\)\\'"
  "A link matching this regexp will be published inline as an image.
Note that whatever the link name is, will be used for the alt field.
Example:

  http:wife.jpg (alt text is the URL)

  [[http://wife.jpg][A picture of my wife]]"
  :type 'regexp
  :group 'emacs-wiki-highlight)

(defvar emacs-wiki-highlight-regexp nil)
(defvar emacs-wiki-highlight-vector nil)

(defcustom emacs-wiki-highlight-markup
  `(;; make emphasized text appear emphasized
    ("\\*+[^ \t*]" ?* emacs-wiki-highlight-emphasized)

    ;; make underlined text appear underlined
    ("_[^ \t_]" ?_ emacs-wiki-highlight-underlined)

    ;; make quadruple quotes invisible
    ("''''" ?\'
     ,(function
       (lambda ()
	 (add-text-properties (match-beginning 0) (match-end 0)
			      '(invisible t intangible t)))))

    ;; guard verbatim sections from being marked up
    ("<\\(?:nop\\|example\\|verbatim\\|lisp\\)>" ?\<
     emacs-wiki-highlight-tagged)

    ("=[^=]" ?= ,(function
		  (lambda ()
		    (if (or (= (point-min) (match-beginning 0))
			    (eq (char-syntax
				 (char-before (match-beginning 0))) ? ))
			(search-forward "=" end t)))))

    (emacs-wiki-url-or-name-regexp t emacs-wiki-highlight-link))
  "Expressions to highlight an Emacs Wiki buffer.
These are arrange in a rather special fashion, so as to be as quick as
possible.

Each element of the list is itself a list, of the form:

  (LOCATE-REGEXP TEST-CHAR MATCH-FUNCTION)

LOCATE-REGEXP is a partial regexp, and should be the smallest possible
regexp to differentiate this rule from other rules.  It may also be a
symbol containing sucha regexp.  The buffer region is scanned only
once, and LOCATE-REGEXP indicates where the scanner should stop to
look for highlighting possibilities.

TEST-CHAR is a char or t.  The character should match the beginning
text matched by LOCATE-REGEXP.  These chars are used to build a vector
for fast MATCH-FUNCTION calling.

MATCH-FUNCTION is the function called when a region has been
identified.  It is responsible for adding the appropriate text
properties to change the appearance of the buffer.

This markup is used to modify the appearance of the original text to
make it look more like the published HTML would look (like making some
markup text invisible, inlining images, etc).

font-lock is used to apply the markup rules, so that they can happen
on a deferred basis.  They are not always accurate, but you can use
\\[font-lock-fontifty-block] near the point of error to force
fontification in that area.

Lastly, none of the regexp should contain grouping elements that will
affect the match data results.  Use \\(?:\\) if you need to group.
This is so that the final rule, for URLs or WikiNames, can be
optimized."
  :type '(repeat
	  (list :tag "Highlight rule"
		(choice (regexp :tag "Locate regexp")
			(symbol :tag "Regexp symbol"))
		(choice (character :tag "Confirm character")
			(const :tag "Default rule" t))
		function))
  :set (function
	(lambda (sym val)
	  (setq emacs-wiki-highlight-regexp
		(concat "\\(?:"
			(mapconcat (function
				    (lambda (rule)
				      (if (symbolp (car rule))
					  (symbol-value (car rule))
					(car rule)))) val "\\|")
			"\\)")
		emacs-wiki-highlight-vector (make-vector 128 nil))
	  (let ((rules val))
	    (while rules
	      (if (eq (cadr (car rules)) t)
		  (let ((i 0) (l 128))
		    (while (< i l)
		      (unless (aref emacs-wiki-highlight-vector i)
			(aset emacs-wiki-highlight-vector i
			      (nth 2 (car rules))))
		      (setq i (1+ i))))
		(aset emacs-wiki-highlight-vector (cadr (car rules))
		      (nth 2 (car rules))))
	      (setq rules (cdr rules))))
	  (set sym val)))
  :group 'emacs-wiki-highlight)

(defvar font-lock-mode nil)

(defun emacs-wiki-use-font-lock ()
  (set (make-local-variable 'font-lock-multiline) 'undecided)
  (set (make-local-variable 'font-lock-defaults)
       `(nil t nil nil 'beginning-of-line
	 (font-lock-fontify-region-function . emacs-wiki-highlight-region)
	 (font-lock-unfontify-region-function
	  . emacs-wiki-unhighlight-region)))
  (font-lock-mode t)
  (if (featurep 'xemacs)
      (emacs-wiki-highlight-region (point-min) (point-max) t)))

(defun emacs-wiki-mode-flyspell-verify ()
  "Return t if the word at point should be spell checked."
  (let* ((word-pos (1- (point)))
	 (props (text-properties-at word-pos)))
    (not (or (bobp)
	     (memq 'display props)
	     (if (and font-lock-mode (cadr (memq 'fontified props)))
		 (memq (cadr (get-text-property word-pos 'face))
		       '(emacs-wiki-link-face emacs-wiki-bad-link-face))
	       (emacs-wiki-link-at-point word-pos))))))

(put 'emacs-wiki-mode 'flyspell-mode-predicate
     'emacs-wiki-mode-flyspell-verify)

(defun emacs-wiki-eval-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (let ((object (eval (read form))))
      (cond
       ((stringp object) object)
       ((and (listp object)
	     (not (eq object nil)))
	(let ((string (pp-to-string object)))
	  (substring string 0 (1- (length string)))))
       ((numberp object)
	(number-to-string object))
       ((eq object nil) "")
       (t
	(pp-to-string object))))))

(defun emacs-wiki-highlight-region (beg end &optional verbose)
  "Apply highlighting according to `emacs-wiki-highlight-markup'.
Note that this function should NOT change the buffer, nor should any
of the functions listed in `emacs-wiki-highlight-markup'."
  (let ((buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t)
	(modified-p (buffer-modified-p))
	deactivate-mark)
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (widen)
	    ;; check to see if we should expand the beg/end area for
	    ;; proper multiline matches
	    (when (and font-lock-multiline
		       (> beg (point-min))
		       (get-text-property (1- beg) 'font-lock-multiline))
	      ;; We are just after or in a multiline match.
	      (setq beg (or (previous-single-property-change
			     beg 'font-lock-multiline)
			    (point-min)))
	      (goto-char beg)
	      (setq beg (line-beginning-position)))
	    (when font-lock-multiline
	      (setq end (or (text-property-any end (point-max)
					       'font-lock-multiline nil)
			    (point-max))))
	    (goto-char end)
	    (setq end (line-beginning-position 2))
	    ;; Undo any fontification in the area.
	    (font-lock-unfontify-region beg end)
	    ;; And apply fontification based on `emacs-wiki-highlight-markup'
	    (let ((len (float (- end beg)))
		  (case-fold-search nil))
	      (goto-char beg)
	      (while
		  (and (< (point) end)
		       (re-search-forward emacs-wiki-highlight-regexp end t))
		(if verbose
		    (message "Highlighting buffer...%d%%"
			     (* (/ (float (- (point) beg)) len) 100)))
		(funcall (aref emacs-wiki-highlight-vector
			       (char-after (match-beginning 0)))))
	      (if verbose (message "Highlighting buffer...done")))))
      (set-buffer-modified-p modified-p))))

(defun emacs-wiki-unhighlight-region (begin end)
  "Remove all visual highlights in the buffer (except font-lock)."
  (let ((buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t)
	(modified-p (buffer-modified-p))
	deactivate-mark)
    (unwind-protect
	(remove-text-properties
	 begin end '(face nil font-lock-multiline nil
			  invisible nil intangible nil display nil
			  mouse-face nil keymap nil help-echo nil))
      (set-buffer-modified-p modified-p))))

(eval-when-compile
  (defvar end))

(defun emacs-wiki-highlight-emphasized ()
  (let* ((b1 (match-beginning 0))
	 (e1 (1- (match-end 0)))
	 (leader (- e1 b1))
	 b2 e2 face)
    (when (search-forward "*" end t)
      (setq b2 (1- (point)))
      (skip-chars-forward "*")
      (setq e2 (point))
      (cond ((= leader 1) (setq face 'italic))
	    ((= leader 2) (setq face 'bold))
	    ((= leader 3) (setq face 'bold-italic)))
      (add-text-properties b1 e1 '(invisible t intangible t))
      (add-text-properties e1 b2 (list 'face face))
      (add-text-properties b2 e2 '(invisible t intangible t))
      (if (> (count-lines b1 e2) 1)
	  (add-text-properties b1 e2 '(font-lock-multiline t))))))

(defun emacs-wiki-highlight-underlined ()
  (let ((start (- (point) 2))
	(finish (1- (point))))
    (when (search-forward "_" end t)
      (add-text-properties start finish '(invisible t intangible t))
      (add-text-properties finish (1- (point)) '(face underline))
      (add-text-properties (1- (point)) (point) '(invisible t intangible t)))))

(defun emacs-wiki-highlight-tagged ()
  (let ((char (char-after (1+ (match-beginning 0)))))
    (cond
     ((eq char ?v)			; <verbatim>
      (search-forward "</verbatim>" nil t))
     ((eq char ?e)			; <example>
      (search-forward "</example>" nil t))
     ((eq char ?n)			; <nop>
      (add-text-properties (match-beginning 0) (point)
			   '(invisible t intangible t))
      (if (looking-at emacs-wiki-name-regexp)
	  (goto-char (match-end 0))))
     ((eq char ?l)			; <lisp>
      (let ((begin (match-beginning 0))
	    (start (point)))
	(if (search-forward "</lisp>" end t)
	    (add-text-properties
	     begin (point)
	     (list 'font-lock-multiline t
		   'display (emacs-wiki-eval-lisp
			     (buffer-substring-no-properties
			      start (- (point) 7)))
		   'intangible t))))))))

(defsubst emacs-wiki-link-properties (help-str &optional face)
  (append (if face
	      (list 'face face)
	    (list 'invisible t 'intangible t))
	  (list 'mouse-face 'highlight
		'help-echo help-str
		'keymap emacs-wiki-local-map)))

(defun emacs-wiki-highlight-link ()
  (if (eq ?\[ (char-after (match-beginning 0)))
      (if (and emacs-wiki-inline-images
	       (save-match-data
		 (string-match emacs-wiki-image-regexp (match-string 3))))
	  (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
				   (match-string 3) (match-string 5))
	(let ((invis-props (emacs-wiki-link-properties
			    (match-string-no-properties 3)))
	      (props (emacs-wiki-link-properties
		      (match-string-no-properties 3) 'emacs-wiki-link-face)))
	  (if (match-string 5)
	      (progn
		(add-text-properties (match-beginning 0)
				     (match-beginning 5) invis-props)
		(add-text-properties (match-beginning 5) (match-end 5) props)
		(add-text-properties (match-end 5) (match-end 0) invis-props))
	    (add-text-properties (match-beginning 0)
				 (match-beginning 3) invis-props)
	    (add-text-properties (match-beginning 3) (match-end 0) props)
	    (add-text-properties (match-end 3) (match-end 0) invis-props)))
	(goto-char (match-end 0)))
    (if (and emacs-wiki-inline-images
	     (save-match-data
	       (string-match emacs-wiki-image-regexp (match-string 0))))
	(emacs-wiki-inline-image (match-beginning 0) (match-end 0)
				 (match-string 0))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       (emacs-wiki-link-properties
	(match-string-no-properties 0)
	(if (let ((base (emacs-wiki-wiki-base (match-string 0))))
	      (or (assoc base emacs-wiki-file-alist)
		  (save-match-data
		    (string-match "\\`[a-z]\\{3,6\\}:" base))))
	    'emacs-wiki-link-face
	  'emacs-wiki-bad-link-face)))
      (goto-char (match-end 0)))))

(defun emacs-wiki-inline-image (beg end url &optional desc)
  "Inline locally available images."
  (let ((filename (cond
		   ((string-match "\\`file:\\(.+\\)" url)
		    (match-string 1 url))
		   ((string-match "\\`http:/?\\([^/].+\\)" url)
		    (expand-file-name (match-string 1 url)
				      emacs-wiki-publishing-directory)))))
    (if (and filename (file-readable-p filename))
	(add-text-properties beg end (list 'display (create-image filename)
					   'help-echo (or desc url))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Publishing (to HTML by default)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-publish nil
  "Options controlling the behaviour of Emacs Wiki publishing.
See `emacs-wiki-publish' for more information."
  :group 'emacs-wiki)

(defcustom emacs-wiki-maintainer (concat "mailto:webmaster@" (system-name))
  "URL where the maintainer can be reached."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-index-page "WikiIndex"
  "Title of the Wiki Index page."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-ignored-extensions-regexp
  "\\.\\(bz2\\|gz\\|[Zz]\\)\\'"
  "A regexp of extensions to omit from the ending of Wiki page name."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-private-by-mode-flag (not emacs-wiki-under-windows-p)
  "If non-nil, Wiki files are private if not readable by the world.
Otherwise the regexps in `emacs-wiki-private-pages' are used."
  :type '(choice (const nil) (repeat regexp))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-private-pages nil
  "A list of regexps to exclude from public view.
This variable only applies if `emacs-wiki-private-by-mode-flag' is nil."
  :type '(choice (const nil) (repeat regexp))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-directory "~/WebWiki"
  "Directory where all wikis are published to."
  :type 'directory
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-file-prefix ""
  "This prefix will be prepended to all wiki names when publishing."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-file-suffix ".html"
  "This suffix will be appended to all wiki names when publishing."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-before-markup-hook nil
  "A hook run in the buffer where markup is done, before it is done."
  :type 'hook
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-after-markup-hook nil
  "A hook run in the buffer where markup is done, after it is done."
  :type 'hook
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-http-equiv "Content-Type"
  "The http-equiv attribute used for the HTML <meta> tag."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-content "text/html;charset=iso-8859-1"
  "The content attribute used for the HTML <meta> tag."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-redirect-delay 1
  "The number of seconds to delay before doing a page redirect."
  :type 'integer
  :group 'emacs-wiki-publish)

(defvar emacs-wiki-current-page-title nil
  "Current page title, used instead of buffer name if non-nil.
This is usually set by code called by `emacs-wiki-publishing-markup'.
It should never be changed globally.")

(defcustom emacs-wiki-publishing-header
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">
<html>
  <head>
    <meta http-equiv=\"<lisp>emacs-wiki-meta-http-equiv</lisp>\"
	  content=\"<lisp>emacs-wiki-meta-content</lisp>\">
    <title><lisp>(or emacs-wiki-current-page-title
		     (emacs-wiki-prettify-title
			(emacs-wiki-page-name)))</lisp></title>
    <lisp>emacs-wiki-style-sheet</lisp>
  </head>
  <body>
    <h1><lisp>(or emacs-wiki-current-page-title
		  (emacs-wiki-prettify-title
		     (emacs-wiki-page-name)))</lisp></h1>
    <!-- Page published by Emacs Wiki begins here -->\n"
  "Text to prepend to a wiki being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-footer
  "
    <!-- Page published by Emacs Wiki ends here -->
    <div class=\"navfoot\">
      <hr>
      <table width=\"100%\" border=\"0\" summary=\"Footer navigation\">
	<tr>
	  <td width=\"33%\" align=\"left\">
	    <span class=\"footdate\">Last updated:
	      <lisp>(format-time-string emacs-wiki-footer-date-format
			 (nth 5 (file-attributes buffer-file-name)))</lisp>
	    </span>
	  </td>
	  <td width=\"34%\" align=\"center\">
	    <span class=\"foothome\">
	      <a href=\"<lisp>(emacs-wiki-published-name
				emacs-wiki-home-page)</lisp>\">Home</a>
	      &nbsp;/&nbsp;
	      <a href=\"<lisp>(emacs-wiki-published-name
				emacs-wiki-index-page)</lisp>\">Index</a>
	      <lisp>
		(if (assoc \"ChangeLog\" emacs-wiki-file-alist)
		    (concat \"&nbsp;/&nbsp;\"
			    \"<a href=\\\"ChangeLog.html\\\">Changes</a>\"))
	      </lisp>
	    </span>
	  </td>
	  <td width=\"33%\" align=\"right\">
	    <span class=\"footfeed\">
	      <lisp>(or emacs-wiki-httpd-search \"&nbsp;\")</lisp>
	    </span>
	  </td>
	</tr>
      </table>
    </div>
  </body>
</html>\n"
  "Text to append to a wiki being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-footer-date-format "%Y-%m-%d"
  "Format of current date for `emacs-wiki-publishing-footer'.
This string must be a valid argument to `format-time-string'."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-style-sheet
  "<style type=\"text/css\">
a.nonexistent {
  font-weight: bold;
  background-color: #F8F8F8; color: #FF2222;
}

a.nonexistent:visited {
  background-color: #F8F8F8; color: #FF2222;
}

body {
  background: white; color: black;
  margin-left: 5%; margin-right: 5%;
  margin-top: 3%
}

hr { margin-left: 10%; margin-right: 10% }

h2,h3,h4,h5,h6 { margin-left: 0% }

em { font-style: italic; }
strong { font-weight: bold }

li { list-style-type: disc }

p.verse {
  white-space: pre;
  margin-left: 5%
}

pre {
  color: green;
  white-space: pre;
  font-family: monospace; font-weight: bold;
  margin-left: 5%
}
</style>"
  "The style sheet used for each wiki page.
This can either be an inline stylesheet, using <style></style> tags,
or an external stylesheet reference using a <link> tag.

Here is an example of using a <link> tag:

  <link rel=\"stylesheet\" type=\"text/css\" href=\"emacs-wiki.css\">"
  :type 'string
  :group 'emacs-wiki-publish)

(defvar emacs-wiki-publishing-p nil
  "Set to t while Wiki pages are being published.
This can be used by <lisp> tags to know when HTML is being generated.")

(defcustom emacs-wiki-downcase-title-words
  '("the" "and" "at" "on" "of" "for" "in" "an" "a")
  "Strings that should be downcased in a Wiki page title."
  :type '(repeat string)
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-block-groups-regexp
  "\\(h[1-9]\\|hr\\|[oud]l\\|table[^>]*\\|center\\|blockquote\\|pre\\)"
  "This regexp identifies HTML tag which defines their own blocks.
That is, they do not need to be surrounded by <p>."
  :type 'regexp
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-table-attributes "border=\"2\" cellpadding=\"5\""
  "The attribute to be used with HTML <table> tags.
Note that since emacs-wiki support direct insertion of HTML tags, you
can easily create any kind of table you want, as long as every line
begins at column 0 (to prevent it from being blockquote'd).  To make
really ANYTHING you want, use this idiom:

  <verbatim>
  <table>
    [... contents of my table, in raw HTML ...]
  </verbatim></table>

It may look strange to have the tags out of sequence, but remember
that verbatim is processed long before table is even seen."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-markup
  `(;; if the buffer begins with #redirect, cause the page the
    ;; redirect users to another WikiPage or URL.  Also, do not
    ;; display any of the actual content of the page, so that people
    ;; can use it to redirect away from works-in-progress.
    ("\\`#redirect\\s-+\\(.+\\)\\(.\\|\n\\)*\\'" . emacs-wiki-redirect-page)

    ;; change the displayed title
    ("\\`#title\\s-+\\(.+\\)\n+" .
     ,(function
       (lambda ()
	 (setq emacs-wiki-current-page-title (match-string 1))
	 "")))

    ;; this is a shortcut tag for program listings
    ("</?example>" .
     ,(function
       (lambda ()
	 (if (eq ?/ (char-after (1+ (match-beginning 0))))
	     "</verbatim></pre>"
	   "<pre><verbatim>"))))

    ;; mark anything in a <verbatim> tag as literal.  The read-only
    ;; property is used to indicate this.
    ("<verbatim>" . emacs-wiki-markup-verbatim)

    ;; =literal word= (also marked as verbatim text)
    (("\\(\\`\\|[ \t\n]\\)\\(=\\)" 2) . emacs-wiki-markup-literal-word)

    ;; handle embedded Lisp constructs; the result is subject to
    ;; further markup by these rules
    ("<lisp>" . emacs-wiki-markup-embedded-lisp)

    ;; defining anchor points
    ("^#\\([A-Za-z0-9_%]+\\)" . "<a name=\"\\1\"/>")

    ;; headings, outline-mode style
    ("^\\(\\*+\\)\\s-+\\(.+\\)$" .
     ,(function
       (lambda ()
	 (let ((len (1+ (length (match-string 1)))))
	   (format "<h%d>\\2</h%d>" len len)))))

    ;; horizontal rule
    ("^----+" . "<hr>")

    ;; *emphasis*, **strong emphasis**, ***very strong emphasis***
    ("\\(\\*+\\)\\([^* \t][^*]+\\)\\*+" .
     ,(function
       (lambda ()
	 (let ((leader (length (match-string 1))))
	   (cond
	    ((= leader 1) "<em>\\2</em>")
	    ((= leader 2) "<strong>\\2</strong>")
	    ((= leader 3) "<strong><em>\\2</em></strong>"))))))

    ;; _underlined_
    ("_\\([^_]+\\)_" . "<u>\\1</u>")

    ;; footnotes section is separated by a horizontal rule in HTML
    ("^\\(\\* \\)?Footnotes:?\\s-*$" . "<hr>")
    ;; footnote definition/reference (def if at beginning of line)
    ("\\[\\([1-9][0-9]*\\)\\]" . emacs-wiki-markup-footnote)

    ;; don't require newlines between numbered and unnumbered lists.
    ;; This must come before paragraphs are calculated, so that any
    ;; extra newlines added will be condensed.
    (("^\\s-*\\(-\\|[0-9]+\\.\\)" 1) . "\n\\1")

    ;; the beginning of the buffer begins the first paragraph
    ("\\`\n*" . "<p>\n")
    ;; plain paragraph separator
    ("\n\\([ \t]*\n\\)+" . "\n\n</p>\n\n<p>\n")

    ;; unnumbered List items begin with a -.  numbered list items
    ;; begin with number and a period.  definition lists have a
    ;; leading term separated from the body with ::.  centered
    ;; paragraphs begin with at least six columns of whitespace; any
    ;; other whitespace at the beginning indicates a blockquote.  The
    ;; reason all of these rules are handled here, is so that
    ;; blockquote detection doesn't interfere with indented list
    ;; members.
    ,(cons
      (list (concat "^\\(\\s-*\\(-\\|[0-9]+\\.\\|\\(.+\\)[ \t]+::\n?\\)\\)?"
		    "\\([ \t]+\\)\\(\\([^\n]\n?\\)+\\)") 2 4)
      'emacs-wiki-markup-list-or-paragraph)

    ;; simple table markup, nothing fancy.
    ;; use || to separate header elements, and ||| for footer elements
    (("^\\s-*\\(\\([^|\n]+\\(|+\\)\\s-*\\)+\\)\\([^|\n]+\\)?$" 1) .
     emacs-wiki-markup-table)

    ;; "verse" text is indicated the same way as a quoted e-mail
    ;; response: "> text", where text may contain initial whitespace
    ;; (see below).
    ("<p>\\s-+> \\(\\([^\n]\n?\\)+\\)\\(\\s-*</p>\\)?" .
     emacs-wiki-markup-verse)

    ;; join together the parts of a list
    ("</\\([oud]l\\)>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<\\1>\\s-*" . "")

    ;; join together the parts of a table
    ,(cons
      (concat "</tbody>\\s-*"
	      "</table>\\s-*" "\\(</p>\\s-*<p>\\s-*\\)?" "<table[^>]*>\\s-*"
	      "<tbody>\\s-*") "")
    ("</table>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<table[^>]*>\\s-*" . "")

    ;; fixup paragraph delimiters
    ,(cons
      (concat "<p>\\s-*\\(</?" emacs-wiki-block-groups-regexp ">\\)") "\\1")
    ,(cons
      (concat "\\(</?" emacs-wiki-block-groups-regexp ">\\)\\s-*</p>") "\\1")

    ;; terminate open paragraph at the end of the buffer
    ("<p>\\s-*\\'" . "<p></p>")
    ;; make sure to close any open text (paragraphs)
    ("\\([^> \t\n]\\)\\s-*\\'" . "\\1\n</p>")
    ;; lists have no whitespace after them, so add a final linebreak
    ("\\(</[oud]l>\\)\\s-*\\'" . "\\1\n<br>")

    ;; bare email addresses
    ("\\([^[]\\)\\([-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+\\)" .
     "\\1<a href=\"mailto:\\2\">\\2</a>")

    ;; replace WikiLinks in the buffer (links to other pages).  <nop>
    ;; before a WikiName guards it from being replaced, and '''' can
    ;; be used to add suffixes, such as WikiName''''s.
    ("<nop>" . emacs-wiki-guard-wikiname)
    emacs-wiki-replace-links
    ("''''" . "")

    ;; insert the default publishing header
    ,(function
      (lambda ()
	(insert emacs-wiki-publishing-header)))

    ;; insert the default publishing footer
    ,(function
      (lambda ()
	(goto-char (point-max))
	(insert emacs-wiki-publishing-footer)))

    ;; handle any remaining embedded Lisp constructs
    ("<lisp>" . emacs-wiki-markup-embedded-lisp))
  "List of markup rules to apply when publishing a Wiki page.
Each member of the list is either a function, or a cons cell of the
form:

  \(REGEXP REPLACEMENT-TEXT/FUNCTION/SYMBOL).

REGEXP is a regular expression to search for using
`re-search-forward'.  REPLACEMENT-TEXT is a string that will be passed
to `replace-match'.  If it is not a string, but a function, it will be
called to determine what the replacement text should be (it must
return a string).  If it is a symbol, the value of that symbol should
be a string.

The first element of each cons cell may also be a list that begins
with a REGEXP.  If it is a list, the remaining members of the list
must specify the matching groups where the actual text to be
highlighted occurs.  This is necessary for regexps that use a
precondition which is not part of the text marked up.  The reason why
the matcher needs to know where the actual text begins, is to check
whether it has been protected by a <verbatim> block or not.

The replacements are done in order, one rule at a time.  Writing the
regular expressions can be a tricky business.  Note that case is never
ignored.  `case-fold-search' is always be bound to nil while
processing the markup rules.

Here is a description of the default markup rules:

Headings

 * First level
 ** Second level
 *** Third level

 Note that the first level is actually indicated using H2, so that
 it doesn't appear at the same level as the page heading (which
 conceptually titles the section of that Wiki page).

Horizontal rules

 ----

Emphasis

 *emphasis*
 **strong emphasis**
 ***very strong emphasis***
 _underlined_
 =verbatim=

 <verbatim>This tag should be used for larger blocks of
 text</verbatim>.

Footnotes

  A reference[1], which is just a number in square brackets,
  constitutes a footnote reference.

  Footnotes:

  [1]  Footnotes are defined by the same number in brackets
       occurring at the beginning of a line.  Use footnote-mode's C-c
       ! a command, to very easily insert footnotes while typing.  Use
       C-x C-x to return to the point of insertion.

Paragraphs

  One or more blank lines separates paragraphs.

Centered paragraphs and quotations

  A line that begins with six or more columns of whitespace (made up
  of tabs or spaces) indicates a centered paragraph.  I assume this
  because it's expected you will use M-s to center the line, which
  usually adds a lot of whitespace before it.

  If a line begins with some whitespace, but less than six columns, it
  indicates a quoted paragraph.

Poetic verse

  Poetry requires that whitespace be preserved, without resorting to
  the monospace typical of <pre>.  For this, the following special
  markup exists, which is reminiscent of e-mail quotations:

    > A line of Emacs verse;
    > forgive its being so terse.

Literal paragraphs

  Use the HTML tags <pre></pre> to insert a paragraph and preserve
  whitespace.  If you're inserting a block of code, you will almost
  always want to use <verbatim></verbatim> *within* the <pre> tags.
  The shorcut for doing this is to use the <example> tag:

    <example>
    Some literal text or code here.
    </example>

  The reason for this funny syntax is so that just the right
  characters get marked as verbatim.  Anything else, and the returns,
  or ending tag, will not be treated as expected.

Lists

  - bullet list

  1. Enumerated list

  Term :: A definition list

  Blank lines between list elements are optional, but required between
  members of a definition list.

Tables

  Only very simple table markup is supported.  The attributes of the
  table are kept in `emacs-wiki-table-attributes'.  The syntax is:

    Double bars || Separate header fields
    Single bars | Separate body fields
    Here are more | body fields
    Triple bars ||| Separate footer fields

  Other paragraph markup applies, meaning that if six or more columns
  of whitespace precedes the first line of the table, it will be
  centered, and if any whitespace at all precedes first line, it will
  occur in a blockquote.

Anchors and tagged links

  #example If you begin a line with \"#anchor\" -- where anchor
  can be any word that doesn't contain whitespace -- it defines an
  anchor at that point into the document.  This anchor text is not
  displayed.

  You can reference an anchored point in another page (or even in the
  current page) using WikiName#anchor.  The #anchor will never be
  displayed in HTML, whether at the point of definition or reference,
  but it will cause browsers to jump to that point in the document.

Redirecting to another page or URL

  Sometimes you may wish to redirect someone to another page.  To do
  this, use \"#redirect WikiName\" or \"#redirect URL\" at the top of
  the page.  The numbers of seconds to delay is kept in
  `emacs-wiki-redirect-delay', and defaults to 2 seconds.  The page
  shown will also contain a link to click on, for browsing which do
  not support automatic refreshing.

URLs

  A regular URL is given as a link.  If it's an image URL, it will
  be inlined using an IMG tag.

Embedded lisp

  <lisp>(concat \"This form gets\" \"inserted\")</lisp>

Special handling of WikiNames

  If you need to add a plural at the end of a WikiName, separate it
  with four single quotes: WikiName''''s.

  To prevent a link name (of any type) from being treated as such,
  surround it with =equals= (to display it in monotype), or prefix it
  with the tag <nop>.

Special Wiki links

  Besides the normal WikiName type links, emacs-wiki also supports
  extended links:

    [[link text][optional link description]]

  An extended link is always a link, no matter how it looks.  This
  means you can use any file in your `emacs-wiki-directories' as a
  Wiki file.  If you provide an optional description, that's what will
  be shown instead of the link text.  This is very useful for
  providing textual description of URLs.

InterWiki names

  There are times when you will want to constantly reference pages on
  another website.  Rather than repeating the URL ad nauseum, you can
  define an InterWiki name.  This is a set of WikiNames to URL
  correlations, that support textual substitution using #anchor names
  (which are appended to the URL).  For example, MeatballWiki is
  defined in the variable `emacs-wiki-interwiki-names'.  It means you
  can reference the page \"MeatBall\" on MeatballWiki using this
  syntax:

    MeatballWiki#MeatBall

  In the resulting HTML, the link is simply shown as
  \"MeatballWiki:MeatBall\"."
  :type '(repeat
	  (choice (cons :tag "Markup rule"
			(choice regexp
				(cons :tag "Prefixed regexp" regexp
				      (repeat (integer :tag "Group"))))
			(choice string function symbol))))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-changelog-markup
  `(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;")

    ("^\\(\\S-\\)" . "* \\1")

    ;; =literal word= (also marked as verbatim text)
    ("\\(\\`\\|[ \t\n]\\)=" . emacs-wiki-markup-literal-word)

    ;; headings, outline-mode style
    ("^\\*\\s-+\\(.+\\)$" . "<h2>\\1</h2>")

    ;; *emphasis*, **strong emphasis**, ***very strong emphasis***
    ("\\(\\*+\\)\\([^* \t][^*]+\\)\\*+" .
     ,(function
       (lambda ()
	 (let ((leader (length (match-string 1))))
	   (cond
	    ((= leader 1) "<em>\\2</em>")
	    ((= leader 2) "<strong>\\2</strong>")
	    ((= leader 3) "<strong><em>\\2</em></strong>"))))))

    ;; _underlined_
    ("_\\([^_]+\\)_" . "<u>\\1</u>")

    ;; don't require newlines between unnumbered lists.
    (("^\\s-*\\(\\*\\)" 1) . "\n\\1")

    ;; the beginning of the buffer begins the first paragraph
    ("\\`\n*" . "<p>\n")
    ;; plain paragraph separator
    ("\n\\([ \t]*\n\\)+" . "\n\n</p>\n\n<p>\n")

    ;; unnumbered List items begin with a -.  numbered list items
    ;; begin with number and a period.  definition lists have a
    ;; leading term separated from the body with ::.  centered
    ;; paragraphs begin with at least six columns of whitespace; any
    ;; other whitespace at the beginning indicates a blockquote.  The
    ;; reason all of these rules are handled here, is so that
    ;; blockquote detection doesn't interfere with indented list
    ;; members.
    ,(cons
      (list (concat "^\\(\\s-*\\(\\*\\)\\)?"
		    "\\([ \t]+\\)\\(\\([^\n]\n?\\)+\\)") 2 3)
      "<ul>\n<li>\\4</ul>\n")

    ;; join together the parts of a list
    ("</\\([oud]l\\)>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<\\1>\\s-*" . "")

    ;; fixup paragraph delimiters
    ,(cons
      (concat "<p>\\s-*\\(</?" emacs-wiki-block-groups-regexp ">\\)") "\\1")
    ,(cons
      (concat "\\(</?" emacs-wiki-block-groups-regexp ">\\)\\s-*</p>") "\\1")

    ;; terminate open paragraph at the end of the buffer
    ("<p>\\s-*\\'" . "<p></p>")
    ;; make sure to close any open text (paragraphs)
    ("\\([^> \t\n]\\)\\s-*\\'" . "\\1\n</p>")
    ;; lists have no whitespace after them, so add a final linebreak
    ("\\(</[oud]l>\\)\\s-*\\'" . "\\1\n<br>")

    ;; bare email addresses
    ("\\([^[]\\)\\([-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+\\)" .
     "\\1<a href=\"mailto:\\2\">\\2</a>")

    ;; replace WikiLinks in the buffer (links to other pages)
    ("<nop>" . emacs-wiki-guard-wikiname)
    emacs-wiki-replace-links
    ("''''" . "")

    ;; insert the default publishing header
    ,(function
      (lambda ()
	(insert emacs-wiki-publishing-header)))

    ;; insert the default publishing footer
    ,(function
      (lambda ()
	(goto-char (point-max))
	(insert emacs-wiki-publishing-footer)))

    ;; handle any remaining embedded Lisp constructs
    ("<lisp>" . emacs-wiki-markup-embedded-lisp))
  "List of markup rules for publishing ChangeLog files.
These are used when the wiki page's name is ChangeLog."
  :type '(repeat
	  (choice (cons :tag "Markup rule"
			(choice regexp
				(cons :tag "Prefixed regexp" regexp
				      (repeat (integer :tag "Group"))))
			(choice string function symbol))))
  :group 'emacs-wiki-publish)

(defun emacs-wiki-private-p (name)
  "Return non-nil if NAME is a private page, and shouldn't be published."
  (if emacs-wiki-private-by-mode-flag
      (let ((filename (cdr (assoc name (emacs-wiki-file-alist t)))))
	(if filename
	    (eq ?- (aref (nth 8 (file-attributes filename)) 7))))
    (let ((private-pages emacs-wiki-private-pages) private)
      (while private-pages
	(if (string-match (car private-pages) name)
	    (setq private t private-pages nil)
	  (setq private-pages (cdr private-pages))))
      private)))

(defun emacs-wiki-visit-published-file (&optional arg)
  "Visit the current wiki page's published result."
  (interactive "P")
  (if arg
      (find-file-other-window (emacs-wiki-published-file))
    (funcall emacs-wiki-browse-url-function
	     (concat "file:" (emacs-wiki-published-file)))))

(defun emacs-wiki-dired-publish ()
  "Publish all marked files in a dired buffer."
  (interactive)
  (emacs-wiki-publish-files (dired-get-marked-files) t))

(defun emacs-wiki-prettify-title (title)
  "Prettify the given TITLE."
  (save-match-data
    (let ((case-fold-search nil))
      (while (string-match "\\([A-Za-z]\\)\\([A-Z0-9]\\)" title)
	(setq title (replace-match "\\1 \\2" t nil title)))
      (let* ((words (split-string title))
	     (w (cdr words)))
	(while w
	  (if (member (downcase (car w))
		      emacs-wiki-downcase-title-words)
	      (setcar w (downcase (car w))))
	  (setq w (cdr w)))
	(mapconcat 'identity words " ")))))

(defun emacs-wiki-publish (&optional arg)
  "Publish all wikis that need publishing.
If the published wiki already exists, it is only overwritten if the
wiki is newer than the published copy.  When given the optional
argument ARG, all wikis are rewritten, no matter how recent they are.
The index file is rewritten no matter what."
  (interactive "P")
  (save-some-buffers)
  (if (emacs-wiki-publish-files
       (let* ((names (emacs-wiki-file-alist))
	      (files (list t))
	      (lfiles files))
	 (while names
	   (setcdr lfiles (cons (cdar names) nil))
	   (setq lfiles (cdr lfiles)
		 names (cdr names)))
	 (cdr files)) arg)
      ;; republish the index if any pages were published
      (with-current-buffer (emacs-wiki-generate-index t t)
	(emacs-wiki-replace-markup "WikiIndex")
	(let ((backup-inhibited t))
	  (write-file (emacs-wiki-published-file emacs-wiki-index-page)))
	(kill-buffer (current-buffer))
	(message "All Wiki pages have been published."))
    (message "No Wiki pages need publishing at this time.")))

(defun emacs-wiki-publish-this-page ()
  "Force publication of the current page."
  (interactive)
  (emacs-wiki-publish-files (list buffer-file-name) t))

(defun emacs-wiki-publish-files (files force)
  "Publish all files in list FILES.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what."
  (let (published-some file page published)
    (while files
      (setq file (car files)
	    files (cdr files)
	    page (emacs-wiki-page-name file)
	    published (emacs-wiki-published-file page))
      (if (and (not (emacs-wiki-private-p page))
	       (or force (file-newer-than-file-p file published)))
	  (with-temp-buffer
	    (insert-file-contents file t)
	    (emacs-wiki-replace-markup)
	    (let ((backup-inhibited t))
	      (write-file published))
	    (setq published-some t))))
    published-some))

(defun emacs-wiki-replace-markup (&optional title)
  "Replace markup according to `emacs-wiki-publishing-markup'."
  (let* ((emacs-wiki-meta-http-equiv emacs-wiki-meta-http-equiv)
	 (emacs-wiki-meta-content emacs-wiki-meta-content)
	 (emacs-wiki-current-page-title nil)
	 (emacs-wiki-publishing-p t)
	 (buffer-file-name (or title buffer-file-name))
	 (case-fold-search nil)
	 (inhibit-read-only t)
	 (rules (if (string= (emacs-wiki-page-name) "ChangeLog")
		    emacs-wiki-changelog-markup
		  emacs-wiki-publishing-markup))
	 (limit (* (length rules) (point-max)))
	 (verbose (> (point-max) 100000))
	 (base 0))
    (run-hooks 'emacs-wiki-before-markup-hook)
    (while rules
      (goto-char (point-min))
      (if (functionp (car rules))
	  (funcall (car rules))
	(let ((regexp (caar rules))
	      (replacement (cdar rules))
	      (group 0) start
	      last-pos pos)
	  (when (listp regexp)
	    (setq group (cdr regexp) regexp (car regexp))
	    (if (= (length group) 1)
		(setq group (car group))))
	  (if verbose
	      (message "Publishing %s...%d%%"
		       (emacs-wiki-page-name)
		       (* (/ (float (+ (point) base)) limit) 100)))
	  (while (and regexp (setq pos (re-search-forward regexp nil t)))
	    (if verbose
		(message "Publishing %s...%d%%"
			 (emacs-wiki-page-name)
			 (* (/ (float (+ (point) base)) limit) 100)))
	    (if (listp group)
		(while (null (setq start (match-beginning (car group))))
		  (setq group (cdr group)))
	      (setq start (match-beginning group)))
	    (unless (get-text-property start 'read-only)
	      (let ((beg (match-beginning 0))
		    (end (copy-marker (match-end 0) t))
		    (text (cond
			   ((functionp replacement)
			    (funcall replacement))
			   ((symbolp replacement)
			    (symbol-value replacement))
			   (t replacement))))
		(if text
		    (replace-match text t))))
	    (if (and last-pos (= pos last-pos))
		(if (eobp)
		    (setq regexp nil)
		  (forward-char 1)))
	    (setq last-pos pos))))
      (setq rules (cdr rules)
	    base (+ base (point-max))))
    (run-hooks 'emacs-wiki-after-markup-hook)
    (if verbose
	(message "Publishing %s...done" (emacs-wiki-page-name)))))

(defun emacs-wiki-link-url (wiki-link)
  "Resolve the given WIKI-LINK into its ultimate URL form."
  (let ((link (emacs-wiki-wiki-link-target wiki-link)))
    (if (or (emacs-wiki-wiki-url-p link)
	    (save-match-data
	      (string-match "/" link)))
	link
      (if (assoc (emacs-wiki-wiki-base link)
		 (emacs-wiki-file-alist t))
	  (if (string-match "#" link)
	      (concat (emacs-wiki-published-name
		       (substring link 0 (match-beginning 0)))
		      (substring link (match-beginning 0)))
	    (emacs-wiki-published-name link))))))

(defun emacs-wiki-link-href (wiki-link)
  "Resolve the given WIKI-LINK into its ultimate <a href> form."
  (let ((url (emacs-wiki-link-url wiki-link))
	(name (emacs-wiki-wiki-visible-name wiki-link)))
    (if (null url)
	(concat "<a class=\"nonexistent\" href=\""
		emacs-wiki-maintainer "\">" name "</a>")
      (if (string-match emacs-wiki-image-regexp url)
	  (concat "<img src=\"" url "\" alt=\"" name "\"/>")
	(concat "<a href=\"" url "\">" name "</a>")))))

(defun emacs-wiki-replace-links ()
  "Replace wiki names with HTML markup."
  (while (re-search-forward emacs-wiki-url-or-name-regexp nil t)
    (unless (get-text-property (match-beginning 0) 'read-only)
      (replace-match (emacs-wiki-link-href (match-string 0)) t))))

(defun emacs-wiki-redirect-page ()
  (let ((link (match-string 1)))
    (setq emacs-wiki-meta-http-equiv "Refresh"
	  emacs-wiki-meta-content
	  (concat (int-to-string emacs-wiki-redirect-delay)
		  ";\nURL=" (emacs-wiki-link-url link)))
    (concat "You should momentarily be redirected [[" link "][here]].")))

(defun emacs-wiki-markup-verbatim ()
  (let ((start (match-beginning 0)) (beg (point)))
    (when (search-forward "</verbatim>" nil t)
      (delete-region (match-beginning 0) (point))
      (delete-region start beg)
      (add-text-properties start (point)
			   '(rear-nonsticky (read-only) read-only t)))
    nil))

(defun emacs-wiki-markup-literal-word ()
  (let ((beg (point)))
    (when (search-forward "=" nil t)
      (delete-char -1)
      (insert "</code>")
      (save-excursion
	(goto-char beg)
	(delete-char -1)
	(setq beg (point))
	(insert "<code>"))
      (add-text-properties beg (point)
			   '(rear-nonsticky (read-only) read-only t)))
    nil))

(defun emacs-wiki-guard-wikiname ()
  (delete-region (match-beginning 0) (point))
  (when (looking-at emacs-wiki-name-regexp)
    (add-text-properties (point) (match-end 0)
			 '(rear-nonsticky (read-only) read-only t))
    (goto-char (match-end 0))
    nil))

(defun emacs-wiki-markup-embedded-lisp ()
  (let ((start (match-beginning 0))
	(beg (point)))
    (when (search-forward "</lisp>" nil t)
      (prog1
	  (emacs-wiki-eval-lisp
	   (buffer-substring-no-properties beg (match-beginning 0)))
	(set-match-data (list start (point)))))))

(defun emacs-wiki-markup-footnote ()
  (if (/= (line-beginning-position) (match-beginning 0))
      "<sup><a name=\"fnr.\\1\" href=\"#fn.\\1\">\\1</a></sup>"
    (prog1
	"<p><sup>[<a name=\"fn.\\1\" href=\"#fnr.\\1\">\\1</a>]</sup>"
      (save-excursion
	(save-match-data
	  (let* ((beg (goto-char (match-end 0)))
		 (end (and (search-forward "\n\n" nil t)
			   (prog1
			       (copy-marker (match-beginning 0))
			     (goto-char beg)))))
	    (while (re-search-forward "^[ \t]+\\([^\n]\\)" end t)
	      (replace-match "\\1" t))))))))

(defun emacs-wiki-markup-list-or-paragraph ()
  (if (null (match-string 2))
      (if (>= (string-width (match-string 4)) 6)
	  "<center>\n<p>\n\\4\\5\n</p>\n</center>\n"
	"<blockquote>\n<p>\n\\4\\5\n</p>\n</blockquote>\n")
    (let ((str (match-string 2)))
      (cond
       ((and (eq (aref str 0) ?-))
	"<ul>\n<li>\\5</ul>\n")
       ((and (>= (aref str 0) ?0)
	     (<= (aref str 0) ?9))
	"<ol>\n<li>\\5</ol>\n")
       (t
	"<dl>\n<dt>\\3</dt>\n<dd>\n<p>\n\\5\n</p>\n</dd>\n</dl>\n")))))

(defun emacs-wiki-markup-table ()
  (let* ((fields (append (save-match-data
			   (split-string (match-string 1) "[ \t]*|+[ \t]*"))
			 (list (match-string 4))))
	 (len (length (match-string 3)))
	 (row (cond ((= len 1) "tbody")
		    ((= len 2) "thead")
		    ((= len 3) "tfoot")))
	 (col (cond ((= len 1) "td")
		    ((= len 2) "th")
		    ((= len 3) "td"))))
    (concat "<table " emacs-wiki-table-attributes ">\n"
	    "<" row ">\n" "<tr>\n<" col ">"
	    (mapconcat 'identity fields (format "</%s><%s>" col col))
	    "</" col ">\n" "</tr>\n" "</" row ">\n"
	    "</table>\n")))

(defun emacs-wiki-markup-verse ()
  (save-match-data
    (let* ((lines (split-string (match-string 1) "\n"))
	   (l lines))
      (while l
	(if (and (> (length (car l)) 2)
		 (string-match "\\`\\s-*> " (car l)))
	    (setcar l (substring (car l) (match-end 0))))
	(setq l (cdr l)))
      (concat "<p class=\"verse\">"
	      (mapconcat 'identity lines "\n") "</p>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki HTTP Server (using httpd.el)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-http nil
  "Options controlling the behaviour of the Emacs Wiki HTTP server.
See the file httpd.el (http://www.chez.com/emarsden/downloads/) for
more information, which is required for this code to work.

Once you have httpd.el, place the following line in your
/etc/inetd.conf file:

  8080 stream tcp nowait.10000 nobody /usr/local/bin/emacs-httpd

Your emacs-httpd script should look something like this:

  #!/bin/sh
  /usr/bin/emacs -batch --no-init-file --no-site-file -batch \\
      -l /usr/share/emacs/site-lisp/httpd \\
      -l /usr/share/emacs/site-lisp/emacs-wiki \\
      --eval \"(setq httpd-document-root emacs-wiki-publishing-directory \\
		    emacs-wiki-maintainer \\\"mailto:joe@where.com\\\")\" \\
      -f httpd-serve

Emacs-wiki can now server pages directly over HTTP on port 8080.  Note
that if you need to configure any variables in emacs-wiki, you will
have to repeat those configurations in the emacs-httpd script."
  :group 'emacs-wiki)

(defvar emacs-wiki-httpd-search nil
  "This is set to non-nil when httpd.el is being used.")

(defcustom emacs-wiki-http-search-form
  "
<form method=\"GET\" action=\"/searchwiki\">
  <center>
    Search for: <input type=\"text\" size=\"50\" name=\"q\" value=\"\">
    <input type=\"submit\" value=\"Search!\">
  </center>
</form>\n"
  "The form presenting for doing searches when using httpd.el."
  :type 'string
  :group 'emacs-wiki-http)

(when (featurep 'httpd)
  ;; Since we're serving directly, change the prefix and suffix
  (setq emacs-wiki-publishing-file-prefix "wiki?"
	emacs-wiki-publishing-file-suffix ""
	emacs-wiki-httpd-search "<a href=\"searchwiki\">Search</a>")

  (defun emacs-wiki-http-send-buffer (&optional title modified code msg)
    (emacs-wiki-replace-markup title)
    (princ "HTTP/1.0 ")
    (princ (or code 200))
    (princ " ")
    (princ (or msg "OK"))
    (princ "\r\nServer: emacs-wiki.el/2.7\r\n")
    (princ "Connection: close\r\n")
    (princ "MIME-Version: 1.0\r\n")
    (princ "Date: ")
    (princ (format-time-string "%a, %e %b %Y %T %Z"))
    (princ "\r\nFrom: ")
    (princ (substring emacs-wiki-maintainer 7))
    (when modified
      (princ "\r\nLast-Modified: ")
      (princ (format-time-string "%a, %e %b %Y %T %Z" modified)))
    (princ "\r\nContent-Type: text/html; charset=iso-8859-1\r\n")
    (princ "Content-Length: ")
    (princ (1- (point-max)))
    (princ "\r\n\r\n")
    (princ (buffer-string)))

  (defun httpd-render-wiki-page (name)
    (if (equal name emacs-wiki-index-page)
	(with-current-buffer (emacs-wiki-generate-index t t)
	  (emacs-wiki-http-send-buffer "WikiIndex"))
      (let ((entry (assoc name (emacs-wiki-file-alist))))
	(if (or (null entry)
		(emacs-wiki-private-p name))
	    (with-temp-buffer
	      (let ((msg (format "Wiki page %s not found" name)))
		(insert msg ".")
		(emacs-wiki-http-send-buffer "Error" nil 404 msg)))
	  (find-file (cdr entry))
	  (goto-char (point-min))
	  (let ((emacs-wiki-httpd-search
		 (concat emacs-wiki-httpd-search
			 "\n\t      &nbsp;/&nbsp;\n"
			 "\t      <a href=\"searchwiki?q=" name
			 "\">Referrers</a>")))
	    (emacs-wiki-http-send-buffer
	     nil (nth 5 (file-attributes (cdr entry)))))))))

  (defun emacs-wiki-wikify-search-results (term)
    "Convert the current buffer's grep results into a Wiki form."
    (goto-char (point-max))
    (forward-line -2)
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (kill-line 2)
    (let ((results (list t)))
      (while (re-search-forward "^.+/\\([^/:]+\\):\\s-*[0-9]+:\\(.+\\)" nil t)
	(let ((page (match-string 1)))
	  (unless (or (emacs-wiki-private-p page)
		      (string-match emacs-wiki-file-ignore-regexp page))
	    (let ((text (match-string 2))
		  (entry (assoc page results)))
	      (if entry
		  (nconc (cdr entry) (list text))
		(nconc results (list (cons page (list text)))))))))
      (delete-region (point-min) (point-max))
      (setq results
	    (sort (cdr results)
		  (function
		   (lambda (l r)
		     (string-lessp (car l) (car r))))))
      (while results
	(unless
	    (insert "[[" (caar results) "]] ::\n")
	  (let ((hits (cdar results)))
	    (while hits
	      (while (string-match (concat "\\([^*?[/]\\)\\<\\(" term "\\)\\>")
				   (car hits))
		(setcar hits (replace-match "\\1**\\2**" t nil (car hits))))
	      (insert "  > " (car hits) "\n")
	      (setq hits (cdr hits))))
	  (insert "\n"))
	(setq results (cdr results)))))

  (httpd-add-handler
   "^index\\.html"
   (function
    (lambda (page)
      (httpd-render-wiki-page emacs-wiki-home-page))))

  (httpd-add-handler
   "^wiki\\?.+"
   (function
    (lambda (page)
      (string-match "^wiki\\?\\(.+\\)" page)
      (httpd-render-wiki-page (match-string 1 page)))))

  (httpd-add-handler
   "^searchwiki"
   (function
    (lambda (page)
      (with-temp-buffer
	(insert "<verbatim>" emacs-wiki-http-search-form "</verbatim>")
	(emacs-wiki-http-send-buffer "SearchWikiPages")))))

  (defvar emacs-wiki-search-term nil)
  (defvar emacs-wiki-search-buffer nil)
  (defvar emacs-wiki-search-ready nil)

  (httpd-add-handler
   "^searchwiki\\?q=.+"
   (function
    (lambda (page)
      (string-match "^searchwiki\\?q=\\(.+\\)" page)
      (setq emacs-wiki-search-term (match-string 1 page)
	    emacs-wiki-search-buffer nil
	    emacs-wiki-search-ready nil)
      (setq compilation-finish-function
	    (function
	     (lambda (buffer msg)
	       (setq emacs-wiki-search-buffer buffer)
	       (with-current-buffer emacs-wiki-search-buffer
		 (emacs-wiki-wikify-search-results emacs-wiki-search-term))
	       (setq emacs-wiki-search-ready msg))))
      (emacs-wiki-grep emacs-wiki-search-term)
      (while (null emacs-wiki-search-ready)
	(sit-for 0 250))
      (with-current-buffer emacs-wiki-search-buffer
	(emacs-wiki-http-send-buffer "SearchResults"))))))

(provide 'emacs-wiki)

;;; emacs-wiki.el ends here
