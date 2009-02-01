;;; jde-java-font-lock.el -- Extra level font locking for java

;; Copyright (C) 1998, 1999, 2000, 2001 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;;             Paul Kinnucan <paulk@mathworks.com>
;; Created: September 28 1998
;; Keywords: java, tools
;; VC: $Id: jde-java-font-lock.el,v 1.3 2000/12/18 05:22:45 paulk Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Adds some extra level font locking for java in `jde-mode'.
;;
;; * Numbers are fontified with `jde-java-font-lock-number-face'.
;;
;; * Modifiers are fontified with `font-lock-builtin-face'.  This face
;;   is based on XEmacs `font-lock-preprocessor-face' if available.
;;
;; * Keywords const and goto are fontified with
;;   `font-lock-warning-face'.  These keywords are reserved, even
;;   though they are not currently used.
;;
;; * The keyword default is fontified with `font-lock-keyword-face'.
;;
;; * User's defined identifiers (see variable
;;   `jde-java-font-lock-api-file') are fontified with
;;   `jde-java-font-lock-api-face'.
;;
;; * Capitalized identifiers, text between `' in comments and javadoc
;;   tags (including non official javadoc tags) are fontified with
;;   `font-lock-constant-face'.  This face is based on XEmacs
;;   `font-lock-reference-face' if available.
;;
;; * Javadoc links (following @link tags or enclosed in HTML <a> tags)
;;   are fontified with `jde-java-font-lock-link-face'
;;
;; * Javadoc code samples (enclosed in HTML <code> tags or following
;;   @see tags) are fontified with `jde-java-font-lock-code-face'.  By
;;   default, this face is based on `font-lock-builtin-face'.
;;  
;; * Javadoc HTML bold style is fontified with
;;   `jde-java-font-lock-bold-face'.  By default, this face is based
;;   on `bold'.
;;
;; * Javadoc HTML italic style is fontified with
;;   `jde-java-font-lock-italic-face'.  By default, this face is based
;;   on `italic'.
;;
;; * Javadoc HTML underlined style is fontified with
;;   `jde-java-font-lock-underline-face'.  By default, this face is
;;   based on `underline'.
;;
;; * Javadoc HTML preformatted style is fontified with
;;   `jde-java-font-lock-pre-face'.  By default, this face is based on
;;   `default'.
;;
;; All font-lock and jde-java-font-lock faces are individually
;; customizable.

;; This code has been tested with FSF Emacs 20.7, 21.0 and XEmacs
;; 21.1.  Any comments, suggestions, bug reports or upgrade requests
;; are welcome.  Please send them to the maintainers.

;;; History:
;;
;; See at end of this file.

;;; Code:

(defcustom jde-use-font-lock t
  "*Turn on font-locking if non-nil.
Set to nil to disable the use of font-locking."
  :group 'jde-project
  :type 'boolean)

;;;;
;;;; Define the faces
;;;;

;; Create a specific face for numbers
(defface jde-java-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'font-lock-highlighting-faces)

;; Create a specific face for user's defined names
(defface jde-java-font-lock-api-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark)) (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "dark goldenrod"))
    (((class color) (background dark)) (:foreground "light goldenrod")))
  "Font Lock mode face used to highlight user's defined names."
  :group 'font-lock-highlighting-faces)

;; Create a specific face for links
(defface jde-java-font-lock-link-face
  '((t (:foreground "blue" :italic nil :underline t)))
  "Font Lock mode face used to highlight links."
  :group 'font-lock-highlighting-faces)

;;; Compatibility
(if jde-xemacsp
    (progn
      
      (defvar font-lock-builtin-face 'font-lock-builtin-face
	"Face name to use for builtins.")

      ;; For consistency try to define the builtin face as the XEmacs
      ;; preprocessor face
      (condition-case nil
          (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face)
        (error
         (defface font-lock-builtin-face
           '((t (:foreground "blue" :italic nil :underline t)))
           "Font Lock mode face used to highlight builtins."
           :group 'font-lock-highlighting-faces)))

      (defvar font-lock-constant-face 'font-lock-constant-face
        "Face name to use for constant and label names.")
      
      ;; For consistency try to define the constant face as the XEmacs
      ;; reference face
      (condition-case nil
          (copy-face 'font-lock-reference-face 'font-lock-constant-face)
        (error
         (defface font-lock-constant-face
           '((((class grayscale) (background light))
              (:foreground "LightGray" :bold t :underline t))
             (((class grayscale) (background dark))
              (:foreground "Gray50" :bold t :underline t))
             (((class color) (background light)) (:foreground "CadetBlue"))
             (((class color) (background dark)) (:foreground "Aquamarine"))
             (t (:bold t :underline t)))
           "Font Lock mode face used to highlight constants and labels."
           :group 'font-lock-highlighting-faces)))

      ))

;; Make new faces based on existing ones
(copy-face 'bold                   'jde-java-font-lock-bold-face)
(copy-face 'italic                 'jde-java-font-lock-italic-face)
(copy-face 'underline              'jde-java-font-lock-underline-face)
(copy-face 'default                'jde-java-font-lock-pre-face)
(copy-face 'font-lock-builtin-face 'jde-java-font-lock-code-face)

;; Define the extra font lock faces
(defvar jde-java-font-lock-number-face    'jde-java-font-lock-number-face
  "Face name to use for numbers.")
(defvar jde-java-font-lock-api-face       'jde-java-font-lock-api-face
  "Face name to use for user's defined names.")
(defvar jde-java-font-lock-link-face      'jde-java-font-lock-link-face
  "Face name to use for links.")
(defvar jde-java-font-lock-bold-face      'jde-java-font-lock-bold-face
  "Face name to use for HTML bold text style.")
(defvar jde-java-font-lock-italic-face    'jde-java-font-lock-italic-face
  "Face name to use for HTML italic text style.")
(defvar jde-java-font-lock-underline-face 'jde-java-font-lock-underline-face
  "Face name to use for HTML underlined text style.")
(defvar jde-java-font-lock-pre-face       'jde-java-font-lock-pre-face
  "Face name to use for HTML preformatted text style.")
(defvar jde-java-font-lock-code-face      'jde-java-font-lock-code-face
  "Face name to use for HTML program code style.")

;;;;
;;;; Useful constants
;;;;

(defconst jde-java-font-lock-capital-letter
  "A-Z\300-\326\330-\337_$"
  "Java identifier capital letter.")

(defconst jde-java-font-lock-letter
  (concat jde-java-font-lock-capital-letter "a-z")
  "Java identifier letter.")

(defconst jde-java-font-lock-capital-letter-or-digit
  (concat jde-java-font-lock-capital-letter "0-9")
  "Java identifier capital letter or digit.")

(defconst jde-java-font-lock-letter-or-digit
  (concat jde-java-font-lock-letter "0-9")
  "Java identifier letter or digit.")

;;;;
;;;; Support for fontification inside javadocs and comments.
;;;;

;; Define font lock keywords for comments and javadocs only
(defun jde-java-font-lock-remove-javadoc-keywords (keywords)
  "Remove existing javadoc font lock keywords from KEYWORDS.
That is those with \"@\" in their matcher regexp."
  (let (kw matcher match)
    (while keywords
      (setq matcher  (car keywords)
            keywords (cdr keywords))
      (if (not (and (consp matcher)
                    (stringp (car matcher))
                    (string-match "@" (car matcher))))
          (setq kw (cons matcher kw))))
    (nreverse kw)))

(defun jde-java-font-lock-in-javadoc-p ()
  "Return non-nil if point is in a javadoc comment."
  (let* ((p (point))
         (in-javadoc-p
          (save-match-data
            (and (re-search-backward "^[ \t]*/\\*\\*" nil t)
                 (skip-chars-forward " \t" p)
                 (eq (get-text-property (point) 'face)
                     'font-lock-comment-face)
                 (forward-comment 1)
                 (< p (point))))))
    (goto-char p)
    in-javadoc-p))

(defun jde-java-font-lock-search-in-comment (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a java comment.  Buffer position END bounds
the search.  The match found must not extend after that position."
  (let (in-comment-p)
    (while (and (not in-comment-p)
                (re-search-forward regexp end t))
      (setq in-comment-p
            (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-face)))
    in-comment-p))

(defun jde-java-font-lock-search-in-javadoc (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a javadoc comment.  Buffer position END
bounds the search.  The match found must not extend after that
position."
  (let (in-javadoc-p)
    (while (and (not in-javadoc-p)
                (re-search-forward regexp end t))
      (setq in-javadoc-p (jde-java-font-lock-in-javadoc-p)))
    in-javadoc-p))

(defun jde-java-font-lock-quote-keyword ()
  "Return a font lock keyword for comment enclosed in \`\'."
  `((lambda (end)
      (jde-java-font-lock-search-in-comment
       "`\\(.*\\)'"
       end))
    1 font-lock-constant-face t))

(defun jde-java-font-lock-html-ahref-keyword ()
  "Return a font lock keyword for HTML A HREF anchor.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
       end))
    1 jde-java-font-lock-link-face t))

(defun jde-java-font-lock-html-strong-keyword ()
  "Return a font lock keyword for HTML STRONG style.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Ss][Tt][Rr][Oo][Nn][Gg]>\\([^<]*\\)</[Ss][Tt][Rr][Oo][Nn][Gg]>"
       end))
    1 jde-java-font-lock-bold-face t))

(defun jde-java-font-lock-html-bold-keyword ()
  "Return a font lock keyword for HTML B style.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Bb]>\\([^<]*\\)</[Bb]>"
       end))
    1 jde-java-font-lock-bold-face t))

(defun jde-java-font-lock-html-italic-keyword ()
  "Return a font lock keyword for HTML I style.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Ii]>\\([^<]*\\)</[Ii]>"
       end))
    1 jde-java-font-lock-italic-face t))

(defun jde-java-font-lock-html-underline-keyword ()
  "Return a font lock keyword for HTML U style.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Uu]>\\([^<]*\\)</[Uu]>"
       end))
    1 jde-java-font-lock-underline-face t))

(defun jde-java-font-lock-html-code-keyword ()
  "Return a font lock keyword for HTML CODE style.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Cc][Oo][Dd][Ee]>\\([^<]*\\)</[Cc][Oo][Dd][Ee]>"
       end))
    1 jde-java-font-lock-code-face t))

(defun jde-java-font-lock-html-pre-keyword ()
  "Return a font lock keyword for HTML PRE style.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "<[Pp][Rr][Ee]>\\([^<]*\\)</[Pp][Rr][Ee]>"
       end))
    1 jde-java-font-lock-pre-face t))

(defun jde-java-font-lock-javadoc-tag-keyword ()
  "Return a font lock keyword for javadoc tags.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "\\(@[" jde-java-font-lock-letter-or-digit "]+\\)")
       end))
    2 font-lock-constant-face t))

(defun jde-java-font-lock-javadoc-docroot-keyword ()
  "Return a font lock keyword for javadoc @docRoot tags.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "{\\(@docRoot\\)}"
       end))
    1 font-lock-constant-face t))

(defun jde-java-font-lock-javadoc-link-keyword ()
  "Return a font lock keyword for javadoc @link tags.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       "{\\(@link\\)\\>[ \t]+\\([^}]*\\)}"
       end))
    (1 font-lock-constant-face t)
    (2 jde-java-font-lock-link-face t)))

(defun jde-java-font-lock-javadoc-see-ref-keyword ()
  "Return a font lock keyword for javadoc @see references.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "@see\\>[ \t]*"
                "\\([.#" jde-java-font-lock-letter-or-digit "]+\\)")
       end))
    2 jde-java-font-lock-code-face t))

(defun jde-java-font-lock-javadoc-param-name-keyword ()
  "Return a font lock keyword for javadoc @param names.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "@param\\>[ \t]*\\(\\sw+\\)?")
       end))
    2 font-lock-variable-name-face prepend t))

(defun jde-java-font-lock-javadoc-exception-type-keyword ()
  "Return a font lock keyword for javadoc exception types.
Only fontify javadoc comments."
  `((lambda (end)
      (jde-java-font-lock-search-in-javadoc
       ,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
                "@\\(exception\\|throws\\)\\>[ \t]*\\(\\S-+\\)?")
       end))
    3 font-lock-type-face prepend t))

;;;;
;;;; Support for fontification of user's defined names.
;;;;

(defcustom jde-java-font-lock-api-file
  (expand-file-name "~/jde-java-font-lock.api")
  "*File which contains a list of user's defined names to fontify.
If nil no name fontification occurs.  Otherwise the specified file must
contain one name by line.  Lines not beginning with a letter are
ignored.  When you change this file or modify its content a new cache
of font lock regular expressions will be rebuilt when restarting
Emacs.  Also, you can manually rebuild the cache and update font lock
keywords by entering the command:

\\[universal-argument] \\[jde-java-font-lock-setup-keywords]."
  :group 'jde-project
  :type '(choice :tag "Names"
                 (const :tag "No" nil)
                 (file  :tag "In file" :format "%t\n%v")))

(defcustom jde-java-font-lock-api-name-filter nil
  "*Function used to filter a name."
  :group 'jde-project
  :type 'function)

(defconst jde-java-font-lock-api-entry-regexp
  (concat "^[" jde-java-font-lock-letter "]"
          "[" jde-java-font-lock-letter-or-digit "]+$")
  "Regexp to match a valid entry in `jde-java-font-lock-api-file'.")

(defconst jde-java-font-lock-api-entry-match 0
  "Index of the match data in `jde-java-font-lock-api-entry-regexp'.")

(defun jde-java-font-lock-api-names (&optional filter)
  "Return the list of names in `jde-java-font-lock-api-file'.
If optional FILTER function is non-nil it is called for each name
found and must return non-nil to include it in the result list."
  (let (k kl)
    (if (and jde-java-font-lock-api-file
             (file-readable-p jde-java-font-lock-api-file))
        (with-temp-buffer
          (erase-buffer)
          (insert-file-contents jde-java-font-lock-api-file)
          (goto-char (point-min))
          (while (re-search-forward jde-java-font-lock-api-entry-regexp nil t)
            (setq k (match-string jde-java-font-lock-api-entry-match))
            ;; Allow filtering of names
            (if (or (null filter) (funcall filter k))
                (setq kl (cons k kl))))))
    kl))

(defun jde-java-font-lock-api-split-list (l n)
  "Split list L in sub listes of N elements.
If L is nil return nil.  If N is less than 1 all elements will be in
one sub list."
  (if l
      (if (<= n 0)
          (list l)
        (let (split-list sub-list i)
          (while l
            (setq i 0 sub-list nil)
            (while (and l (< i n))
              (setq sub-list (cons (car l) sub-list)
                    i        (1+ i)
                    l        (cdr l)))
            (if sub-list
                (setq split-list (cons sub-list split-list))))
          split-list))))

(defun jde-java-font-lock-api-build-regexps (max-matches)
  "Build regular expressions matching names to fontify.
MAX-MATCHES is the maximum number of names that one regular expression
will match.  If MAX-MATCHES is less than 1 one regular expression will
match all the names."
  (let ((max-specpdl-size 2000)) ;; Prevent errors in `regexp-opt'
				 ;; when processing long string listes
    (mapcar (function
             (lambda (k)
               (concat "\\<" (regexp-opt k t) "\\>")))
            (jde-java-font-lock-api-split-list
             (jde-java-font-lock-api-names
              jde-java-font-lock-api-name-filter)
             max-matches))))

(defvar jde-java-font-lock-api-cache nil
  "Cache of regular expressions matching names to fontify..")

(defun jde-java-font-lock-api-cache-file ()
  "Return the filename of the regular expressions cache.
There is a different cache file for each major version of (X)Emacs
because of incompatible regular expressions returned by `regexp-opt'."
  (and jde-java-font-lock-api-file
       (format "%s.%semacs-%d.apicache"
               jde-java-font-lock-api-file
               (if jde-xemacsp "x" "")
               emacs-major-version)))

(defconst jde-java-font-lock-api-cache-file-header
  ";;; Regular expressions matching names to fontify.
;;; Automatically generated by `jde-java-font-lock' on %s.
"
  "Header to be written into the cache file.")

(defun jde-java-font-lock-api-regexps (&optional rebuild)
  "Return regular expressions matching names to fontify.
The list is cached in variable `jde-java-font-lock-api-cache'.  If it
is nil try to initialize it from the cache file (see function
`jde-java-font-lock-api-cache-file').  If optional REBUILD flag is
non-nil or there is no cache file or the cache file is older than the
names file (see variable `jde-java-font-lock-api-file'), a new cache
is created."
  (let ((cache (jde-java-font-lock-api-cache-file)))
    (cond

     ;; Inconditionnal rebuild
     (rebuild
      ;; Clear the cache to rebuild
      (setq jde-java-font-lock-api-cache nil))

     ;; No names file exists
     ((null cache)
      ;; Clear the cache (no fontification)
      (setq jde-java-font-lock-api-cache nil))
     
     ;; A cache file exists
     ((file-readable-p cache)
      (if (file-newer-than-file-p jde-java-font-lock-api-file cache)
          (progn
            (message
             "jde-java-font-lock: names file %s newer than cache file %s"
             jde-java-font-lock-api-file cache)
            ;; The api file has been modified since the cache was
            ;; created, so clear the cache to rebuild
            (setq jde-java-font-lock-api-cache nil))
        ;; Try to load the existing cache if needed
        (or jde-java-font-lock-api-cache
            (condition-case nil
                (load-file cache)
              ;; If load fails clear the cache to rebuild
              (error
               (setq jde-java-font-lock-api-cache nil)))))))

    (or jde-java-font-lock-api-cache
        (not cache)
        ;; Build a new cache if it is empty and available
        (progn
          (message "jde-java-font-lock: building names cache...")
          (when (setq jde-java-font-lock-api-cache
                      (jde-java-font-lock-api-build-regexps
                       ;; WARNING: It seems XEmacs search fails with a
                       ;; very long regexp.  So get regexps for groups
                       ;; of up to 200 names.
                       (if jde-xemacsp 200 0)))
            ;; Save regexps in cache
            (with-current-buffer (find-file-noselect cache)
              (erase-buffer)
              (insert
               (format jde-java-font-lock-api-cache-file-header
                       (current-time-string))
               (format "(setq jde-java-font-lock-api-cache '%S)"
                       jde-java-font-lock-api-cache))
              (save-buffer)
              (kill-buffer (current-buffer))))
          (message "jde-java-font-lock: building names cache...%s"
                   (if jde-java-font-lock-api-cache "done" "empty"))))
          jde-java-font-lock-api-cache))

(defun jde-java-font-lock-api-keywords (&optional rebuild)
  "Return a list of font lock keywords for user's defined names.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (mapcar (function
	   (lambda (k)
	     (cons k 'jde-java-font-lock-api-face)))
	  (jde-java-font-lock-api-regexps rebuild)))

;;;;
;;;; Font lock setup.
;;;;

(defvar java-font-lock-keywords-4 nil
  "Extra level fontification keywords for JDE mode.")

;;;###autoload
(defun jde-java-font-lock-setup-keywords (&optional rebuild)
  "Setup font lock keywords in `java-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (interactive "P")
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq rebuild t))
  (setq
   java-font-lock-keywords-4
   (append

    ;; Feature scoping: These must come first or the Special
    ;; constants, Modifiers and Packages from keywords-1 will catch
    ;; them.
;;; Compatibility
    (if jde-xemacsp
        (list
            
         ;; Special keywords and constants
         '("\\<\\(this\\|super\\)\\>" (1 font-lock-keyword-face))
         '("\\<\\(false\\|null\\|true\\)\\>" (1 font-lock-constant-face))
         ))
       
    (list

     ;; Fontify default as keyword
     '("\\<\\(default\\)\\>" (1 font-lock-keyword-face))

     ;; Fontify const and goto with warning face. These keywords are
     ;; reserved, even though they are not currently used.
     '("\\<\\(const\\|goto\\)\\>" (1 font-lock-warning-face))

     ;; Fontify modifiers.
     (cons (concat "\\<\\("
                   (eval-when-compile
                     (regexp-opt
                      '(
                        "abstract"
                        "const"
                        "final"
                        "native"
                        "private"
                        "protected"
                        "public"
                        "static"
                        "strictfp"
                        "synchronized"
                        "transient"
                        "volatile"
                        )))
                   "\\)\\>")
           'font-lock-builtin-face)
        
     ;; Fontify package directives
     '("\\<\\(package\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face nil t)
       ("\\=\\.\\(\\sw+\\)" nil nil
        (1 font-lock-constant-face nil t)))
        
     ;; Fontify import directives
     '("\\<\\(import\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face nil t)
       ("\\=\\.\\(\\*\\|\\sw+\\)" nil nil
        (1 (if (equal (char-after (match-end 0)) ?\.)
               'font-lock-constant-face
             (if (equal (char-before (match-end 0)) ?\*)
                 'jde-java-font-lock-number-face
               'font-lock-type-face)))))
     )

    ;; Fontify user's defined names
    (jde-java-font-lock-api-keywords rebuild)
       
;;; Compatibility
    (if jde-xemacsp
        java-font-lock-keywords-2
      ;; Remove existing javadoc font lock keywords from FSF Emacs
      ;; `java-font-lock-keywords-3'
      (jde-java-font-lock-remove-javadoc-keywords
       java-font-lock-keywords-3))

;;; Compatibility
    (if jde-xemacsp
        nil
      ;; FSF Emacs don't fontify capitalized types so do it
      (list
       `(eval .
              (list
               (concat "\\<\\([" jde-java-font-lock-capital-letter "]\\sw*\\)\\>"
                       "\\([ \t]*\\[[ \t]*\\]\\)*"
                       "\\([ \t]*\\sw\\)")
               '(font-lock-match-c-style-declaration-item-and-skip-to-next
                 (goto-char (match-beginning 3))
                 (goto-char (match-beginning 3))
                 (1 (if (match-beginning 2)
                        font-lock-function-name-face
                      font-lock-variable-name-face)))))
       (cons
        (concat "\\<\\([" jde-java-font-lock-capital-letter "]\\sw*\\)\\>"
                "\\([ \t]*\\[[ \t]*\\]\\)*"
                "\\([ \t]*\\sw\\)")
        '(1 font-lock-type-face))
            
       '("\\<\\(new\\|instanceof\\)\\>[ \t]+\\(\\sw+\\)"
         2 font-lock-type-face)))

    ;; Some extra fontification
    (list
        
     ;; Fontify numbers
     (cons
      (concat "\\b\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*"
              "\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b")
      'jde-java-font-lock-number-face)
     (cons
      (concat "\\b\\(\\.[0-9]+"
              "\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b")
      'jde-java-font-lock-number-face)
     
     ;; Fontify capitalised identifiers as constant
     (cons
      (concat "\\(\\b[" jde-java-font-lock-capital-letter
              "]+[" jde-java-font-lock-capital-letter-or-digit
              "]*\\b\\)")
      '(1 font-lock-constant-face))

     ;; Fontify text between `' in comments
     (jde-java-font-lock-quote-keyword))

    ;; Fontify javadoc comments
    (list
       
     ;; Fontify javadoc tags (including non official ones)
     (jde-java-font-lock-javadoc-tag-keyword)
     ;; Fontify @param variable name
     (jde-java-font-lock-javadoc-param-name-keyword)
     ;; Fontify @exception or @throws exception type
     (jde-java-font-lock-javadoc-exception-type-keyword)
     ;; Fontify @docRoot
     (jde-java-font-lock-javadoc-docroot-keyword)
     ;; Fontify @link
     (jde-java-font-lock-javadoc-link-keyword)
     ;; Fontify @see reference
     (jde-java-font-lock-javadoc-see-ref-keyword)
     ;; Fontify the text of a HREF anchor
     (jde-java-font-lock-html-ahref-keyword)
     ;; Fontify <strong> style text
     (jde-java-font-lock-html-strong-keyword)
     ;; Fontify <b> style text
     (jde-java-font-lock-html-bold-keyword)
     ;; Fontify <i> style text
     (jde-java-font-lock-html-italic-keyword)
     ;; Fontify <u> style text
     (jde-java-font-lock-html-underline-keyword)
     ;; Fontify <code> style text
     (jde-java-font-lock-html-code-keyword)
     ;; Fontify <pre> style text
     (jde-java-font-lock-html-pre-keyword))
       
    )))

(jde-java-font-lock-setup-keywords)

;; Setup JDE mode for font locking.  Copy `font-lock-defaults' from
;; `java-mode' and add the new `java-font-lock-keywords-4' level in
;; `jde-mode'.

(defun jde-java-font-lock-defaults (java-defaults)
  "Return a new `font-lock-defaults' value from JAVA-DEFAULTS.
That is add the new `java-font-lock-keywords-4' level."
  (cons (append (car java-defaults) '(java-font-lock-keywords-4))
        (cdr java-defaults)))

;;; Compatibility
;; XEmacs
(if jde-xemacsp
    (put 'jde-mode 'font-lock-defaults
         (jde-java-font-lock-defaults
          (get 'java-mode 'font-lock-defaults)))
  
  ;; FSF Emacs
  (add-to-list
   'font-lock-defaults-alist
   (cons 'jde-mode
         (jde-java-font-lock-defaults
          (cdr (assq 'java-mode font-lock-defaults-alist)))))
  )
                 
(defun jde-setup-syntax-coloring()
  "Set up JDE mode syntax coloring."
  (cond (window-system
         
	 ;; If not XEmacs 20.1 turn on font lock.
	 ;; (XEmacs 21 has font-lock on by default.)
	 (if (or
	      (not jde-xemacsp)
	      (not
	       (and
		(eq emacs-major-version 21)
		(eq emacs-minor-version 0))))
	     (turn-on-font-lock))

	 (setq font-lock-maximum-decoration t)

	 (if (not jde-xemacsp)
	     (global-font-lock-mode 1 t))
	 )))


(provide 'jde-java-font-lock)

;;; Change History:

;;
;; $Log: jde-java-font-lock.el,v $
;; Revision 1.3  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.2  2000/10/10 06:41:47  paulk
;; Fixed some XEmacs compatibility problems.
;;
;; Revision 1.1  2000/10/08 12:53:22  paulk
;; Initial revision.
;;

;;; jde-java-font-lock.el ends here
