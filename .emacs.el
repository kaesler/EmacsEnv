;; This file has been formatted so it can be viewed/edited in
;; Folding Minor Mode.
;;

(message "Start of .emacs...")

(cd "~/")

;;{{{  Set some global variables.

(setq user-emacs-directory "~/apps/emacs/")
(defvar esler-elisp-directory (concat user-emacs-directory "elisp"))

(defvar esler-modern-emacs (> emacs-major-version 23))

;;{{{ Package

(if esler-modern-emacs
    (progn
      (require 'package)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize)))

;;}}}

;;{{{  Determine what kind of OS and display is being used.

(defvar running-on-mac (memq window-system '(ns)))
(defvar running-on-w32 (memq window-system '(w32 win32 mswindows)))

(defvar running-as-x-client (memq window-system '(x x11)))
(defvar running-as-terminal-client (null window-system))

;; Boolean variable to indicate if we're an ASCII Emacs in an xterm window
;; (so we can enable the mouse support for this mode of operation).

(defvar running-as-xterm-client (and (not running-as-x-client)
                                     (or (equal "xterm" (getenv "TERM"))
                                         (equal "vt100" (getenv "TERM")))))

;;}}}

;;{{{  Determine what site we're at.

(defvar at-site-work nil)
(defvar at-site-home nil)

(defun esler-command-output-first-line-as-string (command)

  "Capture the first line of a command's output as a string."

  (with-current-buffer (get-buffer-create "*active function*")
    (erase-buffer)
    (call-process shell-file-name
                  nil
                  t
                  nil
                  "-c"
                  command)
    (goto-char (point-min))
    (end-of-line)
    (buffer-substring (point-min) (point))))

(let ((host-name (system-name))
      (domain-name ""))
  ;;      (domain-name (esler-command-output-first-line-as-string "domainname")))
  (cond
   ;; Heuristically decide where we're running.
   ;;
   ((or
     (string-equal "TTLTMAC031" host-name)
     )
    (setq at-site-work t))

   ((or
     (string-match "DURIF" host-name)
     )
    (setq at-site-home t))))

(defvar esler-small-screen (< (display-pixel-width) 1600))
(add-hook 'before-make-frame-hook
          (function
           (lambda()
             (setq esler-small-screen (< (display-pixel-width) 1600)))))

;;}}}

;;{{{  Miscellaneous.

(setq max-specpdl-size 1000)
(setq max-lisp-eval-depth 1000)

(setq gc-cons-threshold 1000000)

;; Inhibit the Gnu warranty message on startup.
;;
(setq inhibit-startup-message t)

;; Use spaces for tabbing.
;;
(setq-default indent-tabs-mode nil)

;; Make Indented Text Mode the default major mode.
;;
(if (<= emacs-major-version 19)
    (setq major-mode 'indented-text-mode)
  (setq major-mode 'text-mode))

;; Make searches case insensitive by default (in all buffers
;; that do not override this).
;;
(setq-default case-fold-search t)

;; Control of auto save.
;; Do auto-saving of every file-visiting buffer.
;;
(setq auto-save-default t)
(setq auto-save-list-file-prefix "~/apps/emacs/auto-save-list/")

;; Let there be no distance limit to the parenthesis/bracket/brace
;; matching.
;;
(setq blink-matching-paren-distance nil)

;; Turn off Emacs' warnings about these functions.
;;
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;; Permit local variables in files.
;; Query local eval, however.
;;
(setq enable-local-variables t)
(setq enable-local-eval 'maybe)

;; Inserting text deletes the current selection.
;;
(delete-selection-mode t)

;;}}}

;;}}}

;;{{{  My identity

(setq user-full-name "Kevin Esler")

(setq user-mail-address "kevin.esler@gmail.com")

;;}}}
;;{{{  Register this emacs process

;;{{{  Functions to register which emacs processes I have running

(defun esler-register-emacs-process ()
  (interactive)
  (let* ((emacs-proc-dir "~/apps/emacs/.emacs_processes")
         (pid-string (int-to-string (emacs-pid)))
         (system-subdir (concat emacs-proc-dir "/" (system-name)))
         (pid-file (concat system-subdir "/" (ignt-to-string (emacs-pid)))))

    (esler-cleanup-emacs-process-registry)

    (if (file-directory-p emacs-proc-dir)
        (progn
          (if (not (file-directory-p system-subdir))
              (make-directory system-subdir))
          (if (file-exists-p pid-file)
              (delete-file pid-file))
          (with-current-buffer (get-buffer-create "*esler-register-emacs-process*")
            (erase-buffer)
            (insert (mapconcat
                     (function
                      (lambda (x)
                        x))
                     command-line-args
                     " "))
            (insert "\n")
            (write-file pid-file))))))

(defun esler-unregister-emacs-process ()
  (interactive)
  (condition-case err
      (let* ((emacs-proc-dir "~/apps/emacs/.emacs_processes")
             (pid-string (int-to-string (emacs-pid)))
             (system-subdir (concat emacs-proc-dir "/" (system-name)))
             (pid-file (concat system-subdir "/" (int-to-string (emacs-pid)))))
        (esler-cleanup-emacs-process-registry)
        (if (file-exists-p pid-file)
            (delete-file pid-file))
        (if (file-directory-p system-subdir)
            (if (equal 2 (length (directory-files system-subdir)))
                (delete-directory system-subdir))))
    (error nil)))

(defun esler-process-exists (pid-string)
  (zerop
   (call-process
    "kill"
    nil                                 ; INFILE
    nil                                 ; BUFFER
    nil                                 ; DISPLAY
    "-0"
    pid-string)))

(defun esler-cleanup-emacs-process-registry ()
  (interactive)
  (let* ((emacs-proc-dir "~/.emacs_processes")
         (system-subdir (concat emacs-proc-dir "/" (system-name))))
    (if (file-directory-p system-subdir)
        (let ((pid-list (directory-files system-subdir
                                         nil
                                         "[0-9]+")))
          (mapcar (function
                   (lambda (pid-string)
                     (if (not (esler-process-exists pid-string))
                         (delete-file (concat system-subdir
                                              "/"
                                              pid-string)))))
                  pid-list)))))

;;}}}

;; Ignore errors
;;
(condition-case err
    (esler-register-emacs-process)
  (error nil))

(add-hook 'kill-emacs-hook 'esler-unregister-emacs-process)

;;}}}

;;{{{  Load .emacs.custom

(setq custom-file "~/apps/emacs/.emacs.custom")
(if (file-exists-p custom-file)
    (progn
      (load-file custom-file)
      (message "Loaded %s" custom-file)))

;;}}}

;;{{{  Useful commands and functions.

;; Launch a file based on its file type as in Windows/Mac.
(defun esler-launch-file (file)
  (interactive "f")
  (cond
   (running-on-mac
     (progn
       (message "Opening %s..." file)
       (call-process "/usr/bin/open" nil 0 nil
                     (expand-file-name file))
       (message "Opening %s...done" file)))

   (running-on-w32
     (w32-shell-execute "open"
                        (esler-w32-canonicalize-path-seps
                         (expand-file-name file))))))

(defun directory-sub-dirs (dir)
  (let* ((entries (directory-files dir))
         (result nil)
         (cursor entries))
    (while (not (null cursor))
      (if (file-directory-p (concat dir "/" (car cursor)))
          (setq result (cons (car cursor) result)))
      (setq cursor (cdr cursor)))
    result))

(defun directory-sub-nondirs (dir)
  (let* ((entries (directory-files dir))
         (result nil)
         (cursor entries))
    (while (not (null cursor))
      (if (not (file-directory-p (concat dir "/" (car cursor))))
          (setq result (cons (car cursor) result)))
      (setq cursor (cdr cursor)))
    result))

(defun esler-insert-iso-date ()
  (interactive)
  (insert (format-time-string "%Y-%b-%d")))

(defun esler-another-line (num-lines)
  "Copies line, preserving cursor column, and increments any numbers found.
Copies a block of optional NUM-LINES lines.  If no optional argument is given,
then only one line is copied."
 (interactive "p")
  (if (not num-lines) (setq num-lines 0) (setq num-lines (1- num-lines)))
  (let* ((col (current-column))
         (bol (save-excursion (forward-line (- num-lines))
                              (beginning-of-line)
                              (point)))
         (eol (progn (end-of-line) (point)))
         (line (buffer-substring bol eol)))
    (goto-char bol)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-number (buffer-substring
                                    (match-beginning 0) (match-end 0)))))
        (replace-match (int-to-string (1+ num))))
      (setq eol (save-excursion (goto-char eol) (end-of-line) (point))))
    (goto-char bol)
    (insert line "\n")
    (move-to-column col)))

;;{{{ Change frame colour

(defun esler-set-frame-colour (colour)
  (interactive "sColour: ")
  (modify-frame-parameters (selected-frame)
                           (list (cons 'background-color colour))))

;;}}}
;;{{{  Autoload various useful functions.

;; time-sheet -- for maintaining a time-sheet.
;;
(autoload 'time-sheet "time-sheet"
  "Updates my daily time-sheet file."
  t)

;; c-format -- run indent(1) to reformat a C source code buffer.
;;
(autoload 'c-format "insitu"
  "Runs indent(1) to format the C source in the current buffer."
  t)

;; filter-current-buffer -- filter a buffer through a Unix command.
;;
(autoload 'filter-current-buffer "insitu"
  "Runs an arbitrary command over the current buffer, replacing the contents."
  t)

;; flame -- gratuitous abuse.
;;
(autoload 'flame "flame"
  "Flamer."
  t)

;; find-emacs-tag -- browsing Emacs code.
;;
(autoload 'find-emacs-tag "emacs-tags"
  "Package for finding tags in emacs sources themselves."
  t)

;; count-region -- info on region.
;;
(autoload 'count-region "what-line"
  "Info on region."
  t)
;; what-cursor-position-and-line -- detailed info about where the curson is in the buffer.
;;
(autoload 'what-cursor-position-and-line "what-line"
  "Info on cursor."
  t)

;; stringset-extract-region -- convert a region of text into an abstract set of strings.
;;
(autoload 'stringset-extract-region "stringset"
  "Convert a text region into a set of strings."
  t)

;; uniq -- remove adjacent duplicate lines.
;;
(autoload 'uniq "uniq"
  "Remove duplicate lines from a sorted region of text."
  t)

;; non-uniq -- remove all but adjacent duplicate lines.
(autoload 'non-uniq "uniq"
  "Preserve only duplicated lines from a sorted region of text."
  t)

;;}}}

;;{{{ oid-at-cursor-to-date

;; Eg:  54999cfe.799c11cd.b38c.00:01:80:31:7a:a7
;;
(defconst case-oid-regexp
  (let ((hex "[0-9a-f]"))
    (concat hex hex hex hex hex hex hex hex
            "\\."
            hex hex hex hex hex hex hex hex
            "\\."
            hex hex hex hex
            "\\."
            hex hex
            ":"
            hex hex
            ":"
            hex hex
            ":"
            hex hex
            ":"
            hex hex
            ":"
            hex hex)))

(defun esler-oid-at-point ()
  (interactive)
  (save-excursion
    (let ((oid nil))
      (while (and (null oid)
                  (not (eolp)))
        (if (looking-at case-oid-regexp)
            (setq oid (buffer-substring (match-beginning 0) (match-end 0)))
          (backward-char 1)))
      oid)))

(defun esler-oid-at-cursor-to-date ()
  (interactive)
  (let ((oid (esler-oid-at-point)))
    (if oid
        (message "%s"
                 (substring
                  (shell-command-to-string (concat "/usr/local/bin/oid_to_date"
                                                   " "
                                                   oid))
                  0 24)))))

;;}}}

;;{{{  now

(defun now ()
  (interactive)
  (message (current-time-string)))

;;}}}

;;{{{  Commands for click-on-pathname.

(defun find-file-at-point (&optional other-frame)
  "Find the file whose name the cursor is over.
Ignores trailing '*' or '@' as in 'ls -F' output."
  (interactive)
  (let ((file-name (extract-file-name-around-point)))
    (if (file-exists-p file-name)
        (if other-frame
            (find-file-other-frame file-name)
          (funcall 'switch-to-buffer (find-file-noselect file-name)))
      (error "Cannot find file \"%s\"" file-name))))

(defun extract-file-name-around-point ()
  (let ((skip-characters "!#-%*-9=?-{}~")
        (skip-at-end '(?* ?@ ?. ?, ?:)))
    (save-excursion
      (skip-chars-backward skip-characters)
      (let ((start (point)))
        (skip-chars-forward skip-characters)
        (let* ((filename (buffer-substring start (point)))
               (last-char (aref filename (- (length filename) 1))))

          ;; Recognise "..", but otherwise, remove characters
          ;; that should be skipped at the end.
          ;;
          (if (not (string-equal ".." filename))
              (if (memq last-char skip-at-end)
                  (setq filename (substring filename 0 -1))))

          ;; Map paths like //nodename to /net/nodename,
          ;; unless we're on Domain.
          ;;
          (if (not (eq system-type 'Domain/OS))
              (if (string-match "^//[^/]" filename)
                  (setq filename (concat "/net/" (substring filename 2)))))

          filename)))))

(defun extract-buffer-name-around-point ()
  (let ((skip-characters "!#-%*-9=?-{}~<>")
        (skip-at-end '(?@ ?. ?, ?: ?<)))
    (save-excursion
      (skip-chars-backward skip-characters)
      (let ((start (point)))
        (skip-chars-forward skip-characters)
        (let* ((buffername (buffer-substring start (point)))
               (last-char (aref buffername (- (length buffername) 1))))
          (if (memq last-char skip-at-end)
              (setq buffername (substring buffername 0 -1)))
          filename)))))

;;}}}

;;{{{  Command for saving a link to an objects.

(defconst esler-links-repository "~/links"
  "A place where I stash away links to various interesting files.")
(defun save-link-to-current-file (link-path)

  "Save a link to the file/directory in the current buffer in PATH."

  (interactive
   ;; This does completion:
   ;;
   (list (completing-read "Link path: "
                          'read-file-name-internal
                          default-directory
                          nil
                          (file-name-as-directory esler-links-repository)
                          'file-name-history)))

  (let ((target (buffer-file-name)))
    (if (null target)
        (if (eq major-mode 'dired-mode)
            (setq target (directory-file-name default-directory))
          (error "No path to link to")))
    (setq link-path (expand-file-name link-path))
    (if (file-directory-p link-path)
        (setq link-path (concat link-path
                                "/"
                                (file-name-nondirectory target))))
    (make-symbolic-link target link-path 1)))

;;}}}

;;{{{  Command to emulate the Apollo keyboard "Again" key.

(defun esler-emulate-apollo-again-key ()
  "Copy the remainder of the current line to the end of the buffer."
  (interactive)
  (set-mark-command nil)
  (end-of-line)
  (copy-region-as-kill (mark) (point))
  (goto-char (point-max))
  (yank))

;;}}}

;;{{{  Command for time-stamping a buffer.

;;     Possible improvement  --  format comment appropriately for
;;     C, Lisp,TeX, text.

(defun time-stamp-buffer ()

  "Write my name and the date and time at the top of the buffer
as a C or Lisp comment."

  (interactive)

  ;; Don't lose context

  (save-excursion

    ;; Delete the existing stamp, if it exists.

    (goto-char 0)
    (if (looking-at ".. Path: [^\n]*\n.. Version: [^\n]*\n.. Author: ")
	(kill-line 4))

    ;; Next put the fixed information.

    (insert
     (concat
      "Path:    "
                                        ;      (system-name) ":"
      (or buffer-file-name "")
      "\n"
      "Version: "
      (current-time-string)
      "\n"
      "Author:  "
      (user-full-name)
      "\n"))

    ;; Now format it as a comment in the appropriate style, depending
    ;; on the major mode of the buffer.

    (cond
     ((eq major-mode 'c-mode)
      (progn
	(goto-char 0)
	(insert "/* ")
	(forward-line 1)
	(insert " * ")
	(forward-line 1)
	(insert " * ")
	(forward-line 1)
	(insert " */\n")))

     ((or
       (eq major-mode 'lisp-mode)
       (eq major-mode 'emacs-lisp-mode)
       (eq major-mode 'lisp-interaction-mode))
      (progn
	(goto-char 0)
	(insert ";; ")
	(forward-line 1)
	(insert ";; ")
	(forward-line 1)
	(insert ";; ")
	(forward-line 1)
	(insert "\n")))

     ((or (eq major-mode 'text-mode) (eq major-mode 'indented-text-mode))
      (progn
	(goto-char 0)
	(insert ";; ")
	(forward-line 1)
	(insert ";; ")
	(forward-line 1)
	(insert ";; ")
	(forward-line 1)
	(insert "\n")))))

  (save-buffer))

;;}}}

;;{{{  Commands for trimming white-space.

(defun trim-white-space-trailing ()

  "Removes trailing white space from a line."

  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)))

(defun trim-white-space-leading ()

  "Removes trailing white space from a line."

  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)))

(defun trim-white-space ()

  "Removes leading and trailing white space from a line."

  (interactive)
  (trim-white-space-leading)
  (trim-white-space-trailing))

(defun trim-region-leading (begin end)

  "Removes leading white space from each line in a region."

  (interactive "r")
  (iterate-over-lines-in-region begin end 'trim-white-space-leading))

(defun trim-region-trailing (begin end)

  "Removes trailing white space from each line in a region."

  (interactive "r")
  (iterate-over-lines-in-region begin end 'trim-white-space-trailing))

(defun trim-region (begin end)

  "Removes leading and trailing white space from each line in a region."

  (interactive "r")
  (iterate-over-lines-in-region begin end 'trim-white-space))

;;}}}

;;{{{  Commands to maintain a stack of window configurations.

(defvar esler-window-config-stack nil)

(defun push-window-config ()
  (interactive)
  (setq esler-window-config-stack
        (cons (current-window-configuration) esler-window-config-stack)))

(defun pop-window-config ()
  (interactive)
  (if esler-window-config-stack
      (if (car esler-window-config-stack)
          (progn
            (set-window-configuration (car esler-window-config-stack))
            (setq esler-window-config-stack
                  (cdr esler-window-config-stack))))))

;;}}}

;;{{{  esler-sort-du-output -- sort the output of du(1) sensibly.

(defun esler-sort-du-output ()

  "Sort the output of du(1) sensibly."

  (interactive)

  ;; Right justify the size field, so we can sort.
  ;;
  (iterate-over-lines-in-region
   (point-min) (point-max)
   '(lambda ()
      (if (looking-at "^\\([0-9]+\\)\\([ \t]+\\)")
          (let ((number (buffer-substring (match-beginning 1) (match-end 1)))
                (space-char 32))
            (delete-region (match-beginning 1) (match-end 2))
            (insert
             (make-string (- 7 (length number)) space-char)
             number
             " ")))))

  ;; Now sort in descending order.
  ;;
  (sort-lines 'reverse (point-min) (point-max)))

;;}}}

;;{{{  uptime -- displays when this Emacs process started.

(setq esler-emacs-start-time (current-time-string))

(defun uptime ()
  (interactive)
  (message "This Emacs started at %s" esler-emacs-start-time))

;;}}}

;;{{{  edit-variable -- edit an elisp variable

;; edit-variable.el - friendlier that set-variable

;; Kevin Broadey, EDS-Scicon, 16 Oct 1992

;; Copyleft and all wrongs unreserved!

(provide 'edit-variable)

(defun edit-variable (var)
  "Edit the value of VARIABLE in the minibuffer.  Typing the help character
displays the documentation string for VARIABLE."
  (interactive "vEdit variable: ")
  (let ((var-value (if (boundp var)
		       (symbol-value var)))
	(minibuffer-help-form '(funcall myhelp))
	(myhelp
	 (function (lambda ()
		     (with-output-to-temp-buffer "*Help*"
		       (prin1 var)
		       (princ "\nDocumentation:\n")
		       (princ (substring (documentation-property
					  var 'variable-documentation)
					 1))
		       (if (boundp var)
			   (let ((print-length 20))
			     (princ "\n\nCurrent value: ")
			     (prin1 (symbol-value var))))
		       nil)))))
    (set var
	 (eval-minibuffer
	  (format "Set %s to value: " var)
	  (if (boundp var)
	      ;; quote value unless it is NIL, T, string or number
	      (format "%s%s"
		      (if (or (null var-value)
			      (eq var-value t)
			      (stringp var-value)
			      (numberp var-value))
                          ""
			"'")
		      (prin1-to-string var-value)
		      ))))))

;;}}}

;;{{{  see-chars -- displays character typed.

;;     From Randal L. Schwartz <merlyn@intelob.intel.com>

(defun see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
	(inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
	    quit-flag nil))		; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))

;;}}}

;;{{{  region-width, buffer-width -- measures the longest line in the region/buffer

(defun region-width (begin end)
  "Find the width of the longest line in the region."
  (interactive "r")
  (let ((esler-maximum-line-length 0))
    (iterate-over-lines-in-region begin end
                                  '(lambda ()
                                     (if (looking-at "^\\(.*\\)$")
                                         (let ((line-length (- (match-end 1) (match-beginning 1))))
                                           (if (> line-length esler-maximum-line-length)
                                               (setq esler-maximum-line-length line-length))))))
    (if (called-interactively-p 'interactive)
        (message (int-to-string esler-maximum-line-length)))
    esler-maximum-line-length))

(defun buffer-width (begin end)
  "Find the width of the longest line in the buffer."
  (interactive "r")
  (let ((width (region-width (point-min) (point-max))))
    (if (called-interactively-p 'interactive)
        (message (int-to-string width)))
    width))

(defun current-line-length ()
  (interactive)
  (- (line-end-position) (line-beginning-position)))

(defun next-long-line ()
  "Find the next line that is longer than permitted by coding standards."
  (interactive)
  (while (and (<= (current-line-length) 90)
              (not (eobp)))
    (forward-line 1))
  (message "%d" (current-line-length)))

;;}}}

;;{{{  A function to determine if I'm on my "home node".

(defun executing-on-my-home-node ()

  "Determine if I'm running on my own node.  I want to restrict certain
functions to be only possible on it, for example reading and writing mail."

  (interactive)
  (or t ;; Remove all restrictions for now
      (string-match "cutter.*" (system-name))
      (string-match "durif.*" (system-name))
      (string-match "^aquinas.*" (system-name))
      (string-match "^captan.*"   (system-name))
      at-site-home))

;;}}}

;;{{{  iterate-over-lines-in-region -- extremely useful for lots of things.

(defun iterate-over-lines-in-region (begin end func)

  "At the beginning of each line in the region defined by BEGIN and END,
execute the function THUNK."

  (save-excursion
    (goto-char begin)

    ;; Make a marker, in case the buffer changes size because
    ;; of the effects of func.
    (let ((end-marker (copy-marker end)))
      (while (< (point) (marker-position end-marker))
        (beginning-of-line)

        ;; Make a marker, in case func moves point in some funny way.
        ;;
        (let ((temp-marker (save-excursion
                             (forward-line 1)
                             (point-marker))))
          (funcall func)
          (goto-char (marker-position temp-marker))
          (set-marker temp-marker nil)))
      (set-marker end-marker nil))))

;;}}}

;;{{{  Generalised sort function.

;;    This is just the standard 'sort-subr with an extra parameter: lessp-predicate
;;    It is used by the gnus-priority package.

(require 'sort)

(defun esler-sort-subr (reverse nextrecfun endrecfun &optional startkeyfun endkeyfun lessp-predicate)
  "General text sorting routine to divide buffer into records and sort them.
Arguments are REVERSE NEXTRECFUN ENDRECFUN &optional STARTKEYFUN ENDKEYFUN LESSP-PREDICATE.

We consider this portion of the buffer to be divided into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of it)
is designated as the sort key.  The records are rearranged in the buffer
in order by their sort keys.  The records may or may not be contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN may moves from the start of the record to the start of the key.
It may return either return a non-nil value to be used as the key, or
else the key will be the substring between the values of point after
STARTKEYFUNC and ENDKEYFUN are called.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDRECFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN.

LESSP-PREDICATE is a function which compares two objects."
  (save-excursion
    (message "Finding sort keys...")
    (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
                                         startkeyfun endkeyfun))
	   (old (reverse sort-lists)))
      (if (null sort-lists)
	  ()
	(or reverse (setq sort-lists (nreverse sort-lists)))
	(message "Sorting records...")
	(setq sort-lists
              (sort sort-lists
                    (cond
                     ((not (null lessp-predicate))
                      (function
                       (lambda (a b)
                         (funcall lessp-predicate
                                  (buffer-substring (car (car a))
                                                    (cdr (car a)))
                                  (buffer-substring (car (car b))
                                                    (cdr (car b)))))))
                     ((numberp (car (car sort-lists)))
                      'car-less-than-car)
                     ((consp (car (car sort-lists)))
                      (function
                       (lambda (a b)
                         (> 0 (compare-buffer-substrings
                               nil (car (car a)) (cdr (car a))
                               nil (car (car b)) (cdr (car b)))))))
                     (t
                      (function
                       (lambda (a b)
                         (string< (car a) (car b))))))))
	(if reverse (setq sort-lists (nreverse sort-lists)))
	(message "Reordering buffer...")
	(sort-reorder-buffer sort-lists old)))
    (message "Reordering buffer... Done")))

;; This is to test the above:
;;
(defun esler-sort-lines (reverse beg end)
  "Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (esler-sort-subr reverse 'forward-line 'end-of-line nil nil '(lambda (a b)
                                                                   (not (string< a b))))))

;;}}}

;;{{{  mapreduce functions.

(defun mapreduce (l predicate)
  (let ((result '())
        (cursor l))
    (while (not (null cursor))
      (let ((mapped-elt (apply predicate (car cursor))))
        (if (not (null mapped-elt))
            (cons mapped-elt result)))
      (setq cursor (cdr cursor)))
    result))

(defun mapreduce1 (l predicate)
  (apply 'append
         (mapcar predicate l)))

;;}}}

;;{{{  frame-rename

(defun frame-rename (new-name)
  (interactive "sNew frame title: ")

  (modify-frame-parameters (selected-frame)
                           (list
                            (cons 'name new-name))))
;;}}}

;;{{{  extract-matches -- extract all matches for a regexp from a buffer.

(defun extract-matches (regexp)

  "Extract all matches for REGEXP in the current buffer, and write
them to the temporary buffer \"*Extract matches*\", separated by newlines."

  (interactive "sRegular expression: ")
  (save-excursion
    (with-output-to-temp-buffer "*Extract matches*"
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (princ (buffer-substring (match-beginning 0) (match-end 0)))
        (terpri)))))

;;}}}

;;}}}

;;{{{  Customise fonts

;; Configure a readable fixed-width font.
;;
(defvar esler-w32-preferred-font
  (if (or at-site-work
          esler-small-screen)
      ;;"-*-Bitstream Vera Sans Mono-normal-r-*-*-12-90-96-96-c-*-iso8859-1"
      "Consolas-12"
    ;; This looks better on my laptop
    ;;
    ;;"-*-Bitstream Vera Sans Mono-normal-r-*-*-16-120-96-96-c-*-iso8859-1"))
      "Consolas-10"))

(if running-on-w32
    (set-frame-font esler-w32-preferred-font))

;; The Bitstream fonts were downloaded from: http://c2.com/cgi/wiki?BitstreamVera
;;
;; Other fonts that looked okay:
;;
;; "-*-Andale Mono-normal-r-*-*-12-90-96-96-c-*-iso8859-1"
;; The above can be downloaded from: http://prdownloads.sourceforge.net/corefonts
;; It is named andale32.exe.
;; Also from  http://prdownloads.sf.net/corefonts/

;; "-*-Lucida Sans Typewriter-normal-r-*-*-12-120-120-120-c-*-iso8859-1"
;; "-outline-Lucida Sans Typewriter-normal-r-normal-normal-16-120-96-96-c-100-iso10646-1"))

;; Also look here: http://c2.com/cgi/wiki?GoodProgrammerTypeface

;;}}}

;;{{{  Customise frame appearance.

(if (not running-as-terminal-client)
    (set-scroll-bar-mode nil))

;; I like the region highlighted.
;;
(setq transient-mark-mode t)
(setq highlight-nonselected-windows nil)

;; Getting pop-up frames only when you really
;; want them takes some work.
;;
(setq pop-up-frames nil)
(setq pop-up-windows t)

(if running-on-w32
    (progn
      (add-to-list 'default-frame-alist
                   `(font . ,esler-w32-preferred-font))))

(if esler-small-screen
    (add-to-list 'default-frame-alist
                 '(height . 45))
  (add-to-list 'default-frame-alist
               '(height . 60)))
(add-to-list 'default-frame-alist
             '(height . 58))
(add-to-list 'default-frame-alist
             '(width . 79))
(add-to-list 'default-frame-alist
             '(top . 0))
(add-to-list 'default-frame-alist
             '(left . 85))
(add-to-list 'default-frame-alist
             '(background-color . "white smoke"))
(add-to-list 'default-frame-alist
             '(cursor-type . box))

;;}}}

;;{{{  Customise global key bindings.

;;{{{ ASCII keys.

(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key "\M-o" 'esler-another-line)

;; Can't seem to get used to the new and approved bindings for
;; folding and unfolding a whole file

(global-set-key "\C-c\C-w" 'folding-whole-buffer)
(global-set-key "\C-c\C-o" 'folding-open-buffer)

;; Always use VM to send mail.
;;
(global-set-key "\C-xm" 'vm-mail)

;; Try out this hippie-expand facility.
;;
(autoload 'hippie-expand "hippie-exp"
  "General purpose completion."
  t)
(global-set-key "\e/" 'hippie-expand)

;; Make BS (C-h) delete character.
;;
;;(global-set-key "\C-h" 'backward-delete-char-untabify)

;; Give me "goto-line" mapped to the key M-g
;;
(global-set-key "\eg" 'goto-line)

;; Background process function.
;;
(global-set-key "\e&" 'cmubackground)

(global-set-key "\C-x3" 'create-placemarker-window)
(defun create-placemarker-window ()
  (interactive)
  (let ((window-min-height 2))
    (split-window nil (- (window-height) 2))))

(global-set-key "\C-x7" 'split-window-horizontally)

(global-set-key "\C-xc" 'toggle-case-fold-search)
(defun toggle-case-fold-search ()
  (interactive)
  (if case-fold-search
      (progn
        (setq case-fold-search nil)
        (message "Search is case sensitive."))
    (setq case-fold-search t)
    (message "Search is case insensitive.")))

(global-set-key "\C-l" 'smart-recenter)
(defun current-line ()                  ; this function from the elisp manual
  "Return the vertical position of point in the selected window.
Top line is 0.  Counts each text line only once, even if it wraps."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun smart-recenter (&optional arg)
  "Move point in window and redisplay screen.
First time, leaves point in the middle of the window.
Second time, leaves point near top of window.
Third time, leaves point near bottom of window.
With just one \\[universal-argument] arg, redraw screen without moving point.
With numeric arg, redraw around that line."
  (interactive "P")
  (let ((line (current-line)))
    (cond ((consp arg)
           (recenter)
           (recenter line)) ;; (redraw-display) bombs in Epoch 3.1.
          (arg
           (recenter (prefix-numeric-value arg)))
          ((eq last-command 'recenter-first)
           (setq this-command 'recenter-second)
           (recenter 1))
          ((eq last-command 'recenter-second)
           (setq this-command nil)
           (recenter -2))
          (t
           (setq this-command 'recenter-first)
           (recenter nil)))))

(if esler-modern-emacs
    (global-set-key "\e=" 'count-words-region)
  (global-set-key "\e=" 'count-region))

(global-set-key "\C-x=" 'what-cursor-position-and-line)

;; M-% is more generally useful at query-replace-regexp.
;;
(global-set-key "\e%" 'query-replace-regexp)

(global-set-key "\C-^" 'enlarge-window)

;; I'd like shell-command-on-region to use the most recently used command
;; as the default.
;;
(global-set-key "\e|" 'esler-shell-command-on-region)
(defvar esler-last-command-on-region nil)
(defun esler-shell-command-on-region ()

  "Execute string COMMAND in inferior shell with region as input.
It defaults to the most recent such command."

  (interactive)
  (let ((command (read-from-minibuffer "Shell command on region: "
                                       (if esler-last-command-on-region
                                           esler-last-command-on-region
                                         nil)
                                       nil
                                       nil)))
    (shell-command-on-region (region-beginning) (region-end) command)
    (setq esler-last-command-on-region command)))

(global-set-key "\C-x\C-b" 'buffer-menu)

;; I keep accidentally hitting C-z and my window iconises.
;;
(global-unset-key "\C-z")

;;}}}

;;{{{ Function keys.

;; Bind some function keys to frequently used commands.
;;
(global-set-key [f1] 'call-last-kbd-macro)
(global-set-key [f2] 'repeat-complex-command)
(global-set-key [f3] 'make-frame-command)
(global-set-key [f4] 'speedbar)
(global-set-key [f5] 'find-file-at-point)

(progn
  (global-set-key [S-up] 'scroll-down-one-line)
  (global-set-key [S-down] 'scroll-up-one-line))

(defun scroll-up-one-line ()
  "Scroll text of window up by one line."
  (interactive nil)
  (scroll-up 1))

(defun scroll-down-one-line ()
  "Scroll text of window down by one line."
  (interactive nil)
  (scroll-down 1))

(global-set-key [f9] 'esler-emulate-apollo-again-key)

;; I hate accidentally toggling into overwrite mode.
;;
(global-unset-key [insert])

;;}}}

;;{{{ Mouse events.

(defun mode-line-resize-dynamically ()
  "Resize a window by dragging the mode-line.
This must be bound to a mouse-down event in the mode-line."
  (interactive "@")
  (let* ((mouse (mouse-position))
         (start-frame (car mouse))
         (prev-y (cdr (cdr mouse)))
         (next (next-window (selected-window) t))
         (cur-win (selected-window))
         (win-to-enlarge (if (window-minibuffer-p next)
                             (minibuffer-window)
                           cur-win))
         (min-height (if (window-minibuffer-p next)
                         0
                       window-min-height)))

    (track-mouse
      (while (and (eq (car-safe (read-event)) 'mouse-movement)
                  (eq next (next-window (selected-window) t)))
        (let* ((mouse (mouse-position))
               (frame (car mouse))
               (new-y (if (cdr (cdr mouse))
                          (cdr (cdr mouse))
                        prev-y))
               (delta (if (window-minibuffer-p win-to-enlarge)
                          (- prev-y new-y)
                        (- new-y prev-y))))
          (cond ((and (eq frame start-frame)
                      (> (+ delta (window-height win-to-enlarge))
                         min-height))
                 (select-window win-to-enlarge)
                 (enlarge-window delta)
                 (setq prev-y new-y)
                 (select-window cur-win))))))
    (select-window cur-win)))

(defun scroll-bar-resize-dynamically ()
  "Resize a window by dragging the scroll-bar
This must be bound to a mouse-down event in the vertical-scroll-bar
and/or the vertical-line."
  (interactive "@")
  (track-mouse
    (let* ((mouse (mouse-position))
           (start-frame (car mouse))
           (prev-x (car (cdr mouse)))
           event
           (next (next-window)))
      (while (and (or (eq (car-safe (setq event (read-event))) 'mouse-movement)
                      (eq (car-safe event) 'scroll-bar-movement))
                  (eq next (next-window)))
        (let* ((mouse (mouse-position))
               (frame (car mouse))
               (new-x (car (cdr mouse)))
               (delta (if prev-x (- new-x prev-x) 0)))
          (if (null prev-x) (setq prev-x new-x))
          (cond ((and (eq frame start-frame)
                      (> (+ delta (window-width (selected-window)))
                         window-min-width))
                 (enlarge-window-horizontally delta)
                 (setq prev-x new-x))))))))

;; Drag mouse-1 always copies to the kill-ring, and the X11 selection buffer.
;;
(defadvice mouse-drag-region (after do-kill-ring-save activate)
  (if mark-active
      (kill-new (buffer-substring (region-beginning) (region-end)))))

;;}}}

;;}}}
;;{{{  Customise the mode line.

;; Provide the time, and Mail notifications on the mode line.
;;
(if running-as-terminal-client
    (if (file-exists-p (concat exec-directory "wakeup"))
        (progn
          (setq display-time-mail-file (concat "/usr/mail/" (user-real-login-name)))
          (setq display-time-day-and-date t)
          (defadvice vm-get-new-mail (after clear-mode-line activate)
            "Clear out the mail indicator in the mode line."
            (display-time-update))
          (let ((process-connection-type nil))
            (display-time)))))

;; Provide machine identification on mode-line.
;;
(set-default 'mode-line-buffer-identification
             (let* ((system-name (system-name))
                    (index (string-match "\\." system-name)))
               (if (not (numberp index))
                   (setq index (length system-name)))
               (list (concat "Emacs" "@"
                             (substring system-name 0 index)
                             ": %17b"))))

;; Get line number display in the mode line,
;; for files of reasonable size.
;;
(setq line-number-display-limit 2000000)
(line-number-mode 1)

;;}}}
;;{{{  Customise the menu bar.

;;{{{ Add a "KAE" menu bar entry

(require 'easymenu)
(setq kae-menu
      (easy-menu-define shortcuts-menu
        (list global-map)
        "Shortcuts menu"
        (list "KAE"
              ["Emacs' ELisp" esler-project-dired-emacs-elisp]
              "---------------------------------"
              ["Find files" find-lisp-find-dired]
              "---------------------------------"
              ["Toggle read-only" toggle-read-only]
              "---------------------------------"
              ["Show trailing whitespace"
               (setq show-trailing-whitespace t)
               :included (null show-trailing-whitespace)]
              ["Hide trailing whitespace"
               (setq show-trailing-whitespace nil)
               :included show-trailing-whitespace]
              ["Delete trailing whitespace in region" trim-region-trailing
               :active mark-active]
              ["Delete trailing whitespace in buffer" delete-trailing-whitespace]
              "---------------------------------"
              ["Insert ISO date" esler-insert-iso-date t]
              "---------------------------------"
              ["Windows Explorer"
               (start-process-shell-command "Windows Explorer"
                                            nil
                                            "explorer"
                                            ".,/e")
               :included running-on-w32
               ]
              ["Windows Shell"
               (esler-launch-file (executable-find "cmd.exe"))
               :included running-on-w32
               ]
              )))
(add-hook 'menu-bar-final-items 'KAE)
(defun esler-project-dired-homedir ()
  (interactive)
  (dired "~/"))

(defun esler-project-dired-my-elisp ()
  (interactive)
  (dired esler-elisp-directory))

(defun esler-project-dired-emacs-elisp ()
  (interactive)
  ;;(dired (concat exec-directory "/../lisp")))
  (dired (concat exec-directory "/../../Resources/lisp")))

;;}}}

;;{{{ Augment the "File" menu

;; Implement a "kill-buffer-and-frame" facility.
;;
(define-key global-map
  [menu-bar files kill-buffer-and-frame]
  '("Kill buffer & frame" . esler-kill-buffer-and-frame))

(defun esler-kill-buffer-and-frame ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame))

;; Implement a "peel-off" frame facility,
;; and put it at the top of the menu bar.
;;
(define-key global-map
  [menu-bar files peel-frame]
  '("Peel-off-frame" . esler-peel-off-selected-window))

(defun esler-peel-off-selected-window ()
  (interactive)
  (esler-peel-off-window (selected-window)))

(defun esler-peel-off-window (window)

  (let* ((buf (window-buffer window))
         (frame (make-frame)))
    (select-frame frame)
    (switch-to-buffer buf)

    (select-window window)
    (if (one-window-p t)
        (switch-to-buffer (other-buffer buf))
      (delete-window window))
    (select-frame frame)))

;;}}}

;;}}}
;;{{{  Customise the minibuffer

;; For the minibuffer, disallow auto-show
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq truncate-lines nil)))

;;}}}
;;{{{  Backups

(setq make-backup-files t)

;; Store backups in ".,".
;;
(setq backup-directory-alist '(("." . ".,")))

;;}}}

;;{{{  Establish lisp load path.

;; Set up search path for my own library of Lisp code,
;; to be searched after the default ones.

(defun esler-directory-subdirectories (dir)
  "Compute a list of the subdirectories of DIR, excluding . and .."
  (let* ((entries (directory-files dir))
         (dir-as-dir (file-name-as-directory dir))
         (result nil))
    (mapc (function
           (lambda (entry)
             (let ((full-path-of-entry (concat dir-as-dir entry)))
               (if (and (not (string= "." entry))
                        (not (string= ".." entry))
                        (file-directory-p full-path-of-entry))
                   (setq result (cons full-path-of-entry result))))))
          entries)
    result))

(defun esler-directory-contains-elisp (dir)
  "Returns true of DIR contains any ELisp files"
  (or (directory-files dir nil "\\.el$" t)
      (directory-files dir nil "\\.elc$" t)))

(defun esler-find-lisp-in-package (dir)
  "Given DIR, return DIR/lisp if it exists and contains Elisp files
otherwise return DIR"
  (let ((lisp-subdir (concat (file-name-as-directory dir) "lisp")))
    (if (and (file-directory-p lisp-subdir)
             (esler-directory-contains-elisp lisp-subdir))
        lisp-subdir
      dir)))

(defun esler-set-loadpath ()
  (setq load-path (append

                   ;; My primary library of Elisp.
                   ;;
                   (list (expand-file-name esler-elisp-directory))

                   load-path)))

(esler-set-loadpath)

;;}}}

;;{{{  Colour

(defun face-describe (face)
  ;; It would be nice if this had completion
  ;;
  (interactive "SDescribe face: ")
  (let ((name (face-name face))
        (font (face-font face))
        (foreground (face-foreground face))
        (background (face-background face))
        (underline (face-underline-p face)))
    (message (concat
              (format "Name=%s" name)
              (if font (format ", Font=%s" font))
              (if foreground (format ", Foreground=%s" foreground))
              (if background (format ", Background=%s" background))
              (if underline  ", underline")))))

;;{{{ facemenu.el

(if esler-modern-emacs
    (require 'facemenu))

;;}}}

;;}}}

;;{{{  Colour themes

(if (not running-as-terminal-client)
    (progn
      (require 'solarized-theme)
      (load-theme 'solarized-light)
      (enable-theme 'solarized-light)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (enable-theme 'solarized-light)))))


;;}}}

;;{{{  Standardise keybindings for "readonly" modes.

(defun esler-standard-readonly-buffer-key-bindings-in-keymap (keymap)

  "This function is intended to be called from the mode hook
for any mode in which the buffer is kept readonly.  In these buffers
characters never self-insert, so we can set up single key abbreviations
for common operations.
      SPC scroll-up
      n next-line
      p previous-line
      q abandon
      0 delete-window
      1 delete-other-windows
      2 split-window-vertically
      5 split-window-horizontally
      > end-of-buffer
      < beginning-of-buffer
      . beginning-of-buffer
      = what-line

      r rename-buffer
      R rename-buffer-and-toggle-readonly
      g goto-line"
  (define-key keymap " " 'scroll-up)
  (define-key keymap "n" 'next-line)
  (define-key keymap "p" 'previous-line)
  (define-key keymap "0" 'delete-window)
  (define-key keymap "1" 'delete-other-windows)
  (define-key keymap "2" 'split-window-vertically)
  (define-key keymap "7" 'split-window-horizontally)
  (define-key keymap ">" 'end-of-buffer)
  (define-key keymap "<" 'beginning-of-buffer)
  (define-key keymap "." 'beginning-of-buffer)
  (define-key keymap "=" 'what-line)
  (define-key keymap "g" 'goto-line))

(defun esler-standard-readonly-buffer-key-bindings ()
  (interactive)
  (esler-standard-readonly-buffer-key-bindings-in-keymap (current-local-map)))

;;}}}

;;{{{  Configure MODES and packages.

(require 'use-package)

;;{{{ Tramp
(require 'tramp)
(setq tramp-debug-buffer t)
(setq tramp-verbose 9)
;;(setq tramp-default-method "ssh")
;;(setq tramp-password-prompt-regexp ".*[Pp]assword: *$")
;;(setq tramp-shell-prompt-pattern "^[^;$#>]*[;$#>] *")
(setq password-cache-expiry nil)
;;}}}
;;{{{ Pivotal
(setq pivotal-api-token "1f9e5ac7febbd04161af414b58024f2c")
;;}}}
;;{{{ Buffer-Menu
(eval-after-load
    "buff-menu"
  '(progn
     (define-key Buffer-menu-mode-map " "
       (function (lambda ()
                   (interactive)
                   (let ((buf (current-buffer)))
                     (Buffer-menu-this-window)
                     (bury-buffer buf)))))))
;;}}}
;;{{{ Magit

(if esler-modern-emacs
    (require 'magit))

;;}}}
;;{{{ Confluence wiki editing mode

(if esler-modern-emacs
    (progn
      (require 'confluence)
      (setq confluence-url "http://timetrade.onconfluence.com/rpc/xmlrpc")
      
      (global-set-key "\C-xwf" 'confluence-get-page)
      
      (add-hook 'confluence-mode-hook
                (local-set-key "\C-xw" confluence-prefix-map)
                (local-set-key "\M-j" 'confluence-newline-and-indent)
                (local-set-key "\M-;" 'confluence-list-indent-dwim))))

;;}}}

;;{{{ Ascii doc mode

(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))

;;}}}

;;{{{ Auto-complete

(if esler-modern-emacs
    (progn
      (require 'auto-complete-config)
      (add-to-list 'ac-dictionary-directories "~/apps/emacs/ac-dict")
      (ac-config-default)))

;;}}}
;;{{{ NXHTML

(defun esler-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installedto do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;;}}}
;;{{{ CCrypt

(require 'ps-ccrypt "ps-ccrypt.el")

;;}}}

;;{{{ Org Mode

(autoload 'org-mode "org" "Org mode" t)
(autoload 'org-diary "org" "Diary entries from Org mode")
(autoload 'org-agenda "org" "Multi-file agenda from Org mode" t)
(autoload 'org-store-link "org" "Store a link to the current location" t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;}}}

;;{{{  Command-line interactive packages

;;{{{  Comint Mode(s).

;; Comint mode is not really a mode, but a layer underlying a collection
;; of interactive process-based modes, including Shell Mode, Telnet Mode...
;; It implements behaviour common to all of them.
;;
(defun esler-comint-mode-bindings ()

  ;; For typing passwords while in a shell buffer.
  ;;
  (define-key comint-mode-map "\C-ch" 'send-invisible)

  ;; Map M-g to a function which copies everything between point
  ;; and the end of the line, to the end of the buffer.
  ;;
  (define-key comint-mode-map "\eg" 'esler-emulate-apollo-again-key)

  ;; Some telnets map ENTER to LF instead of CR.
  ;;
  (define-key comint-mode-map "\n" 'comint-send-input)
  (define-key comint-mode-map "\r" 'comint-send-input)

  ;; I like to type C-a rather than C-cC-a get back to just after the prompt
  ;;
  (define-key comint-mode-map "\C-a" 'comint-bol))

(eval-after-load "comint" '(esler-comint-mode-bindings))

(add-hook 'comint-mode-hook
          '(lambda ()

             ;; Hang the expense.
             ;;
             (setq comint-input-ring-size 50)

             ;; Don't duplicate the last entry on the history ring.
             ;;
             (setq comint-input-ignoredups t)

             ;; Read pre-existing history.
             ;;
             (comint-read-input-ring)

             ;; Expand command history references on completion.
             ;;
             (setq comint-input-autoexpand 'input)

             ;; Scroll the selected window on input.
             ;;
             (setq comint-scroll-to-bottom-on-input 'this)

             ;; Don't scroll the selected window on output.
             ;;
             (setq comint-scroll-to-bottom-on-output 'others)

             ;; List possibilities on partial completion.
             ;;
             (setq comint-completion-autolist t)

             ;; ??
             ;;
             (setq comint-completion-recexact t)

             ;;(esler-comint-mode-bindings)
             ))

(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;; Make it recognise the prompt that rlogin uses:
;; "Password for user esler:"
;;
(setq comint-password-prompt-regexp
      (concat
       "\\("
       "^[Pp]assword"
       "\\|pass phrase"
       "\\|^Enter mailserver password"
       "\\|" (user-login-name) "'s [Pp]assword"
       "\\|" "[Rr]oot " "[Pp]assword"
       "\\)"
       ":\\s *\\'"))

;;}}}
;;{{{  Shell Mode.

(add-hook 'shell-mode-hook 'esler-shell-mode-hook)
(defun esler-shell-mode-hook ()

  ;; Set up bindings common to all Comint-based modes.
  ;;
  (esler-comint-mode-bindings)

  ;; Make absolutely sure that auto-fill is off.
  ;;
  (auto-fill-mode -1)

  ;; Set my own mode line format.
  ;;
  (setq mode-line-format
        (list ""
              'mode-line-modified
              'mode-line-buffer-identification
              "   "
              'global-mode-string
              "   %[("
              'mode-name
              'minor-mode-alist
              "%n"
              'mode-line-process
              ;;")%]----"
              ")%] "
              ;; (-3 . "%p")
              'default-directory
              "-%-")))

;;}}}
;;{{{  Telnet Mode.

(autoload 'telnet "telnet" "Run telnet" t)
(add-hook 'telnet-mode-hook
          '(lambda ()

             ;; Set up bindings common to all Comint-based modes.
             ;;
             (esler-comint-mode-bindings)))

;;}}}
;;{{{  Rlogin Mode.

;; If this is set to true, the user gets a disconcerting prompt
;; for a password, from the ange FTP code, even though no password was
;; needed to rlogin.
;;
(setq rlogin-initially-track-cwd nil)

;;}}}
;;{{{  Background Mode.

;; A fully-fledged background mode would be nice.
;; Desirable features:
;;   o persistent transcript buffers.
;;   o read-only transcript buffers.
;;   o comint-like process control.
;;   o single keystroke "redo this job" command.
;;   o start and stop times recoreded.
;;
;; In the meantime, use CMU background hack.
;;
(autoload 'cmubackground "cmubackground"
  "Run background jobs."
  t)

;;}}}
;;{{{  IELM

(defun esler-ielm-mode-bindings ()
  (message "Running esler-ielm-mode-bindings")
  (define-key ielm-map " " 'self-insert-command))
;; For some reason, this doesn't work: anymore:
;;
(eval-after-load "ielm" '(esler-ielm-mode-bindings))
(add-hook 'inferior-emacs-lisp-mode-hook 'esler-ielm-mode-bindings)

;;}}}

;;}}}

;;{{{ Programming language packages

;;{{{  Haskell related

(if esler-modern-emacs
    (progn
      ;; See https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
      ;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
      ;;(add-hook 'haskell-mode-hook 'turn-on-hi2)
      (eval-after-load 'haskell-mode
        '(define-key haskell-mode-map [f8] 'haskell-navigate-imports)
        )

      (setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
      (add-to-list 'exec-path "~/.cabal/bin")
      (setenv "PATH" (concat "~/Library/Haskell/bin:" (getenv "PATH")))
      (add-to-list 'exec-path "~/Library/Haskell/bin")
      (custom-set-variables '(haskell-tags-on-save t))

      (custom-set-variables
       '(haskell-process-suggest-remove-import-lines t)
       '(haskell-process-auto-import-loaded-modules t)
       '(haskell-process-log t))
      (eval-after-load 'haskell-mode
        '(progn
           (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
           (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
           (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
           (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
           (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
           (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
           (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
      (eval-after-load 'haskell-cabal
        '(progn
           (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
           (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
           (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
           (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

      (custom-set-variables '(haskell-process-type 'cabal-repl))


      (eval-after-load 'haskell-mode
        '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
      (eval-after-load 'haskell-cabal
        '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

      (autoload 'ghc-init "ghc" nil t)
      (autoload 'ghc-debug "ghc" nil t)
      (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

      (require 'company)
      (add-hook 'after-init-hook 'global-company-mode)

      (add-to-list 'company-backends 'company-ghc)
      (custom-set-variables '(company-ghc-show-info t))

      (require 'rainbow-delimiters)
      (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)

      ))

;;}}}
;;{{{  Hideshow Minor Mode

(load-library "hideshow")
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;}}}
;;{{{  Eldoc Minor Mode

(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;}}}
;;{{{  CC Mode.

;; Use CWarn to warn about possibly dodgey C/C++ constructs.
;;
(if (fboundp 'global-cwarn-mode)
    (global-cwarn-mode 1))

;;{{{ Indentation styles

;;{{{ The Esler style

(defconst esler-c-style-description
  '(
    "cc-mode" ;; Inherit from this
    (c-electric-pound-behavior      . (alignleft))
    (c-basic-offset                 . 4)
    (c-tab-always-indent            . t)
    (c-comment-only-line-offset     . 0)
    (c-echo-syntactic-information-p . t)
    (c-hanging-comment-ender-p      . nil)

    (c-hanging-braces-alist . (
                               (substatement-open before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (brace-list-close before after)
                               (class-open before after)
                               (class-close before after)
                               (defun-open before after)
                               (defun-close before after)
                               (inline-open before after)
                               (inline-close before after)
                               ))
    (c-offsets-alist . (
                        ;; Line up continued strings if appropriate,
                        ;; otherwise just indent as normal.
                        ;;
                        (statement-cont . (c-lineup-string-cont
                                           +))
                        (substatement-open . 0)
                        (arglist-close . 0)
                        (inline-open . 0)
                        (label . -)))

    ))

;;}}}

(require 'cc-mode)
(c-add-style "esler" esler-c-style-description)

(defun esler-file-seems-to-be-MFC ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[aA][fF][xX]" nil t)))

;; Function to decide what style to use.
;;
(defun esler-c-choose-style ()
  (let ((result "esler"))
    result))

;;}}}

;; Customisations for C and C++ but not Java.
;;
(add-hook 'c-mode-hook 'esler-c-and-c++-mode-hook)
(add-hook 'c-mode-hook 'esler-c-mode-hook)
(add-hook 'c++-mode-hook 'esler-c-and-c++-mode-hook)

;; Try to turn on C++ mode for .h files in Windows when appropriate.
;;
(defun esler-c-mode-hook ()
  (if (and at-site-work running-on-w32)
      (if (and (buffer-file-name)
               (string-match "\\.h$" (buffer-file-name))
               (esler-file-seems-to-be-MFC))
          (c++-mode))))

(defun esler-c-and-c++-mode-hook ()

  ;; Display trailing whitepace in red.
  ;;
  (setq show-trailing-whitespace t)

  ;; Choose an indentation style.
  ;;
  (c-set-style (esler-c-choose-style)))

;; Customizations common to C, C++, Java.
;;
(defun esler-c-mode-bindings ()


  ;; Turn off electric semi-colon.
  ;;
  (define-key c-mode-map ";" nil)

  ;; Keybindings for both C and C++.  We can put these in c-mode-map
  ;; because c++-mode-map inherits it.
  ;;
  (define-key c-mode-map "\C-m" 'newline-and-indent-if-not-bol)
  (define-key c-mode-map "\n"   'newline-and-indent-if-not-bol)
  (define-key c-mode-map "\r"   'newline-and-indent-if-not-bol))
(eval-after-load "cc-mode" '(esler-c-mode-bindings))

(add-hook 'c-mode-common-hook 'esler-c-mode-common-hook)
(defun esler-c-mode-common-hook ()

  ;; Disable filladapt for C modes.
  ;;
  ;;(turn-off-filladapt-mode)

  ;; Code written in MSDev often has hard tabs, and
  ;; requires a tab-width of 4 to view sensibly.
  ;;
  (setq tab-width 8)
  (if running-on-w32
      (if (eq 'c++mode major-mode)
          (setq tab-width 4)))

  ;; Use spaces instead of hard tabs.
  ;;
  (setq indent-tabs-mode nil)

  ;; We like auto-newline and hungry-delete.
  ;;
  (c-toggle-auto-hungry-state 1)

  (esler-c-mode-bindings))

;; NYI: use c-font-lock-extra-types, c++-font-lock-extra-types
;;
;; Also set up colouring of Atria types.
;;
(if running-on-w32
    (progn
      ;; Add colouring for MFC's special syntax additions
      ;;
      (font-lock-add-keywords 'c++-mode
                              (list
                               "\\<BEGIN_MESSAGE_MAP\\>"
                               "\\<END_MESSAGE_MAP\\>"
                               "\\<DECLARE_MESSAGE_MAP\\>"
                               "\\<ON_[A-Z_]+\\>"))

      (font-lock-add-keywords 'c-mode
                              (list
                               "\\<DECLARE_MESSAGE_MAP\\>"
                               "\\<afx_msg\\>"))))

;;}}}
;;{{{  Java Support

;; This should come after the customisations for cc-mode.

;; Define indentation style.
;;
(defconst atria-java-style-description
  '(
    "java" ;; Super-style
    (c-basic-offset . 4)
    (c-hanging-braces-alist . (
                               (block-open. (after))
                               (brace-list-open . (after))
                               (class-open . (after))
                               (defun-open . (after))
                               (substatement-open . (after))
                               ))
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (arglist-cont . 0)
                        (arglist-close . 0)
                        (case-label . 2)
                        (func-decl-cont . +)
                        (inline-open . 0)
                        (label . 2)
                        (statement-case-intro . 2)
                        (substatement-open . 0)
                        (topmost-intro-cont . 0)
                        ))))
(c-add-style "atria-java" atria-java-style-description)

(defun esler-java-mode-hook ()

  (setq show-trailing-whitespace t)

  ;; Choose an indentation style.
  ;;
  (c-set-style "atria-java")
  (setq fill-column 79))

(add-hook 'java-mode-hook 'esler-java-mode-hook)

;; Refine the automatic expansion of control-flow constructs.
;;
(defun kae-jde-cflow-expand-inappropriate ()
  "Function to decide if JDE's control-flow keyword expansion
should not occur"
  ;; JDE won't expand cflow keywords in comments or quotes.
  ;; I also don't want them expanded unless they are at the
  ;; end of the line (except for whitespace).
  ;;
  (or (not (looking-at "[ \\t]*$"))
      (jde-parse-comment-or-quoted-p)))

;;}}}
;;{{{  C outline Minor Mode.

(autoload 'c-outline "c-outline" nil t)

;;}}}
;;{{{  Emacs Lisp Mode.

;; Turn on auto-fill for Lisp Mode.
;;
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (auto-fill-mode 1)
             (set-fill-column 2000)))

(define-key emacs-lisp-mode-map "\n" 'newline-and-indent-if-not-bol)
(define-key emacs-lisp-mode-map "\r" 'newline-and-indent-if-not-bol)

;;}}}
;;{{{  Lisp Mode.

;; Turn on auto-fill for Lisp Mode.
;;
(add-hook 'lisp-mode-hook
          '(lambda () (auto-fill-mode 1)))

;;}}}
;;{{{  Scheme and related Modes.

(setq auto-mode-alist
      (append '(("\\.stk$" . scheme-mode)
                ("\\.stklos$" . scheme-mode)) auto-mode-alist))

;; Use cmuscheme mode, since we won't be using MIT C Scheme.
;; The latter works best with the xscheme.el that comes with it.

(autoload 'run-scheme "cmuscheme"
  "Run an inferior Scheme process."
  t)

;; This is necessary because, unfortunately, scheme.el defines run-scheme to autoload
;; from xscheme.el.

(defun esler-scheme-mode-bindings ()
  (define-key scheme-mode-map "\n" 'newline-and-indent-if-not-bol)
  (define-key scheme-mode-map "\r" 'newline-and-indent-if-not-bol))
(eval-after-load "scheme" '(esler-scheme-mode-bindings))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (autoload 'run-scheme "cmuscheme"
               "Run an inferior Scheme"
               t)))

(setq scheme-program-name "scheme48")

;;{{{  Scsh Mode.

;; Use cmuscheme mode, since we won't be using MIT C Scheme.
;; The latter works best with the xscheme.el that comes with it.

(setq auto-mode-alist
      (cons '("\\.scsh$" . scheme-mode) auto-mode-alist))

(autoload 'run-scheme "cmuscheme"
  "Run an inferior Scheme process."
  t)

;; This is necessary because, unfortunately, scheme.el defines run-scheme to autoload
;; from xscheme.el.

(add-hook 'scheme-mode-hook
          '(lambda () (autoload 'run-scheme "cmuscheme"
                        "Run an inferior Scheme"
                        t)))

(setq scheme-program-name (cond
                           (running-on-w32 "c:/cygwin/usr/local/bin/scsh")
                           (t "scsh")))
(defun run-scsh ()
  (interactive)
  (let ((scheme-program-name (cond
                              (running-on-w32 "c:/cygwin/usr/local/bin/scsh")
                              (t "scsh"))))
    (run-scheme scheme-program-name)))


;; Indent these special forms like let:
;;
(put 'receive 'scheme-indent-function 1)
(put 'values 'scheme-indent-function 1)

;;}}}

;;}}}
;;{{{  Lisp Interaction Mode.

;; Map M-g to a function which copies everything between point
;; and the end of the line, to the end of the buffer.
(eval-after-load "lisp-mode"
  '(lambda () (define-key lisp-interaction-mode-map "\eg" 'esler-emulate-apollo-again-key)))

;;}}}
;;{{{  Makefile Mode.

;; Use Makefile Mode if it's available,
;; otherwise just Text Mode.
;; In any case, use hard tabs.
;;
(setq auto-mode-alist (cons '("\\.[Mm][Kk]$" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("[Mm]akefile$" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("[Mm]akefile\\..*$" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("[Mm]akefile_.*$" . makefile-mode) auto-mode-alist))

(if running-on-w32
    (progn
      (setq auto-mode-alist (cons '("\\.[Mm][Aa][Kk]$" . makefile-mode) auto-mode-alist))

      ;; VC's Mak-files have conditional directives beginning with "!"
      ;; I'd like to colour them, but the following doesn't work.
      ;;
      (font-lock-add-keywords 'makefile-mode
                              '("^!\\(IF\\|MESSAGE\\|ENDIF\\|ELSEIF\\|ELSE\\|ERROR\\)"))))

;;}}}
;;{{{  Shell script Mode

(setq sh-shell-file "/bin/sh")

;;; 1999-07-12 Noah Friedman <friedman@splode.com>
;;;
(defun make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))

(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

;;}}}

;;}}}

;;{{{  Greedy Delete

(autoload 'gd-add-to-mode "greedy-delete" "" t)
(add-hook 'c-mode-hook 'gd-add-to-mode)
(add-hook 'c++-mode-hook 'gd-add-to-mode)
(add-hook 'emacs-lisp-mode-hook 'gd-add-to-mode)
(add-hook 'comint-mode-hook 'gd-add-to-mode)
(add-hook 'java-mode-hook 'gd-add-to-mode)
(add-hook 'scheme-mode-hook 'gd-add-to-mode)
(add-hook 'lisp-mode-hook 'gd-add-to-mode)
(add-hook 'sml-mode-hook 'gd-add-to-mode)
(add-hook 'text-mode-hook 'gd-add-to-mode)
(add-hook 'makefile-mode-hook 'gd-add-to-mode)
(add-hook 'sh-mode-hook 'gd-add-to-mode)

(setq gd-how-much 'line)

;;}}}

;;{{{  Text Mode.

;;
;;   o  Don't use tabs for indentation.
;;   o  Turn on Auto Fill mode automatically in Text mode and related
;;      modes.
;;   o  When in Indented Text Mode, I'd like mark-paragraph, and
;;      fill-paragraph to recognise a paragraph to be that set of contiguous
;;      lines with the same indentation.  So I rebind M-h and M-q to my
;;      functions which achieve this.
;;   o  When in Indented Text Mode, get lines justified as they are filled.
;;
;; Note that indented-text-mode calls text-mode-hook.
;; Note that auto-fill-hook is a buffer-local variable.

(defun esler-visiting-a-Makefile-p ()
  (let ((name buffer-file-name))

    ;; For some reason, text-mode-hook gets called twice.
    ;; The first time, buffer-file-name is nil, hence the following test
    ;; for stringness:
    ;;
    (if (not (stringp name))
        nil
      (progn
        (setq name (file-name-sans-versions name))
        (or (string-match "[Mm]akefile" name)
            (string-match "\\.mk" name))))))

(defun newline-and-indent-if-not-bol ()

  "Just like newline-and-indent, except when at the left margin,
in which case just newline."

  (interactive)
  (if (bolp)
      (newline 1)
    (newline-and-indent)))

(add-hook 'text-mode-hook
          '(lambda ()

             ;; Turn on auto fill.

             (auto-fill-mode 1)

             ;; I usually don't want indentation when I hit RETURN at the
             ;; beginning of the line.
             ;; My keyboard usually doesn't have a NL,
             ;; so map CR to what NL usually does.

             (local-set-key "\n" 'newline-and-indent-if-not-bol)
             (local-set-key "\r" 'newline-and-indent-if-not-bol)

             ;; If editing a Makefile, use real tabs, otherwise don't.

             (if (esler-visiting-a-Makefile-p)
                 (progn
                   (setq indent-tabs-mode t)
                   (auto-fill-mode -1))
               (setq indent-tabs-mode nil))

             ;;             ;; This conditional is necessary because there isn't an
             ;;             ;; indented-text-mode-hook.  Indented Text Mode just runs
             ;;             ;; the text-mode-hook.\
             ;;             ;; Also, text-mode hook gets run for Mail Mode.
             ;;             ;;
             ;;             (if (memq major-mode '(indented-text-mode mail-mode))
             ;;                 (progn
             ;;                   (local-set-key "\eh" 'esler-mark-indented-paragraph)
             ;;                   (local-set-key "\eq" 'esler-fill-indented-paragraph))
             ;;               (progn
             ;;                 (local-set-key "\eh" 'mark-paragraph)
             ;;                 (local-set-key "\eq" 'fill-paragraph)))))
             ))

(defun esler-mark-indented-paragraph ()

  (interactive)

  (let ((pair (esler-locate-indented-paragraph)))
    (set-mark  (nth 0 pair))
    (goto-char (nth 1 pair))))

(defun esler-fill-indented-paragraph (justify-flag)

  (interactive "P")

  (if (not (eq major-mode 'indented-text-mode))
      (fill-paragraph justify-flag)
    (let ((pair (esler-locate-indented-paragraph)))
      (let ((fill-prefix (make-string (current-indentation) ?\040))
	    (begin-marker (set-marker (make-marker) (nth 0 pair)))
	    (end-marker   (set-marker (make-marker) (nth 1 pair))))
	(fill-region-as-paragraph (nth 0 pair)
				  (nth 1 pair)
				  justify-flag)

	;; As a side effect, leave the paragraph marked
	;; as the current region.

	(set-mark (marker-position begin-marker))
	(goto-char (marker-position end-marker))
	(set-marker begin-marker nil)
	(set-marker end-marker nil)))))

(defun esler-locate-indented-paragraph ()

  "Recognises a paragraph in Indented Text Mode to be a contiguous group of
lines with the same indentation, or bounded by a line consisting only of white
space.  Returns a list with two elements, the beginning and ending of the
paragraph."

  (let ((n (current-indentation)))

    ;; Search backward for start of current indented paragraph.

    (let ((begin (save-excursion
		   (beginning-of-line)
		   (while (and (not (bobp))
			       (not (looking-at "^[ \\\\t]*$"))
			       (not (looking-at "^"))
			       (= n (current-indentation)))
		     (forward-line -1))

		   ;; Did we move one line too far ?

		   (if (or (looking-at "^[ \\\\t]*$")
			   (not (= n (current-indentation))))
		       (forward-line 1))
		   (point)))

	  ;; Search forward for end of current indented paragraph.

	  (end   (save-excursion
		   (end-of-line)
		   (while (and (not (eobp))
			       (not (looking-at "^[ \\\\t]*$"))
			       (not (looking-at "^"))
			       (= n (current-indentation)))
		     (forward-line 1))
		   (point))))
      (list begin end))))

;;}}}
;;{{{  Dired Mode.

(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))

;; Regexp matching "trivial" files at the start of a buffer:
;;  .
;;  ..
;;  .,
;;  .copyarea.db
;;  #---
;;
(setq dired-trivial-filenames
      "^\\.\\.?$\\|^\\.,\\|^\\.copyarea\\.db\\|^#")

(setq dired-dwim-target t)

;; Menu for sorting Dired buffers.
;;
;;(eval-after-load "dired" (require 'dired-sort-menu))

;; Enable recursive deletes and copies.
;;
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'top)

;; Which operations I don't want confirmed:
;;
(setq dired-no-confirm '(byte-compile
                         chgrp
                         chmod
                         chown
                         compress
                         copy
                         load
                         move
                         print
                         shell
                         symlink
                         uncompress
                         ;;delete
                         ;;hardlink
                         ))

(defun esler-dired-mode-bindings ()
  (define-key dired-mode-map " " 'scroll-up)
  (define-key dired-mode-map "\C-?" 'scroll-down) ; DEL
  (define-key dired-mode-map "1" 'delete-other-windows)
  (define-key dired-mode-map "2" 'split-window-vertically)
  (define-key dired-mode-map "5" 'split-window-horizontally)
  (define-key dired-mode-map "a" 'esler-dired-apply-function)
  (define-key dired-mode-map "b" 'dired-byte-recompile)

  ;;(define-key dired-mode-map "h" 'esler-dired-hide-matching-filenames)
  (define-key dired-mode-map "K" 'esler-dired-keep-matching-filenames)

  (define-key dired-mode-map "q" 'esler-dired-kill-current-and-find-superior-dired)
  ;;(define-key dired-mode-map "r" 'esler-dired-edit-filename)
  (define-key dired-mode-map "t" 'esler-dired-visit-tags-table)

  (define-key dired-mode-map "V" 'esler-dired-visit-vm-folder)
  (define-key dired-mode-map "z" 'esler-dired-spawn-shell)

  ;;(define-key dired-mode-map "D" 'esler-dired-keep-directories)
  (define-key dired-mode-map "E" 'esler-dired-edit-linktext)
  (define-key dired-mode-map "F" 'esler-dired-follow-link)

  ;;(define-key dired-mode-map "H" 'esler-dired-hide-file)
  ;;(define-key dired-mode-map "!" 'esler-dired-command-file)
  (define-key dired-mode-map "|" 'esler-dired-pipe-file)
  (define-key dired-mode-map "@"
    '(lambda ()
       (interactive)
       (dired-flag-backup-files)
       (dired-flag-auto-save-files)))
  (define-key dired-mode-map "."
    '(lambda ()
       (interactive)
       (goto-char (point-min))
       (dired-goto-next-nontrivial-file)))
  (define-key dired-mode-map "<" '(lambda ()
                                    (interactive)
                                    (goto-char (point-min))
                                    (forward-line 3)
                                    (dired-move-to-filename)))
  (define-key dired-mode-map ">" '(lambda ()
                                    (interactive)
                                    (goto-char (point-max))
                                    (forward-line -1)
                                    (dired-move-to-filename)))
  ;;(define-key dired-mode-map "\\" 'esler-dired-up)
  (define-key dired-mode-map "^"  'esler-dired-up)
  ;;(define-key dired-mode-map "/" 'esler-dired-down)
  (define-key dired-mode-map "]" 'esler-dired-down)
  (define-key dired-mode-map "\eg" '(lambda ()
                                      (interactive)
                                      (let ((filename (dired-get-filename t)))
                                        (esler-dired-spawn-shell)
                                        (end-of-buffer)
                                        (insert filename)))))
(eval-after-load "dired" '(esler-dired-mode-bindings))

(add-hook 'dired-mode-hook
          '(lambda ()
             (make-local-variable 'dired-associated-shell-buffer)
             (setq dired-associated-shell-buffer nil)
             ;;(esler-dired-mode-bindings)
             ))

;; When I invoke Dired, position me at the most-recently edited file
;; if it can be determined.
;;
(defadvice dired (after goto-relevant-file activate)
  "When I invoke Dired, position at the file I was looking at
when I invoked it, if that makes sense."
  (esler-dired-advice))
(defun esler-dired-advice ()
  (let ((most-recent-buffer (other-buffer (current-buffer) t)))
    (if most-recent-buffer
        (let ((most-recent-file (buffer-file-name most-recent-buffer)))
          (if most-recent-file
              (if (or (file-regular-p most-recent-file)
                      (file-directory-p most-recent-file))
                  (let ((most-recent-dir (file-name-directory most-recent-file)))
                    (if (string= (expand-file-name default-directory)
                                 (expand-file-name most-recent-dir))
                        (let ((entry-name (file-name-nondirectory most-recent-file)))
                          (goto-char (point-min))

                          ;; Probably should wrap this to ignore errors:
                          ;;
                          (dired-goto-file most-recent-file))))))))))

;; When I use "s" to resort the Dired buffer,
;; I like to be left at the top again.
;;
(defadvice dired-sort-toggle-or-edit (after goto-top activate)
  "After resorting the dired buffer, go to the top of it."
  (goto-char (point-min))
  (dired-goto-next-file))

;; If I'm renaming a single file, let me just edit the existing name.
;;
(defadvice dired-do-rename (around rename-by-edit activate)

  "If I'm renaming a single file, let me just edit the existing name."

  ;; Only do this if there's 1 file to be renamed
  ;;
  (if (eq 1 (length (dired-get-marked-files)))
      (let* ((current-name (dired-get-filename))
             (current-basename (file-name-nondirectory current-name)))
        current-name
        (let ((new-name (completing-read (format "Rename %s to: " current-basename) ;; prompt
                                         'read-file-name-internal                   ;; table
                                         default-directory                          ;; predicate
                                         nil                                        ;; require-match
                                         current-basename                           ;; initial-input
                                         'file-name-history)))
          (setq new-name (expand-file-name new-name))

          ;; If the user supplied a directory name after all,
          ;; compute the target path.
          ;;
          (if (file-directory-p new-name)
              (setq new-name (concat (file-name-as-directory new-name)
                                     current-basename)))

          ;; Do the rename operation, updating any Emacs buffer info.
          ;;
          (dired-rename-file current-name new-name nil)

          ;; Update the Dired buffer
          ;;
          (dired-add-file new-name)))
    ad-do-it))

;; When I use "s" to resort the Dired buffer,
;; I like to be left at the top again.
;;
(defadvice dired-sort-toggle-or-edit (after goto-top activate)
  "After resorting the dired buffer, got to the top of it."
  (goto-char (point-min))
  (dired-goto-next-file))

(autoload 'dired-update-file-line "dired-aux")
(defun esler-dired-edit-linktext ()
  "Replace the contents of a symbolic link."
  (interactive)
  (let ((linkname (dired-get-filename)))
    (let ((current-linktext (file-symlink-p linkname)))
      (if current-linktext
          (let ((new-linktext (completing-read "New link text: "
                                               'read-file-name-internal
                                               default-directory
                                               nil
                                               current-linktext
                                               'file-name-history)))
            ;; Delete the old link
            ;;
            (delete-file linkname)

            ;; Create the new link
            ;;
            (make-symbolic-link new-linktext linkname t)

            ;; Redisplay the new link
            ;;
            (dired-update-file-line linkname))

        (error "Not a symbolic link")))))

(defun esler-dired-kill-current-and-find-superior-dired ()
  (interactive)
  (let ((alternate (esler-dired-find-alternate-buffer))
        (superior (esler-dired-find-superior-buffer))
        (inferior (current-buffer)))
    (if dired-associated-shell-buffer
        (progn
          (delete-windows-on dired-associated-shell-buffer)
          (kill-buffer dired-associated-shell-buffer)))
    (if alternate
        (switch-to-buffer alternate)
      (if superior
          (switch-to-buffer superior)))
    (kill-buffer inferior)))

(defun esler-dired-find-alternate-buffer ()

  "Find another dired-mode buffer containing the same dir as the current."

  (let ((current (current-buffer))
        (list-of-buffers (buffer-list))
        (dir-name (file-name-as-directory default-directory)))
    (save-excursion
      (catch 'exit-loop
        (while list-of-buffers
          (let ((b (car list-of-buffers)))
            (set-buffer b)
            (if (and
                 (string= dir-name default-directory)
                 (eq major-mode 'dired-mode)
                 (not (eq current b)))
                (throw 'exit-loop b)))
          (setq list-of-buffers (cdr list-of-buffers)))
        (throw 'exit-loop nil)))))

(defun esler-dired-find-dired-buffer (path)

  "Find a dired-mode buffer containing DIR."

  (let ((list-of-buffers (buffer-list))
        (dir-name (file-name-as-directory path)))
    (save-excursion
      (catch 'exit-loop
        (while list-of-buffers
          (let ((b (car list-of-buffers)))
            (set-buffer b)
            (if (and
                 (string= dir-name default-directory)
                 (eq major-mode 'dired-mode))
                (throw 'exit-loop b)))
          (setq list-of-buffers (cdr list-of-buffers)))
        (throw 'exit-loop nil)))))

(defun esler-dired-find-superior-buffer ()

  "Find a dired-mode buffer whose default directory is the superior
      directory to the the default directory of the current-buffer."

  (let ((current-dir-name default-directory))
    (let ((superior-dir-name (file-name-directory
                              (directory-file-name current-dir-name))))
      (if superior-dir-name
          (esler-dired-find-dired-buffer superior-dir-name)
        nil))))

(defun esler-dired-up ()

  "Find or create a dired buffer for the directory containing the current
      directory."

  (interactive)

  (let ((superior (esler-dired-find-superior-buffer))
        (inferior-leaf-name (file-name-nondirectory (directory-file-name default-directory))))
    (if superior
        (progn
          (switch-to-buffer superior)
          (let ((previous-point (point)))
            (goto-char 0)
            (if (re-search-forward (concat " " (regexp-quote inferior-leaf-name) " *.*$") nil t)
                (dired-move-to-filename)
              (goto-char previous-point))))
      (if (file-directory-p "..")
          (progn
            (dired "..")
            (goto-char 0)
            (re-search-forward (concat " " (regexp-quote inferior-leaf-name) "$") nil t)
            (dired-move-to-filename))
        (message "No containing directory")))))

(defun esler-dired-visit-tags-table ()

  "In dired, visit the file or directory named on this line as a tags file."

  (interactive)
  (visit-tags-table (dired-get-filename))
  (message "Current tags table is %s" tags-file-name))

(defvar esler-dired-last-piped-command nil)
(defun esler-dired-pipe-file ()

  "In dired, run a command with pointed at file as stdin."

  (interactive)

  (let ((command (read-from-minibuffer "Command: "
                                       (if esler-dired-last-piped-command
                                           esler-dired-last-piped-command
                                         nil)
                                       nil
                                       nil)))
    (let ((buffer (get-buffer-create "*Shell Command Output*")))
      (with-current-buffer buffer
        (erase-buffer))
      (call-process shell-file-name
                    (dired-get-filename)
                    buffer
                    nil
                    "-c"
                    command)
      (setq esler-dired-last-piped-command command)
      (if (with-current-buffer buffer
            (> (buffer-size) 0))
          (set-window-start (display-buffer buffer) 1)
        (message "(Shell command completed with no output)")))))

(defun esler-dired-spawn-shell ()

  "Spawn a shell buffer in another window, with current directory set to the
      directory being edited."

  (interactive)

  ;; This code was borrowed from (defun shell ...) and modified.

  (require 'shell)
  (let* ((buf-part-name default-directory)
         (buf-name (concat "*" buf-part-name "*")))

    ;; If the appropriate shell buffer apparently doesn't exist
    ;; create it...
    (if (not (comint-check-proc buf-name))
        (let* ((prog (or explicit-shell-file-name
                         (getenv "ESHELL")
                         (getenv "SHELL")
                         "/bin/sh"))
               (name (file-name-nondirectory prog))
               (startfile (concat "~/.emacs_" name))
               (xargs-name (intern-soft (concat "explicit-" name "-args")))
               (process-environment (cons (concat "DIRED_HOME=" default-directory)
                                          process-environment)))
          (let* ((new-buf (apply 'make-comint buf-part-name prog
                                 (if (file-exists-p startfile) startfile)
                                 (if (and xargs-name (boundp 'xargs-name))
                                     (symbol-value xargs-name)
                                   '("-i")))))
            (setq dired-associated-shell-buffer new-buf)
            (set-buffer new-buf)
            (shell-mode)))

      ;; ...otherwise just associate it with the dired buffer
      ;;
      (setq dired-associated-shell-buffer (get-buffer buf-name)))


    ;; We probably should make sure that the buffer we found,
    ;;    - is in shell mode
    ;;    - has its current directory where we expec.
    ;; Later..

    ;; Now display the shell buffer, splitting the dired window if
    ;; necessary.
    ;;
    (let ((shell-window (get-buffer-window buf-name)))
      (if (null shell-window)
          (setq shell-window (split-window)))
      (set-window-buffer shell-window buf-name)
      (select-window shell-window)
      (set-buffer (get-buffer buf-name))
      (goto-char (point-max)))))

(defun esler-dired-follow-link ()
  "Follow the link under the cursor."
  (interactive)

  (let ((link (dired-get-filename)))
    (let ((linktext (file-symlink-p link)))
      (if linktext
          (cond
           ((file-directory-p linktext) (dired linktext))
           ((file-symlink-p linktext) (dired linktext))
           (t (find-file linktext)))
        (error "Not a symbolic link")))))

(defun esler-dired-down ()

  "Find or create a dired buffer for the directory containing the pointed at
      directory."

  (interactive)

  (let ((dir (dired-get-filename)))
    (cond

     ;; For directories:
     ;;
     ((file-directory-p dir)
      (let ((buf (esler-dired-find-dired-buffer dir)))
        (if buf
            (switch-to-buffer buf)
          (dired dir))))

     ;; For symlinks:
     ;;
     ((file-symlink-p dir)

      ;; See if there is a Dired Mode buffer already.
      ;;
      (let ((buf (esler-dired-find-dired-buffer dir)))
        (if buf
            (switch-to-buffer buf)

          ;; Otherwise, see if the link target is a directory.
          ;;
          (let ((linktext (file-symlink-p dir)))
            (if (file-directory-p linktext)
                (dired linktext)
              (error "Not a link to a directory"))))))
     (t
      (error "Not a directory")))))

(defun esler-dired-keep-matching-filenames (regexp)

  "Filter a Dired Mode buffer, keeping only those files which match
      the supplied REGEXP."

  (interactive "sRegular expression: ")

  ;; Design decision: should we start from the full contents or
  ;; from what is currently displayed.
  ;; For now we choose the former.
  ;;
  (esler-dired-revert-buffer)

  (let ((buffer-read-only nil))

    ;; Keep the two header lines, and filter the rest.
    ;;
    (iterate-over-lines-in-region
     (progn
       (goto-char (point-min))
       (forward-line 2)
       (point))
     (point-max)
     '(lambda ()
        (let ((filename (dired-get-filename t t)))
          (if filename
              (if (not (string-match regexp filename))
                  (kill-line 1)))))))
  ;; Update mode line.
  ;;
  (setq mode-name (concat mode-name " (keeping matches for \"" regexp "\")"))
  (set-buffer-modified-p (buffer-modified-p)))

(defun esler-dired-hide-matching-filenames (regexp)

  "Filter a Dired Mode buffer, hiding those files which match the supplied REGEXP."

  (interactive "sRegular expression: ")

  ;; Design decision: should we start from the full contents or
  ;; from what is currently displayed.
  ;; For now we choose the former.
  ;;
  (esler-dired-revert-buffer)

  (let ((buffer-read-only nil))
    ;; Keep the two header lines, and filter the rest.
    ;;
    (iterate-over-lines-in-region
     (progn
       (goto-char (point-min))
       (forward-line 2)
       (point))
     (point-max)
     '(lambda ()
        (let ((filename (dired-get-filename t t)))
          (if filename
              (if (string-match regexp filename)
                  (kill-line 1)))))))
  ;; Update mode line.
  ;;
  (setq mode-name (concat mode-name " (hiding matches for \"" regexp "\")"))
  (set-buffer-modified-p (buffer-modified-p)))

(defun esler-dired-revert-buffer ()

  "Revert the Dired Mode buffer, resetting the mode line back to normal."
  (interactive)
  (setq mode-name "Dired")
  (dired-revert t t))

;;}}}
;;{{{  Process Mode.

(autoload 'ps-mode "ps-mode"
  "Package for manipulating Unix processes."
  t)

(if (eq system-type 'hpux)
    (progn
      (setq ps-mode-program-args-list (list "-f" "-u" (user-real-login-name)))
      (setq ps-mode-program-args '("ugx"))))

(if (or
     (eq system-type 'irix)
     (eq system-type 'usg-unix-v))
    (progn
      (setq ps-mode-program-args-list (list "-f" "-u" (user-real-login-name)))))

(if (or (eq system-type 'lignux)
        (eq system-type 'linux)
        (eq system-type 'gnu/linux))
    (progn
      (setq ps-mode-program-args-list (list "ux"))))

(defun ps ()
  (interactive)
  (push-window-config)
  (ps-mode))

(defun esler-ps-mode-bindings ()
  ;; Let "g" mean "get or refresh", as it does for
  ;; Dired Mode, VM Mode, GNUS, et al.
  ;;
  (define-key ps-mode-map "g"
    '(lambda ()
       (interactive)
       (message "Reading process information...")
       (ps-mode-build-process-list)
       (message "Reading process information...done")))

  ;; Bind "q" to abandon Process Mode.
  ;;
  (define-key ps-mode-map "q" '(lambda ()
                                 (interactive)
                                 (ps-mode-quit)
                                 (pop-window-config))))
(add-hook 'ps-mode-hook
          '(lambda ()

             ;; Establish my (somewhat) standard set of readonly-buffer bindings.
             ;;
             (esler-standard-readonly-buffer-key-bindings)

             ;; And then override a few.
             ;;
             (esler-ps-mode-bindings)
             ))


;; Use the new one posted on the Net.
;; It's nice, but it's Xemacs->emacs19 compat stuff conflicts with VM.
;;
;;
;;(autoload 'ps "view-process-mode"
;;  "Prints a list with processes in the buffer `View-process-buffer-name'.
;;     COMMAND-SWITCHES is a string with the command switches (ie: -aux).
;;     IF the optional argument REMOTE-HOST is given, then the command will
;;     be executed on the REMOTE-HOST. If an prefix arg is given, then the
;;     function asks for the name of the remote host."
;;  t)

;;}}}
;;{{{  Calendar, Diary

(setq view-diary-entries-initially t)
(display-time)
(add-hook 'diary-hook 'appt-make-list)

;;}}}
;;{{{  Tar Mode

(autoload 'tar-mode "tar-mode"
  "Mode for examining and extracting from tar files,
including compressed ones."
  t)

(setq auto-mode-alist (cons '("\\.tar$" . tar-mode) auto-mode-alist))

(defun esler-tar-mode-bindings ()
  (define-key tar-mode-map "q" '(lambda ()
                                  (interactive)
                                  (kill-buffer (current-buffer))))
  (define-key tar-mode-map "." 'beginning-of-buffer)
  (define-key tar-mode-map " " 'scroll-up)
  (define-key tar-mode-map "\C-?" 'scroll-down) ; DEL
  (define-key tar-mode-map "1" 'delete-other-windows)
  (define-key tar-mode-map "2" 'split-window-vertically)
  (define-key tar-mode-map "5" 'split-window-horizontally)
  (define-key tar-mode-map [mouse-3]
    '(lambda (click)
       "Select the file clicked on."
       (interactive "@e")
       (mouse-set-point click)
       (sit-for 0)
       (tar-extract))))
(eval-after-load "tar-mode" '(esler-tar-mode-bindings))

;;}}}
;;{{{  Archive Mode

(defun esler-archive-mode-bindings ()
  (define-key archive-mode-map "q" '(lambda ()
                                      (interactive)
                                      (kill-buffer (current-buffer))))
  (define-key archive-mode-map "." 'beginning-of-buffer)
  (define-key archive-mode-map " " 'scroll-up)
  (define-key archive-mode-map "\C-?" 'scroll-down) ; DEL
  (define-key archive-mode-map "1" 'delete-other-windows)
  (define-key archive-mode-map "2" 'split-window-vertically)
  (define-key archive-mode-map "5" 'split-window-horizontally))

(eval-after-load "arc-mode" '(esler-archive-mode-bindings))

;;}}}
;;{{{  Emerge Mode.

;; This package can do something like DSEE's merge.

(autoload 'emerge-files "emerge"
  "Run Emerge on two files."
  t)
(autoload 'emerge-files-with-ancestor "emerge"
  "Run Emerge on two files, giving another file as the ancestor."
  t)
(autoload 'emerge-buffers "emerge"
  "Run Emerge on two buffers."
  t)
(autoload 'emerge-buffers-with-ancestor "emerge"
  "Run Emerge on two buffers, giving another buffer as the ancestor."
  t)
(autoload 'emerge-files-command "emerge")
(autoload 'emerge-files-with-ancestor-command "emerge")
(autoload 'emerge-files-remote "emerge")
(autoload 'emerge-files-with-ancestor-remote "emerge")

;;}}}
;;{{{  Ediff Mode.

;; Get side-by-side comparison
;;
(setq ediff-split-window-function 'split-window-horizontally)

;;}}}
;;{{{  Changelog Mode.

(setq auto-mode-alist (cons '("[cC][hH][aA][nN][gG][eE][lL][oO][gG]$" . change-log-mode) auto-mode-alist))

;;}}}
;;{{{  Man Page Mode.

;; Get a new frame for each man page.
;;
(if window-system
    (setq Man-notify 'newframe)
  (setq Man-notify 'aggressive))

;; Allow me to click on a man page reference to read it.
;;
(defun esler-Man-mode-bindings ()
  ;; Point-and-shoot at man page reference.
  ;;
  (local-set-key [mouse-3] 'esler-Man-page-mouse-3-handler))

(add-hook 'Man-mode-hook
          '(lambda () (esler-Man-mode-bindings)))

(defun esler-Man-page-mouse-3-handler (click)
  "Read the man page referred to by the text under the mouse."
  (interactive "@e")
  (mouse-set-point click)
  (sit-for 0)

  (let ((man-args (Man-default-man-entry)))
    (if (string= man-args "")
        (error "No manual reference found."))

    ;; Recognize the subject(section) syntax.
    ;;
    (setq man-args (Man-translate-references man-args))

    (if Man-downcase-section-letters-p
        (setq man-args (Man-downcase man-args)))
    (Man-getpage-in-background man-args)))

;;}}}
;;{{{  View Minor Mode

(autoload 'view-mode "view" nil t)
(setq view-read-only t)
(defun esler-view-exit-action (buf)
  (interactive)
  (delete-windows-on buf)
  (kill-buffer buf))
(setq view-exit-action 'esler-view-exit-action)
(setq view-mode-hook 'esler-view-mode-hook)
(defun esler-view-mode-hook ()
  (esler-standard-readonly-buffer-key-bindings-in-keymap view-mode-map))

;;}}}
;;{{{  Folding Minor Mode.

(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)

(folding-add-to-marks-list 'scheme-mode ";;{{{" ";;}}}" nil t)
(folding-add-to-marks-list 'text-mode "{{{" "}}}" nil t)
(if (<= emacs-major-version 19)
    (folding-add-to-marks-list 'indented-text-mode "{{{" "}}}" nil t))
(folding-add-to-marks-list 'fundamental-mode "{{{" "}}}" nil t)
(folding-add-to-marks-list 'makefile-mode "#{{{" "#}}}" nil t)
(folding-add-to-marks-list 'latex-mode "%{{{" "%}}}" nil t)
(folding-add-to-marks-list 'slitex-mode "%{{{" "%}}}" nil t)
(folding-add-to-marks-list 'c-mode "/* {{{" "/* }}}" nil t)
(folding-add-to-marks-list 'c++-mode "//{{{" "//}}}" nil t)
(folding-add-to-marks-list 'java-mode "//{{{" "//}}}" nil t)

(setq folding-behave-table
      '((close 	folding-hide-current-entry)
        (open   	folding-shift-in) ;; Could also be `folding-enter'.
        (up		folding-shift-out)
        (other	folding-mouse-call-original)
        ))

(defun esler-folding-enfold-indented-buffer ()

  "Insert fold marks according to the indentation in the buffer,
so that the resultant buffer can be edited in folding Minor Mode.
The buffer is assumed to be indented in a well-formed fashion.
This is real useful for making DSEE build descriptions comprehensible."

  ;; How to relax the well-formed-indentation assumption ?
  ;; What to do about empty lines ?

  (interactive)

  (goto-char (point-min))
  (let ((indentation-stack (cons (current-indentation) nil)))
    (iterate-over-lines-in-region
     (point-min)
     (point-max)
     '(lambda ()

        ;; Is is appropriate to insert an end-fold mark on a line of its own before this line ?
        ;; i.e. has the current indentation just decreased ?
        ;;
        (while (and (cdr indentation-stack)
                    (< (current-indentation) (car indentation-stack)))
          (progn
            (setq indentation-stack (cdr indentation-stack))
            (insert (concat "\n" (make-string (car indentation-stack) 32) "}}}\n"))))

        ;; Is is appropriate to put a start-fold mark before the first word of this line ?
        ;; I.e. there is a following line, and it's indentation is more than that of
        ;; the current line.
        ;;
        (let ((indentation-of-following-line (save-excursion
                                               ;; Is there a following-line ?
                                               ;;
                                               (if (zerop (forward-line 1))
                                                   (current-indentation)
                                                 nil))))
          (if (and indentation-of-following-line
                   (> indentation-of-following-line (current-indentation)))
              (progn

                ;; Move to the first non-whitespace character on this line.
                ;;
                (if (re-search-forward "[^ 	]"
                                       (save-excursion (end-of-line) (point))
                                       t)
                    (backward-char 1))
                (insert "{{{ ")
                (end-of-line)
                (insert "\n")
                (setq indentation-stack (cons indentation-of-following-line indentation-stack)))))))

    ;; Now insert any necessary end-folds at the end of the buffer.
    ;;
    (goto-char (point-max))

    (while (cdr indentation-stack)
      (progn
        (setq indentation-stack (cdr indentation-stack))
        (insert (concat "\n" (make-string (car indentation-stack) 32) "}}}\n"))))))

(defun insert-folding-cruft-at-eof ()
  (interactive)
  "Insert Emacs local variables to turn on folding mode."
  (goto-char (point-max))

  (insert "\n")
  (insert ";; Local variables:\n")
  (insert ";; folded-file: t\n")
  (insert ";; End:\n"))

;;}}}
;;{{{  Folding extensions to Outline Mode.

(eval-after-load "outline" '(require 'foldout))

(define-key global-map
  [menu-bar show zoom]
  '("Zoom subtree" . foldout-zoom-subtree))
(define-key global-map
  [menu-bar hide unzoom]
  '("Un-Zoom subtree" . foldout-exit-fold))

(add-hook 'outline-minor-mode-hook
          '(lambda ()
             (progn

               ;; Mouse navigation in a folded outline buffer.
               ;;
               (define-key outline-minor-mode-map [mouse-3] 'esler-mouse19-folding-outline-mouse-3-handler)

               ;; Start off with the file folded.
               ;;
               (hide-sublevels 1))))

(defun esler-mouse19-folding-outline-mouse-3-handler (click)
  "Enter or exit the fold pointed at.
This must be bound to a mouse click."
  (interactive "@e")
  (mouse-set-point click)
  (sit-for 0)

  ;; Clicking on a line containing a fold-top marker
  ;; will either enter or exit the fold, depending
  ;; on what's appropriate.
  ;; Clicking on a blank line will also exit the current fold.
  ;;
  (let ((line-type nil))

    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*$")
          (setq line-type 'blank)
        (if (and (boundp 'outline-regexp)
                 outline-regexp)

            (if (looking-at outline-regexp)
                (if (looking-at (concat outline-regexp
                                        "[^\^M]*"
                                        "\^M"))
                    (setq line-type 'fold-entry)
                  (setq line-type 'fold-exit))))))

    (cond
     ((eq line-type 'blank)
      (foldout-exit-fold 1))
     ((eq line-type 'fold-exit)
      (foldout-exit-fold 1))
     ((eq line-type 'fold-entry)
      (foldout-zoom-subtree))
     ((null line-type)
      (find-file-at-point)))))

;;}}}
;;{{{  Occur Mode

(defun esler-occur-mode-bindings ()
  (local-set-key [mouse-3]
                 '(lambda (click)
                    "Select the occurrence clicked on."
                    (interactive "@e")
                    (mouse-set-point click)
                    (sit-for 0)
                    (occur-mode-goto-occurrence))))

(add-hook 'occur-mode-hook
          '(lambda (esler-occur-mode-bindings) ()))

;;}}}
;;{{{  Compilation (and Grep) Mode.

;; In a buffer output by M-x grep,
;; be able to point-and-click at an occurrence to find it in its file.
;;
(defun esler-compilation-mode-bindings ()
  (local-set-key [mouse-3]
                 '(lambda (click)
                    "Select the occurrence clicked on."
                    (interactive "@e")
                    (mouse-set-point click)
                    (sit-for 0)
                    (compile-goto-error nil))))
(add-hook 'compilation-mode-hook
          '(lambda () (esler-compilation-mode-bindings)))

;;}}}

;;{{{  Load up bookmarks.

(require 'bookmark)
(setq bookmark-default-file "~/apps/emacs/boomarks.el")

;;}}}

;;{{{  Emacs' help facility.

;; Use electric help, instead of normal help.

(require 'ehelp)
(global-set-key "\C-x?" 'ehelp-command)

;;}}}
;;{{{  Lispdir facility.

(autoload 'lisp-dir-apropos "Lispdir/lispdir"
  "Show where to find publicly contributed Emacs lisp programs."
  t)

(setq lisp-code-directory (concat esler-elisp-directory
                                  "/Lispdir/LCD-datafile"))

;; This used to be tut.cis... but that name no longer responds to FTP.
;;
(setq elisp-archive-host "archive.cis.ohio-state.edu")

;;}}}
;;{{{  Crypt facility.

(setq crypt-encryption-type 'crypt)

(setq crypt-encryption-file-extension "\\(\\.e\\)$"
      crypt-bind-insert-file nil
      crypt-freeze-vs-fortran nil
      crypt-confirm-password t)

;; Has crypt++ been loaded already?
;;
(if (featurep 'crypt++)

    ;; Yes.  Just rebuild the encryption and encoding tables
    ;; and key binding.
    (progn
      (crypt-rebuild-tables)
      (crypt-bind-insert-file))

  ;; No.  Load the whole thing.
  ;;
  (require 'crypt++))

;;}}}
;;{{{  Time-stamping facility.

(autoload 'time-stamp "time-stamp" "Update the time stamp in a buffer." t)
(add-hook 'write-file-hooks 'time-stamp)

;;}}}
;;{{{  Follow Mode

(autoload 'follow-mode "follow"
  "Synchronize windows showing the same buffer, minor mode." t)
(global-set-key [f8] 'follow-mode)

(autoload 'follow-delete-other-windows-and-split "follow"
  "Delete other windows, split the frame in two, and enter Follow Mode." t)
(global-set-key [f7] 'follow-delete-other-windows-and-split)

;;}}}
;;{{{  Mkid

(require 'gid)

;;}}}
;;{{{  Bibl Mode

(require 'easymenu)

(autoload 'bibl-visit-bibliography "bibl-mode"
  "Autoload bibliography mode." t)
(autoload 'bibl-mode "bibl-mode" "Autoload bibliography mode." t)
(setq bibl-file-name "~/bibliographies/kae.bibl")
(global-set-key "\C-cb" 'bibl-global-map)

;;}}}
;;{{{  Paren highlighting

;; Get paren highlighting.
;;
(require 'paren)

;;}}}

;;}}}

;;{{{  Mac OS X specifics

;; In Dired Mode: "open" a pointed-at object with the appropriate app.
;; (If it's a directory, fire up the Finder
;;


(if running-on-mac
    (progn

      (add-to-list 'exec-path "~/Library/bin")
      
      (defun esler-dired-launch-file (&optional arg)
        (interactive "P")
        (mapcar
         (function
          (lambda (relative-object)
            (esler-launch-file relative-object)))
         (dired-get-marked-files t arg)))

      (eval-after-load
          "dired"
        '(define-key dired-mode-map "j" 'esler-dired-launch-file))

      ;; Get the desired cursor shape.
      (set-default 'cursor-type 'box)

      ;; Allow press and hold in Emas
      (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")

      ;; Improve scrolling
      (setq mouse-wheel-scroll-amount '(0.01))

      ;; Start in a sensible place.
      ;;
      (cd "~/")

      (setq tab-width 4)))

;;}}}

;;{{{  Win32-specifics

;;{{{ Find Cygwin and Bash

(if running-on-w32
    (progn
      (defun find-cygwin-root ()
        (let ((root (concat "c:\\" "cygwin")))
          (if (file-exists-p (concat root "\\bin\\cygwin1.dll"))
              root)))

      (defvar cygwin-root nil)
      (defvar cygwin-bash-location nil)

      (setq cygwin-root (find-cygwin-root))
      (if (not (null cygwin-root))
          (setq cygwin-bash-location (concat cygwin-root "\\bin\\bash.exe")))))

;;}}}

;;{{{ Some Bash support

(defun bash-toggle-slashes ()
  "Toggle all \\ to / or all / to \\ on the current comint input region."
  (interactive)
  (save-excursion
    (let* (start
           end
           (proc (get-buffer-process (current-buffer))) )
      (end-of-line 1)
      (setq end (point))
      (cond ((and proc (processp proc))
             (setq start (marker-position (process-mark proc))))
            (t
             (comint-bol 1)
             (setq start (point))))
      (cond ((< end start)
             (goto-char end)
             (beginning-of-line 1)
             (setq start (point))))
      (goto-char start)
      (cond ((search-forward "\\" end t)
             (\\/-region start end))
            ((search-forward "/" end t)
             (/\\-region start end))))))

(defun change-character-in-buffer (from to)
  (interactive "cChange character: \ncTo character: \n")
  (change-character-in-region from to (point-min) (point-max)))

(defalias '// 'change-character-in-region)

(defun /\\-buffer ()
  "Change all / to \\ in the buffer."
  (interactive)
  (change-character-in-buffer ?/ ?\\))

(defun /\\-region (start end)
  "Change all / to \\ for the rest of the buffer."
  (interactive "r")
  (change-character-in-region ?/ ?\\ start end))

(defalias '/\\ '/\\-region)

(defun \\/-buffer ()
  "Change all \\ to / in the buffer."
  (interactive)
  (change-character-in-buffer ?\\ ?/))

(defun \\/-region (start end)
  "Change all \\ to / for the region START to END."
  (interactive "r")
  (change-character-in-region ?\\ ?/ start end))

(defun /.-region (start end)
  "Change all / to . for the region START to END."
  (interactive "r")
  (change-character-in-region ?/ ?. start end))

(defun \./-region (start end)
  "Change all . to / for the region START to END."
  (interactive "r")
  (change-character-in-region ?. ?/ start end))

(defalias '\\/ '\\/-region)
(defalias '\./ '\./-region)
(defalias '/. '/.-region)

(defun ^m ()
  "Remove all ^M's from the buffer."
  (interactive)
  (^m-region (point-min) (point-max)))

(defalias '^M '^m)
(defalias '^M '6m)

(defun ^m-buffer ()
  "Remove all ^M's from the buffer."
  (interactive)
  (^m-region (point-min) (point-max)))

(defalias '^m '^m-buffer)
(defalias '^M '^m-buffer)

(defun ^m-region (min max)
  "Remove all ^M's from the region."
  (interactive "r")
  (save-excursion
    (goto-char max)
    (while (re-search-backward "\C-m$" min t)
      (delete-char 1))))

(defun :/-region (start end)
  "Convert a path in the region START to END from Windows format to CygWin32 format,
using cygpath"
  (interactive "r")
  (shell-command-on-region start end (concat "cygpath --unix '" (buffer-substring-no-properties start end) "'") t t)
  (goto-char (mark))
  (backward-delete-char-untabify 1)
  )

(defalias ':/ ':/-region)

(defun /:-region (start end)
  "Convert a path in the region START to END from CygWin32 format to Windows format,
using cygpath"
  (interactive "r")
  (shell-command-on-region start end (concat "cygpath --windows '" (buffer-substring-no-properties start end) "'") t t)
  (goto-char (mark))
  (backward-delete-char-untabify 1))

(defalias ':/ ':/-region)

;;}}}

(if running-on-w32
    (progn

      (defun esler-w32-canonicalize-path-seps (path)
        (subst-char-in-string ?/ ?\\ path t))

      ;; In Dired Mode: I like to be able to change the case of file names.
      ;; NYI: doesn't work for directories. (problem inside rename-file C func).
      ;;
      (defadvice dired-rename-file (before kae-permit-case-change act)
        (if (and (string= (downcase (ad-get-arg 0))
                          (downcase (ad-get-arg 1)))
                 (equal (file-attributes (ad-get-arg 0))
                        (file-attributes (ad-get-arg 1))))
            (ad-set-arg 2 t)))

      ;; In Dired Mode: "open" a pointed-at object with the appropriate app.
      ;; (If it's a directory, fire up the Windows Explorer.)
      ;;
      (defun esler-dired-launch-file (&optional arg)
        (interactive "P")
        (mapcar
         (function
          (lambda (relative-object)
            (cond
             ;; Directory: launch Explorer
             ;;
             ((file-directory-p relative-object)
              (start-process-shell-command "Windows NT Explorer"
                                           nil
                                           "explorer"
                                           (concat relative-object ",/e")))
             ;; .EXE, .COM, .CMD, .BAT: invoke it
             ;;
             ((or (string-match "\\.exe$" (downcase relative-object))
                  (string-match "\\.bat$" (downcase relative-object))
                  (string-match "\\.com$" (downcase relative-object))
                  (string-match "\\.cmd$" (downcase relative-object)))
              (start-process-shell-command relative-object
                                           nil
                                           (concat default-directory "/"
                                                   (substring relative-object 0 -4))))

             ;; .mdp, .dsw, .dsp: invoke VC
             ;;   (Temporary hack until w32-shell-execute learns to invoke the
             ;;    default verb on an object when nil is provided.)
             ;;
             ((string-match "\\.mdp\\|dsw\\|dsp$" (downcase relative-object))
              (w32-shell-execute "&Open with MSDev" (expand-file-name relative-object)))

             (t
              (progn
                (message "Opening %s..." relative-object)
                (w32-shell-execute "open"
                                   (esler-w32-canonicalize-path-seps
                                    (expand-file-name relative-object)))
                (message "Opening %s...done" relative-object))))))
         (dired-get-marked-files t arg)))

      (eval-after-load
          "dired"
        '(define-key dired-mode-map "j" 'esler-dired-launch-file))

      ;; Start in a sensible place.
      ;;
      (cd "~/")

      (setq tab-width 4)

      ;; For the MKS shell
      ;;
      (cond

       ;; Try these in order:
       ;;

       ;;  - Bash
       ;;
       ((file-exists-p  cygwin-bash-location)
        (progn
          (setenv "SHELL" "c:/cygwin/bin/bash.exe")
          (setq shell-file-name "bash")
          (setq explicit-shell-file-name "bash")
          (setq shell-command-switch "-c")
          (load "comint")
          (fset 'original-comint-exec-1 (symbol-function 'comint-exec-1))
          (defun comint-exec-1 (name buffer command switches)
            (let ((binary-process-input t)
                  (binary-process-output nil))
              (original-comint-exec-1 name buffer command switches)))
          ))

       ;;  - The NT shell
       ;;
       (t
        (progn
          (setenv "SHELL" "cmd.exe")
          (setq explicit-cmd.exe-args '("/q"))
          (setq w32-quote-process-args t)))
       )))

;;}}}

;;{{{  Start the Emacs server

;; Note: If you get a "directory is unsafe" message from this
;; take ownership of the directory mentioned.
;;
(server-start)
(setq server-window 'pop-to-buffer)

;;}}}

(message "End of .emacs...")

;;{{{  Emacs local variables for this file.


;; Local variables:
;; folded-file: t
;; End:

;;}}}
