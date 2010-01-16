;; This file has been formatted so it can be viewed/edited in
;; Folding Minor Mode.
;;

(message "Start of .emacs...")

;;{{{ Useful stuff new to Emacs-21

;; M-x check-parens
;; M-x delete-trailing-whitespace
;; M-; aka M-x comment-dwim
;; M-x clone-indirect-buffer
;; query-replace has a binding for "e" to edit replacement
;; M-x comint-redirect-send-command
;; M-x comint-write-output
;; Mouse-2 pulls down a command from a comint buffer.
;; artist-mode
;; Ebrowse
;; glasses-mode

;;}}}

;;{{{  Set some global variables.

(defun running-off-usb-drive ()
  (and (boundp 'usb-drive-letter)
       (not (null usb-drive-letter))))

(defvar esler-elisp-directory "~/apps/emacs/elisp")

(defvar esler-verizon-storage-root "/ftp:kevin.a.esler@members.verizon.net:")


;;{{{  Determine what version of Emacs is running

(defvar esler-xemacs (string-match "XEmacs" emacs-version))
(defvar esler-emacs20 (= emacs-major-version 20))
(defvar esler-emacs21 (= emacs-major-version 21))
(defvar esler-emacs22 (= emacs-major-version 22))
(defvar esler-emacs23 (= emacs-major-version 23))

;;}}}

;;{{{  Determine what kind of display technology is being used.

(defvar running-as-x-client (memq window-system '(x x11)))
(defvar running-as-w32-client (memq window-system '(w32 win32 mswindows)))
(defvar running-as-terminal-client (null window-system))
;;(defvar running-as-cygwin-client (eq system-type 'cygwin))

;; Boolean variable to indicate if we're an ASCII Emacs in an xterm window
;; (so we can enable the mouse support for this mode of operation).

(defvar running-as-xterm-client (and (not running-as-x-client)
                                     (or (equal "xterm" (getenv "TERM"))
                                         (equal "vt100" (getenv "TERM")))))

;;}}}

;;{{{  Determine what site we're at.

(defvar at-site-ibm nil)
(defvar at-site-home nil)

(defun esler-command-output-first-line-as-string (command)

  "Capture the first line of a command's output as a string."

  (let ((buffer (get-buffer-create "*active function*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (call-process shell-file-name
                    nil
                    buffer
                    nil
                    "-c"
                    command)
      (goto-char (point-min))
      (end-of-line)
      (buffer-substring (point-min) (point)))))

(let ((host-name (system-name))
      (domain-name ""))
  ;;      (domain-name (esler-command-output-first-line-as-string "domainname")))
  (cond
   ;; Heuristically decide where we're running.
   ;;
   ((or
     (string-match "IBM\\.com" host-name)
     (string-match "ibm\\.com" host-name)
     (string-match "Rational\\.com" host-name)
     (string-match "rational\\.com" host-name)
     (string-match "atria\\.com" host-name)
     (string-match "atria\\.com" domain-name)
     (string-match "KAESLER-TC" host-name)
     (string-match "kaesler-sun" host-name)
     (string-match "KAESLER-T60P" host-name)
     )
    (setq at-site-ibm t))

   ((or
     (string-match "DURIF" host-name)
     )
    (setq at-site-home t))))

(if (and at-site-ibm
         (string= "KAESLER-T60P" (system-name)))
    (setq system-name "kaesler-t60p.lexma.ibm.com"))
(if (and at-site-ibm
         (string= "KAESLER-TC" (system-name)))
    (setq system-name "kaesler-tc.lexma.ibm.com"))

(defvar esler-small-screen (< (display-pixel-width) 1600))
(add-hook 'before-make-frame-hook
          (function
           (lambda()
             (setq esler-small-screen (< (display-pixel-width) 1600)))))

;;}}}

;;{{{  Miscellaneous.

;;{{{  clearcase-viewtag

(defun clearcase-viewtag ()
  "Return the current ClearCase view tag."

  (let ((clearcase-root (getenv "CLEARCASE_ROOT")))
    (if clearcase-root
        (progn

          ;; Expecting "/view/VIEW-TAG"
          ;;
          (if (not (string-equal "/view/" (substring clearcase-root 0 6)))
              (error "Funny value in CLEARCASE_ROOT"))

          (let ((process-view-tag (substring clearcase-root 6)))
            process-view-tag)))))

;;}}}

(setq max-specpdl-size 1000)
(setq max-lisp-eval-depth 1000)

(setq clearcase-viewtag (clearcase-viewtag))

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
    (setq default-major-mode 'indented-text-mode)
  (setq default-major-mode 'text-mode))

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
(if (not esler-xemacs)
    (delete-selection-mode t))

;;}}}

;;}}}

;;{{{ Find Cygwin and Bash

(defun find-cygwin-root ()
  (if (running-off-usb-drive)
      (let ((root (concat usb-drive-letter ":\\cygwin")))
        (if (file-exists-p (concat root "\\bin\\cygwin1.dll"))
            root))
    (let ((root (concat "c:\\" "cygwin")))
      (if (file-exists-p (concat root "\\bin\\cygwin1.dll"))
          root))))

(defvar cygwin-root nil)
(defvar cygwin-bash-location nil)
(if running-as-w32-client
    (progn
      (setq cygwin-root (find-cygwin-root))
      (if (not (null cygwin-root))
          (setq cygwin-bash-location (concat cygwin-root "\\bin\\bash.exe")))))

;;}}}
;;{{{  My identity

(setq user-full-name "Kevin Esler")

(defvar esler-ultranet-user-name "esler")
(defvar esler-verizon-user-name "kevin.a.esler")

;; Identify myself to GNUS.
;;
(cond
 (at-site-ibm
  (progn
    (setq mail-host-address "us.ibm.com")
    (setq user-mail-address "kaesler@us.ibm.com")
    (setq message-user-organization "IBM Rational Software, Lexington, MA, USA")))

 (at-site-home
  (progn

    (setq mail-host-address "alum.bu.edu")

    ;;(setq user-mail-address "kevin.esler.1989@alum.bu.edu"))
    ;;
    ;; Although I am using mail forwarding from kevin.esler.1989@alum.bu.edu,
    ;; this appears to have to be set to the real ISP-provided final email
    ;; address, otherwise I can' seem to send via Verizon's SMTP server.
    ;; This is where bcc's go.
    ;;
    ;;(setq user-mail-address "kevin.esler.1989@alum.bu.edu"))
    (setq user-mail-address "kevin.a.esler@verizon.net")

    (setq message-user-organization nil))))

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
          (save-excursion
            (set-buffer (get-buffer-create "*esler-register-emacs-process*"))
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
  (let* ((emacs-proc-dir "~/apps/emacs/.emacs_processes")
         (pid-string (int-to-string (emacs-pid)))
         (system-subdir (concat emacs-proc-dir "/" (system-name)))
         (pid-file (concat system-subdir "/" (int-to-string (emacs-pid)))))
    (esler-cleanup-emacs-process-registry)
    (if (file-exists-p pid-file)
        (delete-file pid-file))
    (if (file-directory-p system-subdir)
        (if (equal 2 (length (directory-files system-subdir)))
            (delete-directory system-subdir)))))

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
;;{{{  Configure the Lisp function "advice" facility.

;; Automatically start advice when this file gets loaded.
;;
;; obsolete ?(setq ad-start-advice-on-load t)

;; This enables me to forward advise functions, that is to advise them
;; when they are not yet defined or defined as autoloads.
;;
;; BUT... it seems to screw up byte-compilation, so
;; turn it off.
;;
;; obsolete ?(setq ad-activate-on-definition nil)

;;}}}

;;{{{  Load .emacs.custom

(if (and running-as-w32-client
         at-site-ibm)
    (setq custom-file "k:/apps/emacs/.emacs.custom")
  (setq custom-file "~/apps/emacs/.emacs.custom"))
(if (file-exists-p custom-file)
    (progn
      (load-file custom-file)
      (message "Loaded %s" custom-file)))

;;}}}

;;{{{  Useful commands and functions.

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

;;{{{  Command to set the mouse cursor shape.

(defun set-mouse-shape (form)
  "Sets the mouse shape to FORM.
    The names for the shapes are defined in x-win.el."
  (interactive
   (let (formato)
     (setq formato (completing-read "Shape: " obarray 'boundp t "x-pointer-"))
     (list (intern formato))))
  (setq x-pointer-shape (symbol-value form))
  (set-mouse-color (cdr (assq  'mouse-color (frame-parameters)))))

;;}}}

;;{{{  Commands for click-on-pathname.

(defun find-file-at-point (&optional other-frame)
  "Find the file whose name the cursor is over.
Ignores trailing '*' or '@' as in 'ls -F' output."
  (interactive)
  (let ((ange-ftp-file-name (extract-ange-ftp-file-name-around-point)))
    (if ange-ftp-file-name
        (if other-frame
            (find-file-other-frame ange-ftp-file-name)
          (funcall 'switch-to-buffer (find-file-noselect ange-ftp-file-name)))

      (let ((file-name (extract-file-name-around-point)))
        (if (file-exists-p file-name)
            (if other-frame
                (find-file-other-frame file-name)
              (funcall 'switch-to-buffer (find-file-noselect file-name)))
          (error "Cannot find file \"%s\"" file-name))))))

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

;; Stolen from the Hyperbole code.
;;
(defun extract-ange-ftp-file-name-around-point ()
  "Returns an ange-ftp pathname that point is within or nil.
See the 'ange-ftp' Elisp package for pathname format details.
Always returns nil if the ange-ftp package has not been loaded."
  (require 'ange-ftp)
  (if (featurep 'ange-ftp)
      (let ((user (if (stringp ange-ftp-default-user)
		      ange-ftp-default-user "anonymous"))
	    path)
	(setq path
	      (save-excursion
		(skip-chars-backward "^[ \t\n\"`'\(\{<")
		(cond

                 ;; WWW format:  ftp://[<user>@]<domain><path>
                 ;;
                 ((looking-at "ftp://\\([^@/:]+@\\)?\\([^/:@ \t\n\^M\"`']+\\)[^]@ \t\n\^M\"`'\)\}>]*")
                  (concat
                   "/"
                   ;; user
                   (if (match-beginning 1)
                       (buffer-substring (match-beginning 1) (match-end 1))
                     (concat user "@"))
                   ;; domain
		   (buffer-substring (match-beginning 2) (match-end 2))
		   ":"
		   ;; path
		   (buffer-substring (match-end 2) (match-end 0))))

                 ;; User, domain and path
                 ;;
                 ((looking-at "/?[^@/:]+@[^/:@ \t\n\^M\"`']+:[^]@ \t\n\^M\"`'\)\}]*")
                  (buffer-substring (match-beginning 0) (match-end 0)))

                 ;; @domain and path
                 ;;
                 ((looking-at "@[^/:@ \t\n\^M\"`']+:[^]@ \t\n\^M\"`'\)\}]*")
                  (concat "/" user (buffer-substring
                                    (match-beginning 0) (match-end 0))))
                 ;; Domain and path
                 ;; This matches the output of grep in shell buffers,
                 ;; so don't use it, with some loss of generality.
                 ;;
                 ;;((and (looking-at
                 ;;       "/?\\(\\([^/:@ \t\n\^M\"`']+\\):[^]@:, \t\n\^M\"`'\)\}]*\\)[] \t\n\^M,.\"`'\)\}]")
                 ;;      (setq path (buffer-substring
                 ;;                  (match-beginning 1) (match-end 1)))
                 ;;      (string-match "[^.]\\.[^.]"
                 ;;                    (buffer-substring (match-beginning 2)
                 ;;                                      (match-end 2))))
                 ;; (concat "/" user "@" path))

                 ;; Host and path
                 ;;
                 ((and (looking-at
                        "/\\([^/:@ \t\n\^M\"`']+:[^]@:, \t\n\^M\"`'\)\}]*\\)")
                       (setq path (buffer-substring
                                   (match-beginning 1) (match-end 1))))
                  (concat "/" user "@" path))
                 )))
	(if (and path (= ?. (aref path (1- (length path)))))
	    (substring path 0 -1)
	  path))))

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
  (end-of-buffer)
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
    (if (interactive-p)
        (message (int-to-string esler-maximum-line-length)))
    esler-maximum-line-length))

(defun buffer-width (begin end)
  "Find the width of the longest line in the buffer."
  (interactive "r")
  (let ((width (region-width (point-min) (point-max))))
    (if (interactive-p)
        (message (int-to-string width)))
    width))

(defun current-line-length ()
  (interactive)
  (- (line-end-position) (line-beginning-position)))

(defun next-long-line ()
  "Find the next line that is longer than permitted by Atria coding standards."
  (interactive)
  (while (and (<= (current-line-length) 79)
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
  (if (or at-site-ibm
          esler-small-screen)
      ;;"-*-Bitstream Vera Sans Mono-normal-r-*-*-12-90-96-96-c-*-iso8859-1"
      "Consolas-12"
    ;; This looks better on my laptop
    ;;
    ;;"-*-Bitstream Vera Sans Mono-normal-r-*-*-16-120-96-96-c-*-iso8859-1"))
      "Consolas-10"))

(if (and running-as-w32-client
         (not esler-xemacs))
    (set-default-font esler-w32-preferred-font))

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

;; Get a pencil-shaped mouse cursor.
;;
(if (and (not running-as-w32-client)
         (not esler-xemacs)
         (not running-as-terminal-client))
    (progn
      (setq x-sensitive-text-pointer-shape x-pointer-hand1)
      (setq x-pointer-shape x-pointer-pencil)))

(if (not esler-xemacs)
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


(if (and window-system
         (not esler-xemacs)
         (or esler-emacs21
             esler-emacs22)
             esler-emacs23)
    (progn
      (toggle-scroll-bar nil)

      (let ((alist (list
                    '(minibuffer . t)
                    '(auto-raise . nil)
                    '(auto-lower . nil)
                    '(vertical-scroll-bars . nil)
                    '(horizontal-scroll-bars . nil)
                    '(menu-bar-lines . 1)
                    ;; gainsboro, lavender, azure2, LemonChiffon2, beige, linen, light steel blue
                    '(background-color . "lavender")
                    '(mouse-color . "DarkGreen")
                    ;; firebrick3,sienna
                    '(cursor-color . "deep pink")
                    '(unsplittable . nil)

                    (if esler-small-screen
                        '(height . 45)
                      '(height . 60))
                    '(width . 79)

                    '(top . 0)
                    '(left . 85))))

        (if running-as-w32-client
            (setq alist (cons
                         `(font . ,esler-w32-preferred-font)
                         alist)))

        (setq default-frame-alist alist))))

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

(if (> emacs-major-version 19)
    (global-set-key "\e=" 'count-lines-region)
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

(if (not esler-xemacs)
    (progn
      (global-set-key [S-up] 'scroll-down-one-line)
      (global-set-key [S-down] 'scroll-up-one-line)))
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

;; Behaviour: Click on pathname.
;;
(if (and (not esler-xemacs)
         (not esler-emacs21)
         (not esler-emacs22)
         (not esler-emacs23))
    (progn
      (global-set-key [mouse-3] 'mouse19-global-mouse3-handler)
      (global-set-key [S-mouse-3] 'mouse19-global-mouse3-handler)
      (defun mouse19-global-mouse3-handler (click)

        "Find the file whose name the cursor is over.
This must be bound to a mouse click."

        (interactive "@e")
        (mouse-set-point click)
        (sit-for 0)
        (find-file-at-point (eq 'S-mouse-3 (car click))))

      ;; Behaviour: Resize panes by dragging the dividers.

      ;; Contributed by: philippe@cfmu.eurocontrol.be
      ;;
      ;;       (global-set-key [mode-line down-mouse-2]
      ;;                       'mode-line-resize-dynamically)

      ;;       (global-set-key [vertical-scroll-bar M-down-mouse-2]
      ;;                       'scroll-bar-resize-dynamically)

      ;;       (global-set-key [vertical-line down-mouse-2]
      ;;                       'scroll-bar-resize-dynamically)))

      ))
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
(if (not esler-xemacs)
    (defadvice mouse-drag-region (after do-kill-ring-save activate)
      (if mark-active
          (kill-new (buffer-substring (region-beginning) (region-end))))))

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
                             (if (running-off-usb-drive)
                                 "USB"
                               (substring system-name 0 index))
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
              ["Context" context at-site-home]
              ["Notes (work)" esler-edit-current-project-notes]
              ["Lore" esler-edit-lore]
              ["Creds" esler-edit-creds]
              "---------------------------------"
              ["Eclipse" esler-start-eclipse]
              ["Eclipse - 1gig" esler-start-eclipse-1gig]
              ["Eclipse -debug" esler-start-eclipse-debug]
              "---------------------------------"
              ["My Verizon storage" esler-dired-verizon-storage]
              "---------------------------------"
              ["VM" vm t]
              ["VM - local" esler-vm-get-local-mail t]
              ["GNUS" esler-gnus-new-frame t]
              ["Recover inbox" esler-recover-inbox]
              "---------------------------------"
              ["Start Gnuserv server" esler-start-gnuserv-server]
              ["Stop Gnuserv server"  esler-stop-gnuserv-server]
              "---------------------------------"
              ["Start Unison server" esler-start-unison-server]
              "---------------------------------"
              ["UCM Config Specs" esler-project-dired-ucm-cspecs]
              ["CCWeb: c:/winnt/java/packages/" esler-project-dired-java-packages]
              "---------------------------------"
              ["My ELisp" esler-project-dired-my-elisp]
              ["Emacs' ELisp" esler-project-dired-emacs-elisp]
              "---------------------------------"
              ["~/" esler-project-dired-homedir]
              ["Windows Profile Directory" esler-project-dired-windows-profile
               running-as-w32-client]
              ["~esler on Unix" esler-project-dired-unix-homedir
               running-as-w32-client]
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
              ["Microsoft Java SDK Docs" esler-read-ms-jdk-docs
               running-as-w32-client]
              ["Sun JDK Docs" esler-read-sun-jdk-docs
               running-as-w32-client]
              ["Insert ISO date" esler-insert-iso-date t]
              ["SlashDot.org" slashdot t]
              "---------------------------------"
              ["Windows Explorer"
               (start-process-shell-command "Windows Explorer"
                                            nil
                                            "explorer"
                                            ".,/e")
               :included running-as-w32-client
               ]
              ["Windows Shell"
               (w32-shell-execute "open" "cmd.exe")
               :included running-as-w32-client
               ]
              "---------------------------------"
              ["Browse maple repository"
               (dired "//maple/dfs")
               :included (and at-site-ibm running-as-w32-client)
               ]
              )))
(add-hook 'menu-bar-final-items 'KAE)


(defun esler-gnus-new-frame ()
  (interactive)
  (select-frame (make-frame '((name . "GNUS"))))
  ;;(height . 60)
  ;;(width . 70))))
  (gnus))

(defun esler-recover-inbox ()
  (interactive)
  (recover-file vm-primary-inbox))

(defun esler-dired-verizon-storage ()
  (interactive)
  (dired esler-verizon-storage-root))

(defun esler-read-ms-jdk-docs ()
  (interactive)
  (let ((file "C:/ms_java_sdk_4.0/Docs/sdkdocs.chm"))
    (w32-shell-execute "open" file)))

(defun esler-read-sun-jdk-docs ()
  (interactive)
  (cond
   ((file-exists-p "C:/j2sdk1.4.2/readme.html")
    (w32-shell-execute "open" "C:/j2sdk1.4.2/readme.html"))

   ((file-exists-p "C:/jdk1.3.1_01/readme.html")
    (w32-shell-execute "open" "C:/jdk1.3.1_01/readme.html"))

   ((file-exists-p "C:/jdk1.1.8/docs/index.html")
    (w32-shell-execute "open" "C:/jdk1.1.8/docs/index.html"))))

(defun esler-start-unison-server (port-number)
  "Start a Unison server listening on PORT-NUMBER. Pop to its buffer."
  (interactive "nPort number: ")
  (pop-to-buffer  (make-comint "unison-server"
                               (executable-find "unison")
                               nil
                               "-socket" (int-to-string port-number))))

;;}}}

;;{{{ Add an "Eclipse" menu bar entry

(add-hook 'menu-bar-final-items 'Eclipse)
(defvar esler-eclipse-workspace-directory "~/apps/eclipse/workspaces"
  "The directory where my Eclipse workspaces are catalogued.")

(defun esler-make-eclipse-workspace-menu (ignored)
  "Dynamically construct a menu keymap from the contents of my Eclipse workspace directory"

  (let ((keymap (make-sparse-keymap "Eclipse workspace")))

    ;; If the Eclipse workspace directory exists read its entries.
    ;;
    (let ((entries (if (file-directory-p esler-eclipse-workspace-directory)
                       (directory-sub-dirs esler-eclipse-workspace-directory))))

      ;; If it was empty create a dummy menu entry...
      ;;
      (if (null entries)
          (define-key keymap
            (vector (make-symbol "<empty>"))
            (cons "<empty>" (eval `(lambda ()
                                     (interactive)
                                     (message "%s not found or empty"
                                              esler-eclipse-workspace-directory)))))
        ;; ... otherwise, for each entry...
        ;;
        (let ((cursor entries))
          (while (not (null cursor))
            (let ((entry (car cursor)))
              ;; Skip entries beginning with "."
              ;;
              (if (not (equal ?. (aref entry 0)))
                  ;; Make a command for this keymap entry.
                  ;;
                  (let ((command (eval `(lambda ()
                                          (interactive)
                                          (esler-invoke-eclipse-on-workspace ,entry))))
                        (label entry))

                    ;; Bind the command to the keymap entry labelled with the
                    ;; name of the workspace.
                    ;;
                    (define-key keymap
                      (vector (make-symbol entry))
                      (cons label command)))))
            (setq cursor (cdr cursor)))))
      keymap)))


(defun esler-invoke-eclipse-on-workspace (ws)
  (interactive)
  (let* ((path (concat esler-eclipse-workspace-directory "/" ws))
         ;; Allow per-workspace choice of Eclipse version
         ;;
         (eclipse-program esler-default-eclipse-program)
         (link-path (concat path "/Eclipse.lnk")))
    (if running-as-w32-client
        (if (file-exists-p link-path)
            (setq eclipse-program (ls-lisp-parse-w32-lnk link-path))))
    (start-process "eclipse" nil
                   eclipse-program
                   "-showlocation"
                   "-data"
                   (expand-file-name path)
                   ;; Supply a healthy amount of memory
                   ;;
                   "-vmargs" "-Xmx512m")))


(easy-menu-define eclipse-menu
  (list global-map)
  "Eclipse menu"
  (list "Eclipse"
        :filter 'esler-make-eclipse-workspace-menu))

;;}}}

;;{{{ Add a "Mirrored" menu bar entry

(add-hook 'menu-bar-final-items 'Mirrored)
(defvar esler-mirrored-files-directory "~/AmazonArchivedDocuments/Cpt/")

(defun esler-make-mirrored-files-menu (ignored)
  "Dynamically construct a menu keymap from the contents of my mirrored files directory"

  (let ((keymap (make-sparse-keymap "Mirrored")))

    ;; If the directory exists read its entries.
    ;;
    (let ((entries (if (file-directory-p esler-mirrored-files-directory)
                       ;; Reverse it here so the final keypmap comes out in alphabetical order.
                       ;;
                       (nreverse
                        (directory-files esler-mirrored-files-directory nil ".*\\.cpt$")))))

      ;; If it was empty create a dummy menu entry...
      ;;
      (if (null entries)
          (define-key keymap
            (vector (make-symbol "<empty>"))
            (cons "<empty>" (eval `(lambda ()
                                     (interactive)
                                     (message "%s not found or empty"
                                              esler-mirrored-files-directory)))))
        ;; ... otherwise, for each entry...
        ;;
        (let ((cursor entries))
          (while (not (null cursor))
            (let* ((entry (car cursor))
                   (path (concat esler-mirrored-files-directory entry)))
              ;; Make a command for this keymap entry.
              ;;
              (let ((command (eval `(lambda ()
                                      (interactive)
                                      (find-file ,path))))
                    (label entry))

                ;; Bind the command to the keymap entry labelled with the
                ;; name of the workspace.
                ;;
                (define-key keymap
                  (vector (make-symbol entry))
                  (cons label command))))
            (setq cursor (cdr cursor))))))
    keymap))


(easy-menu-define mirrored-menu
  (list global-map)
  "Mirrored"
  (list "Mirrored"
        :filter 'esler-make-mirrored-files-menu))

;;}}}

;;{{{ Add an "EScripts" menu bar entry

;; NYI:
;;  - a mechanism to prompt for needed parameters to scripts
;;  - run .el scripts too
;;  - capture return code from script execution
;;  - capture and display stdout/stderr from script execution

(add-hook 'menu-bar-final-items 'EScripts)
(defvar esler-escripts-directory "~/apps/emacs/escripts"
  "The directory where my escripts are catalogued.")

(defun esler-make-escripts-menu (ignored)
  "Dynamically construct a menu keymap from the contents of my escripts directory"

  (let ((keymap (make-sparse-keymap "Escripts")))

    ;; Read the directory entries.
    ;;
    (let ((entries (if (file-directory-p esler-escripts-directory)
                       (directory-sub-nondirs esler-escripts-directory))))

      ;; If it was empty create a dummy menu entry...
      ;;
      (if (null entries)
          (define-key keymap
            (vector (make-symbol "<empty>"))
            (cons "<empty>" (eval `(lambda ()
                                     (interactive)
                                     (message "%s not found or empty"
                                              esler-escripts-directory)))))
        ;; For each entry...
        ;;
        (let ((cursor entries))
          (while (not (null cursor))
            (let ((entry (car cursor)))
              ;; Skip entries beginning with "."
              ;;
              (if (not (equal ?. (aref entry 0)))
                  ;; Make a command for this keymap entry.
                  ;;
                  (let ((command (eval `(lambda ()
                                          (interactive)
                                          (invoke-escript ,entry))))
                        (label entry))

                    ;; Bind the command to the keymap entry labelled with the name of the workspace.
                    ;;
                    (define-key keymap
                      (vector (make-symbol entry))
                      (cons label command)))))
            (setq cursor (cdr cursor)))))
      keymap)))


(defun invoke-escript (name)
  (interactive)
  (let ((path (concat esler-escripts-directory "/" name)))
    (start-process-shell-command "bash" nil
                                 ;; nyi: Generalise
                                 ;;
                                 cygwin-bash-location
                                 path)))


(easy-menu-define escripts-menu
  (list global-map)
  "EScripts Menu"
  (list "EScripts"
        :filter 'esler-make-escripts-menu))

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
    (mapcar (function
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

(defun esler-set-loadpath-emacs20 ()
  ""
  (setq load-path (append

                   ;; My primary library of Elisp.
                   ;;
                   (list (expand-file-name esler-elisp-directory))

                   ;; The lisp-containing directory for each package stored
                   ;; beneath a designated subdirectory of my primary
                   ;; library.
                   ;;
                   (mapcar 'esler-find-lisp-in-package
                           (esler-directory-subdirectories
                            (concat esler-elisp-directory "/Installed-packages")))

                   ;; The standard supplied Emacs code,
                   ;; without any site customisation, if possible.
                   ;;
                   (let
                       ((gnu-distributed-lisp-dir
                         (concat (file-name-directory (directory-file-name exec-directory))
                                 "lisp")))
                     (if (file-directory-p gnu-distributed-lisp-dir)
                         ;; NYI: DO this listinf of sub-dirs smarter.
                         ;;
                         (list gnu-distributed-lisp-dir
                               (concat gnu-distributed-lisp-dir "/calendar")
                               (concat gnu-distributed-lisp-dir "/emacs-lisp")
                               (concat gnu-distributed-lisp-dir "/emulation")
                               (concat gnu-distributed-lisp-dir "/gnus")
                               (concat gnu-distributed-lisp-dir "/international")
                               (concat gnu-distributed-lisp-dir "/language")
                               (concat gnu-distributed-lisp-dir "/mail")
                               (concat gnu-distributed-lisp-dir "/play")
                               (concat gnu-distributed-lisp-dir "/progmodes")
                               (concat gnu-distributed-lisp-dir "/term")
                               (concat gnu-distributed-lisp-dir "/textmodes"))

                       ;; They've really screwed things over here, so
                       ;; make do with the load-path they are inflicting on me.
                       ;;
                       load-path)))))

(defun esler-set-loadpath-emacs21 ()
  (setq load-path (append

                   ;; My primary library of Elisp.
                   ;;
                   (list (expand-file-name esler-elisp-directory))

                   ;; The lisp-containing directory for each package stored
                   ;; beneath a designated subdirectory of my primary
                   ;; library.
                   ;;
                   (mapcar 'esler-find-lisp-in-package
                           (esler-directory-subdirectories
                            (concat esler-elisp-directory "/Installed-packages")))

                   load-path)))

(defun esler-set-loadpath-emacs22 ()
  (esler-set-loadpath-emacs21))

(if esler-emacs20
    (esler-set-loadpath-emacs20))

(if esler-emacs21
    (esler-set-loadpath-emacs21))

(if esler-emacs22
    (esler-set-loadpath-emacs22))

(if esler-emacs23
    (esler-set-loadpath-emacs22))

;;}}}
;;{{{  Colour

;; Possible strategy: use font-lock or face-lock for source code.
;;
;; TODO:
;;  be able to adjust colours interactively, and on the fly.
;;  be able to do it on a per-mode-basis
;;

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

(if (> emacs-minor-version 29)
    (require 'facemenu))

;;}}}

;;{{{ Font-lock

(if (and window-system
         (not esler-xemacs)
         (not esler-emacs23))

    ;; Gnu Emacs
    ;;
    (progn
      (global-font-lock-mode 1)
      (if (fboundp 'jit-lock-mode)
          (jit-lock-mode t)
        (setq font-lock-support-mode 'lazy-lock-mode))

      (setq font-lock-maximum-decoration t)
      (set-face-background 'trailing-whitespace "bisque")
      (cond

       (at-site-ibm
        (progn
          (make-face 'comment)
          (set-face-foreground 'comment "DarkGreen")
          (setq font-lock-comment-face 'comment)
          (make-face 'string)
          (set-face-foreground 'string "Maroon")
          (setq font-lock-string-face 'string)
          (make-face 'type)
          (set-face-foreground 'type "DarkOliveGreen")
          (setq font-lock-type-face 'type)
          (make-face 'function)
          (set-face-foreground 'function "blue")
          (setq font-lock-function-name-face 'function)))

       (t nil))))

(if (and window-system
         (not esler-xemacs)
         (not esler-emacs21)
         (not esler-emacs22)
         (not esler-emacs23))
    (progn
      (global-font-lock-mode t)
      (setq font-lock-support-mode 'lazy-lock-mode)
      (setq font-lock-maximum-decoration t)

      ;; Get font-lock-controlling menu options beneath the Edit menu.
      ;;

      ;;  kae: 23.1
      ;;(require 'font-menus)

      (cond

       (at-site-ibm
        (progn
          (make-face 'comment)
          (set-face-foreground 'comment "DarkGreen")
          (setq font-lock-comment-face 'comment)
          (make-face 'string)
          (set-face-foreground 'string "Maroon")
          (setq font-lock-string-face 'string)
          (make-face 'type)
          (set-face-foreground 'type "DarkOliveGreen")
          (setq font-lock-type-face 'type)
          (make-face 'function)
          (set-face-foreground 'function "blue")
          (setq font-lock-function-name-face 'function)))

       (t nil))))

;;}}}

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

;;{{{  Portuguese character set

(if (> emacs-major-version 19)

    ;; Version 20.
    ;;
    (if running-as-w32-client

        ;; Win32, Emacs-20
        ;;
        (defun portuguese ()
          (interactive)
          (standard-display-european 1)
          (iso-accents-mode)
          (iso-accents-customize "portuguese"))

      ;; Unix, Emacs-20
      ;;
      (defun portuguese ()
        (interactive)
        (set-language-environment "Latin-1")
        (set-input-method "latin-1-prefix")))

  ;; Version 19.
  ;;
  (defun portuguese ()
    (interactive)
    (standard-display-european 1)
    (load-library "iso-syntax")
    (iso-accents-mode)
    (iso-accents-customize "portuguese")))

;;}}}

;;{{{  Configure MODES and packages.

;;{{{ NXHTML

;;(load "~/apps/emacs/elisp/Installed-packages/nxhtml-1.26-080325/nxml/autostart.el")

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

;;{{{ Eclipse launching

(defvar esler-default-eclipse-program
  (cond
   ((file-executable-p "c:/eclipse-3.0.1/eclipse.exe")
    "c:/eclipse-3.0.1/eclipse.exe")
   ((file-executable-p "c:/eclipse-3.0/eclipse.exe")
    "c:/eclipse-3.0/eclipse.exe")
   ((file-executable-p "c:/eclipse/eclipse.exe")
    "c:/eclipse/eclipse.exe")))

(defun esler-start-eclipse (heap-megabytes)
  "Start Eclipse with HEAP-MEGABYTES of heap"
  (interactive "nMegabytes of heap: ")
  (if (file-executable-p esler-default-eclipse-program)
      (start-process "eclipse" nil
                     esler-default-eclipse-program
                     "-showlocation"
                     ;; Supply a healthy amount of memory
                     ;;
                     "-vmargs" (format "-Xmx%dm" heap-megabytes))
    (error "%s not executable" esler-default-eclipse-program)))

(defun esler-start-eclipse-1gig ()
  (interactive)
  (esler-start-eclipse 1024))

(defun esler-start-eclipse-debug ()
  (interactive)
  (if (file-executable-p esler-default-eclipse-program)
      ;; For this to work on Windows it seems necessary to
      ;; have an intermediate shell:
      ;;
      ;; cmd /c eclipse.exe -debug
      (start-process "eclipse-debug" nil
                     ;; For some reason this dance was necessary:
                     ;;
                     "cmd" "/c"
                     esler-default-eclipse-program
                     "-debug"
                     "-showlocation"
                     ;; Supply a healthy amount of memory
                     ;;
                     "-vmargs" "-Xmx512m")
    (error "%s not executable" esler-default-eclipse-program)))

;;}}}

;;{{{ Worklog

;; (require 'worklog)
;; (setq worklog-automatic-login t)
;; (add-hook 'emacs-startup-hook
;;           (function (lambda ()
;;                       (condition-case err
;;                           (worklog-do-task "Hacking emacs" t)
;;                         (error nil)))))

;; (add-hook 'kill-emacs-hook
;;           (function (lambda ()
;;                       (condition-case err
;;                           (progn
;;                             (worklog-do-task "logout" t)
;;                             (worklog-finish))
;;                         (error nil)))))

;; (setq worklog-file "~/apps/emacs/.worklog")

;;}}}
;;{{{  Project

(require 'project)
(setq project-file "~/apps/emacs/project/log")

;;}}}

;;{{{  CEDET

;; Configuration variables here:
(setq semantic-load-turn-useful-things-on t)
;; Load CEDET
(load-file "~/apps/emacs/elisp/Installed-packages/cedet-1.0pre4/common/cedet.el")

;;}}}
;;{{{  Message Mode

(eval-after-load "message"
  (progn
    ;; nyi: doesn't work for some reason
    ;;
    (setq message-directory "~/apps/emacs/Message")
    (setq message-signature-file (concat message-directory "/.signature"))))

;;}}}

;;{{{  Eshell
(setq eshell-directory-name "~/apps/emacs/.eshell/")
;;}}}

;;{{{  Mirror

(require 'mirror)

;; Hook to avoid accidentally overwriting the wrong mirror.
;; Check that the mirror file name matches the original file name.
;;
(defun esler-check-mirror-filename ()
  (if (and mirror-file-path
           (not (equal (file-name-nondirectory mirror-file-path)
                       (file-name-nondirectory (buffer-file-name)))))
      (progn
        (setq mirror-file-path nil)
        (error "mirror-file-path appears to be set wrong: %s" mirror-file-path))))

(add-hook 'find-file-hooks 'esler-check-mirror-filename)

(defvar esler-remote-mirror-dir (concat esler-verizon-storage-root "/cpt/"))
(defun esler-standard-mirroring ()
  (setq mirror-update-on-save t)
  (setq mirror-file-path
        (concat esler-remote-mirror-dir
                (file-name-nondirectory buffer-file-name))))

;;}}}

;;{{{  IDO

;;(require 'ido)
;;(ido-mode t)

;;}}}

;;{{{  Table

;;(require 'table)
;;(add-hook 'text-mode-hook 'table-recognize)

;;}}}

;;{{{  Planner

(if (not esler-xemacs)
    (load "planner"))

;;}}}

;;{{{  Ibuffer

(autoload 'ibuffer "ibuffer" "" t)
(global-set-key "\C-x\C-b" 'ibuffer)
(defun esler-ibuffer-mode-bindings ()
  (define-key ibuffer-mode-map " " 'ibuffer-visit-buffer)
  (define-key ibuffer-mode-map "f" 'ibuffer-visit-buffer))

(setq ibuffer-formats '((mark modified read-only " " (name 25 25) " "
                              (size 6 -1 :right) " " (mode 16 16 :center)
                              " " (process 8 -1) " " filename)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

;; For some reason, this doesn't work: anymore:
;;
(eval-after-load "ibuffer" '(esler-ibuffer-mode-bindings))
(add-hook 'ibuffer-mode-hooks 'esler-ibuffer-mode-bindings)

;;}}}

;;{{{  Git-Emacs

(require 'git-emacs)

;;}}}
;;{{{  ClearCase Mode.

;; Load Clearcase/Eacs integration
;;
(setq clearcase-minimise-menus t)
(load "clearcase")
(autoload 'clearcase-dired-mode "clearcase" "" t)

;; Command to print the current ClearCase view tag.
;;
(defun pwv ()
  (interactive)
  "Print the working ClearCase view."

  (let ((tag (clearcase-viewtag)))
    (if tag
        (message tag))))

(setenv "ATRIA_NO_BOLD" "TRUE")

(setq clearcase-dired-show-view t)

;;}}}

;;{{{  MMM

;; When I'm editing HTML code, turn on MMM automatically
;; and allow me to edit imbedded Javascript in Java mode.
;;
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'html-helper-mode ".html" 'universal)

;;}}}

;;{{{  Ishl (highlight interactive searches)

(require 'ishl)
(ishl-mode 1)

;;}}}

;;{{{  ELL

(autoload 'ell-packages "ell" "Browse list of Emacs Lisp packages" t)

;;}}}

;;{{{  ESheet

(autoload 'esheet-mode "esheet" "Makes Emacs act like a spreadsheet" t)
(setq auto-mode-alist (cons (cons "\\.esh\\'" 'esheet-mode) auto-mode-alist))

;;}}}

;;{{{  VC

;; Make a VC menu-bar entry visible.
;;
                                        ; ; (require 'vc-hooks)
                                        ; ; (define-key global-map [menu-bar vc]
                                        ; ;   (cons "VC" vc-menu-map))
                                        ; ; (or (memq
                                        ; ;      'vc menu-bar-final-items)
                                        ; ;     (setq menu-bar-final-items
                                        ; ;           (cons
                                        ; ;            'vc menu-bar-final-items)))

;;}}}

;;{{{  Power-macros

(require 'power-macros)
;;(power-macros-mode)

;;}}}

;;{{{  Internet (mail, news, ...) packages

;;{{{  Sending mail.

;; Set my "Reply-to" address.
;;
(setq mail-default-reply-to
      (cond
       ;; Use forwarding from BU.
       ;;
       (at-site-home "kevin.esler.1989@alum.bu.edu")

       (at-site-ibm "kaesler@us.ibm.com")))


;; Get a copy of all sent mail
;;
(setq mail-self-blind t)

(setq mail-yank-prefix "    > ")

(add-hook 'mail-mode-hook

          ;; Make sure I'm on my home node.
          ;;
          '(lambda ()
             (if (not (executing-on-my-home-node))
                 (progn
                   (kill-buffer (current-buffer))
                   (error "This Emacs process is not on your home node.")))))

;; Get contents of my ~/.signature appended automatically.
;;
(setq mail-signature t)
(setq mail-signature-file "~/mail/.signature")

;; Turn on mail abbrevs.
;;
(defun esler-mail-setup-hook ()
  (mail-abbrevs-setup))

(add-hook 'mail-setup-hook 'esler-mail-setup-hook)

;;{{{  SMTP packages

(if running-as-w32-client
    (cond

     ;; Use bog-ordinary SMTP.
     ;;
     (at-site-ibm
      (progn
        (setq send-mail-function 'smtpmail-send-it)
        ;; nyi: no longer valid after IBM changes
        ;;
        (setq smtpmail-default-smtp-server "sus-ma1it00.rational.com")
        (setq smtpmail-smtp-service "smtp")
        (setq smtpmail-local-domain "Rational.com")
        (setq smtpmail-debug-info t)
        (load-library "smtpmail")
        (setq smtpmail-code-conv-from nil)))

     ;; Verizon insists on using authenticated SMTP.
     ;;
     (at-site-home
      (progn
        (setq send-mail-function 'esmtpmail-send-it)
        (setq esmtpmail-default-smtp-server "outgoing.verizon.net")
        (setq esmtpmail-local-domain "verizon.net")
        (load-library "esmtpmail")
        (setq esmtpmail-send-it-by-alist
              '(
                (t "outgoing.verizon.net" (vm-pop-login "incoming.verizon.net:110:pass:kevin.a.esler:*"))))))))

;;}}}

;;}}}
;;{{{  Reading mail (VM).

;; Designate VM as my mail reader and write.
;;
(setq read-mail-command 'vm)
(setq mail-user-agent 'vm-mail)

;;(setq vm-fill-paragraphs-containing-long-lines 70)
;;(setq vm-paragraph-fill-column 70)

;;; Since version 6.67, VM forces its buffers into unibyte mode.
;;; Unfortunately this prevents GNU Emacs to return sensible charsets
;;; when queried using FIND-CHARSET-REGION.  We circumvent this
;;; problem by adding an artificial mapping for the "unknown" charset
;;; to the one we use.
;;;
(eval-after-load "vm-mime"
  '(setq vm-mime-mule-charset-to-charset-alist
	 (cons (list 'unknown "iso-8859-1")
	       vm-mime-mule-charset-to-charset-alist)))

;; Risk using Emacs-20. I'm told it's safe if `--unibyte' is used.
;;
(eval-after-load "vm" '(defun vm-check-emacs-version () t))

;;{{{  Functions I wrote.

(defun my-vm ()
  (interactive)
  (frame-rename "VMail")
  (vm))

(defun esler-vm-junk-mail ()
  (interactive)
  (vm-save-message "~/mail/folders/saved/junk_mail"))

(defun esler-vm-summary-update-hook-function ()
  (if (and (boundp 'vm-mail-buffer)
           vm-mail-buffer)
      (save-excursion
        (set-buffer vm-mail-buffer)
        (hilit-rehighlight-buffer-quietly)))
  (hilit-rehighlight-buffer-quietly))

(defun vm-mark-all-messages-as-read ()
  "Mark all messages in the current folder as read."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mp vm-message-list))
    (while mp
      (vm-set-new-flag (car mp) nil)
      (vm-set-unread-flag (car mp) nil)
      (vm-mark-for-summary-update (car mp) t)
      (setq mp (cdr mp))))
  (vm-display nil nil '(vm-mark-all-messages-as-read)
	      '(vm-mark-all-messages-as-read marking-message))
  (vm-update-summary-and-mode-line))

;; A function to turn a VM folder into something that can be
;; conveniently printed.
;; Doesn't quite work yet: not all garbage lines look like a header.
;;
(defun esler-print-folder (folder-file)

  "Convert folder stored in FOLDER-FILE to printable form.
Only works for Unix-format folders."

  (interactive
   (list
    (read-file-name "Print folder: " vm-folder-directory t nil)))

  (setq folder-file (expand-file-name folder-file))
  (save-excursion
    (set-buffer (get-buffer-create "*print-folder-work-buffer*"))
    (erase-buffer)
    (insert-file-contents folder-file)
    (iterate-over-lines-in-region
     (point-min)
     (point-max)
     '(lambda ()
        (cond
         ((looking-at "^From ")
          (kill-line 1)
          (insert "\n"))
         ((looking-at "[A-Z][A-Za-z0-9--_]+:")
          (if (not (looking-at (concat
                                "^From"
                                "\\|^Date"
                                "\\|^To"
                                "\\|^Subject"
                                "\\|^Organization"
                                "\\|^Organisation"
                                "\\|^[Cc][Cc]"
                                "\\|^Newsgroups")))
              (kill-line 1))))))))

;; A function to remove "Mail" notification from mode line.
;;
(defun esler-clear-mail-from-mode-line ()
  (if (and (boundp 'display-time-string)
           display-time-string)
      (let ((index (string-match "Mail" display-time-string)))
        (if (numberp index)
            (progn
              (setq display-time-string
                    (substring display-time-string 0 index))

              ;; Now force updating of all buffers' mode lines.

              (save-excursion (set-buffer (other-buffer)))
              (set-buffer-modified-p (buffer-modified-p))

              ;; Do redisplay right now, if no input pending.

              (sit-for 0))))))

;; Function to print a mail message.
;;
(defvar esler-vm-print-message-command "mpage -2 | lp"
  "The most recent command I used to print a mail message from VM.")
(defun esler-vm-print-message (command)

  "Print the current MAIL message."

  (interactive (list (read-string "Command to print: " esler-vm-print-message-command)))

  ;; We could be in the summary buffer.
  ;; If so, temporarily go to the message buffer:
  ;; VM's invariant is that we are in the summary buffer iff:
  ;;       'vm-mail-buffer is bound and non-nil.
  ;;
  (if (and (boundp 'vm-mail-buffer)
           vm-mail-buffer)
      (save-excursion
        (set-buffer vm-mail-buffer)
        (shell-command-on-region (point-min) (point-max) command))
    (shell-command-on-region (point-min) (point-max) command))
  (setq esler-vm-print-message-command command))

;; Bound to "a".
;;
;; This function facilitates maintenance of my .mailrc file
;; of mail address abbreviations. It uses the 'mail-extr package for
;; extracting canonical email addresses from messages.
;;
(setq mail-personal-alias-file "~/mail/.mailrc")

(autoload 'mail-extract-address-components
  "mail-extr"
  "Function to extract stuff from mail address text")
(defun esler-vm-save-return-address-as-alias ()

  "A function to extract the sender's address from a mail buffer and append it to
my .mailrc file."

  (interactive)

  ;; Hop into the message buffer.
  ;;
  (let ((message-buffer (if (and (boundp 'vm-mail-buffer)
                                 vm-mail-buffer)
                            vm-mail-buffer
                          (current-buffer))))
    (save-excursion
      (set-buffer message-buffer)

      ;; Search for an appropriate line containing a reply address.
      ;;
      (goto-char 0)
      (let ((raw-address-string nil))
        (cond
         ((re-search-forward "^Reply-to:[ 	]*\\(.*\\)$" nil t)
          (setq raw-address-string (buffer-substring (match-beginning 1) (match-end 1))))
         ((re-search-forward "^From:[ 	]*\\(.*\\)$" nil t)
          (setq raw-address-string (buffer-substring (match-beginning 1) (match-end 1)))))

        ;; Extract a canonical address from it.
        ;; Then prompt the user for an alias string.
        ;;
        (if raw-address-string
            (let ((tuple (mail-extract-address-components raw-address-string)))
              (if tuple
                  (let* ((canonical-address (nth 1 tuple))
                         (alias (read-from-minibuffer (format "Alias for %s: " canonical-address)
                                                      nil
                                                      nil
                                                      nil)))
                    ;; Write out a new mail abbrev to .mailrc.
                    ;;
                    (save-excursion
                      (find-file mail-personal-alias-file)
                      (goto-char (point-min))
                      (let ((new-alias-string (concat "alias "
                                                      alias
                                                      " "
                                                      canonical-address)))
                        (insert new-alias-string "\n")
                        (save-buffer)
                        (kill-buffer (current-buffer))

                        ;; Display the alias we created.
                        ;;
                        (message "Created: %s" new-alias-string)))

                    ;; Update live mail alias data.
                    ;;
                    (build-mail-abbrevs mail-personal-alias-file))

                (error "'mail-extract-address-components failed to parse address")))

          (error "No address found"))))))

;; Bound to "o".
;;
(defun esler-vm-other-window ()

  "Go to other VM Mode window."

  (interactive)
  (other-window 1))

;; Bound to "=".
;;
(defun esler-vm-toggle-window-configuration ()

  "Toggle between one-window and two-window screen."

  (interactive)

  ;; If w don't have mutable window configured,
  ;; just defer to the normal VM Mode binding.
  ;;
  (if (not vm-mutable-windows)
      (vm-summarize)

    ;; If there are multiple windows, make one.
    ;;
    (if (not (one-window-p t))
        (delete-other-windows)

      ;; Depending on which VM buffer we're in (message or summary),
      ;; take the appropriate action to cause the other one to appear.
      ;;
      (cond
       ((eq major-mode 'vm-mode) (vm-summarize t))
       ((eq major-mode 'vm-summary-mode) (vm-scroll-forward))
       (t (error "Unknown VM-related major mode"))))))

;; Bound to "$"
;;
(defun esler-vm-goto-last-message ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-goto-message (length vm-message-list)))

;; Auto-save my BCC-ed mail to the appropriate archive box.
;;
(defun eslers-mail-archive-name ()
  (let* ((s (current-time-string))
	 (year  (substring s 20 24))
	 (month (substring s  4  7)))
    (concat "sent_in_" month "." year)))

;;}}}

(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)

;; Spam fighter:
;;
(autoload 'vm-forward-message-and-complain "vm-complain")
(setq vm-complain-message "Dear Sir/Madam,

I have received the following message which is either a Spam or an UCE.
Analyzing the various header information included, this message appears
to come from or to have been posted from your site.I would appreciate
if you could take any action to make it stop.

If I get other Spams/UCEs from your site or if you support such use of
the Internet, I will have to ask our Postmaster to filter all the
mails coming from your IP adress.

Thanks in advance,

Spam or UCE message follows:
")

;; Use this VM addon to respond to junk email.
;;
(autoload 'vm-forward-message-and-complain "vm-complain")
(setq vm-complain-nocomplain-domain
      (concat "\\("
              "[Aa]tria\\.com" "\\|"        ; My domain
              "[Pp]ure[Aa]tria\\.com" "\\|" ; My domain
              "cyberpromo\\.com" "\\|"      ; No comment
              "^\\([^.]+\\.\\)*[0-9]+\\.[^.]+$"	; 234234.com often used...
              "\\)"))
(setq vm-complain-stop-received-collect "rational\\.com")
(setq vm-complain-interactive t)

(if (not esler-xemacs)
    (define-key menu-bar-file-menu [rmail] '("Read Mail" . vm)))

;; Because setgid is not honoured across NFS links, I've made a local copy.
;;
(setq vm-movemail-program "movemail")

;; Do base64 encoding and decoding in C, not elisp.
;;
(setq vm-mime-base64-encoder-program "base64-encode")
(setq vm-mime-base64-decoder-program "base64-decode")

;; Indicate location of mailbox, and folders.
;;
(setq vm-folder-directory
      (cond
       ;; At work: keep folders on Unix for backup purposes, even if we're reading on NT.
       ;;
       ((and at-site-ibm running-as-w32-client) "k:/mail/folders/saved/")

       ;; At home:
       ;;
       ((and at-site-home running-as-w32-client) "~/mail/folders/saved/")

       (t "~/mail/folders/saved/")))

(setq vm-primary-inbox
      (cond
       ;; At work: keep folders on Unix for backup purposes, even if we're reading on NT.
       ;;
       ((and at-site-ibm running-as-w32-client) "k:/mail/folders/incoming/inbox")

       ;; At home:
       ;;
       ((and at-site-home running-as-w32-client) "~/mail/folders/incoming/inbox")

       (t "~/mail/folders/incoming/inbox")))

(let ((mail-dir (if (or (eq 'gnu/linux system-type)
                        (eq 'linux system-type)
                        (eq 'lignux system-type))
                    "/var/spool/mail"
                  "/usr/mail")))
  (setq vm-spool-files
        (cond
         (at-site-home (list
                        ;; Verizon.
                        ;;
                        ;;(concat "mail.verizon.net:110:pass:" esler-verizon-user-name ":*")))
                        (concat "incoming.verizon.net:110:pass:" esler-verizon-user-name ":*")))

         (at-site-ibm (list
                       (list vm-primary-inbox
                             ;; nyi: no longer valid after IBM changes.

                             ;; IMAP inbox at Rational:
                             ;;
                             (concat "imap:sus-ma1it01.rational.com:143:inbox:login:"
                                     (user-real-login-name) ":*")
                             (concat vm-primary-inbox ".crash"))
                       )))))

(defun esler-vm-get-local-mail ()
  ""
  (interactive)
  (let ((vm-spool-files (list (concat "/var/spool/mail" "/" (user-real-login-name)))))
    (vm)))

;; Formats.
;;
(setq vm-summary-format "%n %*%A %-20.20F %-3.3m %2d %4l/%-5c [%L]%I\"%s\"\n")
(setq vm-in-reply-to-format "%F's message of %m %d, %y %H (Re: %s)")
(setq vm-forwarding-subject-format "[%F: %s]")
(setq vm-included-text-prefix "  > ")

(setq vm-summary-uninteresting-senders
      (cond
       (at-site-ibm "^esler\\(@[Rr]ational.com\\)*")
       (at-site-home
        (concat
         "\\(^esler@ultranet\\.com.*\\)"
         "\\|"
         "\\(^kevin\\.esler\\.1989.*\\)"
         "\\|"
         "\\(^kevin\\.a\\.esler.*\\)"
         ))))

;; Variables controlling visibility of mail message headers.
;;
(setq vm-invisible-header-regexp
      (concat
       "^Path"
       "\\|^Message-ID"
       "\\|^Sender"
       "\\|^References"
       "\\|^Lines"
       "\\|^X-.*"
       "\\|^Approved"
       "\\|^Received"
       "\\|^Reply-To"
       "\\|^Errors-To"
       "\\|^Apparently-To"
       "\\|^Resent.*"
       "\\|^Return-Receipt-To"
       "\\|^Snarfer"
       ))
(setq vm-visible-headers '(
                           "^From"
                           "^Organization"
                           "^To"
                           "^Subject"
                           "^Newsgroups"
                           "^Date"
                           ))


(setq vm-forwarded-headers '(
                             "From"
                             "Organization"
                             "To"
                             "[Cc][Cc]"
                             "Subject"
                             "Newsgroups"
                             "Date"
                             "In-Reply-To"
                             ))

;; When forwarding, don't include useless headers:
;;
(setq vm-unforwarded-header-regexp
      (concat "Received"
              "\\|"
              "Snarfer"))

;; Frame/window preferences.
;;
(setq vm-mutable-windows t)
(setq vm-mutable-frames nil)
(setq vm-frame-per-folder t)
(setq vm-frame-per-edit t)
(setq vm-frame-per-composition t)
(setq vm-use-menus 1)

;; Miscellaneous config variables.
;;
(setq vm-mime-default-face-charsets
      '("us-ascii" "windows-1251" "windows-1252" "koi8-r" "X-roman8"))
(setq vm-inhibit-startup-message t)
(setq vm-startup-with-summary "neither t nor nil")
(setq vm-follow-summary-cursor t)
(setq vm-move-after-deleting 'except-at-end)
(setq vm-move-after-undeleting 'except-at-end)
(if (<= emacs-major-version 19)
    (setq vm-edit-message-mode 'indented-text-mode))
(setq vm-skip-deleted-messages nil)

(setq vm-mail-header-from
      (cond
       ;; Use forwarding from BU.
       ;;
       (at-site-home "kevin.esler.1989@alum.bu.edu")

       (at-site-ibm "kaesler@us.ibm.com")))

(setq vm-keep-sent-messages t)
(setq vm-check-folder-types t)
(setq vm-convert-folder-types t)
(setq vm-default-folder-type 'From_)
(setq vm-delete-after-saving t)
(setq vm-delete-after-bursting t)
(setq vm-delete-after-archiving t)
(setq vm-search-using-regexps t)
(setq vm-summary-show-threads t)
;;(setq vm-keep-crash-boxes "~/.vm-crash-boxes")

;; Send URLs to existing Netscape process.
;;
(if running-as-w32-client
    (progn
      (setq vm-url-browser 'esler-browse-url-with-default-browser)
      (setq vm-popup-menu-on-mouse-3 nil))
  (setq vm-url-browser 'vm-mouse-send-url-to-netscape))

(setq vm-url-search-limit 20000)

;; This helps prevent screwups due to misspelling folder names.
;;
(setq vm-confirm-new-folders t)

;; Save to a buffer if the folder is currently being visited,
;; otherwise append to the file on disk.
;;
(setq vm-visit-when-saving 0)

;; VM Mode key bindings.
;;
(defun esler-vm-mode-bindings ()

  ;; Make "J" save the message in junk_mail.
  ;;
  (define-key vm-mode-map "J" 'vm-forward-message-and-complain)

  ;; Make "." go to the top of the current message.
  ;;
  (define-key vm-mode-map "." 'vm-beginning-of-message)

  ;; Make "P" print the current message.
  ;;
  (define-key vm-mode-map "P" 'esler-vm-print-message)

  ;; Make "$" take me to the last message in the folder.
  ;;
  (define-key vm-mode-map "$" 'esler-vm-goto-last-message)

  ;; Make "a" save the address of the sender in a database.
  ;;
  (define-key vm-mode-map "a" 'esler-vm-save-return-address-as-alias)

  ;; Make "k" kill the thread, and then move.
  ;;
  (define-key vm-mode-map "k" 'esler-vm-kill-subject-and-move)

  ;; Make "o" put me in the other window.
  ;;
  (define-key vm-mode-map "o" 'esler-vm-other-window)

  ;; Make "=" toggle the screen layout between one and two windows.
  ;;
  (define-key vm-mode-map "=" 'esler-vm-toggle-window-configuration)

  ;; Arrow keys move the message pointer in the summary buffer.
  ;;
  (define-key vm-mode-map [up] '(lambda ()
                                  (interactive)
                                  (if (eq 'vm-summary-mode major-mode)
                                      (vm-previous-message)
                                    (previous-line 1))))
  (define-key vm-mode-map [down] '(lambda ()
                                    (interactive)
                                    (if (eq 'vm-summary-mode major-mode)
                                        (vm-next-message)
                                      (next-line 1))))

  ;; Get expunge-on-quit
  ;;
  (define-key vm-mode-map "q" '(lambda ()
                                 (interactive)
                                 (vm-expunge-folder)
                                 (vm-quit))))
(eval-after-load "vm" '(esler-vm-mode-bindings))

;; My VM mode hook.
;;
(add-hook 'vm-mode-hooks 'esler-vm-mode-hook)
(defun esler-vm-mode-hook ()
  (progn

    ;; Be extra safe with folders.
    ;;
    (make-local-variable 'file-precious-flag)
    (setq file-precious-flag t)

    ;; Make sure I'm on my home node.
    ;;
    (if (not (executing-on-my-home-node))
        (progn
          (kill-buffer (current-buffer))
          (error "This Emacs process is not on your home node.")))

    ;; Make the mail buffer read-only to prevent mistakes.
    ;;
    (setq buffer-read-only t)

    ;; Bind some keys
    ;; (Note: doing this within eval-after-load is not sufficient for
    ;; some reason.)
    ;;
    (esler-vm-mode-bindings)
    ))


(defun esler-vm-kill-subject-and-move ()
  (interactive)
  (vm-kill-subject)
  (let ((vm-skip-deleted-messages t))
    (vm-next-message)))

;; This association list  allows VM to guess an appropriate folder for saving
;; a message in.
;;
(setq vm-auto-folder-alist
      '(
        ("To"
         ("xml-interest" . "technology/xml"))
        ("CC"
         ("xml-interest" . "technology/xml"))
        ("Subject"
         ("\\[gnu.emacs.sources\\]" . "gnu/ehax"))
        ("Subject"
         ("\\[gnu.emacs.vm.bug\\]" . "gnu/ehax"))
        ("Subject"
         ("\\[gnu.emacs.vm.info\\]" . "gnu/ehax"))
        ("Subject"
         ("\\[gnu.emacs.gnus\\]" . "gnu/ehax"))
        ("Subject"
         ("\\[comp.emacs\\]" . "gnu/ehax"))
        ("To"
	 ("scheme48@.*" . "languages/scheme"))
        ("CC"
	 ("scheme48@.*" . "languages/scheme"))
        ("To"
	 ("scsh.*" . "languages/scheme"))
        ("CC"
	 ("scsh.*" . "languages/scheme"))
        ("To"
         ("ntemacs-users" . "gnu/ntemacs"))
        ("CC"
         ("ntemacs-users" . "gnu/ntemacs"))
        ("To"
         ("emacs-pretesters" . "emacs-pretesters"))
        ("CC"
         ("emacs-pretesters" . "emacs-pretesters"))
        ("To"
         ("jde" . "jde"))
        ("CC"
         ("jde" . "jde"))
        ("To"
         ("unison-users" . "unison"))
        ("CC"
         ("unison-users" . "unison"))
        ))

;; The following code makes sure that Emacs will refresh it's set
;; of mail address abbreviations, if the .mailrc file
;; has been written to (by the same Emacs process).
;;
(setq auto-mode-alist
      (cons '("\\.mailrc$" . mailrc-mode) auto-mode-alist))
(defun mailrc-mode ()
  (interactive)
  (fundamental-mode)
  (setq mode-name "Mailrc")
  (make-local-variable 'write-file-hooks)
  (add-hook 'write-file-hooks
            '(lambda ()
               (setq mail-abbrevs nil)
               (message "Mail aliases reset")
               nil))                    ; Must return nil for file to be written
  (message "Mail aliases will be reset automatically"))

(setq mail-yank-hooks nil)

;; Font lock setup for VM
;;
(setq vm-font-lock-words
      '(("^Subject: \\(.*\\)$" . font-lock-type-face)
        ("^Sender: \\(.*\\)$" . font-lock-reference-face)
        ("^From: \\(.*\\)" . font-lock-string-face)
        ("^To: \\(.*\\)" . font-lock-keyword-face)
        ("^CC: \\(.*\\)" . font-lock-keyword-face)
        ("^cc: \\(.*\\)" . font-lock-keyword-face)
        ("^Date: \\(.*\\)" . font-lock-reference-face)
        ("^[ \t]*[a-zA-Z]*[>|}].*[>|}].*[>|}].*$" . font-lock-variable-name-face)
        ("^[ \t]*[a-zA-Z]*[>|}].*[>|}].*$" . font-lock-reference-face)
        ("^[ \t]*[a-zA-Z]*[>|}].*" . font-lock-comment-face)
        ("^.*\\\[Click .*\\\]$" . font-lock-variable-name-face)
        ("\\(file\\|ftp\\|gopher\\|http\\|https\\|news\\|wais\\|www\\)://[^
\t\n\f\r\"|()]*[^ \t\n\f\r\"|.!?(){}]" . font-lock-string-face)
        )
      )

(defun vm-fontify ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(vm-font-lock-words t))
  (turn-on-font-lock))

;; Locate the images for VM's toolbar
;;
(setq vm-toolbar-pixmap-directory
      (concat (file-name-directory (locate-library "vm"))
              "pixmaps"))

;;}}}
;;{{{  Mailcrypt.

(if running-as-w32-client
    (progn
      (load-library "mailcrypt")
      (mc-setversion "gpg")))

(if (not running-as-w32-client)
    (progn
      (autoload 'mc-install-write-mode "mailcrypt" nil t)
      (autoload 'mc-install-read-mode "mailcrypt" nil t)

      ;;(setq mc-default-scheme mc-scheme-pgp)
      (setq mc-passwd-timeout 240)
      (setq mc-ripem-user-id (or (getenv "RIPEM_USER_NAME")
                                 (user-full-name)))
      (setq mc-pgp-always-sign t)

      (setq mc-always-replace nil)
      (setq mc-use-default-recipients nil)
      (setq mc-encrypt-for-me nil)

      (setq mc-pre-encryption-hook nil)
      (setq mc-post-encryption-hook nil)
      (setq mc-pre-decryption-hook nil)
      (setq mc-post-decryption-hook nil)

      ;; For sending messages:
      ;;
      (add-hook 'mail-mode-hook 'mc-install-write-mode)
      ;;(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
      (add-hook 'gnus-mail-forward-hook 'mc-install-write-mode)

      ;; For reading messages:
      ;;
      (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
      (add-hook 'vm-mode-hook 'mc-install-read-mode)))

;;}}}
;;{{{  MIME and associates.

;;{{{ Sending MIME messages.

;;(autoload 'mime-mode "mime" "Minor mode for editing MIME message." t)
;;
;;(setq vm-visible-headers
;;      (append vm-visible-headers
;;              '("Mime-Version:"
;;                "Content-Type:"
;;                "Content-Transfer-Encoding:")))
;;
;;(add-hook 'mail-mode-hook
;;          (function
;;           (lambda ()
;;             (mime-mode))))
;;(add-hook 'vm-mail-mode-hook
;;          (function
;;           (lambda ()
;;             (mime-define-menu-for-emacs19))))

;;}}}

;;}}}
;;{{{  Gnus.

(setq gnus-directory "~/apps/emacs/gnus")

;; Necessary with the server at Rational Lexington.
;; The default value, 'some, causes Gnus to hang.
;;
(setq gnus-read-active-file 1)

(setq gnus-select-method
      (cond
       (at-site-home '(nntp "news.verizon.net"))
       (at-site-ibm  '(nntp "news.verizon.net"))
       (t '(nntp "news"))))

;; Locate my .newsrc.
;;
(setq gnus-startup-file (concat gnus-directory "/.newsrc"))

;; Save articles in a format that is VM compatible.
;;
(setq gnus-default-article-saver 'gnus-summary-save-in-mail)

(setq gnus-article-save-directory
      (cond
       ;; Keep this on a backup-up Unix-resident disk for now.
       ((and at-site-ibm running-as-w32-client) "k:/mail/folders/saved")

       ;; At home:
       ;;
       ((and at-site-home running-as-w32-client) "~/mail/folders/saved")

       (t "~/mail/folders/saved")))

;; Subscription
;;
(setq gnus-check-new-newsgroups 'ask-server)
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically)

;;}}}

;;{{{  Message Mode (factored out of Gnus apparently)

;; The default is "~/Mail"
;;
(setq message-directory "~/Message")

;; This ensures that Gnus can forward mail correctly.
(if running-as-w32-client
    (progn
      (require 'esmtpmail)
      (setq message-send-mail-function 'esmtpmail-send-it)))

;; I prefer my signature at the end of the forwarded message.
;;
(setq message-signature-before-forwarded-message nil)

;; Try to get a forwarded-message-format that VM can unpack nicely.
;;
;; TODO: find the hook (message-send-hook ?) in which to convert
;; all "^--" to "^- -".
;;
(setq message-forward-start-separator
      "------- start of forwarded message (RFC 934 encapsulation) -------\n")
(setq message-forward-end-separator
      "------- end -------\n")

;;}}}
;;{{{  W3.

;; (if (not (and esler-emacs21 running-as-w32-client))

;;     ;; Note: the PROBLEMS files for Emacs-21 gives a patch to w3 which is said
;;     ;; to make it work on Emacs-21.1.  Problem is that it doesn't work on
;;     ;; Windows because Emacs-21.1 in Windows still has no image support.
;;     ;;
;;     (progn
;;       ;; Emacs/W3 Configuration
;;       ;;
;;       (require 'w3-auto "w3-auto")

;;       (if at-site-ibm
;;           (progn
;;             (setq url-proxy-services
;;                   '(("ftp"      . "gw:1001")
;;                     ("http"     . "gw:1001")
;;                     ("gopher"   . "gw:1001")
;;                     ("wais"     . "gw:1001")
;;                     ("no_proxy" . "^.*\\(atria\\|rational\\)\.com")))))

;;       ;; Various variables
;;       ;;
;;       (setq w3-reuse-buffers 'yes
;;             w3-temporary-directory "~/tmp"
;;             ;; From Howard Melman
;;             ;;
;;             ;;w3-default-homepage "file:/d:/howard/html/"
;;             w3-default-stylesheet "~/.w3/style.css"
;;             w3-hotlist-file "~/.w3/hotlist"
;;             url-privacy-level '(email os)
;;             url-keep-history t
;;             url-be-asynchronous t
;;             url-mime-language-string "en"
;;             url-mail-command (get mail-user-agent 'composefunc) ; should be default
;;             w3-delay-image-loads nil
;;             w3-min-img-size 5
;;             w3-user-colors-take-precedence t
;;             w3-user-fonts-take-precedence t)))

;;}}}
;;{{{  Browse-URL

;; Define some functions so I can use various browsers to display HTML.
;; (Windows only at this stage.)
;;
(if running-as-w32-client
    (if at-site-ibm
        (progn
          (defvar mozilla-location "C:/Program Files/Mozilla.org/Mozilla/mozilla.exe"))
      (progn
        (defvar mozilla-location "C:/Program Files/Mozilla.org/Mozilla/mozilla.exe"))))

(defun esler-browse-url-mozilla (url &rest args)
  (setq browse-url-generic-program mozilla-location)
  (let ((browse-url-generic-program mozilla-location)
        (browse-url-generic-args (list "-P" (user-login-name))))
    (apply 'browse-url-generic
           (list url))))

;; Define this function on Windows for invoking the default HTML display tool.
;;
(if running-as-w32-client
    (defun esler-browse-url-with-default-browser (url &rest args)
      (interactive "sURL: ")
      (w32-shell-execute "open" url)))

;; Set the default browser.
;;
(if running-as-w32-client
    (setq browse-url-browser-function 'esler-browse-url-with-default-browser)
  ;; On Unix, use Netscape.
  ;;
  (setq browse-url-browser-function 'browse-url-netscape))

;; Bind the browse-url commands to keys with the `C-c C-z' prefix
;; (as used by html-helper-mode).
;;
(global-set-key "\C-c\C-z." 'browse-url-at-point)
(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
(global-set-key "\C-c\C-zr" 'browse-url-of-region)
(global-set-key "\C-c\C-zu" 'browse-url)
(global-set-key "\C-c\C-zv" 'browse-url-of-file)
(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))

;; Use the Emacs w3 browser when not running under X11.
;;
;; (or (eq window-system 'x)
;;     (eq window-system 'w32)
;;     (setq browse-url-browser-function 'browse-url-w3))

;; To always save modified buffers before displaying the file in a browser.
;;
(setq browse-url-save-file t)

;; To get round the Netscape caching problem, you could EITHER have
;; write-file in html-helper-mode make Netscape reload the document:
;;
;;	(autoload 'browse-url-netscape-reload "browse-url"
;;	  "Ask a WWW browser to redisplay the current file." t)
;;	(add-hook 'html-helper-mode-hook
;;		  (lambda ()
;;		     (add-hook 'local-write-file-hooks
;;			       (lambda ()
;;				  (let ((local-write-file-hooks))
;;				    (save-buffer))
;;				  (browse-url-netscape-reload)
;;				  t)			; => file written by hook
;;			       t)))			; append to l-w-f-hooks
;;
;; OR have browse-url-of-file ask Netscape to load and then reload the
;; file:
;;
;;	(add-hook 'browse-url-of-file-hook 'browse-url-netscape-reload)

;; You may also want to customise browse-url-netscape-arguments, e.g.
;;	(setq browse-url-netscape-arguments '("-install"))
;;
;; or similarly for the other browsers.

;;}}}
;;{{{  Archie.

(autoload 'archie "archie" "Archie interface" t)

(setq archie-program "archie")

(setq archie-server "archie.ans.net")

;;}}}
;;{{{  Webjump

(autoload 'webjump "webjump" "Cause Web-browser to go to a specified site" t)
(global-set-key "\C-c\C-j" 'webjump)

;;}}}

;;{{{  Slashdot

(autoload 'slashdot
  "slashdot"
  "Read the headlines from http://slashdot.org" t)

;;}}}
;;{{{  Watson

(autoload 'watson
  "watson"
  "Do web searches" t)
;;}}}

;;}}}

;;{{{  HTML packages

;;{{{  Html-helper-mode

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

;; Kludges found necessary to get Version 3.0 to work:
;;
(defvar visual-basic-mode-hook nil)
(defvar html-helper-mode-uses-visual-basic nil)

;;}}}
;;{{{  Psgml -- for SGML and HTML docs.

;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )

;; (setq sgml-always-quote-attributes t)
;; (setq sgml-auto-insert-required-elements t)
;; (setq sgml-indent-data t)
;; (setq sgml-indent-step 2)
;; (setq sgml-auto-activate-dtd t)
;; (setq sgml-omittag nil)
;; (setq sgml-shorttag nil)
;; (setq sgml-set-face t)

;; ;; Faces.
;; ;;
;; (setq-default sgml-set-face t)

;; (make-face 'sgml-comment-face)
;; (make-face 'sgml-doctype-face)
;; (make-face 'sgml-end-tag-face)
;; (make-face 'sgml-entity-face)
;; (make-face 'sgml-ignored-face)
;; (make-face 'sgml-ms-end-face)
;; (make-face 'sgml-ms-start-face)
;; (make-face 'sgml-pi-face)
;; (make-face 'sgml-sgml-face)
;; (make-face 'sgml-short-ref-face)
;; (make-face 'sgml-start-tag-face)

;; (set-face-foreground 'sgml-comment-face "dark turquoise")
;; (set-face-foreground 'sgml-doctype-face "red")
;; (set-face-foreground 'sgml-end-tag-face "blue")
;; (set-face-foreground 'sgml-entity-face "magenta")
;; (set-face-foreground 'sgml-ignored-face "gray40")
;; (set-face-background 'sgml-ignored-face "gray60")
;; (set-face-foreground 'sgml-ms-end-face "green")
;; (set-face-foreground 'sgml-ms-start-face "yellow")
;; (set-face-foreground 'sgml-pi-face "lime green")
;; (set-face-foreground 'sgml-sgml-face "brown")
;; (set-face-foreground 'sgml-short-ref-face "deep sky blue")
;; (set-face-foreground 'sgml-start-tag-face "dark green")

;; (setq-default sgml-markup-faces
;;               '((comment . sgml-comment-face)
;;                 (doctype . sgml-doctype-face)
;;                 (end-tag . sgml-end-tag-face)
;;                 (entity . sgml-entity-face)
;;                 (ignored . sgml-ignored-face)
;;                 (ms-end . sgml-ms-end-face)
;;                 (ms-start . sgml-ms-start-face)
;;                 (pi . sgml-pi-face)
;;                 (sgml . sgml-sgml-face)
;;                 (short-ref . sgml-short-ref-face)
;;                 (start-tag . sgml-start-tag-face)))


;; ;; You need these EVs set too:
;; ;;   setenv SGML_CATALOG_FILES CATALOG:~/library/sgml/CATALOG:/usr/local/lib/sgml/CATALOG
;; ;;   setenv SGML_PATH "%S:~/library/sgml/%o/%c/%d:/usr/local/lib/sgml/%o/%c/%d"

;;}}}

;; HTML Helper Mode seems to work better than Psgml for now for HTML.
;;
(defvar esler-use-html-helper-mode-for-html nil)

(if esler-use-html-helper-mode-for-html
    (progn
      (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
      (setq auto-mode-alist (cons '("\\.htm$"  . html-helper-mode) auto-mode-alist)))
  (progn
    (setq auto-mode-alist (cons '("\\.html$" . sgml-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.htm$"  . sgml-mode) auto-mode-alist))))

(defun esler-html-comment ()
  (interactive)
  (if (not mark-active)
      (save-excursion
        (insert "<!-- \n-->\n"))
    (let ((start (min (point) (mark)))
          (end (max (point) (mark))))
      (save-excursion
        (goto-char end)
        (if (looking-at "\n")
            (forward-line 1))
        (insert "-->\n")
        (goto-char start)
        (beginning-of-line)
        (insert "<!-- \n")))))

(defun esler-html-script-narrow-to-script ()
  "Narrows to a script"
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (search-backward-regexp "<script.*>")
      (goto-char (match-end 0))
      (if (looking-at "\n")
          (forward-line 1))
      (let ((beg (point)))
        (search-forward "</script>" nil t)
        (goto-char (match-beginning 0))
        (narrow-to-region beg (point))))))

(defun esler-html-edit-javascript ()
  "Edit the script where the cursor is, in an indirect buffer using C Mode."

  (interactive)
  (if (not (eq major-mode 'html-helper-mode))
      (error "Wrong major mode"))

  ;; Make name for an indirect buffer
  ;;
  (let* ((indirect-buffer-name (concat (buffer-name) "-script-edit"))
         ;; Make the indirect buffer
         ;;
         (indirect-buffer (make-indirect-buffer (current-buffer) indirect-buffer-name)))

    (save-excursion
      (set-buffer indirect-buffer)
      ;; Narrow to script
      ;;
      (esler-html-script-narrow-to-script)
      ;; Turn on Java mode
      ;;
      (java-mode))

    ;; Make the buffer current.
    ;;
    (pop-to-buffer indirect-buffer t)))

(defun esler-html-send-buffer-to-netscape4 ()
  (interactive)
  (let ((browse-url-browser-function 'esler-browse-url-netscape4))
    ;; browse-url-of-file doesn't work for Netscape4 on Windows.
    ;; It produces "file:z:/foo/bar/x.html" whcih NN4 can't parse.
    ;; This seems to work.
    ;;
    (browse-url (buffer-file-name))))

(defun esler-html-send-buffer-to-netscape6 ()
  (interactive)
  (let ((browse-url-browser-function 'esler-browse-url-netscape6))
    (browse-url-of-file)))

(defun esler-html-send-buffer-to-mozilla ()
  (interactive)
  (let ((browse-url-browser-function 'esler-browse-url-mozilla))
    (browse-url-of-file)))

(defun esler-html-send-buffer-to-ie5 ()
  (interactive)
  (let ((browse-url-browser-function 'esler-browse-url-with-default-browser))
    (browse-url-of-file)))

(setq html-helper-user-menu
      '(
        ["Comment" esler-html-comment t]
        ["Edit Javascript" esler-html-edit-javascript t]
        ["Send to IE5"  esler-html-send-buffer-to-ie5 t]
        ["Send to Netscape6"  esler-html-send-buffer-to-netscape6 t]
        ["Send to Mozilla"  esler-html-send-buffer-to-mozilla t]
        ["Send to Netscape4"  esler-html-send-buffer-to-netscape4 t]
        ))

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

  ;; Follow pushd and popd.
  ;;
  (setq shell-pushd-regexp "pushd\\|pd")
  (setq shell-popd-regexp "popd\\|po")

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
;;{{{  Winterp interface.

(autoload 'run-winterp "win-cmulisp" "Interfade to Winterp" t)

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

;;{{{ Scala and yasnippet

(require 'scala-mode-auto)
(setq scala-interpreter "c:/scala-2.7.5/bin/scala.bat")
(require 'yasnippet)
(setq yas/my-directory "~/apps/emacs/snippets")
(yas/initialize)
(yas/load-directory yas/my-directory)
(add-hook 'scala-mode-hook
            '(lambda ()
               (yas/minor-mode-on)
               (scala-electric-mode t)
               (kae-extend-scala-menu)
               ))

(defvar esler-scala-book-path "~/library/src/OpenSrc/scala/book/ProgInScala1EdV6.pdf")

(if (and (file-exists-p esler-scala-book-path)
         (fboundp 'esler-dired-launch-file))
    (add-hook 'scala-mode-hook
              '(lambda ()
                 (kae-extend-scala-menu))))

(defun kae-extend-scala-menu ()
  (let ((menu (lookup-key scala-mode-map [menu-bar scala])))
    (define-key-after menu [browse-book]
      '("Browse Scala book" . kae-browse-scala-book)
      'browse-api)))


(defun kae-browse-scala-book ()
  (interactive)
  (w32-shell-execute "open"
                     (esler-w32-canonicalize-path-seps
                      (expand-file-name esler-scala-book-path))))

;;}}}

;;{{{ Groovy

(autoload 'groovy-mode "groovy-mode"
  "Mode for editing groovy source files" t)
(setq auto-mode-alist
      (append '(("\\.groovy$" . groovy-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("groovy" . groovy-mode))
                                     interpreter-mode-alist))

(autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)
             ))

;;}}}
;;{{{ ECB

(if (and
     (not esler-xemacs))
    (progn
      (require 'ecb)
      (setq ecb-tip-of-the-day nil)
      (setq ecb-major-modes-activate
            'none)))

;;}}}
;;{{{ CamelCase

(autoload 'camelCase-mode "camelCase-mode" nil t)
(add-hook 'java-mode-hook '(lambda () (camelCase-mode 1)))

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
;;{{{  Haskell Mode

(load-library "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;}}}
;;{{{  Smalltalk Mode
(setq auto-mode-alist
      (append  '(("\\.st$" . smalltalk-mode))
     	       auto-mode-alist))
(autoload 'smalltalk-mode "st" "" t)

;;}}}
;;{{{  Python Mode

(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

;; Load the pyimenu index function
;;
(autoload 'imenu-example--create-python-index "pyimenu")

;; Add the index creation function to the python-mode-hook
;;
(add-hook 'python-mode-hook
	  (function
	   (lambda ()
	     (setq imenu-create-index-function
		   (function imenu-example--create-python-index)))))

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
;;{{{ The Atria style

;; This now includes some support for MFC boilerplate.
;;
(defconst atria-c-style-description
  '(
    "cc-mode" ;; Super-style
    (c-electric-pound-behavior    . (alignleft))
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
                        (inline-open . 0)
                        (func-decl-cont . esler-align-mfc-end-message-map)
                        (label . esler-align-mfc-special-labels)
                        (statement-case-intro . 2)
                        (substatement-open . 0)
                        ))))

;;}}}
;;{{{ The MFC style

;; An indenting style suited for MFC code:
;;
;;   1. we align END_MESSAGE_MAP with corresponding BEGIN_MESSAGE_MAP.
;;   2. don't indent the special labels within class declarations:
;;      private, protected, public.
;;
;; Todo:
;;   More drastic measure seem to be needed to make BEGIN/END_MESSAGE_MAP be treated
;;   like top-level braces.
;;   DECLARE_MESSAGE_MAP () at the end of a class decl upsets indentation of closing brace
;;   for decl.

(setq c-label-minimum-indentation 0)

(defvar mfc-c-style-description
  '(
    "cc-mode" ;; Super-style
    (c-electric-pound-behavior    . (alignleft))
    (c-basic-offset . 4)
    (c-hanging-braces-alist . (
                               (block-open. (after))
                               (brace-list-open . (after))
                               (class-open . (after))
                               (defun-open . (after))
                               (substatement-open . (after))
                               ))
    (c-offsets-alist . (
                        (statement-cont . 0)
                        (func-decl-cont . esler-align-mfc-end-message-map)
                        (arglist-intro . +)
                        (arglist-cont . 0)
                        (arglist-cont-nonempty . +)
                        (arglist-close . 0)
                        (label . esler-align-mfc-special-labels)
                        (case-label . 0)
                        (statement-case-intro . +)
                        (substatement-open . 0)
                        ))))

(defun esler-align-mfc-end-message-map (langelem)
  (save-excursion
    (let ((where-we-were (point))
          ;; NB: this moves point:
          ;;
          (langelem-col (c-langelem-col langelem)))
      (goto-char where-we-were)
      (back-to-indentation)
      (if (looking-at "END_MESSAGE_MAP")
          0

        ;; We're not at an END_MESSAGE_MAP macro so just do what we normally
        ;; to for func-decl-cont, i.e. a c-basic-offset.
        ;;
        c-basic-offset))))

(defun esler-align-mfc-special-labels (langelem)
  (save-excursion
    (let ((where-we-were (point))
          ;; NB: this moves point:
          ;;
          (langelem-col (c-langelem-col langelem)))
      (goto-char where-we-were)
      (back-to-indentation)
      (if (looking-at "private:\\|protected:\\|public:")
          ;; For some reason, this doesn't quite work.
          ;; CC Mode seems to insist on a minimum of 1 here.
          ;;
          0
        2))))

;;}}}

(require 'cc-mode)
(c-add-style "esler" esler-c-style-description)
(c-add-style "atria" atria-c-style-description)
(c-add-style "mfc" mfc-c-style-description)

(defun esler-file-seems-to-be-MFC ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[aA][fF][xX]" nil t)))

;; Function to decide what style to use.
;;
(defun esler-c-choose-style ()
  (let ((result "esler"))
    (let ((file-name (buffer-file-name)))
      (if (and file-name
               (string-match "^/vobs/.*" file-name))
          (setq result "atria"))
      (if (and at-site-ibm running-as-w32-client
               file-name)
          (setq result "atria")))
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
  (if (and at-site-ibm running-as-w32-client)
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
  (c-set-style (esler-c-choose-style))
  (if (string-equal c-indentation-style "atria")
      (setq fill-column 79))
  (if (string-equal c-indentation-style "mfc")
      (setq tab-width 4)))

;; Customizations common to C, C++, Java.
;;
(defun esler-c-mode-bindings ()

  ;; Get a back button.
  ;;
  (define-key c-mode-map [mouse-3]  'msb)

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
  ;; On the other hand, most ClearCase core code
  ;; needs tab-width of 8.
  ;;
  (setq tab-width 8)
  (if running-as-w32-client
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
(if (and running-as-w32-client
         (not esler-xemacs))
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

;; Enable JDE
;;
;;(require 'jde)

;; JDE seems to use the wrong name for this Emacs-21 variable.
;;
;; kae
;;(defvar browse-url-new-window-p browse-url-new-window-flag)

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
;;{{{  Javascript support

;; For now, we use a slightly modified Java Mode.
;;
(setq auto-mode-alist (cons '("\\.js" . java-mode) auto-mode-alist))

(defconst atria-javascript-style-description
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
                        (lambda-intro-cont . 0) ;; NB
                        (statement-case-intro . 2)
                        (substatement-open . 0)
                        (topmost-intro-cont . 0)
                        ))))
(c-add-style "atria-javascript" atria-javascript-style-description)

(defun esler-javascript-mode-hook ()
  (if (and (buffer-file-name)
           (string-match "\\.js$" (buffer-file-name)))
      (progn
        (c-set-style "atria-javascript")
        (make-local-variable 'c-lambda-key)
        (setq c-lambda-key "\\<function\\>")
        (setq show-trailing-whitespace t)
        (setq fill-column 79))))

(add-hook 'java-mode-hook 'esler-javascript-mode-hook t)

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
;;{{{  Fortran Mode.

(add-hook 'fortran-mode-hook
          '(lambda ()
             ;; So that tags searching works easier
             (setq case-fold-search t)))

;;}}}
;;{{{  Caml/Tuareg Mode

(setq auto-mode-alist (cons '("\\.ml[iylp]?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'tuareg-run-caml "tuareg" "Run an inferior Caml process." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger." t)

(setq tuareg-indent-leading-comments t)

;;}}}
;;{{{  SML Mode

(autoload 'sml-mode "sml-mode"
  "Mode for editing, compiling and running Standard ML programs"
  t)

(autoload 'sml-shell "sml-mode"
  "Command for invoking the Standard ML interpreter/compiler"
  t)

(autoload 'cmusml "cmusml"
  "Command for invoking the Standard ML interpreter/compiler;
uses CMU comint code"
  t)

(setq auto-mode-alist (cons '("\\.sml$" . sml-mode) auto-mode-alist))

(eval-after-load "sml-mode" '(require 'sml-font))
(setq sml-hilite nil)                   ; Turn off highlighting based on hilit19
(add-hook 'sml-mode-hook 'turn-on-font-lock)

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

(setq scheme-program-name (if running-as-w32-client
                              "c:/cygwin/usr/local/bin/scsh"
                            "scsh"))
(defun run-scsh ()
  (interactive)
  (let ((scheme-program-name (if running-as-w32-client
                                 "c:/cygwin/usr/local/bin/scsh"
                               "scsh")))
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
;;{{{  Imenu package.

(define-key global-map "\C-cj" 'imenu) ;; Or some other key

;; Clashed with Hyperbole
;;
;;(cond (window-system
;;       (define-key global-map [S-down-mouse-3] 'imenu)))
(define-key global-map
  [menu-bar search index]
  '("Index source file" . imenu))

(setq imenu-max-items 300)

;; Scheme support.
;;
(defvar imenu-example--function-name-regexp-scheme
  (concat
   ;;"^[ \t]*(def\\(ine\\|macro\\)[ \t\n]*\\(\\w\\|[:><\\-]\\)+"

   "^[ \t]*("
   "\\("
   "define"
   "\\|"
   "defmacro"
   "\\|"
   "\\(define-[^ \t\n]\\)"
   "\\)"
   "[ \t\n]*\\(\\w\\|[:><\\-]\\)+"
   )
  "")

(setq imenu-example--function-name-regexp-scheme
      (concat
       "^[ \t]*("
       "\\("
       "define"
       "\\|"
       "defmacro"
       "\\|"
       "\\(define-[^ \t\n]\\)"
       "\\)"
       "[ \t\n]*\\(\\w\\|[:><\\-]\\)+"
       ))
(defun ktest ()
  (looking-at imenu-example--function-name-regexp-scheme))

(defun imenu-example--create-scheme-index (&optional regexp)
  "create imenu index for scheme"
  (let ((index-alist '())
	(char)
        (prev-pos))
    (goto-char (point-min))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (save-match-data
      (while (re-search-forward
	      (or regexp imenu-example--function-name-regexp-scheme)
	      nil t)
	(imenu-progress-message prev-pos nil t)
                                        ;(backward-up-list 1)
        (save-excursion
	  (goto-char (scan-sexps (point) 1))
	  (setq char (following-char))
	  )
	(push (imenu-example--name-and-position) index-alist)
	)
      (imenu-progress-message prev-pos 100)
      (nreverse index-alist))))

(add-hook 'scheme-mode-hook
 	  (function
 	   (lambda ()
 	     (setq imenu-create-index-function
 		   (function imenu-example--create-scheme-index)))))

;; The above doesn't work too well.
;;
;; Might be better off to try to modify the code which works for ELisp.
;; Categories:
;;    -defines
;;    -define-syntax
;;    -define-method
;;    -define-structure...
;;    -other
;;

(add-hook 'c-mode-hook
 	  (function
 	   (lambda ()
 	     (setq imenu-create-index-function
 		   (function imenu-example--create-c-index)))))

(add-hook 'emacs-lisp-mode-hook
 	  (function
 	   (lambda ()
 	     (setq imenu-create-index-function
 		   (function imenu-example--create-lisp-index)))))

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

(if (and running-as-w32-client
         (not esler-xemacs))
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
;;{{{  Speedbar tool

;;(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;;(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
;;
;;(define-key-after (lookup-key global-map [menu-bar Shortcuts])
;;  [speedbar] '("Speedbar" . speedbar-frame-mode) [Links])
;;(define-key global-map [f4] 'speedbar-get-focus)

;;}}}

;;{{{  OOBR

(defvar br-directory (concat esler-elisp-directory "/OOBR/")
  "Directory where the OO-Browser executable code is kept.
It must end with a directory separator character.")

(autoload 'oo-browser (expand-file-name "br-start" br-directory)
  "Invoke the OO-Browser" t)
(autoload 'br-env-browse (expand-file-name "br-start" br-directory)
  "Browse an existing OO-Browser Environment" t)

;;(global-set-key "\C-c\C-o" 'oo-browser)

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
;;{{{  Id-select

(autoload 'id-select-and-kill-thing
  "id-select" "Kill syntactical region selection" t)
(autoload 'id-select-and-copy-thing
  "id-select" "Select and copy syntactical region" t)
(autoload 'id-select-double-click-hook
  "id-select" "Double mouse click syntactical region selection" nil)
(autoload 'id-select-thing
  "id-select" "Keyboard-driven syntactical region selection" t)
(autoload 'id-select-thing-with-mouse
  "id-select" "Single mouse click syntactical region selection" t)

;; The following caused cc-mode-5.21 to loop when entering the opening brace
;; of a function.
;;(add-hook 'java-mode-hook
;;          (function
;;           (lambda ()
;;             (setq defun-prompt-regexp
;;                   "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*"))))
;;
;;(add-hook 'c++-mode-hook
;;          (function
;;           (lambda ()
;;             (setq defun-prompt-regexp
;;		   "^[ \t]*\\(template[ \t\n\r]*<[^>;.{}]+>[ \t\n\r]*\\)?\\(\\(\\(auto\\|const\\|explicit\\|extern[ \t\n\r]+\"[^\"]+\"\\|extern\\|friend\\|inline\\|mutable\\|overload\\|register\\|static\\|typedef\\|virtual\\)[ \t\n\r]+\\)*\\(\\([[<a-zA-Z][]_a-zA-Z0-9]*\\(::[]_a-zA-Z0-9]+\\)?[ \t\n\r]*<[_<>a-zA-Z0-9 ,]+>[ \t\n\r]*[*&]*\\|[[<a-zA-Z][]_<>a-zA-Z0-9]*\\(::[[<a-zA-Z][]_<>a-zA-Z0-9]+\\)?[ \t\n\r]*[*&]*\\)[*& \t\n\r]+\\)\\)?\\(\\(::\\|[[<a-zA-Z][]_a-zA-Z0-9]*[ \t\n\r]*<[^>;{}]+>[ \t\n\r]*[*&]*::\\|[[<a-]_~<>a-zA-Z0-9]*[ \t\n\r]*[*&]*::\\)[ \t\n\r]*\\)?\\(operator[ \t\n\r]*[^ \t\n\r:;.,?~{}]+\\([ \t\n\r]*\\[\\]\\)?\\|[_~<a-zA-Z][^][ \t:;.,~{}()]*\\|[*&]?\\([_~<a-zA-Z][_a-zA-Z0-9]*[ \t\n\r]*<[^>;{}]+[ \t\n\r>]*>\\|[_~<a-zA-Z][_~<>a-zA-Z0-9]*\\)\\)[ \t\n\r]*\\(([^{;]*)\\(\\([ \t\n\r]+const\\|[ \t\n\r]+mutable\\)?\\([ \t\n\r]*[=:][^;{]+\\)?\\)?\\)\\s-*"))))

;;}}}
;;{{{  Igrep

(autoload (function igrep) "igrep"
  "*Run `grep' to match EXPRESSION in FILES..." t)
(autoload (function egrep) "igrep"
  "*Run `egrep'..." t)
(autoload (function fgrep) "igrep"
  "*Run `fgrep'..." t)
(autoload (function igrep-recursively) "igrep"
  "*Run `grep' recursively..." t)
(autoload (function egrep-recursively) "igrep"
  "*Run `egrep' recursively..." t)
(autoload (function fgrep-recursively) "igrep"
  "*Run `fgrep' recursively..." t)

;;}}}
;;{{{  Uniquify

;; This seems to break on 19.34.
;;
;;(require 'uniquify)

;;}}}
;;{{{  Framepop

;; (if window-system
;;     (progn
;;       ;;(require 'framepop)
;;       (load-library "framepop")
;;       (setq framepop-auto-resize t)
;;       (define-key global-map [f3] framepop-map)
;;       (setq framepop-frame-parameters
;;             '((name . nil)                     ; use buffer name
;;               (unsplittable . t)               ; always include this
;;               (menu-bar-lines . 0)             ; no menu bar
;;               (minibuffer . nil)               ;    or minubuffer
;;               (left . -1)                      ; top right corner of screen,
;;               (top . 30)                       ;    away from my main frame
;;               (width . 71)                     ; narrower, so it fits nicely
;;               (background-color . "White")
;;               (foreground-color . "Red")
;;               (font . "-*-courier-bold-o-*-*-12-*-*-*-m-*-*-*")))))

;;}}}
;;{{{  Command-other-frame

(if (and window-system
         (not esler-xemacs))
    (progn
      (require 'command-other-frame)
      (autoload 'command-other-frame "command-other-frame"
        "Run any command in another frame" t)

      (defadvice mail (around make-separate-frame activate)
        "Always put Mail in it's own frame."
        (if (and (interactive-p)
                 (not command-other-frame-active-p))
            (command-other-frame 'mail "Mail" nil 'exact)
          ad-do-it))

      (defadvice telnet (around make-separate-frame activate)
        "Always put Telnet in it's own frame."
        (if (and (interactive-p)
                 (not command-other-frame-active-p))
            (command-other-frame 'telnet "Telnet" nil 'exact nil (ad-get-args 0))
          ad-do-it
          ))))

;;}}}

;;{{{  MSB -- Improved mouse-select-buffer

(autoload 'msb "msb" "Select buffer from menus." t)
(autoload 'mouse-select-buffer "msb"
  "Mouse buffer menu with multiple menus." nil)
(substitute-key-definition 'mouse-buffer-menu 'msb (current-global-map))

(require 'msb)
(setq msb-menu-cond msb--many-menus)
;;(define-key java-mode-map [mouse-3]  'msb) ; get a back button

;;}}}

;;{{{  Listbuf

;; Try Noah Friedman's buffer-listing improvement.
;;
(load "listbuf")

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
(eval-after-load "dired" (require 'dired-sort-menu))

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
(if (not esler-xemacs)
    (progn

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
        (dired-goto-next-file))))

;; Display a "universalised" true pathname of directory at the top.
;;
;; (if (not running-as-w32-client)
;;     (progn

;;       (add-hook 'dired-after-readin-hook
;;                 'esler-dired-after-readin-hook)
;;       (defun esler-dired-after-readin-hook ()
;;         (goto-char (point-min))

;;         ;; Only do this if the buffer is not currently narrowed
;;         ;;
;;         (if (= 1 (point))
;;             (progn
;;               (forward-line 1)
;;               (let ((buffer-read-only nil))
;;                 (insert "  ("
;;                         (esler-universalise-truepath
;;                          (file-truename (directory-file-name default-directory)))
;;                         ")\n")))))
;;       (defun esler-universalise-truepath (true-path)
;;         (if (string-match "^/tmp_mnt/net2/" true-path)
;;             (concat "/net" (substring true-path 13))
;;           (if (or (string-match "^/afs/" true-path)
;;                   (string-match "^/vobs/" true-path)
;;                   (string-match "^/net/" true-path)
;;                   (string-match "^/view/" true-path)
;;                   (string-match "^/tmp_mnt/" true-path))
;;               true-path
;;             (concat "/net/" (system-name) true-path))))))

(defun esler-dired-visit-vm-folder ()

  "Visit the file as a VM mail folder."

  (interactive)

  (setq vm-delete-empty-folders t)
  (vm-visit-folder (dired-get-filename)))

(if (not esler-xemacs)
    (autoload 'dired-update-file-line "dired-aux")
  (autoload 'dired-update-file-line "dired"))
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
      (save-excursion
        (set-buffer buffer)
        (erase-buffer))
      (call-process shell-file-name
                    (dired-get-filename)
                    buffer
                    nil
                    "-c"
                    command)
      (setq esler-dired-last-piped-command command)
      (if (save-excursion
            (set-buffer buffer)
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
;;{{{  Dismal spreadsheet

;; It's well named.
;;(load "dismal-mode-defaults")
;;(autoload 'dismal-mode "dismal" "The dismal code." t)

;;}}}
;;{{{  Buffer Menu Mode.

(define-key Buffer-menu-mode-map [mouse-3]
  '(lambda (click)
     (interactive "@e")
     (mouse-set-point click)
     (sit-for 0)
     (Buffer-menu-select)))

;; I don't want buffer-menu to create any windows.
;;
(eval-after-load
    "buff-menu"
  (progn
    (define-key Buffer-menu-mode-map " "
      (function (lambda ()
                  (interactive)
                  (let ((buf (current-buffer)))
                    (Buffer-menu-this-window)
                    (bury-buffer buf)))))))

;;(defun buffer-menu (&optional arg)
;;     "Make a menu of buffers so you can save, delete or select them.
;;With argument, show only buffers that are visiting files.
;;Type ? after invocation to get help on commands available.
;;Type q immediately to make the buffer menu go away and to restore
;;previous window configuration."
;;     (interactive "P")
;;     (let ((temp-buffer-show-function
;;            (function (lambda (buffer)
;;                        nil))))
;;       (list-buffers arg)
;;       (let ((newpoint (save-excursion (set-buffer "*Buffer List*")
;;                                       (point))))
;;         (switch-to-buffer "*Buffer List*")
;;         (goto-char newpoint))
;;       (message
;;        "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help.")))))

;;}}}
;;{{{  Info Mode.

;;

;; I maintain my own Info directory.  It contains links to the
;; contents of the system supplied Info directory.  It also contains
;; files that I have added.  For example, info on VM.

;; kae 23
;;(require 'info)
;;(setq esler-info-directory "~/apps/emacs/info")

;; (if (file-directory-p esler-info-directory)
;;     (setq Info-default-directory-list (cons esler-info-directory
;;                                             Info-default-directory-list)))
;; (if (file-directory-p "/usr/nodelocal/info")
;;     (setq Info-default-directory-list (cons "/usr/nodelocal/info"
;;                                             Info-default-directory-list)))

;; I prefer to use mouse-3 for following Info nodes,
;; because that's what I use to click on pathnames, email messages,
;; and news articles.
;;
;;(autoload 'Info-last "info")
;;(eval-after-load
;; "info"
;; (progn
;;   (define-key Info-mode-map [mouse-3] 'Info-follow-nearest-node)
;;
;;   ;; Make "b" act like Netscape "Back":
;;   ;;
;;   (define-key Info-mode-map "b" 'Info-last)))

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

;; ;; This should speed up ediff comparisons of folded files.
;; ;;
;; (add-hook 'ediff-prepare-buffer-hook '(lambda ()
;;                                          (if folded-file
;;                                              (folding-open-buffer))))

;; That didn't work so try this:
;;
;; (if (not esler-xemacs)
;;     (defadvice ediff-nuke-selective-display (before kae-ediff-fold-speedup act)
;;       (if (and (boundp 'folded-file)
;;                folded-file)
;;           (folding-open-buffer))))

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
;;{{{  Task Mode. [UNDER DEVELOPMENT]

;;
;; A mode to maintain two simple databases:
;;   1. what I am CURRENTLY working on; temporary information.
;;   2. plans, schedules and design notes, to be kept.
;;
;; I keep them as folded text files, hence the manipulation of
;; 'inhibit-local-variables.


(defun context ()
  (interactive)
  (let ((enable-local-variables t))
    (find-file "~/tasks/Context.cpt")))

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
;;{{{  Scroll-in-place

(require 'scroll-in-place)
(setq scroll-allow-blank-lines-past-eob t)

;;}}}
;;{{{  Follow Mode

(autoload 'follow-mode "follow"
  "Synchronize windows showing the same buffer, minor mode." t)
(global-set-key [f8] 'follow-mode)

(autoload 'follow-delete-other-windows-and-split "follow"
  "Delete other windows, split the frame in two, and enter Follow Mode." t)
(global-set-key [f7] 'follow-delete-other-windows-and-split)

;;}}}
;;{{{  Message logging facility.

;;(require 'log-messages)

;;}}}
;;{{{  Etags.

;; I prefer to pop back to the location FROM WHENCE I JUMPED TO A TAG,
;; rather than the last tag found.  To obtain this I have to rewrite
;; this function, find-tag-noselect.
;;

;;(require 'etags)
;;(defun find-tag-noselect (tagname &optional next-p regexp-p)
;;  "Find tag (in current tags table) whose name contains TAGNAME.
;;Returns the buffer containing the tag's definition and moves its point there,
;;but does not select the buffer.
;;The default for TAGNAME is the expression in the buffer near point.
;;
;;If second arg NEXT-P is t (interactively, with prefix arg), search for
;;another tag that matches the last tagname or regexp used.  When there are
;;multiple matches for a tag, more exact matches are found first.  If NEXT-P
;;is the atom `-' (interactively, with prefix arg that is a negative number
;;or just \\[negative-argument]), pop back to the previous tag gone to.
;;
;;If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.
;;
;;See documentation of variable `tags-file-name'."
;;  (interactive (find-tag-interactive "Find tag: "))
;;
;;  ;; Save the current buffer's value of `find-tag-hook', and the current buffer
;;  ;; and position, before selecting the tags table buffer.
;;  ;;
;;  (let ((local-find-tag-hook find-tag-hook)
;;        (launch-buffer (current-buffer))
;;        (launch-position (point)))
;;
;;    (if (eq '- next-p)
;;
;;	;; Pop back to a previous location.
;;        ;;
;;	(if (null tags-location-stack)
;;	    (error "No previous tag locations")
;;	  (let ((marker (car tags-location-stack)))
;;
;;	    ;; Pop the stack.
;;            ;;
;;	    (setq tags-location-stack (cdr tags-location-stack))
;;	    (prog1
;;
;;		;; Move to the saved location.
;;                ;;
;;		(set-buffer (marker-buffer marker))
;;	      (goto-char (marker-position marker))
;;
;;	      ;; Kill that marker so it doesn't slow down editing.
;;              ;;
;;	      (set-marker marker nil nil))))
;;      (if next-p
;;
;;	  ;; Find the same table we last used.
;;          ;;
;;	  (visit-tags-table-buffer 'same)
;;
;;	;; Pick a table to use.
;;        ;;
;;	(visit-tags-table-buffer)
;;
;;	;; Record TAGNAME for a future call with NEXT-P non-nil.
;;        ;;
;;	(setq last-tag tagname))
;;
;;      (prog1
;;          ;; Find the tag.
;;          ;;
;;          (save-excursion
;;            (set-buffer
;;             ;; find-tag-in-order does the real work.
;;             (find-tag-in-order
;;              (if next-p last-tag tagname)
;;              (if regexp-p
;;                  find-tag-regexp-search-function
;;                find-tag-search-function)
;;              (if regexp-p
;;                  find-tag-regexp-tag-order
;;                find-tag-tag-order)
;;              (if regexp-p
;;                  find-tag-regexp-next-line-after-failure-p
;;                find-tag-next-line-after-failure-p)
;;              (if regexp-p "matching" "containing")
;;              (not next-p)))
;;            (current-buffer))
;;
;;        ;; Record the current location so we can pop back to it later.
;;        ;;
;;        (let ((marker (make-marker)))
;;          (set-marker marker launch-position launch-buffer)
;;          (setq tags-location-stack
;;                (cons marker tags-location-stack)))
;;
;;        ;; Run the appropriate hooks.
;;        ;;
;;        (run-hooks 'local-find-tag-hook)))))
;;
;;(defun tags-reset-location-stack ()
;;  "Reset to nil the stack of tags locations."
;;  (interactive)
;;  (setq tags-location-stack nil))

;;}}}
;;{{{  Mkid

(require 'gid)

;;}}}
;;{{{  Hyperbole.

;;(load "hversion")
;;(load "hyperbole")
;;(require 'hsite)

;;(setq hmail:init-function 'Vm-init)
;;(setq smail:comment "")

;;}}}
;;{{{  Bibl Mode

(require 'easymenu)

(autoload 'bibl-visit-bibliography "bibl-mode"
  "Autoload bibliography mode." t)
(autoload 'bibl-mode "bibl-mode" "Autoload bibliography mode." t)
(setq bibl-file-name "~/bibliographies/kae.bibl")
(global-set-key "\C-cb" 'bibl-global-map)

;;}}}
;;{{{  SliTeX Mode

(setq auto-mode-alist (cons '("\\.slitex$" . slitex-mode) auto-mode-alist))

;;}}}
;;{{{  Filladapt

;; (require 'filladapt)
;; (setq-default filladapt-mode t)

;;}}}
;;{{{  Paren highlighting

;; Get paren highlighting.
;;
(require 'paren)

;;}}}

;;}}}

;;{{{  ASCII-display/dialup support.

;;{{{  Xterm mouse support.

(if running-as-xterm-client

    (progn

      ;; Load my mechanism for xterm mousing.
      ;;
      (load-library "xterm-mouse")
      (add-hook 'kill-emacs-hook 'xterm-mouse-disable)
      (add-hook 'suspend-hook 'xterm-mouse-disable)
      (add-hook 'suspend-resume-hook 'xterm-mouse-enable)

      ;; New with 19.30
      ;; (This causes a startup-time error.)
      ;;(xterm-mouse-mode)
      ))

;;}}}

;; Here's the setup for when I'm dialed in from home.

(if running-as-terminal-client

    (progn

      (global-set-key "\C-h" 'backward-delete-char-untabify)

      ;; C-s gets swallowed by the terminal server.
      ;; So, consistently replace it with C-z.
      ;;
      (global-set-key "\C-z" 'isearch-forward)
      (define-key isearch-mode-map "\C-z" 'isearch-repeat-forward)

      (add-hook 'folding-mode-hook
                '(lambda ()
                   (local-set-key "\C-c\C-z" 'folding-shift-in)))

      (global-set-key "\C-x\C-z" 'save-buffer)
      (global-set-key "\M-\C-z" 'isearch-forward-regexp)
      (add-hook 'c-mode-hook
                '(lambda ()
                   (local-set-key "\C-c\C-z" 'c-show-syntactic-information)))
      (add-hook 'c++-mode-hook
                '(lambda ()
                   (local-set-key "\C-c\C-z" 'c-show-syntactic-information)))

      ;; C-q gets swallowed by the terminal server.
      ;; So, consistently replace it with "`".
      ;;
      (global-set-key "`" 'quoted-insert)
      (define-key isearch-mode-map "`" 'isearch-quote-char)

      ;; Use the package which allows multiple virtual screens.
      ;; Use mouse-3 for selecting screens from the screen menu.
      ;;
      (require 'screens)
      (define-key screen-menu-mode-map [mouse-3]
        '(lambda (click)
           (interactive)
           (mouse-set-point click)
           (screen-menu-select)))))

;;}}}
;;{{{  Site-specifics.

;;{{{ Rational

(if at-site-ibm

    (progn
      ;;{{{  PC Printing

      ;; As of Emacs 20.4 this is all you need:
      ;;
      (setq printer-name "//apgmisntsrv/apgeng1")

      ;;}}}

      ;;{{{  Distinguished buffer names for Makefiles

      (defun fix-makefile-names ()
        (if (eq (string-match "Makefile\\(<[0-9]+>\\)?$" (buffer-name)) 0)
            (rename-buffer (concat (file-name-nondirectory
                                    (directory-file-name
                                     (file-name-directory buffer-file-name)))
                                   "/Makefile")
                           t)))
      (add-hook 'find-file-hooks 'fix-makefile-names)

      ;;}}}

      ;;{{{  Ange-ftp via a gateway.

      (setq ange-ftp-binary-file-name-regexp ".")
      (setq ange-ftp-default-user "anonymous")
      ;; nyi: Probably no longer valid after IBM changes.
      ;;
      (setq ange-ftp-default-password "guest@gw.rational.com")

      (setq ange-ftp-ftp-program-name "ftp")
      (setq ange-ftp-gateway-program "telnet")

      ;;}}}

      ;;{{{  Canonicalise current directory

      (cd "~/")

      ;;}}}

      ;;{{{  Misc.

      ;; So I can invoke as "emacs -funcall esler-run-vm"
      ;; Eventually I'll set special fonts, geometries,
      ;; gnuserv socket paths, etc.
      ;;
      (defun esler-run-vm ()
        (vm))

      (define-global-abbrev "mm" "/vobs/atria/bin/mmake")

      (setq compile-command "/usr/atria/bin/clearmake")

      ;; So I can invoke as "emacs -funcall esler-run-gnus"
      ;;
      (defun esler-run-gnus ()
        (gnus))

      (defun esler-create-initial-frames ()
        (save-excursion

          ;;
          (info)
          (iconify-frame (make-frame))


          (find-file "~/tasks/context")
          (iconify-frame (make-frame))

          (shell)
          (iconify-frame (make-frame))))

      ;;}}}

      ;;{{{  Nightly build log scanner

      (load-library "nightly")

      ;;}}}

      )                                 ; progn

  )                                     ; at-site-ibm

;;}}}

;;{{{ Home

(if at-site-home

    (progn

      ;;{{{  Ange-ftp.

      (setq ange-ftp-ftp-program-name "ftp")
      (setq ange-ftp-binary-file-name-regexp ".")
      (setq ange-ftp-default-user "anonymous")
      (setq ange-ftp-default-password

            ;; Used to be "capone". It seems to depend on which version
            ;; of rftp we use.
            ;;
            (concat (user-real-login-name) "@kaesler.verizon.net"))

      ;; Some FTP servers insist on the client host appearing in the password.
      ;; In our case it's usually our proxy host.
      ;;
      (setq ange-ftp-generate-anonymous-password
            (concat (user-real-login-name) "@kaesler.verizon.net"))

      ;;}}}

      )                                 ; progn

  )                                     ; at-site-home

;;}}}

;;{{{ USB drive

(if (running-off-usb-drive)
    (progn
      (setenv "PATH"
              (concat usb-home-dir "apps\\emacs\\bin;" (getenv "PATH")))
      (frame-rename "Emacs@USB")))

;;}}}
;;}}}
;;{{{  Win32-specifics

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

(if (not esler-xemacs)
    (progn

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

      (defalias ':/ ':/-region)))

;;}}}

(if running-as-w32-client
    (progn

      ;; Load the registry interface and mode.
      ;;
      (if (not esler-xemacs)
          (progn
            (require 'w32-reg-int)
            (require 'w32-registry)))

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
(server-start)
;;}}}
;;{{{  Project-specific functions

;; For CCWeb project:
;;   - menu entry for pointing the browser at the local CCWEB server.

(defun esler-project-browse-local-ccweb ()
  (interactive)
  (let ((url (concat "http://" (system-name) "/ccase/bin/ccweb.exe")))
    (esler-browse-url-with-default-browser url)))

(defun esler-project-dired-windows-profile ()
  (interactive)
  (dired (getenv "USERPROFILE")))

(defun esler-project-dired-unix-homedir ()
  (interactive)
  (dired "/esler@isophorone.lexma.ibm.com:"))

(defun esler-project-dired-homedir ()
  (interactive)
  (dired "~/"))

(defvar esler-current-project-notes "~/cpt/Notes.cpt")

(defun esler-edit-current-project-notes ()
  (interactive)
  (find-file esler-current-project-notes))

(defun esler-edit-lore ()
  (interactive)
  (find-file "~/cpt/Lore.cpt"))

(defun esler-edit-creds ()
  (interactive)
  (find-file "~/cpt/Creds.cpt"))

(defun esler-project-dired-java-packages ()
  (interactive)
  (dired "c:/winnt/java/packages"))

(defun esler-project-dired-my-elisp ()
  (interactive)
  (dired esler-elisp-directory))

(defun esler-project-dired-emacs-elisp ()
  (interactive)
  (dired (concat exec-directory "/../lisp")))

(defun esler-project-dired-ucm-cspecs ()
  (interactive)
  (if running-as-w32-client
      (let ((path "//maple/dfs/clearcase/cspecs/UCM"))
        (if (file-exists-p path)
            (dired path)
          ;; This didn't work:
          (start-process-shell-command "Windows NT Explorer"
                                       nil
                                       "explorer"
                                       (concat path ",/e"))))
    (dired "/net/apgsun8/export/home/builder/cspecs/ucm/")))

;;}}}

;;{{{  Warn about being invoked as root.

(if (zerop (user-uid))
    (error "Warning: you have invoked Emacs as root !"))

;;}}}

(message "End of .emacs...")

;;{{{  RCS history of this file.

;; $Log: .emacs.el,v $
;; Revision 1.842  2000/09/12 12:50:01  esler
;; Use nice Andale font on Windows.
;;
;; Revision 1.839  2000/08/24  21:55:59  esler
;; Have one ~/.emacs.custom file at work, common between Unix and
;; windows.
;; Improve how load-path gets computed.
;; Adjust VM to use IMAP at work.
;; Add Java Mode indentation style improvements.
;;
;; Revision 1.838  2000/08/18 15:09:02  esler
;; Move my Rational mailbox to an IMAP protocol on a different server.
;;
;; Revision 1.837  2000/08/18 14:55:07  esler
;; Configure Ishl.
;;
;; Revision 1.836  2000/08/16 17:06:01  esler
;; Configure MMM (Multiple Major Modes in one buffer)
;; Fix problem sending HTML to Netscape6.
;;
;; Revision 1.835  2000/08/03  14:39:47  esler
;; Rename ccase.el ==> clearcase.el.
;; Roll MFC indenting specials into "Atria" style.
;;
;; Revision 1.834  2000/07/20 17:39:56  esler
;; Add some HTML-editing features:
;;   - commenting
;;   - more flexibe narrowing to Javascript.
;;
;; Revision 1.833  2000/07/20 16:51:43  esler
;; Use ccase.el on laptop.
;;
;; Revision 1.832  2000/07/11 16:52:32  esler
;; Provide menu item for Sun JDK and Microsoft Java SDK.
;;
;; Revision 1.831  2000/06/29  15:45:30  esler
;; Allow me to send a HTML file to Netscape 6 on Windows.
;;
;; Revision 1.830  2000/06/29  15:29:27  esler
;; *** empty log message ***
;;
;; Revision 1.829  2000/06/28  22:52:06  esler
;; Set up HTML-Helper mode so I can inoke an arbitrary browser on my
;; HTML.
;;
;; Revision 1.828  2000/06/28  21:41:06  esler
;; Merge some changes from home PC version.
;;
;; Revision 1.826  2000/06/05 03:33:43  esler
;; *** empty log message ***
;;
;; Revision 1.825  2000/05/31 14:45:50  esler
;; Configure ELL.
;;
;; Revision 1.824  2000/05/29 17:05:27  esler
;; Reorganise my elisp library so it's identical on Windows, for easier
;; maintenance.
;;
;; Revision 1.823  2000/05/26 00:33:52  esler
;; CC-Mode styles weren't getting defined.
;;
;; Revision 1.822  2000/05/25 23:58:33  esler
;; Get the latest of a bunch of packages.
;; Save backups in ".,".
;; Use ls-lisp.
;; Turn off JDE for now.
;;
;; Revision 1.821  2000/05/02  14:10:18  esler
;; Olin's backup sub-directory hack: ensure-backdir
;; Save-history.
;; Switch to Exchange POP server at Rational.
;; Reinstate some lost VM binding enhancements.
;; Some Bash support (switching pname-seps).
;;
;; Revision 1.820  2000/03/03 16:14:26  esler
;; Attempt (unsuccessful) to get XEmacs to work.
;; Set up my Ultranet folder as explicitly readable from Rational.
;;
;; Revision 1.819  2000/02/09 18:45:45  esler
;; New packages:
;;   eol-conversion (under the Mule menu)
;;   add Hideshow Minor Mode to C, C++ and Elisp
;;   add dired-sort-menu
;;
;; Revision 1.818  2000/02/02 20:11:32  esler
;; Move to new home Unix box at work: isophorone.
;; Add a usefule fuction to the KAE menu: esler-insert-iso-date
;; Detect when we're editing MFC code on Windows and set:
;;   - indent style
;;   - tab-width
;;   - major mode
;;
;; Revision 1.817  1999/12/17 19:53:10  esler
;; Fix a bug in the code that launches the App for a windows file.
;;
;; Revision 1.816  1999/12/15 20:31:41  esler
;; Use ~/.emacs.custom for customisable variables.
;; In Dired Mode, on Windows "j" can invoke executables.
;;
;; Revision 1.815  1999/12/06 17:04:56  esler
;; Change the highlight colour from an obscuring "LightSeaGreen" to
;; "pink".
;; Change the installed path of the bash shell on NT.
;; Configure the new and apparently slick EShell-2.1.
;;
;; Revision 1.814  1999/11/24 17:28:56  esler
;; Use standard shell-command-to-string.
;;
;; Revision 1.813  1999/11/23 22:07:54  esler
;; Speedup ediff for folded files.
;;
;; Revision 1.812  1999/11/19 17:03:59  esler
;; Update View Minor Mode config.
;;
;; Revision 1.811  1999/10/28 00:33:35  esler
;; Fiddle with foreign NNTP servers.
;; Printing on NT set up at last.
;;
;; Revision 1.810  1999/10/21 19:06:28  esler
;; Remove a bunch of clearcase stuff to go into CCase Modes package.
;; Add a meny bar entry for VC.
;;
;; Revision 1.809  1999/10/18 19:27:56  esler
;; Get site-lisp out of load-path. It contains a stale vc.el.
;; New html-helper-mode.
;; Initial version of new ccase Minor Mode.
;;
;; Revision 1.808  1999/10/13 21:17:11  esler
;; Rename my vc-cc version to ccase-.
;;
;; Revision 1.807  1999/10/07 21:16:00  esler
;; Configure watson.el
;;
;; Revision 1.806  1999/10/07 21:04:29  esler
;; Configure slashdot.el.
;;
;; Revision 1.805  1999/10/07 20:55:06  esler
;; Various.
;;
;; Revision 1.804  1999/09/20 20:14:20  esler
;; While my home PC is broken, read Ultranet mail at work.
;;
;; Revision 1.803  1999/09/16 21:13:37  esler
;; Tweak my menus.
;;
;; Revision 1.802  1999/09/10 17:34:56  esler
;; Add a cleartrack entry to my personal KAE menu.
;; Add browsing //maple/dfs to my personal KAE menu.
;; Add code for diffing against the branch base in ClearCase.
;; Add some font-lock support for MFC C++ macros.
;; Add some font-lock keywords for MFC Makefiles.
;; Add dired-a package.
;;
;; Revision 1.801  1999/08/27 22:07:26  esler
;; Add the Rational FTP site to my personal menu.
;; Use MFC indentation style whe it seems appropriate.
;; make-buffer-file-executable-if-script-p ()
;; Simplify code for launching Windows documents.
;;
;; Revision 1.800  1999/08/26 16:10:36  esler
;; Fix bug in computing the predecessor of a ClearCase version.
;;
;; Revision 1.799  1999/08/25 19:43:27  esler
;; Add ways to start the NT Explorer from Dired Mode and from the KAE
;; menu.
;;
;; Revision 1.798  1999/08/24  20:52:06  esler
;; In Dired Mode: "j" opens Windows Documents using ShellExecute().
;;
;; Revision 1.797  1999/08/24 16:56:30  esler
;; Cosmetic.
;;
;; Revision 1.796  1999/08/23 22:57:12  esler
;; ClearCase menu: diff with predecessor.
;;
;; Revision 1.795  1999/08/23 19:48:50  esler
;; ClearCase menu item for launching the Vtree browser.
;;
;; Revision 1.794  1999/08/23 16:41:42  esler
;; Only enable entries in ClearCase menu when they are relevant.
;;
;; Revision 1.793  1999/08/20 21:30:59  esler
;; Add a menu entry for showing/hiding "bad" whitespace (hard TABs,
;; trailing space).
;; Use Auto-show Mode.
;;
;; Revision 1.792  1999/08/17  16:01:48  esler
;; Adjust my "KAE" menu.
;; Make an indentation style for MFC code.
;; Allow downcasing of file names on NT using dired-rename.
;;
;; Revision 1.791  1999/08/16 15:04:23  esler
;; Rename and reduce my menu-bar menu.
;;
;; Revision 1.790  1999/08/11 20:40:04  esler
;; Add colour for trailing whitespace and hard TABs.
;;
;; Revision 1.789  1999/08/06 20:34:50  esler
;; Added a next-long-line function for facilitating
;; conformance with line-length restrictions.
;;
;; Revision 1.788  1999/05/28 19:00:13  esler
;; Added the dired-sort patckage.
;;
;; Revision 1.787  1999/04/13 17:51:14  esler
;; Add colouring for Psgml Mode.
;;
;; Revision 1.786  1999/04/13 17:27:07  esler
;; Set up Psgml Mode.
;;
;; Revision 1.785  1999/04/13 16:48:42  esler
;; Setup HTML Helper Mode.
;;
;; Revision 1.784  1999/02/04 20:40:36  esler
;; Tweak C Mode on NT.
;;
;; Revision 1.783  1999/02/04 18:14:58  esler
;; Tweak C Mode on NT.
;;
;; Revision 1.782  1999/01/19 16:49:31  esler
;; Merge in changes from home.
;;
;; Revision 1.781  1999/01/19 16:37:21  esler
;; More W3 tweaks.
;;
;; Revision 1.780  1999/01/19 16:35:45  esler
;; Configure new W3 version.
;;
;; Revision 1.779  1999/01/08 00:11:04  esler
;; Dired-Drag/Drop doesn't work on NT yet.
;;
;; Revision 1.778  1999/01/07 20:02:03  esler
;; Get Gnus articles saved in nix format.
;;
;; Revision 1.777  1999/01/07 19:45:36  esler
;; Overhaul Gnus customisations, removing obsolete cruft.
;;
;; Revision 1.776  1999/01/07 18:28:34  esler
;; Make Archive Mode bindings like Tar Mode bindings.
;;
;; Revision 1.775  1999/01/07 18:21:12  esler
;; Fix problem causing Gnus to hang when reading NNTP server.
;;
;; Revision 1.774  1999/01/06 17:59:30  esler
;; Indented-text-mode seems to be obsolete in Emacs-20.
;;
;; Revision 1.773  1999/01/06 17:20:06  esler
;; Use VM in Emacs-20.3.
;;
;; Revision 1.772  1999/01/06 17:14:56  esler
;; Allow "g" to work in ps-mode.
;;
;; Revision 1.771  1998/12/22 16:18:58  esler
;; Tweak Portuguese character set stuff.
;; Turn off a log of Gnus customisations to try to get it working right.
;; Configure Dired Drag&Drop.
;; Turn obsolete my etags hacks; subsumed by 20.3 or earlier 20.x.
;;
;; Revision 1.770  1998/11/04 19:53:54  esler
;; Changes for Emacs-20.3 on Unix and NT.
;; Allow input of Portuguese accents.
;; Get rid of Atria references.
;;
;; Revision 1.769  1998/09/08 20:55:03  esler
;; Fix bash config.
;;
;; Revision 1.768  1998/09/04 20:33:52  esler
;; Fix bash shell setup for NT.
;;
;; Revision 1.767  1998/08/28 14:08:28  esler
;; A few VM tweaks.
;;
;; Revision 1.766  1998/08/26 16:34:18  esler
;; Update vm-auto-folder-alist to reflect non-flat folder tree.
;;
;; Revision 1.765  1998/05/07 15:21:03  esler
;; Added useful function: esler-another-line.
;;
;; Revision 1.764  1998/04/28 16:17:41  esler
;; Customise key bindings more efficiently: set them only once (per-mode)
;; using `eval-after-load', rather than repeatedly using mode-hooks.
;;
;; Dired-mode: when quitting a wildcard dired buffer, go to another
;; dired buffer for the dame dir, if there is one.
;;
;; Revision 1.763  1998/04/27 17:48:01  esler
;; Fix C++ config to work woth CC-Mode 5.21.
;;
;; Revision 1.762  1998/04/21 22:46:20  esler
;; Get rid of id-select stuff that was causing loop in cc-mode-5.21.
;;
;; Revision 1.761  1998/02/26 15:58:07  esler
;; Fix bug in shell-execute-url.
;;
;; Revision 1.760  1998/02/25 23:21:34  esler
;; New PC "cutter" to replace "greenall".
;;
;; Revision 1.759  1998/02/25 00:54:11  esler
;; Tweak Gnuserv.
;;
;; Revision 1.758  1998/02/25 00:31:51  esler
;; Create a singleton Gnuserv frame at startup.
;;
;; Revision 1.757  1998/02/24 23:31:21  esler
;; Use a version of gnuserv on NT.
;;
;; Revision 1.756  1998/02/24 23:26:50  esler
;; Merge changes from home.
;;
;; Revision 1.702  1998/02/23 01:13:49  esler
;; Re-do my C/C++ indentation styles to catch up with how it's
;; done in CC-Mode-5.19.
;;
;; Revision 1.701  1998/02/22 18:42:38  esler
;; Merge with work version.
;;
;; Revision 1.755  1998/02/21 00:31:58  esler
;; Configure JDE.
;; Tweak CC-mode.
;;
;; Revision 1.754  1998/02/20 00:46:43  esler
;; Merge NT changes, inter alia.
;;
;; Revision 1.753  1997/07/31  16:37:53  esler
;; Rename the company to Rational.
;;
;; Revision 1.752  1997/07/29  20:33:28  esler
;; Set highlight-nonselected-windows.
;;
;; Revision 1.751  1997/07/24  15:31:27  esler
;; Various:
;;   - use delete-selection-mode (inserting text deletes the selection);
;;   - configure vm-complain.el, a SPAM responder;
;;   - special tweaks so that cc-mode-5.13 would work;
;;   - configure greedy-delete for various modes;
;;   - fix problem with completion when renaming files from Dired Mode;
;;   - a few win32 tweeks;
;;
;; Revision 1.750  1997/06/03  20:09:47  esler
;; Move Win32 customisations.
;;
;; Revision 1.749  1997/06/03  17:31:00  esler
;; Install and configure:
;;   - custom-1.9907
;;   - gnus-5.4.56
;;   - cc-mode-5.09
;;
;; Revision 1.748  1997/06/03  16:36:23  esler
;; Install and configure w3-3.0.86.
;;
;; Revision 1.747  1997/06/03  15:54:18  esler
;; Merge mods from home.
;;
;; Revision 1.746  1997/05/24  17:35:47  esler
;; Edit all files in binary, unless told otherwise.
;;
;; Revision 1.745  1997/04/29  14:11:24  esler
;; Add font-lock support for VM.
;;
;; Revision 1.744  1997/03/25  16:47:01  esler
;; Use outboard C programs to do base64 encoding and decoding.
;;
;; Revision 1.743  1997/03/20  16:45:25  esler
;; Some Win32 shell mode hacks.
;;
;; Revision 1.742  1997/03/19  23:17:25  esler
;; Configure the new Tuareg mode for Caml programs.
;;
;; Revision 1.741  1997/03/14  21:54:34  esler
;; Misc.
;;
;; Revision 1.740  1997/02/18  20:58:18  esler
;; Various.
;;
;; Revision 1.739  1997/01/22  22:40:27  esler
;; Adjust font-lock colours a bit.
;;
;; Revision 1.738  1997/01/22  21:23:48  esler
;; Convert to use Font Lock and Lazy Lock for
;; everything except Dired Mode and Caml Mode.
;;
;; Revision 1.737  1997/01/22  20:06:57  esler
;; Remove HP-site-specific cruft.
;;
;; Revision 1.736  1997/01/22  19:45:58  esler
;; Reinstate my old bindings for folding and
;; unfolding a whole buffer.
;;
;; Revision 1.735  1997/01/21  22:57:34  esler
;; Merge with NT version.
;;
;; Revision 1.734  1997/01/21  21:53:08  esler
;; Configure new Folding Minor Mode code.
;;
;; Revision 1.733  1997/01/21  21:39:18  esler
;; Configure nifty Eldoc package.
;;
;; Revision 1.732  1997/01/21  21:17:22  esler
;; Configure Follow Mode.
;;
;; Revision 1.731  1997/01/21  21:11:29  esler
;; Configure:
;;   - anders-java-font-lock code
;;   - id-select package.
;;
;; Revision 1.730  1997/01/21  21:00:23  esler
;; Adjust vm-url-regexp for new version of VM (6.06).
;;
;; Revision 1.729  1997/01/20  23:20:01  esler
;; Get OCaml support right.
;;
;; Revision 1.728  1997/01/20  23:13:10  esler
;;   1. Use a different colour background for emacss that
;;      are in a ClearCase view.
;;   2. Out with SML mode and in with Caml mode to support OCaml hacking.
;;   3. Remove obsolete HP-site-specific cruft.
;;   4. Get view-tag into mode line.
;;
;; Revision 1.727  1996/11/27  21:48:38  esler
;; Merge in home changes.
;;
;; Revision 1.726  1996/11/27  00:08:45  esler
;; More NT updates.
;;
;; Revision 1.725  1996/11/26  23:12:46  esler
;; More NT changes.
;;
;; Revision 1.723  1996/10/25  15:05:25  esler
;; Various.
;;
;; Revision 1.722  1996/08/22  21:31:41  esler
;; Get S-mouse3 to read the pointed-at file in a new frame.
;; Configure webjump package.
;; Use new gnuserv.
;;
;; Revision 1.721  1996/08/09  14:59:44  esler
;; Configure webjump package.
;;
;; Revision 1.720  1996/06/26  16:38:55  esler
;; Junk Hyperbole.
;;
;; Revision 1.719  1996/06/18  14:48:20  esler
;; Configure Dismal.
;;
;; Revision 1.718  1996/05/24  15:45:20  esler
;; Tweak vm-folder-directory.
;; Add some preferred newsgroups.
;; Add a regexp for password prompts on AIX.
;;
;; Revision 1.717  1996/05/20  15:30:34  esler
;; Turn off various VM hooks in an attempt to fix a performance
;; problem.
;;
;; Revision 1.716  1996/05/01  14:03:14  esler
;; Change gnus-article-save-directory to be "~/folders/saved",
;; in line with recent VM config changes.
;;
;; Revision 1.715  1996/05/01  14:02:06  esler
;; Merge from home version.
;;
;; Revision 1.714  1996/04/30  03:01:33  esler
;; Add cleanup side to the registry of Emacs processes.
;;
;; Revision 1.713  1996/04/02  21:17:26  esler
;; Add code to track on disk what emacs processes are running.
;; Other changes too.
;;
;; Revision 1.712  1996/02/28  22:18:30  esler
;; Improve save-link function.
;; Rebind C-h when dialed in.
;; Implement M-x pwv.
;; Load the nightly build log scanner always.
;;
;; Revision 1.711  1996/02/20  17:27:41  esler
;; Various including:
;;   -revert to my xterm mouse code, since the code supplied with
;;    19.30 causes an error at .emacs load-time.
;;   -(setenv "ATRIA_NO_BOLD" "TRUE")
;;
;; Revision 1.710  1996/01/17  20:03:30  esler
;; Remove junk.
;;
;; Revision 1.709  1996/01/11  17:03:22  esler
;; Sync up with version running at home.
;;
;; Revision 1.688  1996/01/10  22:00:17  esler
;; Fix bug preventing sendmail from working.
;;
;; Revision 1.687  1996/01/10  21:39:45  esler
;; Merge changes from Atria.
;;
;; Revision 1.686  1996/01/10  19:53:02  esler
;; Load Smalltalk colouring: "hilit-st".
;; Configure Smalltalk mode.
;; Configure Igrep.
;;
;; Revision 1.685  1995/08/26  14:57:07  esler
;; Configure Python support.
;;
;; Revision 1.684  1995/08/26  14:52:40  esler
;; Add some useful Dired enhancements.
;;
;; Revision 1.683  1995/08/18  04:16:33  esler
;; Rearrange the order of the file.
;; Configure experimental Nfolding.el.
;;
;; Revision 1.682  1995/07/18  03:27:28  esler
;; Merge changes from Atria:
;;   -use emacs bitmap;
;;   -framepop-auto-resize = t;
;;   -adjust framepop frame colours;
;;   -resolve conflict between hyperbole and Imenu mouse bindings;
;;   -change command to print mail messages;
;;   -use outboard Netscape to browse URLs from VM and GNUS;
;;   -hyperbole config;
;;   -improve esler-vc-cc-browse-version;
;;
;; Revision 1.681  1995/07/18  03:13:45  esler
;; Misc.
;;
;; Revision 1.680  1995/07/08  18:55:57  esler
;; Configure Framepop package.
;;
;; Revision 1.679  1995/07/08  18:44:00  esler
;; Merge changes from Atria.
;;
;; Revision 1.678  1995/06/26  04:54:11  esler
;; Various adjustments for version 19.29.
;;
;; Revision 1.677  1995/06/24  18:05:11  esler
;; Merge with changes from Atria.
;;
;; Revision 1.676  1995/06/19  01:02:30  esler
;; Various.
;;
;; Revision 1.675  1995/05/31  05:08:20  esler
;; Rip out old GNUS stuff.
;;
;; Revision 1.674  1995/05/31  05:04:31  esler
;; Various.
;;
;; Revision 1.673  1995/05/29  04:08:01  esler
;; Remove garbage.
;;
;; Revision 1.672  1995/05/29  04:06:15  esler
;; Merge changes from Atria:
;;   - Shortcuts menu: create new frame for GNUS.
;;   - change esler-universalise-truepath to recognise /vobs/... and
;;     /view/... paths.
;;   - configure new VM, so no need for win-vm.
;;   - Mailcrypt: always sign messages.
;;   - promote comp.lang.smalltalk.
;;   - GNUS: forward articles in RFC934 format.
;;   - turn off Mime for now.
;;
;; Revision 1.678  1995/05/16  17:59:27  esler
;; GNUS: rewrite my RFC934 forwarder in accordance with latest Ding code.
;;
;; Revision 1.677  1995/05/15  17:22:41  esler
;; Fix esler-universalise-truepath.
;;
;; Revision 1.676  1995/05/15  15:06:50  esler
;; Get Gnus articles mail-forwarded in RFC-934 encapsulation format.
;;
;; Revision 1.675  1995/05/15  14:30:27  esler
;; Make Gnus entry in Shortcuts menu make a new frame.
;;
;; Revision 1.674  1995/04/26  14:54:23  esler
;; Buffer Menu Mode: keep the buffer list buffer buried.
;;
;; Revision 1.673  1995/04/24  16:58:37  esler
;; Turf off obnoxious hook.
;;
;; Revision 1.672  1995/04/21  18:01:40  esler
;; Clean up autloads for Browse-URL.
;;
;; Revision 1.671  1995/04/21  17:53:55  esler
;; Configure Html-helper-mode.
;;
;; Revision 1.670  1995/04/21  17:45:47  esler
;; Configure browse-url.el, an Emacs->Netscape interface.
;;
;; Revision 1.669  1995/04/21  17:33:40  esler
;; Paul Smith's hack for distinguished buffer names for Makefiles.
;;
;; Revision 1.668  1995/04/20  18:57:48  esler
;; Info Mode: Make "b" act like Netscape "Back".
;;
;; Revision 1.667  1995/04/20  15:44:24  esler
;; Implement extract-matches command.
;;
;; Revision 1.666  1995/04/19  19:46:07  esler
;; Improvement.
;;
;; Revision 1.665  1995/04/19  15:01:16  esler
;; I don't want buffer-menu to create or use any other windows.
;;
;; Revision 1.664  1995/04/18  15:26:07  esler
;; Extend timeout on PGP passphrase.
;;
;; Revision 1.663  1995/04/14  15:59:50  esler
;; Allow Mailcrypt when forwarding articles via mail from Gnus.
;;
;; Revision 1.662  1995/04/14  15:56:00  esler
;; Add "Compile" to my shortcuts menu.
;;
;; Revision 1.661  1995/04/11  17:07:21  esler
;; Turn off esler-c-electric-lparen when editing Atria code.
;;
;; Revision 1.660  1995/04/11  17:03:56  esler
;; Start to get Ange-FTP going at Atria.
;;
;; Revision 1.659  1995/04/07  21:41:49  esler
;; Merge with stuff from home.
;;
;; Revision 1.658  1995/03/22  05:22:07  esler
;; 1. Changes so this will work at Atria.
;; 2. Implement esler-command-output-as-string ().
;; 3. Prevent accidentally toggling overwrite mode.
;; 4. Rename "Packages" menu to "Shortcuts".
;; 5. Make my shell-mode-hook a named function.
;; 6. Clean up the setting of the prompt-recognising regexp for Shell Mode.
;; 7. Make ps-mode work on an SGI/IRIX system.
;; 8. Get hilit19 working right in VM mail folders.
;; 9. Configure Calendar and Diary packages.
;; 10.Define a frame-rename function.
;;
;; Revision 1.657  1995/03/14  03:45:13  esler
;; Add to my "Packages" menu.
;;
;; Revision 1.656  1995/03/13  15:24:28  esler
;; Had the wrong form of POP authorization.
;;
;; Revision 1.655  1995/03/12  23:14:54  esler
;; Create "Packages" menu for quick access to my commonly-used functions.
;; VM: read from POP server directly, as well as local spool.
;;
;; Revision 1.654  1995/03/10  06:05:55  esler
;; Added defun: vm-mark-all-messages-as-read.
;; Configured new W3.
;; Simplify a regexp in folding outline code to avoid stack overflow.
;; Use Bill Sommerfeld's version of VC.
;;
;; Revision 1.653  1995/03/07  04:26:30  esler
;; Promote comp.lang.c++.
;;
;; Revision 1.652  1995/03/06  15:12:06  esler
;; Promote comp.lang.python.
;;
;; Revision 1.651  1995/03/06  05:44:11  esler
;; Added another case to comint-password-prompt-regexp.
;; Got GNUS newsgroups sorted for Linux system.
;; Fixed bug in esler-sort-subr.
;; Fixed other misc bugs.
;;
;; Revision 1.650  1995/03/03  15:53:31  esler
;; News to Ultranet works.
;;
;; Revision 1.649  1995/03/02  07:00:39  esler
;; Various adjustments for use on my Linux system at home.
;;
;; Revision 1.648  1995/02/14  20:07:39  esler
;; VM Mode: when forwarding, don't include some useless headers.
;;
;; Revision 1.647  1995/02/14  19:58:51  esler
;; Get the MView integration right.  I was turning off MView Mode even
;; when it hadn't been turned on, screwing up cc-mode buffer local
;; variables.
;;
;; Revision 1.646  1995/02/08  20:14:26  esler
;; Tweak Ediff configuration, and also font-lock.
;;
;; Revision 1.645  1995/02/08  03:32:30  esler
;; Merge in changes to support my home Linux system.
;;
;; Revision 1.643  1995/02/08  00:23:31  esler
;; Configure MView Minor Mode, instead of crufty old view-buf.el.
;;
;; Revision 1.642  1995/02/07  22:32:42  esler
;; *** empty log message ***
;;
;; Revision 1.641  1995/01/30  20:46:25  esler
;; Back out buggy mode line change made at 1.635.
;;
;; Revision 1.640  1995/01/25  16:41:44  esler
;; When I print a mail message from VM, add a "printed" label to the
;; message.
;;
;; Revision 1.639  1995/01/25  16:38:20  esler
;; Ange-FTP stopped working again.
;;
;; Revision 1.638  1995/01/20  16:58:52  esler
;; Install HM--MTML code.
;;
;; Revision 1.637  1995/01/17  15:20:54  esler
;; Another bug.
;;
;; Revision 1.636  1995/01/17  15:20:11  esler
;; Fix bug.
;;
;; Revision 1.635  1995/01/17  00:23:12  esler
;; Move the line number to the left.
;;
;; Revision 1.634  1995/01/16  23:38:24  esler
;; Remove cruft from mode line.
;;
;; Revision 1.633  1995/01/16  19:32:05  esler
;; Ange FTP was choking on messages from SOCKSified FTP client.
;;
;; Revision 1.632  1995/01/16  19:22:46  esler
;; MIME support in VM.
;; Also: keybindings for Folding Outline Minor Mode.
;;
;; Revision 1.631  1994/12/20  21:21:54  esler
;; Add mouse-3 support to folding outline minor mode.
;;
;; Revision 1.630  1994/12/12  21:35:22  esler
;; More MIME tweaks.
;;
;; Revision 1.629  1994/12/12  21:14:13  esler
;; More MIME support.
;;
;; Revision 1.628  1994/12/12  20:48:42  esler
;; Add MIME support (via piping to metamail command).
;;
;; Revision 1.627  1994/11/29  19:18:14  esler
;; Various.
;;
;; Revision 1.626  1994/11/08  00:14:15  esler
;; Tweak a few colours.
;;
;; Revision 1.625  1994/10/19  17:12:29  esler
;; Various.
;;
;; Revision 1.624  1994/09/29  23:18:10  esler
;; Tweak W3.
;;
;; Revision 1.623  1994/09/28  22:19:03  esler
;; More W3 tweaking.
;;
;; Revision 1.622  1994/09/28  22:11:36  esler
;; Configure W3, and add interfaces between VM and GNUS to allow
;; me to click on URL's in messages.
;;
;; Revision 1.621  1994/09/28  20:24:09  esler
;; Configure new version of W3 -- one that works at last.
;; Fine tune some of its colours and fonts.
;; Minor fix to Ielm Mode.
;;
;; Revision 1.620  1994/09/27  15:58:28  esler
;; Typo.
;;
;; Revision 1.619  1994/09/27  14:55:52  esler
;; In Comint modes: make C-a do like it used to, before 19.27.
;;
;; Revision 1.618  1994/09/24  18:29:23  esler
;; Fine-tune scroll-in-place.
;; VM: "kill subject" should move to next message.
;;
;; Revision 1.617  1994/09/20  19:18:17  esler
;; Get Bookmark menu up, even before we use it.
;; Some mouse bindings for navigating a folded outline.
;; (It would be better if they were just like for folded mode; later...)
;;
;; Revision 1.616  1994/09/14  15:25:15  esler
;; Fix typo.
;;
;; Revision 1.615  1994/09/14  15:04:52  esler
;; Get Bookmark menu at startup.
;;
;; Revision 1.614  1994/09/12  19:31:36  esler
;; Try out scroll-in-place.
;;
;; Revision 1.613  1994/09/09  20:34:33  esler
;; Dired Mode improvement.
;;
;; Revision 1.612  1994/09/05  16:59:23  esler
;; Hyperbole 3.15.
;; GNUS: sort the summary buffer handily.
;;
;; Revision 1.611  1994/08/29  02:18:37  esler
;; *** empty log message ***
;;
;; Revision 1.610  1994/08/25  22:02:41  esler
;; New CC Mode patch.
;;
;; Revision 1.609  1994/08/24  15:58:24  esler
;; Put full name in reply-to field of mail messages.
;;
;; Revision 1.608  1994/08/24  15:19:25  esler
;; Add top-level funcs: esler-run-vm, esler-run-gnus.
;;
;; Revision 1.607  1994/08/23  14:40:48  esler
;; GNUS: set gnus-auto-mail-to-author.
;;
;; Revision 1.606  1994/08/22  22:41:51  esler
;; Various.
;;
;; Revision 1.605  1994/08/11  14:23:32  esler
;; Add clo_rel_eng to my folder alist.
;;
;; Revision 1.604  1994/08/10  19:19:48  esler
;; Configure colouring via hilit19.el for slitex-mode.
;;
;; Revision 1.603  1994/08/10  19:12:17  esler
;; VM: get new mail clears the mode line "Mail" indication.
;; GNUS: when mailing articles, send them as RFC 934 digests.
;; Folding mode: configure marks for slitex.
;; Don't load Hyperbole; it screws up the order of load-path,
;; causing me to get the wrong version of some files.
;;
;; Revision 1.602  1994/08/09  15:46:36  esler
;; Dired Mode: get a "universal" directory path at the top of each
;; buffer.
;;
;; Revision 1.601  1994/08/02  22:16:41  esler
;; Typo.
;;
;; Revision 1.600  1994/08/02  22:15:27  esler
;; Fiddle with colours.
;; Get canonical true-path at top of Dired Mode buffers.
;;
;; Revision 1.599  1994/07/28  20:10:53  esler
;; Use hl319.el, instead of hilit19.el.
;;
;; Revision 1.598  1994/07/28  03:02:10  esler
;; Use build-mail-abbrevs, instead of build-mail-aliases.
;;
;; Revision 1.597  1994/07/21  20:48:49  esler
;; Provide menu for killing buffer AND frame.
;;
;; Revision 1.596  1994/07/21  14:02:58  esler
;; Configure Bibl Mode.
;;
;; Revision 1.595  1994/07/21  13:04:35  esler
;; Finish it.
;;
;; Revision 1.594  1994/07/21  13:02:22  esler
;; Update for cc-mode.
;;
;; Revision 1.593  1994/07/20  18:55:12  esler
;; Rearrange newsgroup-name-priority-list.
;;
;; Revision 1.592  1994/07/18  18:11:05  esler
;; Configure Winterp interface.
;;
;; Revision 1.591  1994/07/18  17:54:08  esler
;; Configure foldout.el: folding extensions to outline mode.
;;
;; Revision 1.590  1994/07/17  17:29:28  esler
;; Tidy up GNUS config.
;;
;; Revision 1.589  1994/07/06  15:19:00  esler
;; Configure HTML Mode.
;;
;; Revision 1.588  1994/07/05  22:43:06  esler
;; Configure gnus-hide.el, for hiding quotations etc.
;;
;; Revision 1.587  1994/07/05  19:21:26  esler
;; Adjust nntp-maximum-request down, to fix GNUS hang problem.
;;
;; Revision 1.586  1994/07/02  19:50:50  esler
;; Adjust for new version of cc-mode.el.
;; Try out gnus-em19.el: menus for GNUS.
;; Bill Sommerfeld's VC extensions require cl.el(gensym).
;; Turn off loading Hyperbole because of errors.
;; Code for making a request to Mosaic process (doesn't work at the
;; Mosaic end).
;; Be more flexible about form of local system name.
;; Get isearch bindings right when dialed up over XON/XOFF line.
;;
;; Revision 1.585  1994/06/02  19:20:18  esler
;; Various.
;;
;; Revision 1.584  1994/05/23  22:22:16  esler
;; Move cmubackground.el out of .../CMU/.
;;
;; Revision 1.583  1994/05/23  18:37:00  esler
;; Changed load-path to get my private stuff first.
;; I often get updates before they get into the GNU distribution.
;;
;; Revision 1.582  1994/05/23  15:22:42  esler
;; Imenu: Update config.
;; GNUS: Don't append signature.
;;
;; Revision 1.581  1994/05/20  21:19:13  esler
;; Configure new win-vm.el.
;;
;; Revision 1.580  1994/05/20  21:08:49  esler
;; Fix colour scheme for highlighting at Emacs 19.23.
;;
;; Revision 1.579  1994/05/19  14:45:11  esler
;; Turn off electric semi-colon, in C mode.
;;
;; Revision 1.578  1994/05/19  14:43:49  esler
;; Use comint-watch-for-password-prompt, new with 19.23.
;; Use mail-signature-file, new with 19.23.
;; Configure MIME software.
;; Make the colour of strings, in C code, more visible against a dark
;; background.
;;
;; Revision 1.577  1994/05/11  14:23:46  esler
;; Add alias for pushd and popd.
;;
;; Revision 1.576  1994/05/09  21:54:00  esler
;; Get better comint scrolling behaviour.
;;
;; Revision 1.575  1994/05/08  17:42:17  esler
;; Begin to configure a C indentation style.
;;
;; Revision 1.574  1994/05/06  14:52:08  esler
;; Configure Imenu.
;;
;; Revision 1.573  1994/05/05  00:38:59  esler
;; Configured C Outline Minor Mode.
;;
;; Revision 1.572  1994/04/25  17:26:27  esler
;; Don't use a pty for display-time.
;;
;; Revision 1.571  1994/04/11  14:50:57  esler
;; GNUS: save articles posted.
;;
;; Revision 1.570  1994/04/08  20:23:20  esler
;; *** empty log message ***
;;
;; Revision 1.569  1994/04/07  18:17:26  esler
;; Configure new W3.
;;
;; Revision 1.568  1994/04/04  15:55:24  esler
;; Configure new crypt++.el.
;;
;; Revision 1.567  1994/03/30  19:07:52  esler
;; Adjust vm-auto-folder-alist.
;;
;; Revision 1.566  1994/03/29  18:54:29  esler
;; We work for HP Company, not Corporation.
;;
;; Revision 1.565  1994/03/28  16:09:04  esler
;; Configure Gnuserv.
;; Mail saving alist update.
;; GNUS Mode: when mail-forwarding, don't append signature.
;;
;; Revision 1.564  1994/03/18  04:30:06  esler
;; *** empty log message ***
;;
;; Revision 1.563  1994/03/18  03:48:28  esler
;; *** empty log message ***
;;
;; Revision 1.562  1994/03/16  19:06:05  esler
;; Try using Gnuserv for a while.
;;
;; Revision 1.561  1994/03/16  18:57:46  esler
;; Minor VM adjustments.
;;
;; Revision 1.560  1994/03/11  15:09:09  esler
;; More XON/XOFF avoidance: c-mode and c++-mode.
;;
;; Revision 1.559  1994/03/11  06:41:18  esler
;; *** empty log message ***
;;
;; Revision 1.558  1994/03/09  02:29:06  esler
;; *** empty log message ***
;;
;; Revision 1.557  1994/03/09  02:02:19  esler
;; VM Mode: arrow keys in summary buffer move the message pointer.
;;
;; Revision 1.556  1994/03/09  01:53:43  esler
;; Cc Mode: get DDE-style arglist-intro indentation.
;;
;; Revision 1.555  1994/03/08  19:08:29  esler
;; Bind \C-x\C-b to 'buffer-menu.
;;
;; Revision 1.554  1994/03/07  22:08:56  esler
;; *** empty log message ***
;;
;; Revision 1.553  1994/03/07  21:03:47  esler
;; *** empty log message ***
;;
;; Revision 1.552  1994/03/07  20:14:28  esler
;; Refine the code for extracting Ange FTP paths from a buffer.
;;
;; Revision 1.551  1994/03/07  16:34:47  esler
;; More of : get Indented Text Mode behaviour in Mail Mode buffers.
;;
;; Revision 1.550  1994/03/04  23:54:16  esler
;; Get Indented Text Mode behaviour in Mail Mode buffers.
;;
;; Revision 1.549  1994/03/04  22:27:04  esler
;; Various.
;;
;; Revision 1.548  1994/03/04  17:19:37  esler
;; VM Mode "$" binding.
;;
;; Revision 1.547  1994/03/02  19:20:42  esler
;; Implement point-and-click on Ange-FTP extended pathnames.
;;
;; Revision 1.546  1994/02/25  16:35:50  esler
;; Fiddle with VM summary format.
;; M-C-z for isearch-forward-regexp when dialed in.
;;
;; Revision 1.545  1994/02/24  16:32:38  esler
;; VM Mode: marked messages ought to be shown as such.
;;
;; Revision 1.544  1994/02/16  18:12:26  esler
;; Various.
;;
;; Revision 1.543  1994/02/11  02:05:40  esler
;; Get View Mode turned off if we revert a buffer and the file
;; has become writeable (happens when we check out a Clearcase elt.)
;;
;; Revision 1.542  1994/02/10  05:42:48  esler
;; Tweak ediff mode.
;; Temporarily turn off hilighting of C mode buffers.
;; Replace c-mode and c++-mode with cc-mode.
;;
;; Revision 1.541  1994/02/07  23:04:25  esler
;; Configure Ediff package.
;;
;; Revision 1.540  1994/02/01  16:34:45  esler
;; Tidy up the VM config.
;;
;; Revision 1.539  1994/01/27  17:53:42  esler
;; Use filladapt.
;;
;; Revision 1.538  1994/01/27  02:25:44  esler
;; Warn if Emacs is invoked as root (usually a mistake).
;;
;; Revision 1.537  1994/01/26  20:02:42  esler
;; Clean up the VM configuration, for vm-5.49beta, and to find bug
;; with vm-visit-when-saving==t.
;;
;; Revision 1.536  1994/01/24  01:17:43  esler
;; Improve Ange FTP config. for non-HP connections.
;;
;; Revision 1.535  1994/01/24  00:48:44  esler
;; Configure w3, the World Wide Web browser.
;;
;; Revision 1.534  1994/01/24  00:29:10  esler
;; Gateway Ange FTP works now, using "rftp".
;; Misc. other changes.
;;
;; Revision 1.533  1993/12/15  20:35:15  esler
;; Not as trivial as I thought...
;;
;; Revision 1.532  1993/12/15  20:34:38  esler
;; Trivial.
;;
;; Revision 1.531  1993/12/15  19:55:56  esler
;; Tweaked vm-auto-folder-alist.
;;
;; Revision 1.530  1993/12/14  20:36:52  esler
;; Various.
;;
;; Revision 1.529  1993/12/03  20:21:09  esler
;; Remove shell mode filter. It is probably unnecessary now.
;;
;; Revision 1.528  1993/12/02  05:01:03  esler
;; Customise the menu bar with a "peel-off-frame" function.
;;
;; Revision 1.527  1993/12/02  04:57:51  esler
;; Install and configure (but don't enable, yet) Hyperbole.
;;
;; Revision 1.526  1993/11/30  07:47:28  esler
;; New comint features.
;; Better window behaviour for editclient requests, when dialed in.
;; Fix bug in Man mode customizations.
;;
;; Revision 1.525  1993/11/30  02:45:26  esler
;; Remove esler-comint-find-preceding-input.
;; It never worked 100%, and the comint code at 19.22 has one that
;; works right.
;;
;; Revision 1.524  1993/11/29  20:55:40  esler
;; Tags: provide command, tags-reset-location-stack.
;;
;; Revision 1.523  1993/11/24  19:58:28  esler
;; I forgot to remove debug code.
;;
;; Revision 1.522  1993/11/24  19:57:01  esler
;; Improve tags: I maintain a stack of "locations from whence I found a
;; tag", rather than a stack of visited tags.  This is better for
;; perusing code, usually.
;;
;; Revision 1.521  1993/11/24  18:02:16  esler
;; GNUS: improve the colour scheme.
;;
;; Revision 1.520  1993/11/23  22:36:51  esler
;; Configure the hilit19 package.
;;
;; Revision 1.519  1993/11/23  21:51:54  esler
;; Enable file local variables, but not file local evals.
;;
;; Revision 1.518  1993/11/22  23:46:44  esler
;; Finally get the emacs server window/frame behaviour reasonable.
;;
;; Revision 1.517  1993/11/19  21:34:15  esler
;; Clean up compilation errors.
;;
;; Revision 1.516  1993/11/19  16:33:09  esler
;; VM performance improvement.
;;
;; Revision 1.515  1993/11/18  17:51:09  esler
;; Further XON/XOFF avoidance: C-xC-z for save-buffer, instead of C-xC-s.
;;
;; Revision 1.514  1993/11/17  22:23:06  esler
;; Grep (and compilation mode): mouse-3 to select an occurrence.
;;
;; Revision 1.513  1993/11/17  22:19:08  esler
;; Unsuccessful attempt to fix strange frame/buffer/window behaviour when
;; taking an emacsclient request.
;;
;; Revision 1.512  1993/11/15  23:43:52  esler
;; Beginnings of decent frame-control, at least for Man Mode:
;;   - separate frame per Man Mode buffer
;;   - "q" deletes the frame and the buffer.
;;
;; Revision 1.511  1993/11/15  23:25:22  esler
;; Man Mode: get a new fram for each man page.
;;
;; Revision 1.510  1993/11/15  22:17:36  esler
;; Occur Mode: mouse-3 for selecting occurrence.
;;
;; Revision 1.509  1993/11/12  16:29:08  esler
;; Configure message logging.
;;
;; Revision 1.508  1993/11/11  17:05:30  esler
;; VM: Bug fix to advice to vm-save-message.
;;
;; Revision 1.507  1993/11/09  17:15:30  esler
;; Update list of newsgroups.
;;
;; Revision 1.506  1993/11/09  16:32:30  esler
;; *** empty log message ***
;;
;; Revision 1.505  1993/11/09  16:31:21  esler
;; VM: bind "f" to vm-followup.
;;
;; Revision 1.504  1993/11/04  16:32:38  esler
;; The advice on vm-save-message was wrong.
;;
;; Revision 1.503  1993/11/02  21:20:30  esler
;; VM improvement: buffers are saved after messages are written or save
;; to them.
;;
;; Revision 1.502  1993/10/29  18:32:56  esler
;; Dired Mode: editing linktext wasn't displaying the resultant link.
;;
;; Revision 1.501  1993/10/27  22:32:24  esler
;; No signatures on messages forwarded from VM.
;;
;; Revision 1.500  1993/10/27  18:20:13  esler
;; Attempt to get right window behaviour with emacs server, when dialed
;; in.
;;
;; Revision 1.499  1993/10/26  03:47:51  esler
;; Added to server-temp-file-regexp for Clearcase edit requests.
;;
;; Revision 1.498  1993/10/25  23:59:46  esler
;; More Meta key stuff.
;; Tweaked favourite newsgroup list.
;;
;; Revision 1.497  1993/10/22  17:15:37  esler
;; When XON/XOFF swallows C-q, use "`" for isearch-quote-char.
;;
;; Revision 1.496  1993/10/20  21:42:29  esler
;; Server-client frame stuff improved.
;;
;; Revision 1.495  1993/10/20  19:44:15  esler
;; Improve Emacs server frame behaviour slighly.
;;
;; Revision 1.494  1993/10/14  16:31:45  esler
;; Enable line number display in mode line.
;;
;; Revision 1.493  1993/10/12  16:47:13  esler
;; Get better prompt for getting rid of server screen.
;;
;; Revision 1.492  1993/10/08  14:52:55  esler
;; Fix problems with cut-and-paste mouse-behaviour.
;;
;; Revision 1.491  1993/10/07  20:49:07  esler
;; Drag mouse-1 always copies to the kill-ring, and the X11 selection
;; buffer, so that things work a lot more like an xterm.
;;
;; Revision 1.490  1993/10/06  21:19:35  esler
;; Added to vm-auto-folder-alist.
;;
;; Revision 1.489  1993/10/06  20:24:04  esler
;; Factor out comint-mode bindings correctly.
;;
;; Revision 1.488  1993/10/06  19:07:17  esler
;; Tidy up vm-auto-folder-alist.
;;
;; Revision 1.487  1993/10/04  22:18:08  esler
;; Man Mode: mouse-3 will follow the pointed-at man page reference.
;;
;; Revision 1.486  1993/10/04  21:22:30  esler
;; Remove binding for "G" in GNUS Group Mode.
;; No longer necessary; subsumed into "g".
;;
;; Revision 1.485  1993/09/30  17:53:09  esler
;; Configure the Meta key automatically.
;;
;; Revision 1.484  1993/09/28  21:07:46  esler
;; Enable the buffer time stamping facility.
;;
;; Revision 1.483  1993/09/27  15:14:22  esler
;; Factor some customizations into comin-mode-hook.
;;
;; Revision 1.482  1993/09/24  20:21:25  esler
;; Clean up.
;;
;; Revision 1.481  1993/09/22  21:09:20  esler
;; Clean up typo.
;;
;; Revision 1.480  1993/09/22  20:59:59  esler
;; Nothing much.
;;
;; Revision 1.479  1993/09/22  15:00:48  esler
;; Improved gnus-summary-mode "k" binding.
;;
;; Revision 1.478  1993/09/17  13:07:57  esler
;; Fiddle with gatewayed Ange FTP, unsuccessfully.
;; Use mouse-3 to switch screens when dialed in.
;;
;; Revision 1.477  1993/09/15  21:56:20  esler
;; Add folding mode syntax for makefile-mode.
;;
;; Revision 1.476  1993/09/15  21:33:01  esler
;; Clicking on ".." didn't work.
;;
;; Revision 1.475  1993/09/15  21:08:27  esler
;; Further simplification and cleanup.
;;
;; Revision 1.474  1993/09/15  16:52:44  esler
;; Further cleanup.
;;
;; Revision 1.473  1993/09/15  15:39:34  esler
;; Cut the rope: remove support for Emacs 18 and Epoch.
;; Remove old Domain cruft too.
;;
;; Revision 1.472  1993/09/14  18:31:22  esler
;; Get xterm-mouse.el working for Emacs 19.
;;
;; Revision 1.471  1993/09/13  18:56:11  esler
;; Set mail-yank-prefix, which affects GNUS citation.
;;
;; Revision 1.470  1993/09/13  18:54:29  esler
;; Use tabs for indentation when editing Makefiles.
;;
;; Revision 1.469  1993/09/03  18:46:33  esler
;; For various mouse-click-bloudnf functions: do (sit-for 0) after
;; (mouse-wet-point), so that the movement of point is evident.
;;
;; Revision 1.468  1993/09/02  15:48:19  esler
;; Fix typo.
;;
;; Revision 1.467  93/09/02  17:40:22  17:40:22  esler (Kevin Esler)
;; Try to get Ange FTP going through a gateway.
;;
;; Revision 1.466  1993/08/30  20:15:14  esler
;; *** empty log message ***
;;
;; Revision 1.465  1993/08/25  17:21:12  esler
;; Emacs 19: at last, correct code for resizing panes by dragging the
;;           dividers.
;;
;; Revision 1.464  1993/08/24  01:53:43  esler
;; *** empty log message ***
;;
;; Revision 1.463  1993/08/23  18:23:53  esler
;; Map //nodename to /net/nodename now, rather than /apollo/nodename.
;;
;; Revision 1.462  1993/08/12  18:25:25  esler
;; Emacs 19: mail abbrevs weren't being reset correctly after editing
;; ~/.mailrc.
;;
;; Revision 1.461  1993/08/10  19:52:20  esler
;; Emacs 19.18: get rid of scroll bar in initial frame.
;;
;; Revision 1.460  1993/08/10  19:48:58  esler
;; Emacs 19.19: set initial-frame-alist.
;;              hippie.el was renamed hippie-exp.el.
;;
;; Revision 1.459  1993/08/10  18:21:22  esler
;; Emacs 19 Version 19.18: needed to adjust
;; 'mouse19-global-mouse3-handler.
;;
;; Revision 1.458  1993/08/09  13:43:07  esler
;; GNUS Summary Mode: rebind some keys from C-s to C-z so I can
;; sort summaries when dialed in over an XON/XOFF line.
;;
;; Revision 1.457  1993/08/07  00:14:33  esler
;; Fix typo.
;;
;; Revision 1.456  1993/08/06  23:24:59  esler
;; Clean up.
;;
;; Revision 1.455  1993/08/06  23:14:17  esler
;; Emacs 19: don't bother setting up man page libraries.
;;
;; Revision 1.454  1993/08/06  18:18:18  esler
;; Fix typo.
;;
;; Revision 1.453  1993/08/06  18:15:18  esler
;; Configure hippie, the general purpose, extensible completer/expander.
;;
;; Revision 1.452  1993/08/06  16:54:46  esler
;; Only start an edit server in the "primary" Emacs process, and not in
;; the ones I use just to read Mail and News.
;;
;; Revision 1.451  1993/08/06  15:41:11  esler
;; Remove typo.
;;
;; Revision 1.450  1993/08/05  23:02:21  esler
;; Emacs 19: VM Mode: bugfix to allow vm-edit-message to work.
;;
;; Revision 1.449  1993/08/05  13:55:10  esler
;; Insert a progn.
;;
;; Revision 1.448  1993/08/04  19:07:08  esler
;; Fix problem with Info Mode customisation in Emacs 19.
;; Rename putenv to setenv.
;;
;; Revision 1.447  1993/08/03  20:03:15  esler
;; Emacs 19: Info Mode: add my personal Info directory to the list of
;; them.
;;
;; Revision 1.446  1993/07/30  19:49:52  esler
;; Changed my email address from @apollo.hp.com to @ch.hp.com.
;; (sigh)
;;
;; Revision 1.445  1993/07/29  19:55:04  esler
;; Adjusted newsgroup-name-priority-list.
;;
;; Revision 1.444  1993/07/28  17:10:53  esler
;; Point-and-click: don't include ":" in pathnames.
;; Makes it easier to use with grep output.
;;
;; Revision 1.443  1993/07/14  22:16:20  esler
;; Emacs 19: got frame-popping behaviour for emacsserver correct at last.
;;
;; Revision 1.442  1993/07/14  21:53:09  esler
;; Emacs 19: get emacsserver working as I want it.
;;
;; Revision 1.441  1993/07/14  19:55:29  esler
;; Updated the "TO DO" list.
;;
;; Revision 1.440  1993/07/14  16:46:23  esler
;; Added warning comment about View Buffer Mode.
;;
;; Revision 1.439  1993/07/14  16:43:58  esler
;; I think the folding-mode-hooks have to run after the
;; view-buffer-mode-hooks.
;;
;; Revision 1.438  1993/07/14  15:22:03  esler
;; Emacs 19: Tar Mode: point-and-click at tar tile entries.
;;
;; Revision 1.437  1993/07/14  04:04:21  esler
;; I prefer View Buffer minorish mode over View Major Mode.
;;
;; Revision 1.436  1993/07/14  04:00:26  esler
;; Emacs 19:  ps-mode didn't work.
;;
;; Revision 1.435  1993/07/13  21:52:12  esler
;; vmsort.el doesn't apply to Emacs 18, probably.
;;
;; Revision 1.434  1993/07/13  16:15:38  esler
;; Fix syntax error.
;;
;; Revision 1.433  1993/07/13  16:11:37  esler
;; View Mode uses C-c to kill the buffer.
;; This clashes with Folding Minor Mode's bindings, so remove it.
;;
;; Revision 1.432  1993/07/13  16:05:51  esler
;; Remove "q" from the set of keys bound in
;; 'esler-standard-readonly-buffer-key-bindings.
;;
;; Revision 1.431  1993/07/13  05:15:10  esler
;; Emacs 19: try out View Major Mode for readonly files.
;;
;; Revision 1.430  1993/07/13  04:53:46  esler
;; Emacs 19: make incremental search work in the presence of XON/XOFF
;; flow control.
;;
;; Revision 1.429  1993/07/13  04:16:35  esler
;; Clean up.
;;
;; Revision 1.428  1993/07/13  04:14:57  esler
;; Get back the default VM behaviour for "R" (reply with citation).
;;
;; Revision 1.427  1993/07/12  22:34:22  esler
;; There were some dangling references to a stale pathname.
;;
;; Revision 1.426  1993/07/12  15:27:37  esler
;; Dump Pascal mode.
;;
;; Revision 1.425  1993/07/12  14:34:08  esler
;; Apparently, the code in .emacs is protected by a condition-case,
;; so setting debug-on-error is useless.
;;
;; Revision 1.424  1993/07/09  21:46:33  esler
;; Bind F1 to 'call-last-kbd-macro
;; Bind F2 to 'repeat-complex-command.
;;
;; Revision 1.423  1993/07/09  19:24:06  esler
;; Try out Zawinski's vmsort.el package, which looks promising.
;;
;; Revision 1.422  1993/07/09  13:25:03  esler
;; The signature of build-mail-aliases changed for Emacs 19.
;;
;; Revision 1.421  1993/07/08  04:05:22  esler
;; Emacs 19: VM Mode: replace value for vm-generic-header-regexp, with a
;;                    supposedly more performant version.
;;
;; Revision 1.420  1993/07/07  22:21:38  esler
;; Had non-existent colour for the region background.
;;
;; Revision 1.419  1993/07/07  20:55:16  esler
;; Various.
;;
;; Revision 1.418  1993/07/06  15:58:58  esler
;; esler-dired-down didn't work.
;;
;; Revision 1.417  1993/07/06  13:12:32  esler
;; I was setting 'kill-emacs-hook wrongly for emacs 18.
;;
;; Revision 1.416  1993/06/30  22:30:03  esler
;; Just testing VC checkin.
;;
;; Revision 1.415  1993/06/30  22:26:27  esler
;; Emacs 19: use VC Mode instead of the old RCS mode.
;;           It's much slicker.
;;
;; Revision 1.414  1993/06/30  17:47:05  esler
;; Emacs 19: set up a separate personal elisp tree for Emacs 19,
;;           since the output of Emacs 19's elisp compiler won't
;;           run in Emacs 18.
;;
;; Revision 1.413  93/06/29  12:03:00  12:03:00  esler (Kevin Esler)
;; Get the Clearcase hacks in slightly better shape.
;; I now get the version name in the Emacs 19 frame title.
;;
;; Revision 1.412  93/06/28  15:10:51  15:10:51  esler (Kevin Esler)
;; Fix typo.
;;
;; Revision 1.411  93/06/28  11:41:07  11:41:07  esler (Kevin Esler)
;; Emacs 19: mouse-driven resizing of panes.  Still not quite right,
;;           though.
;;
;; Revision 1.410  93/06/28  10:25:16  10:25:16  esler (Kevin Esler)
;; VM Mode: keep all message buffers.
;;
;; Revision 1.409  93/06/25  15:40:56  15:40:56  esler (Kevin Esler)
;; Emacs 19: Dired Mode: "K" keeps only those entries matching a regexp.
;;                       This still doesn't maintain the mode line
;;                       correctly.
;;
;; Revision 1.408  93/06/25  11:53:00  11:53:00  esler (Kevin Esler)
;; Remove debug.
;;
;;
;; Revision 1.407  93/06/25  11:50:48  11:50:48  esler (Kevin Esler)
;; Emacs 19: VM Mode: Mouse-3 reads the mail message.
;;
;; Revision 1.406  93/06/25  11:16:23  11:16:23  esler (Kevin Esler)
;; VM Mode: get a better "From" header in outgoing messages.
;;
;; Revision 1.405  93/06/24  11:12:51  11:12:51  esler (Kevin Esler)
;; Emacs 19: get the highlighted region dynamically updated by
;;           dragging button-1.
;;
;;
;; Revision 1.404  93/06/23  23:01:04  23:01:04  esler (Kevin Esler)
;; Small bug in 'lisp-interaction-mode-hook.
;;
;; Revision 1.403  93/06/23  16:49:59  16:49:59  esler (Kevin Esler)
;; Emacs 19: Dired Mode: turn off confirmation for various things.
;; Lisp Interaction Mode: make "\eg" be like the apollo "Again" key.
;;
;;
;; Revision 1.402  93/06/22  12:09:26  12:09:26  esler (Kevin Esler)
;; Emacs 19: get a more useful colour for the highlighted region.
;;
;; Revision 1.401  93/06/21  15:06:48  15:06:48  esler (Kevin Esler)
;; Not much.
;;
;; Revision 1.400  93/06/21  14:47:52  14:47:52  esler (Kevin Esler)
;; Emacs 19: turn off autoraise and presumably leave it up to the
;;           window manager.
;;
;; Revision 1.399  93/06/21  14:42:27  14:42:27  esler (Kevin Esler)
;; Save the beginnings of a functionf or code reviews.
;;
;;
;;
;;
;;
;; Revision 1.398  93/06/21  09:35:24  09:35:24  esler (Kevin Esler)
;; Emacs 19: bug in mouse handler when text buffer was not folded.
;;
;;
;;
;;
;; Revision 1.397  93/06/21  09:29:01  09:29:01  esler (Kevin Esler)
;; Emacs 19: shell buffers no longer force themselves to be visible with
;;           each new output.
;;
;;
;;
;; Revision 1.396  93/06/18  10:24:46  10:24:46  esler (Kevin Esler)
;; Fix small bug in Emacs 19 mouse bindings.
;;
;; Revision 1.395  93/06/17  15:16:19  15:16:19  esler (Kevin Esler)
;; Emacs 19: get buffer menu from mouse3 in minibuffer.
;;
;;
;;
;; Revision 1.394  93/06/17  10:03:04  10:03:04  esler (Kevin Esler)
;; Fix typo.
;;
;;
;; Revision 1.393  93/06/17  10:02:26  10:02:26  esler (Kevin Esler)
;; In C and C++ Modes: M-C-h now bound to 'mark-c-function.
;;
;; Revision 1.392  93/06/17  09:53:56  09:53:56  esler (Kevin Esler)
;; Clean up the way hooks are used by uniformly using 'add-hook.
;;
;; Revision 1.391  93/06/17  08:44:55  08:44:55  esler (Kevin Esler)
;; Emacs 19: Buffer selection by mouse.
;;
;; Revision 1.390  93/06/15  19:49:48  19:49:48  esler (Kevin Esler)
;; Fix bug in GNUS mouse binding code.
;;
;; Revision 1.389  93/06/15  16:20:45  16:20:45  esler (Kevin Esler)
;; Emacs 19: to get desired frame configuration, I needed to set
;;           'default-frame-alist.
;;
;; Revision 1.388  93/06/15  16:08:41  16:08:41  esler (Kevin Esler)
;; Emacs 19: GNUS Modes: get mouse bindings working.
;;
;; Revision 1.387  93/06/15  15:26:38  15:26:38  esler (Kevin Esler)
;; Emacs 19: Info Mode: Mouse-3 for navigation.
;;
;; Revision 1.386  93/06/15  15:15:30  15:15:30  esler (Kevin Esler)
;; Emacs 19: use it's native mail alias expanding facility.
;;
;; Revision 1.385  93/06/14  11:35:27  11:35:27  esler (Kevin Esler)
;; Emacs 19 had no binding for 'split-window-horizontally.
;; Bind it to C-x 7.
;;
;; Revision 1.384  93/06/13  22:07:58  22:07:58  esler (Kevin Esler)
;; Configure mail-abbrevs.el correctly for Emacs 1y9.
;;
;; Revision 1.383  93/06/10  13:07:51  13:07:51  esler (Kevin Esler)
;; Emacs 19: using the mouse to move around in a folded buffer.
;;
;;
;;
;;
;; Revision 1.382  93/06/10  12:13:54  12:13:54  esler (Kevin Esler)
;; More Emacs 19 stuff: coloured cursors and pencil-shaped mouse-cursor.
;;
;;
;;
;; Revision 1.381  93/06/09  18:08:49  18:08:49  esler (Kevin Esler)
;; More Emacs 19 stuff: turn off scroll bars.
;;
;;
;; Revision 1.380  93/06/09  15:44:08  15:44:08  esler (Kevin Esler)
;; More Emacs 19 stuff.
;;
;; Revision 1.379  93/06/09  11:36:05  11:36:05  esler (Kevin Esler)
;; More reorganisation for Emacs19.
;;
;; Revision 1.378  93/06/08  19:24:59  19:24:59  esler (Kevin Esler)
;; Updated newsgroup-name-priority-list.
;;
;;
;; Revision 1.377  93/06/05  18:31:17  18:31:17  esler (Kevin Esler)
;; Shell mode customisation for Emacs 19.
;;
;; Revision 1.376  93/06/05  17:55:36  17:55:36  esler (Kevin Esler)
;; More Dired Mode Emacs 19 repairs.
;;
;; Revision 1.375  93/06/05  13:54:42  13:54:42  esler (Kevin Esler)
;; More Dired Mode adjustments for Emacs 19.
;; Display Emacs major version on default mode line.
;;
;; Revision 1.374  93/06/05  13:34:32  13:34:32  esler (Kevin Esler)
;; Make Dired Mode more Emacs 19 compatible.
;;
;; Revision 1.373  93/06/04  15:04:45  15:04:45  esler (Kevin Esler)
;; Add some Dired bindings more like Emacs 19.
;;
;; Revision 1.372  93/05/30  02:11:47  02:11:47  esler (Kevin Esler)
;; More conversion to GNUS 3.15.
;;
;; Revision 1.371  93/05/30  01:08:32  01:08:32  esler (Kevin Esler)
;; Tweaked my newsgroup list.
;;
;; Revision 1.370  93/05/28  16:21:56  16:21:56  esler (Kevin Esler)
;; Yet more GNUS 3.15 conversion.
;;
;;
;; Revision 1.369  93/05/28  16:16:13  16:16:13  esler (Kevin Esler)
;; More GNUS 3.15 conversion.
;;
;;
;; Revision 1.368  93/05/28  16:14:25  16:14:25  esler (Kevin Esler)
;; More Emacs 19 work: convert to GNUS 3.15, which is what comes
;; with Emacs 19.
;;
;;
;; Revision 1.367  93/05/28  15:18:19  15:18:19  esler (Kevin Esler)
;; For Emacs 19: use the inboard GNUS, instead of the outboard.
;;
;; Revision 1.366  93/05/28  14:59:09  14:59:09  esler (Kevin Esler)
;; Initial stab at making this work for Emacs version 19.9.
;;
;; Revision 1.365  93/05/12  10:49:08  10:49:08  esler (Kevin Esler)
;; Allowed folded Scheme files.
;;
;; Revision 1.364  93/04/16  15:36:07  15:36:07  esler (Kevin Esler)
;; Used the new ctypt++.el in place of crypt.el.
;; Note that this will probably look for the
;; "gzip" binary program to execute it.
;;
;; Revision 1.363  93/04/12  11:52:52  11:52:52  esler (Kevin Esler)
;; *** empty log message ***
;;
;; Revision 1.362  93/03/17  23:07:03  23:07:03  esler (Kevin Esler)
;; VM Mode: turned off vm-skip-deleted-messages.
;;
;; Revision 1.361  93/03/16  12:37:59  12:37:59  esler (Kevin Esler)
;; Various.
;;
;; Revision 1.360  93/03/03  12:03:07  12:03:07  esler (Kevin Esler)
;; In Dired Mode: when renaming an object, if the target is an existing
;;                directory, it will be assumed that the user wishes to
;;                move the object INTO that directory, and will be given
;;                the option of denying this assumption.
;;
;; Revision 1.359  93/02/24  23:59:25  23:59:25  esler (Kevin Esler)
;; In VM Mode: use the "movemail" program that came with the invoking Emacs.
;;
;; Revision 1.358  93/02/23  18:00:48  18:00:48  esler (Kevin Esler)
;; Tweaked vm-auto-folder-alist.
;;
;; Revision 1.357  93/02/22  11:38:09  11:38:09  esler (Kevin Esler)
;; Souped up my vm-auto-folder-alist.
;;
;; Revision 1.356  93/02/19  17:44:22  17:44:22  esler (Kevin Esler)
;; VM Mode: go back to grouping by physical order so my primary INBOX
;;          is sensibly ordered.
;;
;;
;; Revision 1.355  93/02/19  17:21:57  17:21:57  esler (Kevin Esler)
;; GNUS Mode: use threading to hide subtrees.
;;
;; Revision 1.354  93/02/19  11:54:11  11:54:11  esler (Kevin Esler)
;; Various.
;;
;; Revision 1.353  93/02/11  23:30:16  23:30:16  esler (Kevin Esler)
;; In VM Mode: group by subject is now the default.
;;
;; Revision 1.352  93/02/10  19:58:15  19:58:15  esler (Kevin Esler)
;; In Dired Mode: give message indicating copying in progress during a
;;                file copy.
;;
;; Revision 1.351  93/02/09  11:20:52  11:20:52  esler (Kevin Esler)
;; In Dired Mode: copying or renaming to a file which already exists
;;                will request confirmation, instead of just failing.
;;
;;
;; Revision 1.350  93/02/05  17:15:15  17:15:15  esler (Kevin Esler)
;; Add an entry to vm-invisible-header-regexp.
;;
;; Revision 1.349  93/02/05  16:24:39  16:24:39  esler (Kevin Esler)
;; VM Mode: change my incoming mailbox location because now I use POP for
;; mail delivery.
;;
;; Revision 1.348  93/02/05  14:26:09  14:26:09  esler (Kevin Esler)
;; I had screwed up the parentheses, so the file wouldn't parse.
;;
;;
;; Revision 1.347  93/02/05  13:01:34  13:01:34  esler (Kevin Esler)
;; Fixed the doc string in 'esler-dired-keep-directories.
;;
;; Revision 1.346  93/02/05  12:55:05  12:55:05  esler (Kevin Esler)
;; In Dired Mode: "D" hides all entries which are not directories.
;;
;;
;; Revision 1.345  93/02/05  12:07:51  12:07:51  esler (Kevin Esler)
;; Added hack: pak-ninstallers.
;;
;; Revision 1.344  93/01/26  13:55:38  13:55:38  esler (Kevin Esler)
;; In Dired Mode: "L" adds an entry into ChangeLog, in the current directory.
;;
;; Revision 1.343  93/01/23  16:17:00  16:17:00  esler (Kevin Esler)
;; File suffixed ".ml" and ".ML" imply SML Mode.
;;
;; Revision 1.342  93/01/20  11:18:50  11:18:50  esler (Kevin Esler)
;; Tweaked newsgroup-name-priority-list.
;;
;; Revision 1.341  93/01/18  14:07:42  14:07:42  esler (Kevin Esler)
;; Improved my predicate 'clearcase-element-p.
;;
;;
;;
;;
;; Revision 1.340  93/01/12  16:49:25  16:49:25  esler (Kevin Esler)
;; In Process Mode: give indication that process information is being refreshed.
;;
;;
;; Revision 1.339  93/01/12  13:50:49  13:50:49  esler (Kevin Esler)
;; In Dired Mode: use "/bin/ls", instead of "ls".
;;
;; Revision 1.338  93/01/07  11:53:43  11:53:43  esler (Kevin Esler)
;; In Dired Mode: always use /bin/ls.
;;
;; Revision 1.337  93/01/06  10:44:05  10:44:05  esler (Kevin Esler)
;; Configured Scheme support.
;; Changed the way load-path gets set, so I can avoid site-local
;; customisations, yet still get the GNU-supplied Lisp code.
;;
;; Revision 1.336  93/01/05  09:30:37  09:30:37  esler (Kevin Esler)
;; Mail abbrev expansion package was not always being activated when needed.
;;
;; Revision 1.335  93/01/02  17:47:57  17:47:57  esler (Kevin Esler)
;; In Process Mode: "g" refreshes the process list.
;;
;;
;; Revision 1.334  92/12/31  12:18:04  12:18:04  esler (Kevin Esler)
;; Fixed a bug in esler-dired-copy-file.
;;
;; Revision 1.333  92/12/23  11:43:11  11:43:11  esler (Kevin Esler)
;; In Process Mode: set my standardised bindings for readonly-buffers.
;;
;; Revision 1.332  92/12/22  13:44:59  13:44:59  esler (Kevin Esler)
;; Configured the package, 'location, which enhances the tags facility,
;; to enable backtracking through tags already located.
;;
;; Revision 1.331  92/12/22  11:23:19  11:23:19  esler (Kevin Esler)
;; Removed incorrect comment.
;;
;; Revision 1.330  92/12/18  11:19:21  11:19:21  esler (Kevin Esler)
;; Added completion to 'save-link-to-current-file.
;;
;; Revision 1.329  92/12/11  12:18:50  12:18:50  esler (Kevin Esler)
;; First stab at a set of standardised "readonly buffer" key bindings.
;;
;; Revision 1.328  92/12/07  17:56:27  17:56:27  esler (Kevin Esler)
;; Disabled my C Mode hook. It was slowing things down too much.
;;
;; Revision 1.327  92/12/04  23:26:10  23:26:10  esler (Kevin Esler)
;; Use C-z for incremental search when XON/XOFF is an obstacle.
;;
;; Revision 1.326  92/12/04  19:29:47  19:29:47  esler (Kevin Esler)
;; Two changes: 1. fixed bug in esler-clear-mail-from-mode-line.
;;              2. In Dired Mode: when running in a Clearcase view:
;;                 "/" at a directory entry "foo.c" which is neither a directory
;;                 nor a link directory, will attempt to invoke Dired Mode
;;                 on the supposed Clearcase version directory "foo.c@@",
;;                 allowing me to easily browse the version tree.
;;
;; Revision 1.325  92/12/03  23:19:58  23:19:58  esler (Kevin Esler)
;; Deal with terminal server swallowing C-q, when I'm dialed in.
;;
;; Revision 1.324  92/12/03  17:20:57  17:20:57  esler (Kevin Esler)
;; In Indented Text Mode: CR at the left margin just does newline (no indenting).
;;
;; Revision 1.323  92/12/03  10:54:17  10:54:17  esler (Kevin Esler)
;; VM Mode needs Jamie Zawinski's mail-abbrev package preloaded.
;;
;; Revision 1.322  92/12/01  21:21:29  21:21:29  esler (Kevin Esler)
;; Added getenv and putenv functions.
;;
;; Revision 1.321  92/12/01  14:57:15  14:57:15  esler (Kevin Esler)
;; Configured the Gnuserv package, so that Clearcase will use Epoch/Emacs
;; for editing.
;;
;; Revision 1.320  92/12/01  10:51:37  10:51:37  esler (Kevin Esler)
;; In Dired Mode: if "/" was used to enter a linked-to subdirectory,
;;                "\" would not leave the curson in the right position
;;                when returning to the superior directory.
;;
;;
;; Revision 1.319  92/11/30  22:00:42  22:00:42  esler (Kevin Esler)
;; Added some hooks to enable/disable xterm mouse event to character translations.
;;
;; Revision 1.318  92/11/29  12:08:30  12:08:30  esler (Kevin Esler)
;; Fixed a typo.
;;
;; Revision 1.317  92/11/28  21:26:10  21:26:10  esler (Kevin Esler)
;; In Dired Mode: 'esler-dired-edit-file-name (bound to "r")
;; and 'esler-dired-edit-linktext (bound to "E"), now do completion.
;;
;; Revision 1.316  92/11/25  11:57:56  11:57:56  esler (Kevin Esler)
;; Added ts -- an abbreviation for tags-search.
;;
;;
;; Revision 1.315  92/11/23  11:25:13  11:25:13  esler (Kevin Esler)
;; In Dired Mode: replaced dired-copy-file with esler-dired-copy-file,
;;                which will allow a directory name as the destination.
;;
;;
;; Revision 1.314  92/11/21  13:31:19  13:31:19  esler (Kevin Esler)
;; I had left a call to 'debug in.
;;
;; Revision 1.313  92/11/21  13:05:28  13:05:28  esler (Kevin Esler)
;; Cleaned up mail and VM Modes.
;; Obsoleted some stuff.
;;
;; Revision 1.312  92/11/20  12:25:54  12:25:54  esler (Kevin Esler)
;; Make carriage return do newline-and-indent in Indented Text Mode.
;;
;; Revision 1.311  92/11/20  00:05:00  00:05:00  esler (Kevin Esler)
;; Make mouse cutting and pasting work over x-sb-mouse, like in xterms.
;;
;;
;; Revision 1.310  92/11/19  23:34:39  23:34:39  esler (Kevin Esler)
;; Cleaned up my Indented Text Mode code.
;;
;; Revision 1.309  92/11/18  13:27:05  13:27:05  esler (Kevin Esler)
;; Defined tqr as an abbrev for tags-query-replace-regexp.
;;
;; Revision 1.308  92/11/13  16:36:53  16:36:53  esler (Kevin Esler)
;; Adjusted GNUS mouse bindings.
;;
;;
;; Revision 1.307  92/11/13  11:46:36  11:46:36  esler (Kevin Esler)
;; Adjusted gnus-large-newsgroup.
;;
;; Revision 1.306  92/11/12  15:56:49  15:56:49  esler (Kevin Esler)
;; Added some mode-specific x-sb-mouse bindings:
;;    button 3 selects:
;;           newsgroup
;;           news article
;;           email message.
;;
;; Revision 1.305  92/11/12  12:17:03  12:17:03  esler (Kevin Esler)
;; Unsuccessful attempt to correct DDE C indentation style
;; so braces weren't electric.
;;
;; Revision 1.304  1992/11/10  21:16:21  esler
;; Cleaned up NU site specifics.
;;
;; Revision 1.303  1992/11/10  19:51:05  esler
;; Fixed typo in esler-dired-follow-link.
;;
;; Revision 1.302  1992/11/10  06:30:47  esler
;; Tweak x-sb-mouse.
;;
;; Revision 1.301  1992/11/09  21:20:03  esler
;; X-SB-Mouse now works in conjunction with xterm-mouse.
;; We just need to refine the event bindings so we don't bind to
;; functions that attempt to perform X11 functions (and fail).
;;
;; Revision 1.300  1992/11/09  18:11:44  esler
;; Fix problem with x-sbm-mouse configuration.
;;
;; Revision 1.299  1992/11/09  17:46:50  esler
;; Cosmetic.
;;
;; Revision 1.298  1992/11/09  17:45:36  esler
;; Configured the X-SB-Mouse package, giving very flexible mouse support
;; for Emacs (not Epoch).
;;
;; Revision 1.297  1992/11/09  05:17:24  esler
;; Installed new and improved RCS package.
;;
;; Revision 1.296  1992/11/09  05:09:21  esler
;; Initial cut at xterm-mouse package installed.
;; It allows use of the mouse, when Emacs is running inside an xterm
;; window via a serial interface.
;;
;; Revision 1.295  1992/11/06  21:22:12  esler
;; Configured the c-style package, so I can have automaticaly selected,
;; buffer-specific C layout styles.
;;
;; Revision 1.294  1992/11/03  17:19:28  esler
;; In Dired Mode: bind "F" to 'esler-dired-follow-link
;; (there might be a better name for what this function does).
;;
;; Revision 1.293  1992/11/02  22:45:57  esler
;; Further NU hacks.
;;
;; Revision 1.292  1992/11/02  19:20:23  esler
;; Fixed (I hope) a bug in my CMU Shell Mode process filter.
;;
;; Revision 1.291  1992/11/01  23:28:51  esler
;; In CMU Shell Mode: map LF to 'comint-send-input, because Apollo telnet
;; seems to map ENTER to LF, rather than CR.
;;
;; Revision 1.290  1992/11/01  19:32:37  esler
;; Incorporated more site-specific stuff for Northeastern University
;; environment.
;;
;; Revision 1.289  1992/11/01  00:13:37  esler
;; Added site-specific hacks for the environment at Northeastern.
;;
;; Revision 1.288  1992/10/28  21:36:07  esler
;; Bind: C-w to kill-region safe.
;;
;; Revision 1.287  1992/10/27  16:27:35  esler
;; In Process Mode: "n" and "p" bound to the appropriate cursor motion
;; commands.
;;
;; Revision 1.286  1992/10/27  04:07:15  esler
;; More dial-in specials.
;;
;; Revision 1.285  1992/10/26  20:36:46  esler
;; Fixed bug in esler-sort-du-output.
;;
;; Revision 1.284  1992/10/25  22:57:37  esler
;; Configured the "screens" package to give me multiple virtual screens
;; when dialed in from home.
;;
;; Revision 1.283  1992/10/24  18:13:41  esler
;; Wrote longer, wider for resizing the screen dynamically.
;;
;; Revision 1.282  1992/10/23  03:07:27  esler
;; Added mailrc-mode: causes the mail abbrevs to be updated after any
;; update to ~/.mailrc.
;;
;; Revision 1.281  1992/10/22  16:56:58  esler
;; Wrote esler-sort-du-output.
;;
;; Revision 1.280  1992/10/17  01:03:01  esler
;; First attempt at 'esler-print-folder.
;;
;; Revision 1.279  1992/10/16  18:40:24  esler
;; Added the useful function edit-variable.
;;
;; Revision 1.278  1992/10/06  15:23:20  esler
;; Added to my list of favourite newsgroups.
;; Incorporated Tony Tye's improvement to esler-vm-print-message.
;;
;; Revision 1.277  1992/09/09  15:22:56  esler
;; Added to 'newsgroup-name-priority-list.
;;
;; Revision 1.276  1992/09/04  21:25:18  esler
;; Fixed bug in function esler-dired-visit-vm-folder.
;;
;; Revision 1.275  1992/09/04  10:52:32  esler
;; Fixed a bug in function esler-fold-enfold-indented-buffer.
;;
;; Revision 1.274  92/09/03  16:30:59  16:30:59  esler (Kevin Esler)
;; Rearranged newsgroup-name-priority-list.
;;
;; Revision 1.273  1992/09/02  19:28:35  esler
;; Get my ~/.signature automatically appended to my mail messages.
;;
;; Revision 1.272  1992/09/02  15:50:01  esler
;; Improved the manual reader function, so that "q" makes the man page
;; window disappear, but keeps the buffer around, so I can look at it
;; again.
;;
;; Revision 1.271  1992/09/01  20:45:50  esler
;; VM Mode: change the format of the subject of a forwarded message.
;;
;; Revision 1.270  1992/09/01  16:53:19  esler
;; Wrote function mmdf-to-unix to convert mail folders.
;;
;; Revision 1.269  1992/09/01  15:00:38  esler
;; Nothing significant.
;;
;; Revision 1.268  1992/08/31  18:08:33  esler
;; Set 'debug-on-error to true for the duration of .emacs.
;;
;; Revision 1.267  1992/08/27  19:33:15  esler
;; VM Mode: add some single-key bindings for manipulating the window
;; arrangement.
;;
;; Revision 1.266  1992/08/26  19:52:46  esler
;; Re-arrange a few functions.
;;
;; Revision 1.265  1992/08/26  17:51:18  esler
;; Enhanced esler-dired-down, for the case when the pointed-at file is a
;; symbolic link.
;;
;; Revision 1.264  1992/08/25  21:15:24  esler
;; Minor change to gnus-article-save-directory variable.
;;
;; Revision 1.263  1992/08/25  20:39:08  esler
;; GNUS Mode: made the function which saves articles a bit more
;; intelligent about choosing a default folder name.  It will use the
;; last folder that was saved to, unless the newsgroup has changed since
;; then, in which case it will use name of rht newsgroup.
;;
;; Revision 1.262  1992/08/24  18:42:02  esler
;; Tweaked 'vm-invisible-header-regexp.
;;
;; Revision 1.261  1992/08/21  19:09:20  esler
;; In GNUS Mode: the default folder name should always be the name of the
;; current newsgroup, rather than the last folder saved to.
;;
;; Revision 1.260  1992/08/19  16:45:06  esler
;; Because of incompatible change to Epoch, "\M-x" no longer means the
;; same as "\ex".  Since I don't yet use a real Meta key, I am just
;; changing all the "\M"s to "\e"s.
;;
;; Revision 1.259  1992/08/17  19:14:02  esler
;; Removed some junk.
;;
;; Revision 1.258  1992/08/17  18:53:28  esler
;; Wrote 'esler-iterate-over-tag-table-files.
;;
;; Revision 1.257  1992/08/17  16:34:07  esler
;; Made 'vm-invisible-header-regexp more complete, to eliminate more
;; clutter.
;;
;; Revision 1.256  1992/08/17  15:26:20  esler
;; Wrote a crude memoised version of 'shell-command-on-region and bound
;; it to "\M-|".
;;
;; Revision 1.255  1992/08/16  22:18:34  esler
;; In GNUS Mode: in the Subject buffer: "o" will now correctly save the
;; article in a VM-Mode-readable folder.
;;
;; Revision 1.254  1992/08/10  19:53:43  esler
;; In Dired Mode: "t" to select a tags file, now tells you the result of
;; the action.
;;
;; Revision 1.253  1992/08/07  18:32:57  esler
;; Use a safer way of replacing the Dired Mode functions which I
;; override, so that re-evaluating .emacs will not have nasty
;; consequences.
;;
;; Revision 1.252  1992/08/06  19:47:16  esler
;; In Dired Mode: "\M-g" copies the filename under the cursor to the end
;; of the associated CMU Shell Mode buffer.
;; Renamed the Lispdir datafile.
;;
;; Revision 1.251  1992/08/06  15:13:10  esler
;; Tweaked list of favourite newsgroups.
;;
;; Revision 1.250  1992/08/05  20:26:48  esler
;; Back out the changes I had made to 'esler-cmushell-process-filter.
;; They seemed to cause strange problems, whereby the buffer context
;; would be wrong sometimes.
;;
;; Revision 1.249  1992/08/05  19:16:12  esler
;; Bind C-^ to 'enlarge-window.
;;
;; Revision 1.248  1992/08/05  14:45:08  esler
;; Renamed VMail to VM.
;;
;; Revision 1.247  1992/08/04  23:57:38  esler
;; Get it right.
;;
;; Revision 1.246  1992/08/04  23:51:44  esler
;; In VM Mode: "a" creates a new alias in ~/.mailrc for the sender of the
;; current message.
;;
;; Revision 1.245  1992/08/04  22:49:17  esler
;; Installed Jamie Zawinski's mail-abbrev package for expanding mail
;; addresses.
;;
;; Revision 1.244  1992/08/03  20:52:08  esler
;; Tweaked 'newsgroup-name-priority-list.
;;
;; Revision 1.243  1992/08/03  15:06:51  esler
;; Tweaked 'newsgroup-name-priority-list.
;;
;; Revision 1.242  1992/07/28  18:17:29  esler
;; Added to my list of favourite newsgroups.
;;
;; Revision 1.241  1992/07/23  23:52:44  esler
;; Make "s" not create too many Epoch screens if we've redefined
;; split-window to create a new screen.
;;
;; Revision 1.240  1992/07/23  15:06:13  esler
;; Bug in vm-mode-hook-hooks.
;;
;; Revision 1.239  1992/07/22  16:56:59  esler
;; Tweak VM variables controlling visibility of message headers.
;;
;; Revision 1.238  1992/07/22  14:56:24  esler
;; Wrote the command 'uptime, which tells me when the Emacs was started.
;;
;; Revision 1.237  1992/07/22  14:35:37  esler
;; In Dired Mode: "H" hides the file under the cursor.
;;
;; Revision 1.236  1992/07/20  21:47:25  esler
;; VM tweaks.
;;
;; Revision 1.235  1992/07/20  21:33:56  esler
;; "." does the right thing in VM Mode now.
;;
;; Revision 1.234  1992/07/20  15:30:42  esler
;; With the new CMU code, CMU Shell Mode buffers seem to start with Auto
;; Fill turned on.  So turn it off explicitly.
;;
;; Revision 1.233  1992/07/20  14:05:02  esler
;; Save news articles in ~/folders now.
;;
;; Revision 1.232  1992/07/17  21:56:01  esler
;; Set 'gnus-article-save-directory to a subdirectory of ~/folders.
;;
;; Revision 1.231  1992/07/17  21:44:56  esler
;; Give 'vm-visit-when-saving a better value.
;;
;; Revision 1.230  1992/07/17  19:53:26  esler
;; In Dired Mode: "v" visits the file as a VM mail folder.
;;
;; Revision 1.229  1992/07/17  19:27:21  esler
;; Installed new GNUS.
;; No real change to this.
;;
;; Revision 1.228  1992/07/17  15:43:51  esler
;; Unsuccessful attempt to make "." always do the right thing in VM Mode.
;;
;; Revision 1.227  1992/07/16  18:57:30  esler
;; Rebound "r" in Dired Mode to 'esler-dired-edit-filename.
;;
;; Revision 1.226  1992/07/16  14:22:35  esler
;; Tweak my Gnus-Group-mode-hook.
;;
;; Revision 1.225  1992/07/15  17:24:49  esler
;; Made CMU Shell mode output less obnoxious when running Epoch.
;;
;; Revision 1.224  1992/07/14  22:43:21  esler
;; Don't turn on SML Mode for files with suffix ".sml" on Domain/OS.
;;
;; Revision 1.223  1992/07/09  22:14:36  esler
;; Tweaked function bound to "." in Dired Mode.
;;
;; Revision 1.222  1992/07/08  22:05:18  esler
;; Changed the mode line format for CMU Shell Mode buffers.
;;
;; Revision 1.221  1992/07/08  19:44:23  esler
;; Dired Mode: "h" will Hide files matching a given regex.
;;
;; Revision 1.220  1992/07/08  19:03:16  esler
;; Added the ability to selectively view a directory's contents in Dired
;; Mode. "k" is bound to 'esler-dired-keep-matching-filenames, which
;; prompts for a regexp.  To make this work right, I also had to rebind
;; "g" to 'esler-dired-revert-buffer.
;;
;; Revision 1.219  1992/07/06  15:56:42  esler
;; Completion didn't work properly on News folders.
;;
;; Revision 1.218  1992/07/06  15:21:25  esler
;; GNUS now saves articles in VM-readable folders under ~/News/folders.
;; "f" at the newsgroup screen of GNUS invokes VM to read such a folder.
;;
;; Revision 1.217  1992/07/04  23:32:12  esler
;; Cosmetic.
;;
;; Revision 1.216  1992/07/04  21:22:37  esler
;; Tweaked GNUS Mode a bit.
;;
;; Revision 1.215  1992/07/01  14:46:56  esler
;; *** empty log message ***
;;
;; Revision 1.214  1992/07/01  14:37:15  esler
;; Installed C++ Mode.
;;
;; Revision 1.213  1992/06/29  18:11:56  esler
;; Update newsgroup-name-priority-list.
;;
;; Revision 1.212  1992/06/29  14:43:11  esler
;; The news server is now "news" rather than "hpway".
;;
;; Revision 1.211  1992/06/23  19:26:32  esler
;; I had inadvertently removed a function that 'smart-recenter needed.
;;
;; Revision 1.210  1992/06/23  18:56:44  esler
;; Tidied up a lot.
;;
;; Revision 1.209  1992/06/23  18:00:25  esler
;; My Dired customisations get partly overriden, unless they come AFTER
;; loading Ange FTP.
;;
;; Revision 1.208  1992/06/23  17:20:44  esler
;; Re-arranged the mode-specific pieces.
;;
;; Revision 1.207  1992/06/23  17:13:41  esler
;; Installed Bob Weiner's Pascal Mode.
;;
;; Revision 1.206  1992/06/16  21:49:16  esler
;; In Dired Mode, start with the cursor on the first file after ".."
;;
;; Revision 1.205  1992/06/16  21:44:22  esler
;; Rearrange a few things into a more logical layout.
;;
;; Revision 1.204  1992/06/16  15:33:29  esler
;; Bind "." in Tar Mode.
;;
;; Revision 1.203  1992/06/16  14:23:32  esler
;; Updated elisp-archive-host.
;;
;; Revision 1.202  1992/06/11  01:17:29  esler
;; Wrote useful function: esler-fold-enfold-indented-buffer.
;;
;; Revision 1.201  1992/06/10  20:49:04  esler
;; Remove obsolete table of contents.
;; I don't need this, now that I have Folding Minor Mode.
;;
;; Revision 1.200  1992/06/10  15:47:47  esler
;; Fix bug in 'esler-vm-print-message.
;;
;; Revision 1.199  1992/06/04  17:57:47  esler
;; I had inadvertently and erroneously removed some important ^L
;; characters at version 1.192.
;;
;; Revision 1.198  92/06/04  17:37:57  17:37:57  esler (Kevin Esler)
;; Have another go -- brain is at half mast today.
;;
;; Revision 1.197  92/06/04  17:35:56  17:35:56  esler (Kevin Esler)
;; Get the order of find-file hooks correct again.
;;
;; Revision 1.196  92/06/03  14:22:30  14:22:30  esler (Kevin Esler)
;; Get the autoloading of folding Minor Mode correct.
;;
;; Revision 1.195  92/06/01  19:08:39  19:08:39  esler (Kevin Esler)
;; Folding Minor Mode: specified syntax for Indented Text Mode.
;;
;; Revision 1.194  92/06/01  18:42:15  18:42:15  esler (Kevin Esler)
;; Finally got the interaction between Folding Minor Mode and View Buffer
;; Minor Mode sorted out.  You just need to:
;;   o  run the Folding Mode find file hook after the View Buffer Mode
;;      find file hook;
;;   o  use file variables to set 'folded-file (rather than 'mode)
;;
;; By the way, View Buffer Minor Mode seems not to be a well behaved
;; Minor Mode, in that it ought to copy and change the current local map,
;; rather than have its own persistent keymap.
;;
;; Revision 1.193  92/05/29  19:15:21  19:15:21  esler (Kevin Esler)
;; Moved View Minor Mode stuff around a bit to try to fix the clash
;; between Folding Minor Mode and View Minor Mode.
;;
;; Revision 1.192  92/05/29  18:58:50  18:58:50  esler (Kevin Esler)
;; Converted this .emacs file into a format amenable to Folding Minor
;; Mode editing.
;;
;; Revision 1.191  92/05/29  12:14:46  12:14:46  esler (Kevin Esler)
;; Inhibit Folding Minor Mode's startup message.
;;
;; Revision 1.190  92/05/28  16:49:46  16:49:46  esler (Kevin Esler)
;; Customise Folding Minor Mode's key bindings.
;;
;; Revision 1.189  92/05/28  15:02:15  15:02:15  esler (Kevin Esler)
;; Added Jamie Lokier's folding Minor Mode.
;;
;; Revision 1.188  92/05/22  14:26:28  14:26:28  esler (Kevin Esler)
;; Fixed bug in 'check-dsee-name.
;;
;; Revision 1.187  92/05/22  10:47:26  10:47:26  esler (Kevin Esler)
;; Removed some old rubbish.
;;
;; Revision 1.186  92/05/21  16:08:18  16:08:18  esler (Kevin Esler)
;; I had left some garbage around in the file.
;;
;; Revision 1.185  92/05/21  16:03:50  16:03:50  esler (Kevin Esler)
;; Fixed bug in 'esler-vm-save-return-address.
;;
;; Revision 1.184  92/05/21  15:45:07  15:45:07  esler (Kevin Esler)
;; More rationalisation of the X11 support.
;;
;; Revision 1.183  92/05/21  11:33:10  11:33:10  esler (Kevin Esler)
;; Bug: not getting mouse bound for Epoch.
;;
;; Revision 1.182  92/05/20  17:27:58  17:27:58  esler (Kevin Esler)
;; Reorganised the X11 stuff.
;;
;; Revision 1.181  92/05/20  13:56:25  13:56:25  esler (Kevin Esler)
;; Update 'vm-auto-folder-alist.
;;
;; Revision 1.180  92/05/20  11:08:46  11:08:46  esler (Kevin Esler)
;; Finish what I attempted in 1.176.
;;
;; Revision 1.179  92/05/20  10:07:26  10:07:26  esler (Kevin Esler)
;; Improved my gnus-Group-mode-hook.
;;
;; Revision 1.178  92/05/19  18:04:40  18:04:40  esler (Kevin Esler)
;; Tweak for new Epoch built for Domain.
;;
;; Revision 1.177  92/05/19  14:54:42  14:54:42  esler (Kevin Esler)
;; Add more custom bindings in Tar Mode.
;;
;; Revision 1.176  92/05/19  12:24:53  12:24:53  esler (Kevin Esler)
;; Turn off "//" --> "/apollo/" mapping when on Domain/OS.
;;
;; Revision 1.175  92/05/18  12:29:18  12:29:18  esler (Kevin Esler)
;; Added a prefix "esler-" to many of my own functions.
;;
;; Revision 1.174  92/05/18  11:59:14  11:59:14  esler (Kevin Esler)
;; Fixed a bug in my function 'esler-mail-print-message.
;; The bug was that if the cursor was in the VM summary buffer,
;; "P" would print the summary buffer instead of the message under the
;; cursor.
;;
;; Revision 1.173  92/05/12  18:04:46  18:04:46  esler (Kevin Esler)
;; Add to keybindings for Epoch.
;; Set up some bindings for HP keyboard.
;;
;; Revision 1.172  92/05/12  17:18:56  17:18:56  esler (Kevin Esler)
;; Fix slight bug in Epoch point and click.
;;
;; Revision 1.171  92/05/12  17:10:41  17:10:41  esler (Kevin Esler)
;; Click-on-pathname works in Epoch.
;; Needs to be cleaned up.
;;
;; Revision 1.170  92/05/12  16:10:06  16:10:06  esler (Kevin Esler)
;; First stab at organizing the X11 support in a reasonable way.
;;
;; Revision 1.169  92/05/08  13:56:30  13:56:30  esler (Kevin Esler)
;; Add a binding for "q" in Tar Mode.
;;
;; Revision 1.168  92/05/08  13:39:07  13:39:07  esler (Kevin Esler)
;; Added Tar Mode.
;;
;; Revision 1.167  92/04/30  19:06:13  19:06:13  esler (Kevin Esler)
;; Got rid of "Apollo Division" from news postings.
;;
;; Revision 1.166  92/04/28  16:22:35  16:22:35  esler (Kevin Esler)
;; Autoload cmusml code.
;;
;; Revision 1.165  92/04/25  16:57:11  16:57:11  esler (Kevin Esler)
;; Add the ".sml" suffix to auto-mode-alist, to turn on sml-mode.
;;
;; Revision 1.164  92/04/25  16:52:35  16:52:35  esler (Kevin Esler)
;; Put the RCS Log at the end of the file.
;;
;; Revision 1.163  92/04/25  16:46:26  16:46:26  esler (Kevin Esler)
;; Install SML Mode.
;;
;; Revision 1.162  92/04/23  14:50:07  14:50:07  esler (Kevin Esler)
;; Made dired-edit-link safer to use.
;;
;; Revision 1.161  92/04/21  20:05:12  20:05:12  esler (Kevin Esler)
;; Fixed a bug in clear-mail-from-mode-line.
;;
;; Revision 1.160  92/04/21  12:49:20  12:49:20  esler (Kevin Esler)
;; Let both "bogong" and "kanangra" be my home node for a while.
;;
;; Revision 1.159  92/04/15  17:33:20  17:33:20  esler (Kevin Esler)
;; Installed XDB Mode.
;;
;; Revision 1.158  92/04/07  11:28:25  11:28:25  esler (Kevin Esler)
;; Augmented newsgroup-name-priority-list.
;;
;; Revision 1.157  92/04/06  16:41:38  esler
;; Use load-file instead of load-library to get my version of
;; ps-mode.elc.
;;
;; Revision 1.156  92/03/26  18:41:57  esler
;; Improved dired-edit-link.
;;
;; Revision 1.155  92/03/26  18:21:27  esler
;; In Dired Mode: "E" will allow me to edit the text of a link.
;;
;; Revision 1.154  92/03/26  13:25:51  esler
;; Augmented 'newsgroup-type-priority-list.
;;
;; Revision 1.153  92/03/21  21:28:52  esler
;; Fixed a bug in dired-readin.
;;
;; Revision 1.152  92/03/18  19:50:45  esler
;; Improved my replacement for dired-readin even more.
;;
;; Revision 1.151  92/03/18  17:22:57  esler
;; Improved my replacement for dired-readin.
;;
;; Revision 1.150  92/03/17  16:01:12  esler
;; Added stuff for Epoch customisation, borrowed from John Vasta.
;;
;; Revision 1.149  92/03/17  10:31:57  esler
;; My Dired function replacements were clashing with those of Ange FTP.
;; I've now solved this by rolling my own form of procedure inheritance.
;;
;; Revision 1.148  92/03/13  11:52:02  esler
;; Renamed CMU's background command to cmubackground to avoid clash with
;; an Epoch function name.
;;
;; Revision 1.147  92/03/11  21:28:29  esler
;; Epoch in the mode line.
;;
;; Revision 1.146  92/03/11  20:58:06  esler
;; Attempted some repairs to dired-mode.
;;
;; Revision 1.145  92/03/11  19:23:03  esler
;; Deal with 18.58 behaviour of getenv.
;;
;; Revision 1.144  92/03/11  16:20:51  esler
;; The getenv function works differently in Epoch.
;; This .emacs is now acceptable to Epoch.
;;
;; Revision 1.143  92/03/11  14:34:51  esler
;; Added 'gnus-Suspend-gnus-hook.
;;
;; Revision 1.142  92/03/11  14:32:49  esler
;; Moved the setting of 'using-epoch and 'using-huge
;; earlier in the file.
;;
;; Revision 1.141  92/03/02  17:12:18  esler
;; Installed Emerge Mode, a mode for merging divergent versions of a
;; file.
;;
;; Revision 1.140  92/02/24  18:32:00  esler
;; Updated newsgroup-name-priority-list.
;;
;; Revision 1.139  92/02/10  13:57:33  esler
;; Make sure that, on HP-UX, we get the repaired version of ps-mode.el.
;;
;; Revision 1.138  92/02/06  17:21:35  esler
;; Adjusted newsgroup-name-priority-list.
;;
;; Revision 1.137  92/02/06  17:15:11  esler
;; Make sure we get my fixed version of ps-mode.el under HP-UX.
;;
;; Revision 1.136  92/02/06  17:12:48  esler
;; Re-ordered vm-auto-folder-alist.
;;
;; Revision 1.135  92/01/31  14:17:53  esler
;; Added an entry to newsgroup-type-priority-list.
;;
;; Revision 1.134  92/01/30  09:43:16  esler
;; Updated vm-auto-folder-alist.
;;
;; Revision 1.133  92/01/25  14:09:46  esler
;; Augmented vm-auto-folder-alist.
;;
;; Revision 1.132  92/01/16  16:05:35  esler
;; Fix typo.
;;
;; Revision 1.131  92/01/16  15:32:40  esler
;; Added a new favourite newsgroup.
;;
;; Revision 1.130  92/01/16  14:08:18  esler
;; In Dired Mode: bind "5" to 'split-window-horizontally.
;;
;; Revision 1.129  92/01/14  15:23:29  esler
;; Made comp.arch a favourite newsgroup.
;;
;; Revision 1.128  92/01/10  11:42:42  esler
;; Dired Mode: my previous change (version 1.126) was incorrect; fix it.
;;
;; Revision 1.127  92/01/10  11:01:13  esler
;; Added Font Selection Mode.
;;
;; Revision 1.126  92/01/09  17:38:28  esler
;; Dired Mode: put the directory pathname at the top of the buffer.
;;
;; Revision 1.125  92/01/06  14:13:13  esler
;; Adjusted newsgroup-name-priority-list.
;;
;; Revision 1.124  92/01/06  10:08:17  esler
;; Fixed bug in save-link-to-current-file.
;;
;; Revision 1.123  92/01/02  09:54:47  esler
;; Fixed a bug in kill-whole-line.
;;
;; Revision 1.122  91/12/31  10:03:03  esler
;; Created function untabify-buffer.
;;
;; Revision 1.121  91/12/20  13:59:58  esler
;; Improved save-link-to-curent-file so it can save links to directories
;; contained in dired-mode buffers.
;;
;;
;; Revision 1.120  91/12/16  11:14:26  esler
;; Turn off more ugly HPUX-isms.
;;
;; Revision 1.119  91/12/16  10:45:12  esler
;; Undo some more of the arbitrarily imposed HP extensions to Emacs.
;; These were done very crudely.
;;
;; Revision 1.118  91/12/13  16:13:32  esler
;; Wrote command 'dm, which invokes the Apollo DM on the file in the
;; current buffer.
;;
;; Revision 1.117  91/12/10  15:17:04  esler
;; Eliminated some HUGE features that offended.
;;
;; Revision 1.116  91/12/09  20:51:05  esler
;; Tweak HP-UX "HUGE" specifics.
;;
;; Revision 1.115  91/12/09  20:38:50  esler
;; Initial stab at getting set up for Darryl Okadata's HUGE, a
;; preconfigured/extended Emacs.
;;
;; Revision 1.114  91/12/06  13:54:03  esler
;; Fixed bug in 'check-dsee-name.
;;
;; Revision 1.113  91/12/06  10:58:44  esler
;; Added to 'newsgroup-type-priority-list.
;;
;; Revision 1.112  91/12/06  10:08:19  esler
;; Added a message to my gnus-Group-mode-hook.
;;
;; Revision 1.111  91/12/05  15:22:55  esler
;; Wrote 'esler-comint-find-preceding-input, and bound it to C-c p in CMU
;; Shell mode.
;;
;; Revision 1.110  91/12/05  14:30:01  esler
;; Made 'ci-buffer and 'co-buffer cause autoload of rcs2.elc.
;;
;; Revision 1.109  91/12/04  15:18:18  esler
;; Made comp.sys.hp a favourite newsgroup.
;;
;; Revision 1.108  91/12/04  13:43:48  esler
;; Added to newsgroup-name-priority-list.
;;
;; Revision 1.107  91/12/04  12:39:01  esler
;; Added "hurd" to vm-auto-folder-alist.
;;
;; Revision 1.106  91/12/04  10:26:17  esler
;; Tweaked newsgroup-name-priority-list.
;;
;; Revision 1.105  91/12/03  20:57:26  esler
;; Wrote a generalised buffer sort function: 'esler-sort-subr.
;; Changed 'newsrc-sort in gnus-priority.el to use it.
;;
;; Revision 1.104  91/12/03  16:39:18  esler
;; Added the function 'save-link-to-current-file.
;;
;; Revision 1.103  91/12/02  15:29:19  esler
;; Forgot to make the previously-added function interactively callable.
;;
;; Revision 1.102  91/12/02  15:25:04  esler
;; In GNUS, bind "G" so it reopens the NNTP connection, and then gets new
;; news.
;;
;; Revision 1.101  91/12/02  12:05:47  esler
;; Added customisation for Fortran mode.
;;
;; Revision 1.100  91/11/27  14:42:39  esler
;; Added 'trim-region-trailing and 'trim-region-leading.
;;
;; Revision 1.99  91/11/26  17:37:11  esler
;; Added RCS Mode.
;;
;; Revision 1.98  91/11/13  15:16:11  esler
;; Fixed bug in dired mode that has been bothering me for a while:
;;   renaming a symbolic link would discard the link target information.
;;
;; Revision 1.97  91/11/07  10:14:13  esler
;; Wrote 'trim-region.
;;
;; Revision 1.96  91/11/06  10:08:23  esler
;; Turn off filling when editing Makefiles.
;;
;; Revision 1.95  91/10/31  11:23:48  esler
;; Wrote uniq.el and set it up for autoload.
;;
;; Revision 1.94  91/10/31  11:00:25  esler
;; Documented 'iterate-over-lines-in-region.
;;
;; Revision 1.93  91/10/30  13:50:02  esler
;; Wrote the stringset package, and set it up for autoload.
;;
;; Revision 1.92  91/10/29  09:56:43  esler
;; Add to my list of favourite newsgroups.
;;
;; Revision 1.91  91/10/29  09:30:16  esler
;; I prefer M-% to invoke query-replace-regexp, since this subsumes the
;; functionality of query-replace.
;;
;; Revision 1.90  91/10/24  14:53:50  esler
;; "s" in Dired Mode now always goes to the end of the associated Cmushell mode buffer.
;;
;; Revision 1.89  91/10/22  16:09:17  esler
;; Improved comint-prompt-regexp.
;;
;; Revision 1.88  91/10/21  12:36:30  esler
;; The predicate, boundp, was being used incorrectly.
;;
;; Revision 1.87  91/10/21  10:13:41  esler
;; Half an attempt to mape "//" to "/net/apollo/".
;;
;; Revision 1.86  91/10/16  20:54:38  esler
;; Made "q" in ps-mode get rid of the ps-mode window.
;;
;; Revision 1.85  91/10/16  18:10:33  esler
;; Changed function conditional-man-dir-contents.
;;
;; Revision 1.84  91/10/16  11:08:57  esler
;; Added fpnotes function.
;;
;; Revision 1.83  91/10/09  13:30:58  esler
;; Fixed ps-mode so it works on HP-UX.
;;
;; Revision 1.82  91/10/04  16:28:50  esler
;; Renumber the sections to reflect the real order of things.
;; Setting up 'load-path needed to occur before custimising various packages.
;;
;; Revision 1.81  91/10/04  16:10:20  esler
;; Plug in the Ange FTP package, so I can painlessly peruse files in the Cupertino and Fort Collins labs.
;;
;; Revision 1.80  91/10/03  17:15:53  esler
;; Added the commands:: 'region-width, 'buffer-width.
;;
;; Revision 1.79  91/09/30  13:31:52  esler
;; Use "remote-dm" package.
;;
;; Revision 1.78  91/09/30  09:04:12  esler
;; Bound Apollo "Redo" key to do something useful in CMU shell buffers.
;;
;; Revision 1.76  91/09/27  17:36:51  esler
;; Unsuccessful attempt to get remote cut and paste (via the DM's paste buffers)
;; to work.
;;
;; Revision 1.75  91/09/27  13:20:53  esler
;; Further fine tuning of the X key bindings: apollo-cut and apollo-paste wouldn't work.
;;
;; Revision 1.74  91/09/27  13:03:02  esler
;; Fine tune and rearrange slightly the X key bindings.
;;
;; Revision 1.73  91/09/27  12:12:29  esler
;; Added John Vasta's X key bindings for the Apollo keyboard.
;; This also required editing the keyboard file which Xapollo reads on startup,
;; so that the left keypad keys would actually be sent to the X server.
;; This file was /usr/lib/X11/keyboard/keyboard.config .
;;
;; Revision 1.72  91/09/11  17:39:39  esler
;; Cleaned up the previous change.
;;
;; Revision 1.71  91/09/11  16:37:34  esler
;; Implemented 'dired-edit-with-apollo-DM and bound it to "L" in dired mode.
;;
;; Revision 1.70  91/08/27  16:24:40  esler
;; Tweak 'task again.
;;
;; Revision 1.69  91/08/27  10:12:23  esler
;; Made the mail printing command sticky.
;;
;; Revision 1.68  91/08/26  15:57:26  esler
;; Tweaked vm-auto-folder-alist.
;;
;; Revision 1.67  91/08/23  14:24:07  esler
;; Tweaked 'task function.
;;
;; Revision 1.66  91/08/15  17:08:00  esler
;; Added 'mybugs.
;;
;; Revision 1.65  91/08/08  15:10:06  esler
;; In CMUshell buffers, M-g copies the rest of the line at point,
;; to the end of the buffer (like Apollo's `again' key).
;;
;; Revision 1.64  91/07/26  14:20:33  esler
;; Installed the public domain what-line.el package, and
;; rebound M-= and C-x=.
;;
;; Revision 1.63  91/07/25  16:02:30  esler
;; Added /tex/man to the manual search list.
;;
;; Revision 1.62  91/06/24  15:53:40  esler
;; Set the variables that affect posting articles through GNUS.
;;
;; Revision 1.61  91/06/19  10:40:11  esler
;; Only use 'esler-background-wrapper if on Domain/OS.
;;
;; Revision 1.60  91/06/19  10:12:58  esler
;; Forced CMU's 'background to use a pipe for process output.
;; It wouldn't work with Apollo ptys.
;;
;; Revision 1.59  91/06/13  12:36:27  esler
;; Changed vm-included-text-prefix.
;;
;; Revision 1.58  91/06/12  09:55:23  esler
;; Made locate-catable-manuals a bit more robust in the presence of network glitches.
;; It will quietly ignore man-page directories it expects to see but can't.
;;
;; Revision 1.57  91/06/11  11:35:58  esler
;; Added bindings to VM-mode:
;;                      "$" views the last message,
;;                      "g" clears the mode line notification after getting fresh mail.
;;
;; Revision 1.56  91/06/05  13:27:24  esler
;; Invented esler-save-return-address, to maintain a database of people's email
;; addresses.
;;
;; Revision 1.55  91/05/17  15:53:40  esler
;; Changed my home node.
;;
;; Revision 1.54  91/05/10  14:07:39  esler
;; Invented dired-down.
;; Improved dired-up so that it leaves the cursor at the directory you just
;; came up from.
;;
;; Revision 1.53  91/04/25  18:12:20  esler
;; In Dired Mode: "\" finds or creates a Dired Mode Buffer for the containing directory.
;;
;; Revision 1.52  91/04/24  13:57:28  esler
;; If "/news/spool" doesn't exist, GNUS should use NNTP.
;;
;; Revision 1.51  91/04/19  17:35:56  esler
;; Customised view-buf mode.
;;
;; Revision 1.50  91/04/17  15:56:57  esler
;; For Makefiles, use normal Text Mode (not Indented Text Mode),
;; and use real tab characters.
;;
;; Revision 1.49  91/04/16  15:49:41  esler
;; Gracefully handle the non-existence of the "loadst" program.
;;
;; Revision 1.48  91/03/29  10:32:56  esler
;; Added haskell folder to vm-auto-folder-alist.
;;
;; Revision 1.47  91/03/27  14:48:31  esler
;; Made shell-command-on-region set the default-directory of the output
;; buffer, "*Shell Command Output*" to be the working directory in which
;; the command was executed.
;;
;; Revision 1.46  91/03/26  12:27:48  esler
;; Prevent mail reading and writing except on my home node.
;;
;; Revision 1.45  91/03/26  12:05:02  esler
;; In dired-mode, bound "2" to 'split-window-vertically.
;;
;; Revision 1.44  91/03/26  10:37:32  esler
;; Another stab at getting the comint prompt matching regexp correct.
;;
;; Revision 1.43  91/03/26  10:27:54  esler
;; Set shell-prompt-pattern to default prompt for /bin/ksh, when
;; PS1 environment variable has not been set.
;;
;; Revision 1.42  91/03/21  16:28:59  esler
;; Added some code for the case when (eq system-type 'OSF/1).
;;
;; Revision 1.41  91/03/14  11:21:14  esler
;; In dired-mode, bound "1" to 'delete-other-windows.
;;
;; Revision 1.40  91/02/06  10:00:25  esler
;; Only start Emacs server on home Domain node.
;;
;; Revision 1.39  91/02/05  17:24:05  esler
;; Misspelled Domain/OS.
;;
;; Revision 1.38  91/02/04  18:57:15  esler
;; Bound C-l to 'smart-recenter.
;;
;; Revision 1.37  91/01/31  17:25:02  esler
;; Parametrised parts over system-type so it works on HPUX.
;;
;; Revision 1.36  91/01/30  12:02:36  esler
;; Added (server-start).
;;
;; Revision 1.35  91/01/29  19:56:32  esler
;; Installed the Lispdir package (directory of public domain Emacs lisp code).
;;
;; Revision 1.34  91/01/29  19:31:49  esler
;; Added 'highlight-region function.
;;
;; Revision 1.33  91/01/24  14:53:26  esler
;; Added some X11 support.
;;
;; Revision 1.32  91/01/14  10:23:08  esler
;; Made my vmv command preserve the window configuration.
;;
;; Revision 1.31  91/01/09  10:08:53  esler
;; Made push-window-config and pop-window-config commands.
;;
;; Revision 1.30  91/01/08  17:41:44  esler
;; Fixed a bug in esler-cmushell-process-filter.
;;
;; Revision 1.29  91/01/07  11:28:11  esler
;; Added push-window-config and pop-window-config, and a window-saving wrapper for
;; 'vm.
;;
;; Revision 1.28  91/01/04  15:29:42  esler
;; Yet another bug in dired-spawn-cmushell bites the dust.
;;
;; Revision 1.27  91/01/03  15:03:06  esler
;; Fixed bug in dired-spawn-cmushell.
;;
;; Revision 1.26  91/01/02  10:19:46  esler
;; Added 'toggle-case-fold-search and its binding.
;;
;; Revision 1.25  90/12/17  15:03:19  esler
;; Set up Calendar and Diary package.
;;
;; Revision 1.24  90/12/04  11:39:09  esler
;; Adjusted vm-auto-folder-alist.
;;
;; Revision 1.23  90/11/26  11:13:58  esler
;; Bug fix in dired-spawn-cmushell.
;;
;; Revision 1.22  90/11/20  14:57:03  esler
;; Made dired-spawn-cmushell split the dired window, rather than viewing the shell
;; buffer in any "other" window.
;;
;; Revision 1.21  90/11/20  11:19:36  esler
;; Fixed bug in create-placemarker-window, and bound it to C-x 3.
;;
;; Revision 1.20  90/11/14  17:20:19  esler
;; Fixed some bugs in 'check-dsee-name.
;;
;; Revision 1.19  90/11/12  10:31:48  esler
;; Added auto-load of 'ps-mode.
;;
;; Revision 1.18  90/11/09  17:32:33  esler
;; Fixed a bug in check-dsee-name.  It was not allowing me to
;; read two files in with the same simple name.
;;
;; Revision 1.17  90/11/09  15:46:51  esler
;; Fixed dired-mode-hook to do the right thing when the object being dired-ed
;; is a symbolic link.
;;
;; Revision 1.16  90/11/02  10:33:01  esler
;; Made 'clear-mail-from-mode-line a separate function.
;;
;; Revision 1.15  90/11/01  16:38:46  esler
;; Included Bill Sommerfeld's find-file-hook to do the right thing when
;; visiting an element in a DSEE source library.
;;
;; Revision 1.14  90/11/01  15:19:07  esler
;; VMail: made "q" save any modified folders before exiting.
;;
;; Revision 1.13  90/10/31  09:56:30  esler
;; Augmented and cleaned up vm-mode-hooks.
;;
;; Revision 1.12  90/10/30  12:16:19  esler
;; Try out Brezak's pas-mode.
;;
;; Revision 1.11  90/10/23  15:55:30  esler
;; Adjusted vm-auto-folder-alist.
;;
;; Revision 1.10  90/10/19  10:48:33  esler
;; Move CMU stuff into a subdirectory.
;; Made my setting of load-path a bit more intelligent.
;;
;; Revision 1.9  90/10/16  16:58:59  esler
;; Fixed bug in dired-spawn-cmushell
;;
;; Revision 1.8  90/10/16  16:22:36  esler
;; Made "q" in dired mode get rid of the associated spawned cmushell buffer,
;; if there is one.
;;
;; Revision 1.7  90/10/16  14:52:26  esler
;; Fixed a bug in dired-spawn-cmushell.
;;
;; Revision 1.6  90/10/16  14:33:01  esler
;; Added the dired-mode feature: dired-spawn-cmushell.
;;
;; Revision 1.5  90/10/15  16:05:31  esler
;; Changed dired-listing-switches from "-alL" to "-al".
;;
;; Revision 1.4  90/10/12  09:51:35  esler
;; Added (defun gnus-reopen ...)
;;
;; Revision 1.3  90/10/09  14:58:23  esler
;; Memoised some of my dired functions, so they will default to using the
;; previous command/function applied to a file.
;;
;; Revision 1.2  90/10/09  14:44:00  esler
;; Fixed up RCS headers.
;;
;; Revision 1.1  90/10/09  14:39:48  esler
;; Initial revision

;;}}}
;;{{{  Emacs local variables for this file.


;; Local variables:
;; folded-file: t
;; End:

;;}}}