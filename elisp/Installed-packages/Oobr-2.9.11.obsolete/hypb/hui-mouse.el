;;!emacs
;;
;; FILE:         hui-mouse.el
;; SUMMARY:      Use key or mouse key for many functions, e.g. Hypb menus.
;;               See the "${data-directory}/hypb-mouse.txt" file and the
;;               documentation strings for functions herein.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:    04-Feb-89
;; LAST-MOD:      1-Nov-95 at 20:45:57 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1989-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;  This code is machine independent.  It works best with a pointing device but
;;  may also be used from a keyboard.  When used with a pointing device it
;;  requires an Emacs command that sets point to the location of the pointing
;;  device's cursor.
;;
;;  If you want to use your shift-middle mouse button to select Hyperbole menu
;;  items and Hyperbole buttons, follow these instructions.
;;
;;  If you plan to use a mouse only with X windows (Lucid Emacs, GNU Emacs
;;  19, or Epoch), NEXTSTEP, SunView, Apollo's DM, and you want to use the
;;  shift-middle and shift-right buttons, you need not do any mouse
;;  configuration.  Your Emacs executable must have been built so as to
;;  include the mouse support files for your window system, however.  These
;;  are in the Emacs "src" directory: for X "x*.c", for SunView "sunfns.c",
;;  and for Apollo DM "apollo.c" and "apollo.el".
;;
;;  To use a different mouse key or a different window system, modify the
;;  mouse key bindings in "hmouse-key.el".
;;
;; Using the Action Mouse Key to browse through and delete files from
;; Dired listings is exceptionally nice, just as it is when reading mail.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hmouse-set-point-command nil
  "*Command that sets point to mouse cursor position.")

(defvar action-key-default-function 'hui:menu
  "*Symbol name of function run by the Action Key in an unspecified context.")

(defvar assist-key-default-function 'hkey-summarize
  "*Symbol name of function run by the Assist Key in an unspecified context.")

;;; ************************************************************************
;;; Hyperbole context-sensitive keys dispatch table
;;; ************************************************************************

(defvar hkey-value nil
  "Communicates a value between a Smart Key predicate and its actions.")

(defvar hkey-alist
  '(
    ;;
    ;; If click in the minibuffer and reading an argument,
    ;; accept argument or give completion help.
    ((and (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (not (eq hargs:reading-p 'hmenu))) .
     ((exit-minibuffer) . (smart-completion-help)))
    ;;
    ;; If reading a Hyperbole menu item or a Hyperbole completion-based
    ;; argument, allow selection of an item at point.
    ((if (> (minibuffer-depth) 0) (setq hkey-value (hargs:at-p))) .
     ((hargs:select-p hkey-value) .
      (hargs:select-p hkey-value 'assist)))
    ;;
    ((if (not (eobp))
	  (or (eolp) (if selective-display
			 (= (following-char) ?\^M)))) .
     ((smart-scroll-up) . (smart-scroll-down)))
    ;;
    ((eq major-mode 'smart-menu-mode) . 
     ((smart-menu-select) . (smart-menu-help)))
    ;;
    ;; If on a Hyperbole button, perform action or give help.
    ((if (fboundp 'hbut:at-p) (or (hbut:at-p) (hbut:label-p))) .
     ((hui:hbut-act 'hbut:current) . (hui:hbut-help 'hbut:current)))
    ;;
    ;; The Smart Menu system provides menus within Emacs running on a dumb
    ;; terminal.  It is part of InfoDock and is not available separately.
    ((and (fboundp 'smart-menu-choose-menu)
	  (setq hkey-value (and hkey-always-display-menu
				(smart-menu-choose-menu)))
	  (not (and (get-buffer-window *smart-menu-buffer*)
		    (eq hkey-value *smart-menu-curr*)))) .
     ((smart-menu hkey-value) .
      (smart-menu hkey-value)))
    ;;
    ;;
    ;; View minor mode
    ((if (boundp 'view-minor-mode) view-minor-mode) .
     ((cond ((last-line-p)
	     (view-quit))
	    ((pos-visible-in-window-p (point-max))
	     (goto-char (point-max)))
	    (t (scroll-up))) .
      (scroll-down)))
    ;;
    ;; View major mode
    ((eq major-mode 'view-mode) .
     ((View-scroll-lines-forward) . (View-scroll-lines-backward)))
    ;;
    ((eq major-mode 'kotl-mode) . 
     ((kotl-mode:action-key) . (kotl-mode:help-key)))
    ;;
    ;; Support direct selection and viewing on in-memory relational databases.
    ;; Rdb-mode has not been publicly released.
    ;; It is not included with Hyperbole.
    ((eq major-mode 'rdb-mode) . ((smart-rdb) . (smart-rdb-assist)))
    ;;
    ;; Restore window config and hide help buffer when click at buffer end.
    ((if (= (point) (point-max)) (string-match "Help\\*$" (buffer-name))) .
     ((hkey-help-hide) . (hkey-help-hide)))
    ;;
    ;; Support the OO-Browser, a part of InfoDock, XEmacs, and soon to be a
    ;; part of Emacs.
    ((or (br-in-browser) (eq major-mode 'br-mode)) .
     ((smart-br-dispatch) . (smart-br-assist-dispatch)))
    ;;
    ((and (memq major-mode '(c-mode c++-c-mode))
	  buffer-file-name (setq hkey-value (smart-c-at-tag-p))) .
     ((smart-c) . (smart-c nil 'next-tag)))
    ;;
    ((and (eq major-mode 'asm-mode)
	  buffer-file-name (setq hkey-value (smart-asm-at-tag-p))) .
     ((smart-asm) . (smart-asm nil 'next-tag)))
    ;;
    ((if (smart-lisp-mode-p) (smart-lisp-at-tag-p)) .
     ((smart-lisp) . (smart-lisp 'next-tag)))
    ;;
    ((and (eq major-mode 'c++-mode) buffer-file-name
	  ;; Don't use smart-c++-at-tag-p here since it will prevent #include
	  ;; lines from matching.
	  (setq hkey-value (smart-c-at-tag-p))) .
     ( ;; Only fboundp if OO-Browser has been loaded.
      (if (fboundp 'c++-to-definition)
	  (smart-c++-oobr) (smart-c++)) .
      (if (fboundp 'c++-to-definition)
	  (smart-c++-oobr)
	(smart-c++ nil 'next-tag))))
    ;;
    ((and (eq major-mode 'objc-mode) buffer-file-name
	  (setq hkey-value (smart-objc-at-tag-p))) .
     ( ;; Only fboundp if OO-Browser has been loaded.
      (if (fboundp 'objc-to-definition)
	  (smart-objc-oobr) (smart-objc)) .
      (if (fboundp 'objc-to-definition)
	  (smart-objc-oobr)
	(smart-objc nil 'next-tag))))
    ;;
    ((and (eq major-mode 'fortran-mode)
	  buffer-file-name (setq hkey-value (smart-fortran-at-tag-p))) .
     ((smart-fortran) . (smart-fortran nil 'next-tag)))
    ;;
    ((eq major-mode 'occur-mode) .
     ((occur-mode-goto-occurrence) . (occur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'moccur-mode) .
     ((moccur-mode-goto-occurrence) . (moccur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'calendar-mode) .
     ((smart-calendar) . (smart-calendar-assist)))
    ;;
    ((eq major-mode 'unix-apropos-mode) .
     ((smart-apropos) . (smart-apropos-assist)))
    ;;
    ((eq major-mode 'outline-mode) .
     ((smart-outline) . (smart-outline-assist)))
    ;;
    ((eq major-mode 'Info-mode) .
     ((smart-info) .  (smart-info-assist)))
    ;;
    ((if (boundp 'hmail:reader)
	 (or (eq major-mode hmail:reader)
	     (eq major-mode hmail:lister))) .
     ((smart-hmail) . (smart-hmail-assist)))
    ;;
    ((eq major-mode 'gnus-group-mode)
     (smart-gnus-group) . (smart-gnus-group-assist))
    ;;
    ((eq major-mode 'gnus-summary-mode)
     (smart-gnus-summary) . (smart-gnus-summary-assist))
    ;;
    ((eq major-mode 'gnus-article-mode)
     (smart-gnus-article) . (smart-gnus-article-assist))
    ;;
    ((eq major-mode 'Buffer-menu-mode) .
     ((smart-buffer-menu) . (smart-buffer-menu-assist)))
    ;;
    ((eq major-mode 'dired-mode) . 
     ((smart-dired) . (smart-dired-assist)))
    ;;
    ((eq major-mode 'tar-mode) . 
     ((smart-tar) . (smart-tar-assist)))
    ;;
    ;; Follow references in man pages.
    ((setq hkey-value (smart-man-entry-ref)) .
     ((smart-man-display hkey-value) .
      (smart-man-display hkey-value)))
    ;;
    ((eq major-mode 'w3-mode) . 
     ((w3-follow-link) . (w3-goto-last-buffer)))
    ;;
    ((if (boundp 'rolo-display-buffer)
	 (equal (buffer-name) rolo-display-buffer)) .
     ((smart-wrolo) . (smart-wrolo-assist)))
    ;;
    ;; Gomoku game
    ((eq major-mode 'gomoku-mode) . 
     ((gomoku-human-plays) . (gomoku-human-takes-back)))
    ;;
    ;; Outline minor mode is on and usable.
    (selective-display .
     ((smart-outline) . (smart-outline-assist)))
    )
  "Alist of predicates and form-conses for Action and Assist Keys.
When the Action or Assist Key is pressed, the first or second form,
respectively, associated with the first non-nil predicate is evaluated.")

;;; ************************************************************************
;;; driver code
;;; ************************************************************************

;; The following autoload is needed if another subsystem besides
;; Hyperbole uses this mouse handling code.
(autoload 'var:append "hvar" "Append to a list variable." nil)

(require 'hargs)
(require 'hmouse-key)
(if hyperb:window-system
    (progn
      (defvar hmouse-alist hkey-alist
	"Alist of predicates and form-conses for context-sensitive smart key mouse actions.
When the action-key or the assist-key is pressed, the first or
second form, respectively, associated with the first non-nil predicate is
evaluated.")
      (load "hui-window")))

;;; ************************************************************************
;;; support code
;;; ************************************************************************

;; The 'load' line below loads any local Smart Key function definitions.
;; The public distribution contains none.  You may leave it commented out if
;; you prefer.
;; (load "smart-local" t)

;;; ************************************************************************
;;; Required Init functions
;;; ************************************************************************

(defun first-line-p ()
  "Returns true if point is on the first line of the buffer."
  (save-excursion (beginning-of-line) (bobp)))

(defun last-line-p ()
  "Returns true if point is on the last line of the buffer."
  (save-excursion (end-of-line) (eobp)))

(defun smart-completion-help ()
  "Offer completion help for current minibuffer argument, if any."
  (if (where-is-internal 'minibuffer-completion-help (current-local-map))
      (minibuffer-completion-help)))

(defun smart-symlink-expand (path)
  "Returns referent for possible symbolic link, PATH."
  (if (not (fboundp 'symlink-referent))
      path
    (let ((start 0) (len (length path)) (ref) (part))
      (while (and (< start len) (setq part (string-match "/[^/]*" path start)))
	(setq part (concat ref
			   (substring path start (setq start (match-end 0))))
	      ref (symlink-referent part)))
      ref)))

;;; ************************************************************************
;;; smart-buffer-menu functions
;;; ************************************************************************

(defun smart-buffer-menu (&optional in-browser)
  "Uses a single key or mouse key to manipulate buffer-menu entries.

Invoked via a key press when in Buffer-menu-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

Optional non-nil IN-BROWSER indicates use within the OO-Browser.

If key is pressed:
 (1) on the first column of an entry, the selected buffer is marked for
     display; 
 (2) on the second column of an entry, the selected buffer is marked to be
     saved;
 (3) anywhere else within an entry line, all saves and deletes are done, and
     selected buffers are displayed, including the one just clicked on (if
     IN-BROWSER, only the selected buffer is displayed);
 (4) on or after the last line in the buffer, all saves and deletes are done."

  (interactive)
  (cond ((last-line-p) (Buffer-menu-execute))
	((bolp) (Buffer-menu-mark))
        ((save-excursion
             (goto-char (1- (point)))
	     (bolp))
	 (Buffer-menu-save))
	(in-browser (br-buffer-menu-select))
	(t (Buffer-menu-select))))

(defun smart-buffer-menu-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate buffer-menu entries.

Invoked via an assist-key press when in Buffer-menu-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on the first or second column of an entry, the selected buffer is unmarked
     for display and for saving or deletion; 
 (2) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (3) on or after the last line in the buffer, all display, save, and delete
     marks on all entries are undone."

  (interactive)
  (cond ((last-line-p) (progn (list-buffers) (forward-line 3)))
	((bolp) (Buffer-menu-unmark))
        ((save-excursion
             (goto-char (1- (point)))
	     (bolp))
	 (Buffer-menu-unmark))
	(t (Buffer-menu-delete))))

;;; ************************************************************************
;;; smart-calendar functions
;;; ************************************************************************

(defun smart-calendar ()
  "Uses a single key or mouse key to manipulate the scrolling calendar.

Invoked via a key press when in calendar-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) at the end of the buffer, the calendar is scrolled forward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     backward 3 months;
 (3) on a date, the diary entries for the date, if any, are displayed."

  (interactive)
  (cond ((eobp) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-left-three-months 1))
	((< (current-column) 5) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-right-three-months 1))
	(t (calendar-cursor-to-nearest-date)
	   (view-diary-entries 1))))

(defun smart-calendar-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate the scrolling calendar.

Invoked via an assist-key press when in calendar-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) at the end of the buffer, the calendar is scrolled backward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     forward 3 months;
 (3) anywhere else, all dates with marking diary entries are marked in the
     calendar window."

  (interactive)
  (cond ((eobp) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-right-three-months 1))
	((< (current-column) 5) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-left-three-months 1))
	(t (mark-diary-entries))))


;;; ************************************************************************
;;; smart-dired functions
;;; ************************************************************************

(defun smart-dired ()
  "Uses a single key or mouse key to manipulate directory entries.

Invoked via a key press when in dired-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the selected file/directory is displayed for
     editing in the other window;
 (2) on or after the last line in the buffer, if any deletes are to be
     performed, they are executed after user verification, otherwise, this
     dired invocation is quit."

  (interactive)
  (cond ((last-line-p)
	 (let (flagged)
	   (save-excursion
	     (goto-char 1)
	     (setq flagged (re-search-forward "^D" nil t)))
	   (if flagged
	       (cond ((fboundp 'dired-do-deletions)
		      (dired-do-deletions))
		     ;; For Tree-dired compatibility
		     ((fboundp 'dired-do-flagged-delete)
		      (dired-do-flagged-delete))
		     (t (error "(smart-dired): No Dired expunge function.")))
	     (dired-quit))))
	(t (hpath:find-other-window (dired-get-filename)))))

(defun smart-dired-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate directory entries.

Invoked via an assist-key press when in dired-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on a '~' character, all backup files in the directory are marked for
     deletion;
 (2) on a '#' character, all auto-save files in the directory are marked for
     deletion;
 (3) anywhere else within an entry line, the current entry is marked for
     deletion;
 (4) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((last-line-p)
	 (dired-unflag (- (count-lines (point-min) (point-max))))
	 (goto-char (point-max)))
	((looking-at "~") (dired-flag-backup-files))
	((looking-at "#") (dired-flag-auto-save-files))
	(t (dired-flag-file-deleted 1))))

;;; ************************************************************************
;;; smart-gnus functions
;;; ************************************************************************

(defun smart-gnus-group ()
  "Uses a key or mouse key to move through Gnus Newsgroup listings.
Invoked via a key press when in gnus-group-mode.  It assumes that its caller
has already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed within:
 (1) a GNUS-GROUP line, that newsgroup is read;
 (2) to the left of any GNUS-GROUP line, on any of the whitespace, the current
     group is unsubscribed or resubscribed;
 (3) at the end of the GNUS-GROUP buffer, after all lines, checks for new
     news."

  (interactive)
  (cond ((last-line-p) (gnus-group-get-new-news))
	((progn (skip-chars-backward " U") (bolp))
	 (gnus-group-unsubscribe-current-group))
	(t (gnus-group-read-group nil))))

(defun smart-gnus-group-assist ()
  "Uses an assist-key or assist-mouse key to move through Gnus Newsgroup listings.
Invoked via an assist-key press when in gnus-group-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) a GNUS-GROUP line, that newsgroup is read;
 (2) to the left of any GNUS-GROUP line, on any of the whitespace, the user is
     prompted for a group name to subscribe or unsubscribe to;
 (3) at the end of the GNUS-GROUP buffer, after all lines, quits from the
     newsreader."

  (interactive)
  (cond ((last-line-p) (gnus-group-exit))
	((progn (skip-chars-backward " U") (bolp))
	 (call-interactively 'gnus-group-unsubscribe-group))
	(t (gnus-group-read-group nil))))

(defun smart-gnus-summary ()
  "Uses a key or mouse key to move through Gnus News article listings.
Invoked via a key press when in gnus-summary-mode.  It assumes that its caller
has already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed within:
 (1) to the left of an article number, that article is marked as unread;
 (2) a GNUS-SUMMARY line, that article is read, marked deleted, and scrolled
     forward;
 (3) at the end of the GNUS-SUMMARY buffer, the next undeleted article
     is read or the next group is entered."

  (interactive)
  (cond ((last-line-p)
	 (if gnus-current-article
	     (progn (goto-char (point-min))
		    (re-search-forward
		      (format "^.[ ]+%d:" gnus-current-article) nil t)
		    (setq this-command 'gnus-summary-next-page)
		    (call-interactively 'gnus-summary-next-page))
	   (goto-char (point-min))
	   (setq this-command 'gnus-summary-first-unread-article)
	   (call-interactively 'gnus-summary-first-unread-article)))
	((save-excursion (skip-chars-backward " D") (bolp))
	 (gnus-summary-mark-as-unread-forward 1))
	(t (setq this-command 'gnus-summary-next-page)
	   (call-interactively 'gnus-summary-next-page))))

(defun smart-gnus-summary-assist ()
  "Uses an assist-key or assist-mouse key to move through Gnus News articles.
Invoked via an assist-key press when in gnus-summary-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) to the left of an article number, that article is marked as unread;
 (2) a GNUS-SUMMARY line, that article is read and scrolled backward;
 (3) at the end of the GNUS-SUMMARY buffer, the summary is exited, the user
     is returned to group mode."

  (interactive)
  (cond ((last-line-p)
	 (setq this-command 'gnus-summary-prev-page)
	 (call-interactively 'gnus-summary-exit))
	((save-excursion (skip-chars-backward " D") (bolp))
	 (gnus-summary-mark-as-unread-backward 1))
	(t (setq this-command 'gnus-summary-prev-page)
	   (call-interactively 'gnus-summary-prev-page))))

(defun smart-gnus-article ()
  "Uses a key or mouse key to move through Gnus netnews articles.

Invoked via a key press when in gnus-article-mode.
It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) the first line or end of an article, the next unread message is displayed;
 (2) the first line of an Info cross reference, the reference is followed;
 (3) anywhere else, the window is scrolled up a windowful."
  (interactive)
  (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
	 (unwind-protect
	     (progn (set-buffer gnus-summary-buffer)
		    (setq this-command 'gnus-summary-next-unread-article)
		    (gnus-summary-next-unread-article)
		    (gnus-summary-goto-subject gnus-current-article)
		    )
	   (let ((artic (get-buffer-window gnus-article-buffer)))
	     (if artic (select-window artic)))))
	((and (not (eolp)) (Info-handle-in-note)))
	(t (smart-scroll-up))))

(defun smart-gnus-article-assist ()
  "Uses an assist-key or mouse assist-key to move through Gnus netnews articles.

Invoked via an assist-key press when in gnus-article-mode.
It assumes that its caller has already checked that the assist-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.

If assist-key is pressed within:
 (1) the first line or end of an article, the previous message is displayed;
 (2) the first line of an Info cross reference, the reference is followed;
 (3) anywhere else, the window is scrolled down a windowful."
  (interactive)
  (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
	 (unwind-protect
	     (progn (set-buffer gnus-summary-buffer)
		    (setq this-command 'gnus-summary-prev-article)
		    (gnus-summary-prev-article nil)
		    (gnus-summary-goto-subject gnus-current-article)
		    )
	   (let ((artic (get-buffer-window gnus-summary-buffer)))
	     (if artic (select-window artic)))))
	((and (not (eolp)) (Info-handle-in-note)))
	(t (smart-scroll-down))))

;;; ************************************************************************
;;; smart-hmail functions
;;; ************************************************************************

(defun smart-hmail ()
  "Uses a key or mouse key to move through e-mail messages and summaries.

Invoked via a key press when in hmail:reader or hmail:lister mode.
It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) a msg buffer, within the first line or at the end of a message,
     the next undeleted message is displayed;
 (2) a msg buffer within the first line of an Info cross reference, the
     reference is followed;
 (3) anywhere else in a msg buffer, the window is scrolled up a windowful; 
 (4) a msg summary buffer on a header entry, the message corresponding to
     the header is displayed in the msg window;
 (5) a msg summary buffer, on or after the last line, the messages marked
     for deletion are expunged."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode hmail:reader)
	 (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
		(rmail:msg-next))
	       ((and (not (eolp)) (Info-handle-in-note)))
	       ((smart-scroll-up))))
	;;
	;; Assume are in msg summary buffer
	;;
	((last-line-p) (lmail:expunge))
	(t (lmail:goto))))

(defun smart-hmail-assist ()
  "Uses an assist key or mouse key to move through e-mail messages and summaries.

Invoked via an assist key press when in hmail:reader or hmail:lister mode.
It assumes that its caller has already checked that the assist-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.

If assist-key is pressed within:
 (1) a msg buffer, within the first line or at the end of a message,
     the previous undeleted message is displayed;
 (2) a msg buffer within the first line of an Info cross reference, the
     reference is followed;
 (3) anywhere else in a msg buffer, the window is scrolled down a windowful; 
 (4) a msg summary buffer on a header entry, the message corresponding to
     the header is marked as deleted;
 (5) a msg summary buffer, on or after the last line, all messages are
     marked undeleted."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode hmail:reader)
	 (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
		(rmail:msg-prev))
	       ((and (not (eolp)) (Info-handle-in-note)))
	       ((smart-scroll-down))))
	;;
	;; Assume are in msg summary buffer
	;;
	((last-line-p) (lmail:undelete-all))
	(t (lmail:delete))))


;;; ************************************************************************
;;; smart-info functions
;;; ************************************************************************
;;; Autoloaded in "hyperbole.el".

;;; ************************************************************************
;;; smart-man functions
;;; ************************************************************************

;; "unix-apropos.el" is a publicly available Emacs Lisp package that
;; allows man page browsing from apropos listings.  "superman.el" is a
;; newer, much more complete package that you would probably prefer at
;; this point, but there is no Smart Key apropos support for it.  There
;; is smart key support within the man page buffers it produces, however.
;;

(defun smart-apropos ()
  "Moves through UNIX man apropos listings by using one key or mouse key.

Invoked via a key press when in unix-apropos-mode.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled up
     a windowful."

  (interactive)
  (if (last-line-p)
      (scroll-other-window)
    (unix-apropos-get-man)))

(defun smart-apropos-assist ()
  "Moves through UNIX man apropos listings by using one assist-key or mouse assist-key.

Invoked via an assist-key press when in unix-apropos-mode.  It assumes that
its caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled down
     a windowful."

  (interactive)
  (if (last-line-p)
      (scroll-other-window (- 3 (window-height)))
    (unix-apropos-get-man)))

(defun smart-man-display (lisp-form)
  "Evaluates LISP-FORM returned from 'smart-man-entry-ref' to display a man page."
  (eval lisp-form))

(defun smart-man-entry-ref ()
  "Returns form which displays referenced manual entry that point is on or nil.
Handles references in sections: NAME, SEE ALSO, or PACKAGES USED.  Also can
display C routine definitions selected in a man page, see
'smart-man-c-routine-ref'.

Man page buffer must either have an attached file or else a `man-path'
local variable containing its pathname."
  (interactive)
  (let ((ref ""))
    (if (not (or (if (string-match "Manual Entry\\|\\*man "
				   (buffer-name (current-buffer)))
		     (progn (and (boundp 'man-path) man-path
				 (setq ref (smart-symlink-expand man-path)))
			    t))
		 (if buffer-file-name
		     (string-match "/man/" (setq ref (smart-symlink-expand
						      buffer-file-name))))))
	(setq ref nil)
      (or (setq ref (or (smart-man-file-ref)
			(smart-man-c-routine-ref)))
	  (save-excursion
	    (let ((opoint (point))
		  (case-fold-search))
	      (and
	       (re-search-backward "^[.A-Z]" nil t)
	       (looking-at
		"\\(\\.SH[ \t]+\\)?\\(SEE ALSO\\|NAME\\|PACKAGES USED\\)")
	       (progn (goto-char opoint)
		      (skip-chars-backward "-_a-zA-Z0-9?.(")
		      (let ((start (point)))
			(skip-chars-forward "-_a-zA-Z0-9?.()")
			(setq ref (buffer-substring start (point)))
			;; Leave only one char within ref parens
			(if ref
			    (if (string-match "(\\(.\\)\\(.+\\))" ref)
				(setq ref (concat (substring ref 0 (match-end 1))
						  "\)"))))
			)))))))
    (cond ((equal ref "") nil)
	  ((stringp ref) (list 'manual-entry ref))
	  (t ref))))

(defun smart-man-c-routine-ref ()
  "Returns form to jump to def of C function whose name is at point, if any.
Valid sections within the man page are: ROUTINES, MACROS or FUNCTIONS.
Uses (smart-tags-file) function to determine etags file from which to
locate the definition.

Returns etags file name if point is on an identifier in the appropriate
section and the jump is done, otherwise, returns nil."
  (let ((ref)
	(opoint (point))
	(case-fold-search))
    (save-excursion
      (and (re-search-backward "^[.A-Z]" nil t)
	   (looking-at "^\\(FUNCTIONS\\|ROUTINES\\|MACROS\\)[ \t\n]")
	   (progn (goto-char opoint)
		  (skip-chars-backward "_~<>:a-zA-Z0-9(")
		  (if (or (looking-at "\\([_~<>:a-zA-Z0-9]+\\)[ \t\n]*(")
			  (looking-at "\\([_~<:A-Z][_<>:A-Z0-9]+\\)"))
		      (setq ref (buffer-substring
				 (match-beginning 1) (match-end 1))
			    )))))
    (if ref
	(let ((tags-file-name
	       (smart-tags-file (if (and (boundp 'man-path) man-path)
				    man-path
				  default-directory))))
	  (and (file-exists-p tags-file-name)
	       (file-readable-p tags-file-name)
	       (list 'let (list (list 'tags-file-name tags-file-name))
		     (list (if (br-in-browser)
			       'find-tag 'find-tag-other-window)
			   ref)))))))

(defun smart-man-file-ref ()
  "Returns form to eval to display file whose name point is on, within a FILES man page section.
If not on a file name, returns nil."
  (let ((ref)
	(opoint (point))
	(case-fold-search))
    (save-excursion
      (and (re-search-backward "^[.A-Z]" nil t)
	   (looking-at "^FILES[ \t\n]")
	     (progn (goto-char opoint)
		    (skip-chars-backward "^ \t")
		    (if (looking-at "/[^ \t\n]+")
			(setq ref (buffer-substring
				   (match-beginning 0) (match-end 0))
			      )))))
    (if ref
	(list (if (br-in-browser)
		  'find-file 'find-file-other-window)
	      ref))))

;;; ************************************************************************
;;; smart-outline functions
;;; ************************************************************************

;; The functions in this section require InfoDock's version of outline.el
;; in order to work properly.

(defvar smart-outline-cut nil
  "Non-nil means outline region was cut and is ready to be pasted at point.")

(let ((proc
	'((lambda ()
	    (make-local-variable 'smart-outline-cut)
	    ;; Non-nil means outline region was cut and is available to be
	    ;; pasted at point.
	    (setq smart-outline-cut nil)
	    ))))
  (if (boundp 'outline-mode-map)
      (eval proc)
    (var:append 'outline-mode-hook proc)))

(defun smart-outline ()
  "Collapses, expands, and moves outline entries.
Invoked via a key press when in outline-mode.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) after an outline heading has been cut via the Action Key, then paste the
     cut heading at point;
 (2) at the end of buffer, show all buffer text 
 (3) at the beginning of a heading line, cut the headings subtree from the
     buffer;
 (4) on a header line but not at the beginning or end, if headings subtree is
     hidden then show it, otherwise hide it;
 (5) anywhere else, scroll up a windowful."

  (interactive)
  (cond (smart-outline-cut
	 (setq smart-outline-cut nil) (yank))
	((eobp) (show-all))
	((and (bolp) (looking-at outline-regexp))
	 (setq smart-outline-cut t)
	 (kill-region
	  (point)
	  (or (outline-get-next-sibling)
	      ;; Skip past start of current entry
	      (progn (re-search-forward outline-regexp nil t)
		     (smart-outline-to-entry-end t (outline-level))))))

	((or (eolp) (zerop (save-excursion (beginning-of-line)
					   (outline-level))))
	 (smart-scroll-up))
	;; On an outline header line but not at the start/end of line.
	((smart-outline-subtree-hidden-p)
	 (show-subtree))
	(t (hide-subtree))))


(defun smart-outline-assist ()
  "Collapses, expands, and moves outline entries.
Invoked via an assist-key press when in outline-mode.  It assumes that
its caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) after an outline heading has been cut via the action-key, allow multiple
     pastes throughout the buffer (last paste should be done with the Action Key,
     not the Assist Key);
 (2) at the end of buffer, hide all bodies in buffer;
 (3) at the beginning of a heading line, cut the current heading (sans
     subtree) from the buffer;
 (4) on a header line but not at the beginning or end, if heading body is
     hidden then show it, otherwise hide it;
 (5) anywhere else, scroll down a windowful."

  (interactive)
  (cond (smart-outline-cut (yank))
	((eobp) (hide-body ))
	((and (bolp) (looking-at outline-regexp))
	 (setq smart-outline-cut t)
	 (kill-region (point) 
		      ;; Skip past start of current entry
		      (progn (re-search-forward outline-regexp nil t)
			     (smart-outline-to-entry-end
			      nil (outline-level)))))
	((or (eolp) (zerop (save-excursion (beginning-of-line)
					   (outline-level))))
	 (smart-scroll-down))
	;; On an outline header line but not at the start/end of line.
	((smart-outline-subtree-hidden-p)
	 (show-entry))
	(t (hide-entry))))

(defun smart-outline-to-entry-end
  (&optional include-sub-entries curr-entry-level)
  "Goes to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL is an integer representing the length of the current level
string which matched to 'outline-regexp'.  If INCLUDE-SUB-ENTRIES is nil,
CURR-ENTRY-LEVEL is not needed."
  (let (next-entry-exists)
    (while (and (setq next-entry-exists
		      (re-search-forward outline-regexp nil t))
		include-sub-entries
		(save-excursion
		  (beginning-of-line)
		  (> (outline-level)
		     curr-entry-level))))
    (if next-entry-exists
	(progn (beginning-of-line) (point))
      (goto-char (point-max)))))

(defun smart-outline-subtree-hidden-p ()
  "Returns t if at least initial subtree of heading is hidden, else nil."
  (save-excursion
    (if (re-search-forward "[\n\^M]" nil t) (= (preceding-char) ?\^M))))

;;; ************************************************************************
;;; smart-tar functions
;;; ************************************************************************

(defun smart-tar ()
  "Uses a single key or mouse key to manipulate tar file entries.

Invoked via a key press when in tar-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the selected file/directory is displayed for
     editing in the other window;
 (2) on or after the last line in the buffer, if any deletes are to be
     performed, they are executed after user verification, otherwise, this
     tar file browser is quit."

  (interactive)
  (cond ((last-line-p)
	 (let (flagged)
	   (save-excursion
	     (goto-char 1)
	     (setq flagged (re-search-forward "^D" nil t)))
	   (if flagged
	       (tar-expunge)
	     (kill-buffer nil))))
	(t (tar-extract-other-window))))

(defun smart-tar-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate tar file entries.

Invoked via an assist-key press when in dired-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on an entry line, the current entry is marked for deletion;
 (2) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((last-line-p)
	 (tar-unflag (- (count-lines (point-min) (point-max))))
	 (goto-char (point-max)))
	(t (tar-flag-deleted 1))))

;;; ************************************************************************
;;; smart-wrolo functions
;;; ************************************************************************

(defun smart-wrolo ()
  "In wrolo match buffer, edits current entry.
Uses one key or mouse key.

Invoked via a key press when in the 'rolo-display-buffer'.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer."
  (interactive)
  (rolo-edit-entry))

(fset 'smart-wrolo-assist 'smart-wrolo)

(provide 'hui-mouse)
