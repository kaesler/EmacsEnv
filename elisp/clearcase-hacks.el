;; Emacs code to support use of Clearcase.
;;
;; Features we would like:
;;   check-in
;;   check-out
;;   display view in mode line.

(provide 'clearcase-hacks)

(defun clearcase-current-view ()
              
  "Returns the currently active Clearcase view tag."
              
  (substring (getenv "CLEARCASE_ROOT") 6))
            
(defun file-inode (file)
              
  "Return the inode number for FILE."
              
  (let ((stat-info (file-attributes file)))
    (if stat-info
        (nth 10 stat-info)
      nil)))
            
(defun clearcase-element-p (path)
              
  "Determine if PATH refers to a Clearcase element."
              
  (interactive)
  (let ((extended-path (concat path "@@")))
    (and (file-exists-p path)
         (file-directory-p extended-path)

         ;; Non-checked-out elements have the same inode-number
         ;; as the extended name ("foo@@").
         ;; Not so for checked out, and therefore writeable elements.
         ;;
         (or (file-writable-p path)
             (eq (file-inode path)
                 (file-inode extended-path))))))
            
;; Emacs find-file hook to rename a buffer containing a version-extended
;; ClearCase element so that the buffer name starts with the element name
;; and the buffer's default directory is the directory containing the element.
;;
;; Written by John Vasta, based on Bill Sommerfelds check-dsee-name function.
;; Modified by Adam Matusiak to "auto" extend clearcase version number.

(defconst clearcase-version-prefix "@@/")
(defconst clearcase-cleartool-path "/usr/atria/bin/cleartool")
(defconst clearcase-cleartool-ls-buff " *cleartool-ls-ouput*")
(defconst clearcase-cleartool-error-string "^cleartool: Error:")
(defvar do-cleartool-ls t)

(defun clearcase-get-full-file-name()
  (let
      ;; Start off with the original buffer name
      ((extended-file-name (buffer-file-name)))

    ;; Only try to expand the name if O.K. to do so. 
    (if (and do-cleartool-ls (file-exists-p clearcase-cleartool-path))
        (save-excursion
          (call-process clearcase-cleartool-path
                        nil
                        (get-buffer-create clearcase-cleartool-ls-buff)
                        nil
                        "ls"
                        (expand-file-name(buffer-file-name)))
          (set-buffer clearcase-cleartool-ls-buff)

          ;; Only reset the string if we don't encounter a cleartool error
          (if (not (string-match clearcase-cleartool-error-string (buffer-string) 0))
              (progn
                (string-match "^[^ \t\n]*" (buffer-string) 0)
                (setq extended-file-name (buffer-substring 1 (1+ (match-end 0))))))
          (kill-buffer clearcase-cleartool-ls-buff)))

    ;; Return the current value of extended-file-name.
    extended-file-name))

(defun clearcase-find-file-hook ()

  ;; If the user supplied a Clearcase extended pathname,
  ;; we only need to set the correct mode.
  ;;
  (let ((version-suffix (string-match clearcase-version-prefix (buffer-file-name))))
    (if version-suffix
        (let* ((orig-buffer-file-name buffer-file-name))
          (setq buffer-file-name (substring orig-buffer-file-name 0 version-suffix))
          (set-auto-mode)
          (setq buffer-file-name orig-buffer-file-name))

      ;; Otherwise, we need to rename the buffer, to contain
      ;; the version.
      ;;
      (let ((extended-file-name (clearcase-get-full-file-name)))
        (if (string-match clearcase-version-prefix extended-file-name)
            (progn
              (let ((elements-directory (file-name-directory (directory-file-name (substring extended-file-name 0 (match-end 0))))))
                (let ((new-buffer-name (substring extended-file-name (length elements-directory))))
                  (or (string= new-buffer-name (buffer-name))
                    
                      ;; Uniquify the name, if necessary.
                      ;;
                      (let
                          ((n 2)
                           (uniquifier-string ""))
                        (while (get-buffer (concat new-buffer-name uniquifier-string))
                          (setq uniquifier-string (format "<%d>" n))
                          (setq n (1+ n)))
                        (rename-buffer (concat new-buffer-name uniquifier-string))))))))))))
  
(add-hook 'find-file-hooks 'clearcase-find-file-hook)


;; ;;Here's all the mail I've collected on hacks for clearcase
;; ;;under emacs.  No warranty provided, use at your own risk,
;; ;;etc...
;; ;;						-Larry
;; ;;
;; ;;Date: Fri, 20 Dec 91 9:51:26 EST
;; ;;
;; ;;Attilio asked me about this, so I thought I'd publicize it ...
;; ;;
;; ;;Following is an emacs macro for use when you open ("find") a file,
;; ;;try to edit it, but discover that it is not checked out to your
;; ;;view.
;; ;;
;; ;;The use of the "enlarge_window" function is an attempt to make
;; ;;cleartool's checkout message unobtrusive. You might play with
;; ;;the numeric argument to suit your window size.
;; ;;
;; ;; -jjp
;; ;;
;; 
;; (defun cleartool-checkout-file ()
;;   "Checkout the ClearCase element currently in buffer"
;;   (interactive)
;;   (shell-command (concat "cleartool checkout " buffer-file-name))
;;   (revert-buffer 1 1)
;;   (enlarge-window 25))
;; 
;; 
;; ;;Subject: view name in your mode line
;; ;;
;; ;;Here's a quick emacs hack to pick up your setview view from your
;; ;;environment and stick it into your mode line, a la:
;; ;;
;; ;;- --**-Emacs: *mail*                (Mail)----All---(henrik_view)---------------
;; ;;
;; ;;Since most of the Emacs modes modify the default-mode-line-format (but
;; ;;don't seem to chuck it entirely) you'll automatically pick this up in
;; ;;RMAIL, C mode, etc.
;; ;;
;; ;;					larry...
;; 
;; ;; **** Let's overwrite the modeline and create havoc!
;; (if (equal (setq cc_root (getenv "CLEARCASE_ROOT")) nil)
;;     (setq my-current-view "** NONE **")
;;   (setq my-current-view (substring cc_root 6)))
;; 
;; (setq-default mode-line-format
;;   (list (purecopy "")
;;    'mode-line-modified
;;    'mode-line-buffer-identification
;;    (purecopy "   ")
;;    'global-mode-string
;;    (purecopy "   %[(")
;;    'mode-name 'minor-mode-alist "%n" 'mode-line-process
;;    (purecopy ")%]----")
;;    (purecopy '(-3 . "%p"))
;;    (purecopy "---%[(")
;;    (purecopy 'my-current-view)
;;    (purecopy ")%]")
;;    (purecopy "-%-")))
;; 
;; 
;; ;;Subject: view in emacs mode line, version 2.0
;; ;;
;; ;;The code below improves on the mode line hack.  In addition to choosing the
;; ;;setview view name by default, if a view-extended name is given (recognized 
;; ;;by the "/view" prefix) the view name is parsed out of it.
;; ;;
;; ;;					larry...
;; ;;
;; 
;; ;; **** Let's overwrite the modeline and create havoc!
;; 
;; ;; first, get the setview view for this process ...
;; (if (equal (setq cc_root (getenv "CLEARCASE_ROOT")) nil)
;;     (setq my-current-view "** NONE **")
;;   (setq my-current-view (substring cc_root 6)))
;; 
;; ;; this function does the setup of the modeline for the given viewtag
;; (defun mode-line-with-view-tag (view-tag)
;;   (list (purecopy "")
;; 	'mode-line-modified
;; 	'mode-line-buffer-identification
;; 	(purecopy "   ")
;; 	'global-mode-string
;; 	(purecopy "   %[(")
;; 	'mode-name 'minor-mode-alist "%n" 'mode-line-process
;; 	(purecopy ")%]----")
;; 	(purecopy '(-3 . "%p"))
;; 	(purecopy "---%[(")
;; 	(purecopy view-tag)
;; 	(purecopy ")%]")
;; 	(purecopy "-%-")))
;; 
;; ;; this function adds a view tag to the current buffer's mode line
;; (defun view-tag-to-mode-line (view-tag)
;;   (setq mode-line-format (mode-line-with-view-tag view-tag)))
;; 
;; ;; this function adds a view tag to the default mode line
;; (defun view-tag-to-default-mode-line (view-tag)
;;   (setq-default mode-line-format (mode-line-with-view-tag view-tag)))
;; 
;; ;; this function gets the name of the view for the current file
;; (defun extract-view-from-pname (my-pname)
;;   (if (equal my-pname nil)
;;       "NIL_PNAME"
;;     (if (equal (substring my-pname 0 6) "/view/")
;; 	(substring my-pname 6 (+ 6 (string-match "/" (substring my-pname 6)
;; 						 )))
;;       my-current-view))
;; )
;; 
;; ;; **** Set the default view tag
;; (view-tag-to-default-mode-line my-current-view)
;; 
;; ;; **** Set up the find-file-hooks to set the current buffer every other time
;; (setq find-file-hooks 
;;       (list (function (lambda ()
;; 		 (view-tag-to-mode-line 
;; 		      (extract-view-from-pname (buffer-file-name)))
;; ))))
;; 
;; ;; **** Set up the find-file-not-found hooks to set the current buffer if 
;; ;;      file not found
;; (setq find-file-not-found-hooks 
;;       (list (function (lambda ()
;; 		 (view-tag-to-mode-line 
;; 		      (extract-view-from-pname (buffer-file-name)))
;; ))))
;; 
;; ;; **** Set up the write-file-hooks to set the current buffer on write
;; ;;
;; (setq write-file-hooks
;;       (list (function (lambda ()
;; 		 (view-tag-to-mode-line 
;; 		      (extract-view-from-pname (buffer-file-name)))
;; ))))
;; 
;; 
;; 
;; ;;Subject: patch for write-mode-hooks ...
;; ;;
;; ;;view-tag-to-mode-line should return nil (since they are going on the
;; ;;hooks list).  Reading works OK with the examples given, but the
;; ;;write-file-hooks assume the write has failed and do not update the
;; ;;file.
;; ;;
;; ;;If you modify view-tag-to-mode-line as follows it works properly (I 
;; ;;changed view-tag-to-default-mode-line to be consistent with it, though
;; ;;it really doesn't matter since that's only invoked at startup time and
;; ;;the return value is not checked).
;; ;;
;; ;;					larry...
;; 
;; 
;; ;; this function adds a view tag to the current buffer's mode line
;; ;; must return NIL so rest of processing occurs at read/write time...
;; (defun view-tag-to-mode-line (view-tag)
;;   (setq mode-line-format (mode-line-with-view-tag view-tag))
;;   nil)
;; 
;; ;; this function adds a view tag to the default mode line
;; (defun view-tag-to-default-mode-line (view-tag)
;;   (setq-default mode-line-format (mode-line-with-view-tag view-tag))
;;   nil)
;; 
;; 
;; 
;; ;;Subject: view in emacs mode line, version 2.0
;; ;;
;; ;;The code below improves on the mode line hack.  In addition to choosing the
;; ;;setview view name by default, if a view-extended name is given (recognized 
;; ;;by the "/view" prefix) the view name is parsed out of it.
;; 
;; ;; **** Let's overwrite the modeline and create havoc!
;; 
;; ;; first, get the setview view for this process ...
;; (if (equal (setq cc_root (getenv "CLEARCASE_ROOT")) nil)
;;     (setq my-current-view "** NONE **")
;;   (setq my-current-view (substring cc_root 6)))
;; 
;; ;; this function does the setup of the modeline for the given viewtag
;; (defun mode-line-with-view-tag (view-tag)
;;   (list (purecopy "")
;; 	'mode-line-modified
;; 	'mode-line-buffer-identification
;; 	(purecopy "   ")
;; 	'global-mode-string
;; 	(purecopy "   %[(")
;; 	'mode-name 'minor-mode-alist "%n" 'mode-line-process
;; 	(purecopy ")%]----")
;; 	(purecopy '(-3 . "%p"))
;; 	(purecopy "---%[(")
;; 	(purecopy view-tag)
;; 	(purecopy ")%]")
;; 	(purecopy "-%-")))
;; 
;; ;; this function adds a view tag to the current buffer's mode line
;; (defun view-tag-to-mode-line (view-tag)
;;   (setq mode-line-format (mode-line-with-view-tag view-tag)))
;; 
;; ;; this function adds a view tag to the default mode line
;; (defun view-tag-to-default-mode-line (view-tag)
;;   (setq-default mode-line-format (mode-line-with-view-tag view-tag)))
;; 
;; ;; this function gets the name of the view for the current file
;; (defun extract-view-from-pname (my-pname)
;;   (if (equal my-pname nil)
;;       "NIL_PNAME"
;;     (if (equal (substring my-pname 0 6) "/view/")
;; 	(substring my-pname 6 (+ 6 (string-match "/" (substring my-pname 6)
;; 						 )))
;;       my-current-view))
;; )
;; 
;; ;; **** Set the default view tag
;; (view-tag-to-default-mode-line my-current-view)
;; 
;; ;; **** Set up the find-file-hooks to set the current buffer every other time
;; (setq find-file-hooks 
;;       (list (function (lambda ()
;; 		 (view-tag-to-mode-line 
;; 		      (extract-view-from-pname (buffer-file-name)))
;; ))))
;; 
;; ;; **** Set up the find-file-not-found hooks to set the current buffer if 
;; ;;      file not found
;; (setq find-file-not-found-hooks 
;;       (list (function (lambda ()
;; 		 (view-tag-to-mode-line 
;; 		      (extract-view-from-pname (buffer-file-name)))
;; ))))
;; 
;; ;; **** Set up the write-file-hooks to set the current buffer on write
;; ;;
;; (setq write-file-hooks
;;       (list (function (lambda ()
;; 		 (view-tag-to-mode-line 
;; 		      (extract-view-from-pname (buffer-file-name)))
;; ))))
;; 
;; 
;; ;;Subject: patch for write-mode-hooks ...
;; ;;
;; ;;view-tag-to-mode-line should return nil (since they are going on the
;; ;;hooks list).  Reading works OK with the examples given, but the
;; ;;write-file-hooks assume the write has failed and do not update the
;; ;;file.
;; ;;
;; ;;If you modify view-tag-to-mode-line as follows it works properly (I 
;; ;;changed view-tag-to-default-mode-line to be consistent with it, though
;; ;;it really doesn't matter since that's only invoked at startup time and
;; ;;the return value is not checked).
;; ;;
;; ;;					larry...
;; 
;; ;; this function adds a view tag to the current buffer's mode line
;; ;; must return NIL so rest of processing occurs at read/write time...
;; (defun view-tag-to-mode-line (view-tag)
;;   (setq mode-line-format (mode-line-with-view-tag view-tag))
;;   nil)
;; 
;; ;; this function adds a view tag to the default mode line
;; (defun view-tag-to-default-mode-line (view-tag)
;;   (setq-default mode-line-format (mode-line-with-view-tag view-tag))
;;   nil)
;; 
;; 
;; ;; Operations on RCS-controlled files (check in, check out, logs, etc.)
;; 
;; (defun rcs-check-out (filename &optional switches)
;;   "Attempt to check the specified file out using RCS.
;; If a prefix argument is supplied, will let you edit the `co' switches
;; used, defaulting to \"-l\" to check out locked.
;; Use \"-rREV\" or \"-lREV\" to check out specific revisions."
;;   ;; Returns non-nil for success, signals an error else.
;;   (interactive (list (rcs-read-file-name "Check out file: ")
;; 		     (and current-prefix-arg
;; 			  (rcs-read-switches "Switches for co: " "-l"))))
;;   (message "Working...")
;;   (setq filename (expand-file-name filename))
;;   (let ((output-buffer (rcs-get-output-buffer filename)))
;;     (delete-windows-on output-buffer)
;;     (save-excursion
;;       (set-buffer output-buffer)
;;       (apply 'call-process "co" nil t nil
;; 	     ;; -q: quiet (no diagnostics)
;; 	     (append switches rcs-default-co-switches (list "-q" filename)))
;;       (run-hooks 'rcs-check-out-buffer-hook)
;;       (if (or (not (file-readable-p filename))
;; 	      (> (buffer-size) 0))
;; 	  (rcs-error "co" filename output-buffer)))
;;     (rcs-refresh-buffer filename))
;;   (message ""))
;; 
;; (defun rcs-check-in (filename log &optional switches)
;;   "Attempt to check the specified file back in using RCS.
;; You are prompted for a LOG string describing your changes.
;; If a prefix argument is supplied, will let you edit the `ci' switches used,
;; with default \"-l\" switch to keep file locked.
;; Use \"-uREV\" or \"-lREV\" to check in as specific revision."
;;   (interactive
;;    (let ((prefix current-prefix-arg))
;;     ;; have to test prefix before reading log string since this may
;;     ;; clear it if it uses recursive edit
;;      (list (rcs-read-file-name "Check in file: ")
;; 	   (rcs-read-log-string "Log message: ")
;; 	   (and prefix 
;; 		(rcs-read-switches "Switches for ci: " "-l")))))
;;   (setq filename (expand-file-name filename))
;;   (setq switches (cons "-u" switches))
;;   ;; A "-l" in SWITCHES will override this "-u", so we can
;;   ;; unconditionally prepend it to SWITCHES.  We always need an
;;   ;; existing working file, to have something to put in the buffer
;;   ;; afterwards.
;;   (or (zerop (length log))
;;       ;; Use -m for log message if not nil or ""
;;       (setq switches (cons (concat "-m" log) switches)))
;;   (let* ((output-buffer (rcs-get-output-buffer filename))
;; 	 (f-buffer (get-file-buffer filename)))
;;     (and (interactive-p)
;; 	 f-buffer
;; 	 (buffer-modified-p f-buffer)
;; 	 (not (and (y-or-n-p "Save buffer first? ")
;; 		   (save-excursion
;; 		     (set-buffer f-buffer)
;; 		     (save-buffer)
;; 		     t)))
;; 	 (not (y-or-n-p "Check in despite unsaved changes to file? "))
;; 	 (error "Check-in aborted"))
;;     (message "Working...")
;;     (delete-windows-on output-buffer)
;;     (save-excursion
;;       (set-buffer output-buffer)
;;       (apply 'call-process "ci" nil t nil
;; 	     (append switches rcs-default-ci-switches
;; 		     (list "-q" filename)))	;  -q: quiet
;;       (run-hooks 'rcs-check-in-buffer-hook)
;;       (if (> (buffer-size) 0)
;; 	  (rcs-error "ci" filename output-buffer)))
;;     (rcs-refresh-buffer filename))
;;   (message ""))
;; 
;; (defun rcs-toggle-read-only ()
;;   "If the buffer is read-only and under RCS, adjust RCS status.
;; That is, make buffer writable and check file out locked for editing."
;;   (interactive)
;;   (if (and rcs-active
;; 	   rcs-mode
;; 	   buffer-read-only
;; 	   buffer-file-name
;; 	   (not (rcs-we-locked buffer-file-name rcs-mode)))
;;       (if (y-or-n-p "Check buffer out from RCS for edit? ")
;; 	  (if (and (file-exists-p buffer-file-name)
;; 		   (file-writable-p buffer-file-name))
;; 	      (if (yes-or-no-p "\
;; Illegally writable copy of RCS controlled file - force checkout anyway? ")
;; 		  (rcs-check-out buffer-file-name '("-f" "-l"))
;; 		(error "Illegally writable copy of RCS controlled file %s"
;; 		       buffer-file-name))
;; 	    ;; Try to get a locked version. May error.
;; 	    (rcs-check-out buffer-file-name '("-l")))
;; 	(if (y-or-n-p
;; 	     "File is RCS controlled - make buffer writable anyway? ")
;; 	    (toggle-read-only)		; this also updates the modeline
;; 	  (barf-if-buffer-read-only)))
;;     (toggle-read-only))
;;   (message ""))
