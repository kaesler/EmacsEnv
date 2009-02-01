;; Run xdb (the HP-UX proprietary debugger) under Emacs 
;; $Id: xdb.el,v 1.2 1992/04/06 12:53:47 tml Exp $
;; Copyright (C) 1988 Free Software Foundation, Inc.
;; Copyright (C) 1992 Tor Lillqvist

;; This file is not a part of GNU Emacs, but gdb.el was.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.

;; Some ideas are due to  Masanobu. 

;; Adapted for xdb by tml@tik.vtt.fi

;; Description of XDB interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using gdb to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; We use the undocumented -SDE protocol of xdb (which softdebug seems
;; to use).  As always when using undocumented interfaces, be prepared
;; that it might change sometime in the future when you update your system,
;; and this file might not longer work.

;; Starting up:

;; In order to use this facility, invoke the command XDB to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Xdb will be invoked on this file, in a
;; window named *xdb-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the xdb command "lb" on \M-B
;; (def-xdb lb "\eB")

;; This causes the emacs command xdb-lb to be defined, and runs
;; xdb-display-frame after the command.

;; xdb-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the xdb window.  For example after a xdb-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the xdb buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; xdb-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.

(require 'shell)
(provide 'xdb)

(defvar xdb-prompt-pattern "^>"
  "A regexp to recognize the prompt for xdb.") 

(defvar xdb-mode-map nil
  "Keymap for xdb-mode.")

(if xdb-mode-map
   nil
  (setq xdb-mode-map (copy-keymap shell-mode-map))
  (define-key xdb-mode-map "\C-l" 'xdb-refresh))

(define-key ctl-x-map " " 'xdb-break)
(define-key ctl-x-map "&" 'send-xdb-command)

;;Of course you may use `def-xdb' with any other xdb command, including
;;user defined ones.   

(defmacro def-xdb (name key &optional doc)
  (let* ((fun (intern (format "xdb-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'xdb-call cstr))
	  (list 'define-key 'xdb-mode-map key  (list 'quote fun)))))

(def-xdb "s" "\es" "Step one source line with display")
;; (def-xdb "stepi" "\ei" "Step one instruction with display")
(def-xdb "S" "\en" "Step one source line (skip functions)")
(def-xdb "c" "\ec" "Continue with display")

;; (def-xdb "finish" "\C-c\C-f" "Finish executing current function")
(define-key xdb-mode-map "\C-c\C-f" 'xdb-finish)
;; (def-xdb "up" "\eu"   "Go up N stack frames (numeric arg) with display")
;; (def-xdb "down"   "\ed"   "Go down N stack frames (numeric arg) with display")

(defun xdb-mode ()
  "Major mode for interacting with an inferior Xdb process.
The following commands are available:

\\{xdb-mode-map}

\\[xdb-display-frame] displays in the other window
the last line referred to in the xdb buffer.

\\[xdb-step],\\[xdb-next], and \\[xdb-nexti] in the xdb window,
call xdb to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[xdb-break].

Commands:
Many commands are inherited from shell mode. 
Additionally we have:

\\[xdb-display-frame] display frames file in other window
\\[xdb-step] advance one line in program
\\[xdb-next] advance one line in program (skip over calls).
\\[send-xdb-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'xdb-mode)
  (setq mode-name "Inferior Xdb")
  (setq mode-line-process '(": %s"))
  (use-local-map xdb-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'xdb-last-filename)
  (setq xdb-last-filename nil)
  (make-local-variable 'xdb-last-lineno)
  (setq xdb-last-lineno nil)
  (make-local-variable 'xdb-last-frame-displayed-p)
  (setq xdb-last-frame-displayed-p t)
  (make-local-variable 'xdb-delete-prompt-marker)
  (setq xdb-delete-prompt-marker nil)
  (make-local-variable 'xdb-filter-accumulator)
  (setq xdb-filter-accumulator nil)
  (make-local-variable 'xdb-ignore-output)
  (setq xdb-ignore-output nil)
  (make-local-variable 'xdb-show-locations)
  (setq xdb-show-locations nil)
  (make-local-variable 'xdb-set-frame)
  (setq xdb-set-frame t)
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern xdb-prompt-pattern)
  (run-hooks 'shell-mode-hook 'xdb-mode-hook))

(defvar current-xdb-buffer nil)

(defvar xdb-command-name "xdb"
  "Pathname for executing xdb.")

(defun xdb (path)
  "Run xdb on program FILE in buffer *xdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for XDB.  If you wish to change this, use
the XDB commands `cd DIR' and `directory'."
  (interactive "FRun xdb on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*xdb-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (make-shell (concat "xdb-" file) xdb-command-name nil  "-SDE"
		"-d" default-directory file)
    (xdb-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'xdb-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'xdb-sentinel)
    (xdb-set-buffer)))

(defun xdb-set-buffer ()
  (cond ((eq major-mode 'xdb-mode)
	(setq current-xdb-buffer (current-buffer)))))

;; This function is responsible for inserting output from XDB
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that XDB prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun xdb-filter (proc string)
  (let ((inhibit-quit t))
    (xdb-filter-2 proc (if xdb-filter-accumulator
			   (concat xdb-filter-accumulator string)
			 string))))

(defun xdb-filter-2 (proc string)
  (setq xdb-filter-accumulator nil)
  (let (beg end)
    (while (and (setq beg (string-match "\016" string))
		(setq end (string-match "\017" string))
		(> end beg))
      (let* ((match (string-match
		     "\016[0-9]+ \\([A-Z]+\\) +\\([^\017]*\\)\017" string))
	     (keyword (substring
		       string (match-beginning 1) (match-end 1)))
	     (func (intern (concat "xdb-" keyword))))
	(if (fboundp func)
	    (eval (list func proc (substring string (match-beginning 2)
					     (match-end 2)))))
	(xdb-filter-insert proc (substring string 0 beg))
	(setq string (substring string (1+ end)))))
    (if beg
	(setq xdb-filter-accumulator (substring string beg)
	      string (substring string 0 beg)))
    (xdb-filter-insert proc string)))

(defun xdb-filter-insert (proc string)
  (if (not xdb-ignore-output)
      (let ((moving (= (point) (process-mark proc)))
	    (output-after-point (< (point) (process-mark proc)))
	    (old-buffer (current-buffer))
	    start)
	(set-buffer (process-buffer proc))
	(unwind-protect
	    (save-excursion
	      ;; Insert the text, moving the process-marker.
	      (goto-char (process-mark proc))
	      (setq start (point))
	      (insert string)
	      (set-marker (process-mark proc) (point))
	      (xdb-maybe-delete-prompt)
	      ;; Check for a filename-and-line number.
	      (xdb-display-frame
	       ;; Don't display the specified file
	       ;; unless (1) point is at or after the position where output appears
	       ;; and (2) this buffer is on the screen.
	       (or output-after-point
		   (not (get-buffer-window (current-buffer))))
	       ;; Display a file only when a new filename-and-line-number appears.
	       t))
	  (set-buffer old-buffer))
	(if moving (goto-char (process-mark proc))))))

(defun xdb-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the xdb buffer.
	     (set-buffer obuf))))))

(defun xdb-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (xdb-display-frame))

(defun xdb-display-frame (&optional nodisplay noauto)
  "Obey the last filename-and-line marker from XDB.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (xdb-set-buffer)
  (and xdb-last-filename xdb-last-lineno (not nodisplay)
       (or (not xdb-last-frame-displayed-p) (not noauto))
       (progn (xdb-display-line xdb-last-filename xdb-last-lineno)
	      (setq xdb-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun xdb-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun xdb-call (command)
  "Invoke xdb COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq xdb-delete-prompt-marker (point-marker))
  (xdb-set-buffer)
  (send-string (get-buffer-process current-xdb-buffer)
	       (concat command "\n")))

(defun xdb-maybe-delete-prompt ()
  (if (and xdb-delete-prompt-marker
	   (> (point-max) (marker-position xdb-delete-prompt-marker)))
      (let (start)
	(goto-char xdb-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq xdb-delete-prompt-marker nil))))

(defun xdb-break ()
  "Set XDB breakpoint at this source line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point))))))
    (send-string (get-buffer-process current-xdb-buffer)
		 (concat "b " file-name ":" line "\n"))))

(defun xdb-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))

(defvar xdb-commands nil
  "List of strings or functions used by send-xdb-command.
It is for customization by you.")

(defun send-xdb-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the xdb buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list xdb-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of xdb-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg xdb-commands)))
    (setq addr (xdb-read-address))
    (if (eq (current-buffer) current-xdb-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-xdb-buffer)
    (goto-char (dot-max))
    (insert-string comm)))

(defun xdb-finish (arg)
  (interactive "p")
  (xdb-call "bu 1 {}; c\n"))

;; These functions are called when the corresponding keyword is seen.

(defun xdb-PRINTFILETAG (proc s)
  (if xdb-set-frame
      (progn
	(setq xdb-last-filename s)
	(setq xdb-last-frame-displayed-p nil))))

(defun xdb-PRINTLINETAG (proc s)
  (if xdb-set-frame
      (progn
	(setq xdb-last-lineno (string-to-int s))
	(setq xdb-last-frame-displayed-p nil))))
  
(defun xdb-PROMPT (proc s)
  (xdb-filter-insert proc s))

(defun xdb-YESNO (proc s)
  (string-match "[^ ]+ \\(.*\\)" s)
  (if (y-or-n-p (substring s (match-beginning 1) (match-end 1)))
      (process-send-string proc (concat (substring s 0 1) "\n"))
    (process-send-string proc "-\n")))

(defun xdb-PRINTPOSSTART (proc s)
  (if xdb-show-locations
      nil
    (setq xdb-ignore-output t)))

(defun xdb-PRINTPOSSTOP (proc s)
  (setq xdb-ignore-output nil))

(defun xdb-LISTBREAKPOINTSTART (proc s)
  (setq xdb-show-locations t
	xdb-set-frame nil))

(defun xdb-LISTBREAKPOINTSTOP (proc s)
  (setq xdb-show-locations nil
	xdb-set-frame t))

(defun xdb-STACKTRACESTART (proc s)
  (setq xdb-set-frame nil))

(defun xdb-STACKTRACESTOP (proc s)
  (setq xdb-set-frame t))
