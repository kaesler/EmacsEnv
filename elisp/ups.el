The following is minimally tested code. Use it as is, if you wish to do so.

;; ups.el
;;
;; A modified version of `ps-mode.el', an interface to the Unix ``ps'' command
;;      by Levent N Atasoy <L_Atasoy@MacSch.com>
;; Font-lock, menus, mouse button, ... are defined for XEmacs only.

;; Originally written by Dirk Grunwald, grunwald@m.cs.uiuc.edu
;; University of Illinois

;; This file is not (yet) part of GNU Emacs, but is made available under
;; the same conditions.

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

;;; Installation and Customization:
;;  --------------------
;;
;; To use ups put this in your .emacs file:
;;
;;    (autoload 'ups "ups" "A major-mode for sending signals to processes." t)
;;
;; If you want ups to be loaded from the very beginning, you should have
;;
;;  (require 'ups)
;;  
;; You may want to modify the first set of variables listed below.
;; In addition, you may want to redefine font-lock-deemphasize-face if you
;; with

;;; Major Changes from `ps-mode':
;;     Added XEmacs features: menus, hilighting, font-lock, icon, ...
;;     Added Non berkeley-unix definitions for SGI, HPUX, ...?
;;     Changed ps-mode-all to a ups-program-list, which is nicer INMO 
;;     Changed some of the key-mappings

;;
;; You may want to set these variables in your .emacs
;;

(defvar ups-nice-args "+4"
  "*Default niceness.")

(defvar ups-mouse-mark 'ups-mark-kill
  "*Default mark for the function ups-mouse-mark bound to mouse button2")

(cond
 ((string-equal system-type "berkeley-unix")
  (defvar ups-program-list
    (list "ps ugx" "ps ugxa")
    "*Sequence of arguments passed to ups-program."))
 (t 
  (defvar ups-program-list
    (list (concat  "ps -f -u " (getenv "USER")) "ps -df" "ps -ef" )
    "*Sequence of arguments passed to ups-program.")))

(defvar ups-hook nil
     "*Hook to run in the *ps* buffer after starting ups.") 

(defvar ups-program (car ups-program-list)
  "*Program to call to generate process information.
Initially defaults to the first element of ups-program-list.")


;;
;; The following variables are less likely to be of interest to the user.
;;

(defvar ups-buffer "*Unix ps*" 
   "*Buffer name of ups information.")

(defvar ups-uptime-program "uptime"
  "*Program to call to generate uptime information.")

(defvar ups-kill-program "kill"
  "*Program to call to kill a process.")

(defvar ups-nice-program "/etc/renice"
  "*Program to call to nice a process.")

(defvar ups-bogus-lines 3
  "Number of non-process lines at the top of the display")

(defvar ups-pid-array nil
  "Array of process id's. Array index corresponds to line number in
   current ups-buffer")

(defvar ups-signal-array nil
  "Array of signals to be sent to individual processes. Each signal is
either a signal number or a signal name")

(defvar ups-pid-position nil
  "Position of the PID field in the ups buffer")

(defvar ups-lines nil
  "Number of lines in the current ups buffer")

(defvar ups-map nil 
  "Keymap used in ups mode")

(if ups-map
    ()
  (setq ups-map (make-sparse-keymap))
  (if (string-match "XEmacs" emacs-version)
      (progn
	(define-key ups-map '[button3] 'ups-popup-menu)
	(define-key ups-map '[button2] 'ups-mouse-mark)))
  (define-key ups-map "\C-c\C-c" 'ups-issue-signals)     
  (define-key ups-map "x" 'ups-issue-signals)            
  (define-key ups-map "X" 'ups-issue-signals)            
  (define-key ups-map " " 'next-line)                    
  (define-key ups-map "H" 'ups-mark-hangup)              
  (define-key ups-map "h" 'ups-mark-hangup)              
  (define-key ups-map "I" 'ups-mark-int)                 
  (define-key ups-map "i" 'ups-mark-int)                 
  (define-key ups-map "Q" 'ups-mark-quit)                
  (define-key ups-map "K" 'ups-mark-kill)                
  (define-key ups-map "k" 'ups-mark-kill)                
  (define-key ups-map "B" 'ups-mark-bus)                 
  (define-key ups-map "b" 'ups-mark-bus)                 
  (define-key ups-map "V" 'ups-mark-segv)                
  (define-key ups-map "v" 'ups-mark-segv)                
  (define-key ups-map "A" 'ups-mark-alarm)               
  (define-key ups-map "a" 'ups-mark-alarm)               
  (define-key ups-map "T" 'ups-mark-term)                
  (define-key ups-map "t" 'ups-mark-term)                
  (define-key ups-map "S" 'ups-mark-stop)                
  (define-key ups-map "s" 'ups-mark-stop)                
  (define-key ups-map "C" 'ups-mark-cont)                
  (define-key ups-map "c" 'ups-mark-cont)                
  (define-key ups-map "N" 'ups-mark-nice)                
  (define-key ups-map "n" 'ups-mark-nice)                
  (define-key ups-map "u" 'ups-mark-unmark)              
  (define-key ups-map "U" 'ups-mark-unmark)              
  (define-key ups-map "l" 'ups-next-arg)                 
  (define-key ups-map "L" 'ups-next-arg)                 
  (define-key ups-map "g" 'ups-build-process-list)       
  (define-key ups-map "G" 'ups-build-process-list)       
  (define-key ups-map "?" 'ups-build-process-list)       
  )


;;
;; Definitions for XEmacs only 
;;
(if (string-match "XEmacs" emacs-version)
 (progn

   (if (featurep 'frame-icon)
       (fi-add-mode '(ups . "axe.xbm")))

   (require 'mode-motion)

   (defvar ups-menu
     '("ups Commands"
       ["stop"	ups-mark-stop	t]
       ["interrupt"	ups-mark-int	t]
       ["terminate"	ups-mark-term	t]
       ["quit"	ups-mark-quit	t]
       ["kill"	ups-mark-kill	t]
       ["continue"	ups-mark-cont	t]
       ["hangup"	ups-mark-hangup	t]
       ["bus"	ups-mark-bus	t]
       ["segv"	ups-mark-segv	t]
       ["alarm"	ups-mark-alarm	t]
       ["nice"	ups-mark-nice	t]
       ["unmark"	ups-mark-unmark	t]
       "----"
       ["send signals"	ups-issue-signals	t]
       ["next-arg"	ups-next-arg	t]
       ["refresh"	ups-build-process-list	t]
       "----"
       ["exit ups"	(kill-buffer (current-buffer))	t]
       )
     "Popup menu called by the function `ups-menu'.")

   (add-hook 'ups-hook 'ups-install-menubar)

   (defun ups-install-menubar ()
     "Installs the-ups menu at the menubar."
     (if (and current-menubar (not (assoc "ups" current-menubar)))
	 (progn
	   (set-buffer-menubar (copy-sequence current-menubar))
	   (add-menu nil "ups" (cdr ups-menu))))
     (make-local-variable 'mode-motion-hook)
     (setq mode-motion-hook 'mode-motion-highlight-line)
     )

   (add-hook 'ups-hook 'turn-on-font-lock)

   (defun ups-popup-menu (event)
     "Display the ups-menu."
     (interactive "@e")
     (mouse-set-point event)
     (popup-menu ups-menu))

   (defun ups-mouse-mark (event)
     "Marks the process under the cursor. See variable ups-mouse-mark."
     (interactive "@e")
     (mouse-set-point event)
     (funcall ups-mouse-mark))

   (make-face 'font-lock-deemphasize-face)
   (or (face-differs-from-default-p 'font-lock-deemphasize-face)
       (set-face-foreground 'font-lock-deemphasize-face "Seagreen"))
   
   (defvar ups-font-lock-keywords
     (list
      '("^\\S .*" . italic)
      '("^\\s +root\\s +.*" . font-lock-deemphasize-face)
      (cons (concat "^\\s +" (getenv "USER") "\\s +.*") 'bold))
      "*Expressions to highlight in ups buffers.")))


(require 'cl)

;; Issue a ups-command to the current buffer, then build the array
;; of process id's and signal numbers.
;;
(defun ups-build-process-list ()
  "Kill processes from status display."
  (interactive)
  (let
      ((buffer-read-only nil))
					;
					; call ups-program
					;
    (delete-region (point-min) (point-max))
    (call-process ups-uptime-program nil t nil)
    (insert "\n")
    (shell-command ups-program t)
;    (apply 'call-process ups-program nil t nil ups-program)
    ;
    ; signals have four spaces for their symbols
    ;
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert "     ")
      (forward-line 1))
					;
					; find the word position of the
					; string ``PID'' in the header, since
					; this varies with different
					; versions of ps
					;
    (save-excursion
      (let ( bol eol (i 1) )
	(goto-char (point-min))
	(search-forward "PID")		; get to the ps output
	(beginning-of-line)
	(setq bol (point))
	(end-of-line)
	(setq eol (point))
	(narrow-to-region bol eol)
	(goto-char bol)
	(setq ups-pid-position nil)
	(while (or (not (eolp)) (not (numberp ups-pid-position)))
	  (if (looking-at " *PID")
	      (setq ups-pid-position i))
	  (setq i (+ i 1))
	  (forward-word 1))
	(widen)))
    (setq ups-lines (count-lines (point-min) (point-max)))
    (setq ups-pid-array (make-vector (+ ups-lines 1) nil))
    (setq ups-signal-array (make-vector (+ ups-lines 1) nil))
    (goto-char (point-min))
    (let
	( (i ups-bogus-lines)
	  (to-skip (- ups-pid-position 1))
	  pid-start pid-end this-pid)
      (forward-line i)
      (while (not (eobp))
	(beginning-of-line)
	(forward-word to-skip)	; skip to beginning of PID field
	(setq pid-start (point))
	(forward-word 1)
	(setq pid-end (point))
	(setq this-pid
	      (string-to-int (buffer-substring pid-start pid-end)))
	(aset ups-pid-array i
	      (if (> this-pid 0) this-pid nil))
	(setq i (+ i 1))
	(forward-line 1))))
  (goto-char (point-min))
  (forward-line ups-bogus-lines))

(defun ups ()
  "A major-mode for sending signals to processes.
In ups, you indicate signals to send to UNIX processes.
Signals are marked on the left hand side of the display using
an abbreviated name.

The following signals may be sent. To mark a process, move to the line
corresponding to that process and hit one of the captialized letters
in the list below (the lower case letters work as well).
If the process already has a mark, reissuing it clears the mark

Hup	-- SIGHUP
Int	-- SIGINT
Quit	-- SIGQUIT
Kill	-- SIGKILL
Bus	-- SIGBUS
segV	-- SIGSEGV
Alrm	-- SIGALRM
Term	-- SIGTERM
Stop	-- SIGSTOP
Cont	-- SIGCONT
Nice	-- Nice that sucker

U	-- clear a previously marked signal
G,?	-- Update the process list
L	-- Use the next member of ups-program-list
X	-- Issue these signals

button-2  marks the process under the cursor. See variable ups-mouse-mark.
"

  (interactive)
  (pop-to-buffer (get-buffer-create ups-buffer))
  (use-local-map ups-map)
  (setq major-mode 'ups)
  (setq mode-name "ps")
  (setq truncate-lines t)
  (set-syntax-table text-mode-syntax-table)
  (setq buffer-read-only t)
  (ups-build-process-list)
  (run-hooks 'ups-hook))

(defun ups-next-arg ()
  "Use the next member of ups-program-list.
Cycles ups-program through the ups-program-list."
  (interactive)
  (pop-to-buffer ups-buffer)
  (setq ups-program
     (cond
      ((cadr (member ups-program ups-program-list)))
      (t (car ups-program-list))))
  (ups-build-process-list))


;;
;;	mark line and go forward signal line. Don't allow them to
;;	mark the first line (line 0)
;;
(defun ups-mark-line (signal-symbol this-signal)
  (let
      ((this-line (count-lines (point-min) (point))))
    (if (not (bolp))
	(setq this-line (- this-line 1)))
    (save-excursion
      (if (> this-line 0)
	  (if (aref ups-pid-array this-line)
	      (let
		  ((buffer-read-only nil))
		(if (equal this-signal (aref ups-signal-array this-line))
		    (progn
		      (setq signal-symbol "     ")
		      (setq this-signal nil )))
		(beginning-of-line)
		(delete-char (length signal-symbol))
		(insert signal-symbol)
		(aset ups-signal-array this-line this-signal)))))
    (next-line 1)))

(defun ups-mark-unmark ()
  (interactive)
  (ups-mark-line "     " nil))

(defun ups-mark-hangup ()
  (interactive)
  (ups-mark-line "Hup" "HUP"))

(defun ups-mark-int ()
  (interactive)
  (ups-mark-line "Int" "INT"))

(defun ups-mark-quit ()
  (interactive)
  (ups-mark-line "Quit" "QUIT"))

(defun ups-mark-kill ()
  (interactive)
  (ups-mark-line "Kill" "KILL"))

(defun ups-mark-bus ()
  (interactive)
  (ups-mark-line "Bus" "BUS"))

(defun ups-mark-segv ()
  (interactive)
  (ups-mark-line "segV" "SEGV"))

(defun ups-mark-alarm ()
  (interactive)
  (ups-mark-line "Alrm" "ALRM"))

(defun ups-mark-term ()
  (interactive)
  (ups-mark-line "Term" "TERM"))

(defun ups-mark-stop ()
  (interactive)
  (ups-mark-line "Stop" "STOP"))

(defun ups-mark-cont ()
  (interactive)
  (ups-mark-line "Cont" "CONT"))

(defun ups-mark-nice ()
  (interactive)
  (ups-mark-line "Nice" "NICE"))

(defun ups-signal-process (this-pid this-signal)
  (if (string-equal this-signal "NICE")
      (call-process ups-nice-program nil nil nil
		      ups-nice-args (int-to-string this-pid) )
    (call-process ups-kill-program nil nil nil
		  (concat "-" 
			  (if (numberp this-signal)
			      (int-to-string this-signal)
			    this-signal))
		  (int-to-string this-pid))))

(defun ups-issue-signals ()
  (interactive)
  (let
      ((i 0))
    (beginning-of-buffer)
    (while (< i ups-lines)
      (if (aref ups-signal-array i)
	    (ups-signal-process (aref ups-pid-array i)
				    (aref ups-signal-array i)))
      (setq i (+ i 1))
      ))
  (ups-build-process-list))

(provide 'ups)

;(defun dired-sort-toggle-or-edit (&optional arg)
;  "Toggle between sort by date/name and refresh the dired buffer.
;With a prefix argument you can edit the current listing switches instead."
;  (interactive "P")
;  (if arg
;      (dired-sort-other
;       (read-string "ls switches (must contain -l): " dired-actual-switches))
;    (dired-sort-toggle)))

-- 
Thanks,

v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
      __   __      _____     _______		 Levent N. Atasoy
     / |  /  |    / ___ \   / _____/            Product Management
    /  | / | |    \ \ /_/  / /               Levent.Atasoy@MacSch.com
   / / |/ /| |  _  \ \    / /       	  MacNeal-Schwendler Corporation
  / /| | / | | / /__\ |  / /_____         815 Colorado Blvd LA, CA 90041
 /_/ |__/  |_| \_____/  /_______/      Tel: 213-259-3824  Fax: 213-259-4999

v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
