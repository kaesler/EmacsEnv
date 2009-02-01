------- Start of forwarded message -------
From: LEVITTE@e.kth.se (Richard Levitte)
Newsgroups: gnu.emacs.sources
Subject: Re: webster.el refined to use network streams
Date: 01 Apr 1994 10:57:26 GMT
Organization: The Royal Institute of Technology
NNTP-Posting-Host: eliza.e.kth.se
Mime-Version: 1.0
Content-Type: text/plain; charset=iso-8859-1
Content-Transfer-Encoding: 8bit
In-reply-to: LEVITTE@e.kth.se's message of 01 Apr 1994 05:17:20 GMT

Of course, my webster.el had to have bugs.  Here is the corrected version:

;; Copyright (C) 1989 Free Software Foundation

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Author Jason R. Glasgow (glasgow@cs.yale.edu)
;; Modified from telnet.el by William F. Schelter
;; But almost entirely different.
;;
;; Modified by Dirk Grunwald to maintain an open connection.
;;
;; Hacked by Richard Levitte to use network streams instead of
;; a telnet subprocess.
;;
;; 3/18/89 Ashwin Ram <Ram-Ashwin@yale.edu>
;; Added webster-mode.
;; Fixed documentation.
;;
;; 3/20/89 Dirk Grunwald <grunwald@flute.cs.uiuc.edu>
;; Merged Rams changes with new additions: smarter window placement,
;; correctly handles un-exposed webster windows, minor cleanups.
;; Also, ``webster-word'', akin to ``spell-word''.
;;
;; 3/21/89 Dave Sill <dsill@relay.nswc.navy.mil>
;; Removed webster-word and webster-define, adding default of current word to 
;; webster, webster-spell, and webster-endings instead.
;; 12/12/93 Hardy Mayer <mmayer@uci.edu> -- replaced shell by comint for 
;; emacs-19 compatibility
;;
;; 4/1/94 Richard Levitte <levitte@e.kth.se>
;; Replaced the call to a telnet subprocess with network stream commands.
;; To maintain compatibility with previous versions, webster-send-request
;; still accepts webster-port to be a string, by simply coercing it's
;; value to an integer.
;;
;; To use this, you might want to add these lines to your .emacs file:
;;
;;  (autoload 'webster "webster-19" "look up a word in Webster's 9th edition" t)
;;  (setq webster-host "xxx.xxx.xxx.xxx") ; [your local webster-server].
;;  (setq webster-port "103") ;[if 2627 is not your port]
;; Then just hit M-x webster to look up a word.

(defvar webster-host " "
  "Host that is a webster server (use whatever convenient). Try also 26.0.0.73, which is sri-nic")
(defvar webster-port 2627
  "The port to connect to. Either 103 or 2627")

(defvar webster-process nil
  "The current webster process")

(defvar webster-process-name "webster"
  "The current webster process")

(defvar webster-buffer nil
  "The current webster process")

(defvar webster-running nil
  "Used to determine when connection is established")

;;;
;;; Initial filter for ignoring information until successfully connected
;;;
(defun webster-initial-filter (proc string)
  (let
      (( this-buffer (current-buffer)))
    (set-buffer webster-buffer)
    (goto-char (point-max))
    (cond
     ((not (eq (process-status webster-process) 'open))
      (progn
	(setq webster-running t)
	(message "Webster died")))
     
     ((string-match "]" string)
      (progn
	(setq webster-running t)
	(set-process-filter proc 'webster-filter))))
    (set-buffer this-buffer)))

(defun webster-filter (proc string)
  (let
      ((closed-message (string-match "Connection closed" string))
       (end-def-message (or (string-match "\200" string) (string-match "\0" string)))
       ( this-buffer (current-buffer)))

    (set-buffer webster-buffer)
    (cond

     ((not (eq (process-status webster-process) 'open))
      (message "Webster died"))

     ((string-match "SPELLING 0" string)
      (insert-string "...Word not found in webster\n"))
     
     ((string-match "SPELLING 1" string)
      (insert-string "...Spelled correctly\n"))

     (end-def-message
      (webster-filter proc (concat (substring string 0 (- end-def-message 1)) "\n\n"))
      (goto-char (point-max)))

     (t
      (goto-char (point-max))
      (let ((now (point)))
	(insert string)
	(delete-char-in-region now (point) ?\^m ?\ ))
      (if (process-mark proc)
	  (set-marker (process-mark proc) (point)))))

    ;;
    ;; if webster is visible, move the last line to the bottom of that window
    ;;
    (let ((here (selected-window)))
      (let ((webster-window (get-buffer-window webster-buffer)))
	(if (windowp webster-window)
	    (progn
	      (message "")
	      (select-window webster-window)
	      (recenter -1)
	      (select-window here)))))

    (set-buffer this-buffer)))

;;;
;;; delete char1 and char2 if it precedes char1
;;; used to get rid of <space><return>
(defun delete-char-in-region (start end char1 char2)
  (goto-char start)
  (while (search-forward (char-to-string char1) end t)
    (backward-delete-char 1)
    (if (equal (char-after (- (point) 1)) char2)
	(backward-delete-char 1))))

(defun webster (arg)
  "Look up a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat "Look up word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "DEFINE" arg))

(defun webster-endings (arg)
  "Look up endings for a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat
		  "Find endings for word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "ENDINGS" arg))

(defun webster-spell (arg)
  "Look spelling for a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat
		  "Try to spell word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "SPELL" arg))

(defun webster-send-request (kind word)
  (let ((webster-port-int (if (stringp webster-port)
			      (string-to-int webster-port)
			    webster-port)))
    (if (or
	 (not webster-buffer)
	 (not webster-process)
	 (not (eq (process-status webster-process) 'open)))
	(progn
	  (message
	   (concat "Attempting to connect to server " webster-host "..."))
	  (setq webster-buffer
		(get-buffer-create (concat "*webster-" word "*")))
	  (let
	      ((this-buffer (current-buffer)))
	    (set-buffer webster-buffer)
	    (webster-mode)
	    (set-buffer this-buffer))

	  (setq webster-process (open-network-stream "*webster-stream*"
						     webster-buffer
						     webster-host
						     webster-port-int))
	  (set-process-filter webster-process 'webster-filter)
	  (setq webster-running t)	;
	  (message
	   (concat "Attempting to connect to server " webster-host "... done"))
	  ))				;
    (display-buffer webster-buffer nil)
    (process-send-string webster-process (concat kind " " word "\n"))))

(defun webster-quit ()
  "Close connection and quit webster-mode.
Buffer is not deleted."
  (interactive)
  (message "Closing connection to %s..." webster-host)
					;  (kill-process webster-process)
  (delete-process webster-process)
  (message "Closing connection to %s...done" webster-host)
  (bury-buffer))

(defun webster-mode ()
  "Major mode for interacting with on-line Webster's dictionary.
\\{webster-mode-map}
Use webster-mode-hook for customization."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'webster-mode)
  (setq mode-name "Webster")
  (use-local-map webster-mode-map)
  (run-hooks 'webster-mode-hook))

(defvar webster-mode-map nil)
(if webster-mode-map
    nil
  (setq webster-mode-map (make-sparse-keymap))
  (define-key webster-mode-map "?" 'describe-mode)
  (define-key webster-mode-map "d" 'webster)
  (define-key webster-mode-map "e" 'webster-endings)
  (define-key webster-mode-map "q" 'webster-quit)
  (define-key webster-mode-map "s" 'webster-spell))

;; Snatched from unix-apropos by Henry Kautz
(defun current-word ()
  "Word cursor is over, as a string."
  (save-excursion
    (let (beg end)
      (re-search-backward "\\w" nil 2)
      (re-search-backward "\\b" nil 2)
      (setq beg (point))
      (re-search-forward "\\w*\\b" nil 2)
      (setq end (point))
      (buffer-substring beg end))))
--
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
! Richard Levitte, VMS GNU Emacs hacker   ! tel: int+46-8-18 30 99            !
! Sulv"agen 57, II                        ! fax: none for the moment          !
! S-126 40 H"agersten                     ! Internet: levitte@e.kth.se        !
! SWEDEN                                  !                                   !
!-----------------------------------------------------------------------------!
------- End of forwarded message -------
