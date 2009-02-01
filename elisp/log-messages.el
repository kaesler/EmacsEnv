;; -*-Emacs-Lisp-*-

;;; log-messages.el - facility to log the output of the "message" function.
;;; 
;;; Have you ever been frustrated by not being able to read an emacs
;;; message because it gets overwritten by another message?  Then this
;;; package is for you.  By setting the user variable
;;; "logmsg-log-messages" to non-nil, you can have the messages logged
;;; for you in the buffer "*Message Log*".
;;; 
;;; The variable "logmsg-log-messages" lets you filter out messages you
;;; know you don't want logged.  I'm not sure how useful this really is, but
;;; it's there if you want it.
;;; 
;;; Unfortunately, some messages you see in the minibuffer are not produced
;;; using the "message" function, so they will not be logged.

;;; Copyright (C) 1990,1993 Robert Potter.
;;;
;;; Author: Robert Potter (rpotter@grip.cis.upenn.edu)
;;; Modified by: David Smith (dsmith@stats.adelaide.edu.au)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author or from the Free Software Foundation, Inc., 675 Mass Ave,
;;; Cambridge, MA 02139, USA.

(require 'advice)

;;
;; user variables
;; 

(defvar logmsg-log-messages t
  "*If non-nil, the output of the \"message\" function will be logged
in the buffer \"*Message Log*\".")

(defvar logmsg-nolog-regexps
  '("^Mark set$"
    "^Undo!$"
    "^\\(Failing \\|Wrapped \\)?I-search\\( backward\\)?:"
    "^I-search\\( backward\\)?:"
    "^NNTP: Reading \\."
    "^(No changes need to be saved)$"
    "^<<< Press Space to bury the buffer list >>>$"
    "^fill-prefix: \".*\""
    "^Type .* to remove help window."
    "^Type .* to restore old contents of help window\\.$"
    "^Type .* to continue editing\\.$")
  "*A list of regexps that match messages to leave out of the message log.
For best efficiency, keep them in decreasing order of likelihood.")

(defadvice message (after log-messages activate)
  "Log messages into the *Message Log* buffer.
Messages matching any element of logmsg-nolog-regexps are not logged."
  (let ((msg ad-return-value)
	(rgx logmsg-nolog-regexps))
    (and logmsg-log-messages
	 (> (length msg) 0)
	 (progn
	   (while (and rgx (not (string-match (car rgx) msg)))
	     (setq rgx (cdr rgx)))
	   (not rgx))
	 (save-excursion
	   (set-buffer (get-buffer-create "*Message Log*"))
	   (goto-char (point-max))
	   (insert msg ?\n)))
    msg))
  
(provide 'log-messages)
