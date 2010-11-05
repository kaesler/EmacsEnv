;;!emacs
;;
;; FILE:         hhist.el
;; SUMMARY:      Maintains history of Hyperbole buttons selected.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:    24-Apr-91 at 03:36:23
;; LAST-MOD:     14-Apr-95 at 16:02:05 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   This is minimal right now and will be extended.
;;   Currently, it implements a push-pop stack of traversed locations.
;;
;;   It will be extended to allow random access to previous locations
;;   and to store traversal histories for later recall.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hhist:add (elt)
  "Adds ELT to hyper-history list if not the same as current or previous loc.
ELT must have been created via a call to 'hhist:element'."
  ;; Even though this next line looks useless, it cures a problem with
  ;; window buffer correspondences on startup, so don't remove it.
  (set-buffer (window-buffer (selected-window)))
  (let ((prev-buf (car elt)))
    (if (or (equal prev-buf (buffer-name))
	    (equal prev-buf (car (car *hhist*))))
	nil
      (setq *hhist* (cons elt *hhist*)))))

(defun hhist:element ()
  "Returns a history element for current point location."
  (list (current-buffer) (point)))

(defun hhist:remove (&optional arg)
  "Removes optional prefix ARG entries from history, returns to ARGth location.
The command is ignored with ARG < 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((prev-buf-line))
    (if (null *hhist*)
	(and (> arg 0)
	     (message "(hhist:remove): No previous source to which to return.")
	     (beep))
      (while (and (> arg 0) *hhist*)
	(setq prev-buf-line (car *hhist*)
	      *hhist* (cdr *hhist*)
	      arg (1- arg)))
      (switch-to-buffer (car prev-buf-line))
      (goto-char (car (cdr prev-buf-line)))
      )))

(defun hhist:init ()
  "Resets history list."
  (interactive)
  (setq *hhist* nil))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hhist:wind-line ()
  "Returns window relative line number that point is on."
  (max 0 (1- (- (count-lines 1 (1+ (point)))
		(count-lines 1 (window-start))))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst *hhist* nil
  "List of previously visited Hyperbole button source locations.
Car of list is most recent.")

(provide 'hhist)
