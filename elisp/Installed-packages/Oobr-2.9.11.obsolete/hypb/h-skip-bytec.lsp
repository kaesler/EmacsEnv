;;!emacs
;;
;; FILE:         h-skip-bytec.lsp
;; SUMMARY:      Functions that should not be byte-compiled.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PPG
;;
;; ORIG-DATE:     8-Oct-92 at 17:17:10
;; LAST-MOD:      9-May-95 at 16:18:42 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1992-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   DON'T byte-compile this file or its functions may not work.
;;   If we knew why they won't work, they wouldn't be in here.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; For some reason, using this in byte-compiled form causes first character
;;; after mouse key depress to be dropped from input queue when running
;;; Emacs under X.  The non-byte-compiled form works fine.

(defun hmouse-set-point (args)
  "Sets point to Smart Key press/release location given by ARGS.
Returns argument list including x and y frame coordinates in characters and
lines."
  (and (car args) (listp (car args)) (setq args (car args)))
  (if (not hyperb:window-system)
      (point-marker)
    (let ((point-args (hmouse-set-point-at args)))
      (cond (hyperb:xemacs-p
	     (if (eventp current-mouse-event)
		 (copy-event current-mouse-event)))
	    (hyperb:lemacs-p
	     (cond ((and (fboundp 'mouse-position)
			 ;; mouse-position returns nil coords when not over
			 ;; existing text within a window, so we can only use
			 ;; its coordinates when non-nil.  It returns a cons
			 ;; of (device X . Y) in chars.  We drop the device
			 ;; and assume the selected frame.
			 (car (cdr (setq point-args (mouse-position)))))
		    (cdr point-args))
		   ((and (fboundp 'read-mouse-position)
			 ;; read-mouse-position returns nil coords when not
			 ;; over existing text within a window, so we can
			 ;; only use its coordinates when non-nil.  It
			 ;; returns a cons of (X . Y) in chars.
			 (car (setq point-args (read-mouse-position
						(selected-frame)))))
		    point-args)
		   (t
		    ;; We just compute X and Y from event's location.
		    (cons (event-x current-mouse-event)
			  (event-y current-mouse-event)))))
	    (hyperb:epoch-p
	      ;; Modeline clicks return nil for point position so we
	      ;; must compute it instead of using the arguments given.
	      (let ((x-char (/ (* mouse::x (window-width))
			       (window-pixwidth)))
		    (y-char (/ (* mouse::y (window-height))
			       (window-pixheight))))
		(apply 'list x-char y-char args)))
	    ((or (equal hyperb:window-system "next")
		 (equal hyperb:window-system "sun"))
	     (let ((win (car args)))
	       (list win
		     (+ (nth 1 args) (nth 0 (window-edges win)))
		     (+ (nth 2 args) (nth 1 (window-edges win))))))
	    ((equal hyperb:window-system "apollo") point-args)
	    (t args)))))

(provide 'h-skip-bytec)
