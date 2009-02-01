;;!emacs
;;
;; FILE:         hmouse-sh.el
;; SUMMARY:      System-dependent Smart Mouse Key bindings (using shift key).
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:     3-Sep-91 at 21:40:58
;; LAST-MOD:     14-Sep-95 at 18:39:12 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   See description in "hmouse-key.el".
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-get-bindings ()
  "Returns list of bindings for mouse keys prior to their use as Smart Keys."
  (eval
   (cdr (assoc
	 ;; Get mouse bindings under Emacs 19 or XEmacs, even if not under a
	 ;; window system since it can have frames on ttys and windowed
	 ;; displays at the same time.
	 (or hyperb:window-system
	     (and (not noninteractive) hyperb:xemacs-p "lemacs")
	     (and (not noninteractive) hyperb:emacs19-p "emacs19"))
	 '(("emacs19" .
	    (mapcar (function
		     (lambda (key) (cons key (lookup-key global-map key))))
		    (if (memq window-system '(ns dps))
			;; NEXTSTEP offers only 2 shift-mouse buttons which we use
			;; as the Smart Keys.
			'([S-down-mouse-1] [S-mouse-1] [S-down-mouse-2]
			  [S-mouse-2] [S-double-mouse-1] [S-triple-mouse-1]
			  [S-double-mouse-2] [S-triple-mouse-2]
			  [vertical-line S-down-mouse-1]
			  [vertical-line S-mouse-1]
			  [vertical-line S-down-mouse-2]
			  [vertical-line S-mouse-2]
			  [mode-line S-down-mouse-1] [mode-line S-mouse-1]
			  [mode-line S-down-mouse-2] [mode-line S-mouse-2]
			  )
		      ;; X
		      '([S-down-mouse-2] [S-mouse-2] [S-down-mouse-3]
			[S-mouse-3] [S-double-mouse-2] [S-triple-mouse-2]
			[S-double-mouse-3] [S-triple-mouse-3]
			[vertical-line S-down-mouse-2]
			[vertical-line S-mouse-2]
			[vertical-line S-down-mouse-3]
			[vertical-line S-mouse-3]
			[mode-line S-down-mouse-2] [mode-line S-mouse-2]
			[mode-line S-down-mouse-3] [mode-line S-mouse-3]
			))))
	   ("lemacs" .
	    (nconc
	     (mapcar (function
		      (lambda (key)
			(cons key (lookup-key global-map key))))
		     '([(shift button2)] [(shift button2up)]
		       [(shift button3)] [(shift button3up)]))
	     (if (boundp 'mode-line-map)
		 (mapcar (function
			  (lambda (key)
			    (cons key (lookup-key mode-line-map key))))
			 '([(shift button3)] [(shift button3up)])))))
	   ("xterm" .
	    (mapcar (function
		     (lambda (key) (cons key (lookup-key mouse-map key))))
		    (list x-button-s-middle x-button-s-middle-up
			  x-button-s-right  x-button-s-right-up)))
	   ("epoch" .
	    (mapcar (function
		     (lambda (key) (cons key (aref mouse::global-map key))))
		    (list (mouse::index mouse-middle mouse-shift)
			  (mouse::index mouse-middle mouse-shift-up)
			  (mouse::index mouse-right mouse-shift)
			  (mouse::index mouse-right mouse-shift-up)
			  ;; Modeline mouse map
			  (mouse::index mouse-mode-middle mouse-shift)
			  (mouse::index mouse-mode-middle mouse-shift-up)
			  (mouse::index mouse-mode-right mouse-shift)
			  (mouse::index mouse-mode-right mouse-shift-up)
			  )))
	   ("next" .
	    (mapcar (function
		     (lambda (key)
		       (cons key (mousemap-get
				  (mouse-list-to-mouse-code key)
				  current-global-mousemap))))
		    (apply 'nconc
			   (mapcar (function
				    (lambda (region)
				      (mapcar (function
					       (lambda (key)
						 (cons region key)))
					      '((shift left) (shift up left)
						(shift right)
						(shift up right)
						))))
				   '(text scrollbar modeline minibuffer)))
		    ))
	   ;; SunView
	   ("sun" .
	    (mapcar (function
		     (lambda (key)
		       (setq key (mouse-list-to-mouse-code key))
		       (cons key (mousemap-get
				  key current-global-mousemap))))
		    (apply 'nconc
			   (mapcar (function
				    (lambda (region)
				      (mapcar (function
					       (lambda (key)
						 (cons region key)))
					      '((shift middle)
						(shift up middle)
						(shift right)
						(shift up right)
						))))
				   '(text scrollbar modeline minibuffer)))
		    ))
	   ("apollo" .
	    (mapcar (function
		     (lambda (key-str) (apollo-mouse-key-and-binding
					key-str)))
		    '("M2S" "M2U" "M3S" "M3U")))
	   )))))

(defun hmouse-setup ()
  "Binds mouse keys for use as Smart Keys."
  (interactive)
  (or hmouse-bindings-flag hmouse-previous-bindings
      (setq hmouse-previous-bindings (hmouse-get-bindings)))
  ;; Ensure Gillespie's Info mouse support is off since
  ;; Hyperbole handles that.
  (setq Info-mouse-support nil)
  ;;
  (cond ;; GNU Emacs 19
   ((if (not noninteractive) hyperb:emacs19-p)
    (setq hmouse-set-point-command 'mouse-set-point)
    (if (memq window-system '(ns dps))
	;; NEXTSTEP offers only 2 shift-mouse buttons which we use
	;; as the Smart Keys.
	(progn
	  (global-set-key [S-down-mouse-1]      'action-key-depress-emacs19)
	  (global-set-key [S-mouse-1]           'action-mouse-key-emacs19)
	  (global-set-key [S-double-mouse-1]    'action-mouse-key-emacs19)
	  (global-set-key [S-triple-mouse-1]    'action-mouse-key-emacs19)
	  (global-set-key [S-down-mouse-2]      'assist-key-depress-emacs19)
	  (global-set-key [S-mouse-2]           'assist-mouse-key-emacs19)
	  (global-set-key [S-double-mouse-2]    'assist-mouse-key-emacs19)
	  (global-set-key [S-triple-mouse-2]    'assist-mouse-key-emacs19)
	  (global-set-key [vertical-line S-down-mouse-1] 'action-key-depress-emacs19)
	  (global-set-key [vertical-line S-mouse-1]   'action-mouse-key-emacs19)
	  (global-set-key [vertical-line S-down-mouse-2]
			  'assist-key-depress-emacs19)
	  (global-set-key [vertical-line S-mouse-2]
			  'assist-mouse-key-emacs19)
	  (global-set-key [mode-line S-down-mouse-1] 'action-key-depress-emacs19)
	  (global-set-key [mode-line S-mouse-1]      'action-mouse-key-emacs19)
	  (global-set-key [mode-line S-down-mouse-2] 'assist-key-depress-emacs19)
	  (global-set-key [mode-line S-mouse-2]  'assist-mouse-key-emacs19))
      ;; X
      (global-set-key [S-down-mouse-2]      'action-key-depress-emacs19)
      (global-set-key [S-mouse-2]           'action-mouse-key-emacs19)
      (global-set-key [S-double-mouse-2]    'action-mouse-key-emacs19)
      (global-set-key [S-triple-mouse-2]    'action-mouse-key-emacs19)
      (global-set-key [S-down-mouse-3]      'assist-key-depress-emacs19)
      (global-set-key [S-mouse-3]           'assist-mouse-key-emacs19)
      (global-set-key [S-double-mouse-3]    'assist-mouse-key-emacs19)
      (global-set-key [S-triple-mouse-3]    'assist-mouse-key-emacs19)
      (global-set-key [vertical-line S-down-mouse-2] 'action-key-depress-emacs19)
      (global-set-key [vertical-line S-mouse-2]   'action-mouse-key-emacs19)
      (global-set-key [vertical-line S-down-mouse-3]
		      'assist-key-depress-emacs19)
      (global-set-key [vertical-line S-mouse-3]
		      'assist-mouse-key-emacs19)
      (global-set-key [mode-line S-down-mouse-2] 'action-key-depress-emacs19)
      (global-set-key [mode-line S-mouse-2]      'action-mouse-key-emacs19)
      (global-set-key [mode-line S-down-mouse-3] 'assist-key-depress-emacs19)
      (global-set-key [mode-line S-mouse-3]  'assist-mouse-key-emacs19)))
   ;;
   ;; XEmacs
   ((if (not noninteractive) hyperb:xemacs-p)
    ;; Set mouse bindings under XEmacs, even if not under a window
    ;; system since it can have frames on ttys and windowed displays at
    ;; the same time.
    (setq hmouse-set-point-command 'hmouse-move-point-xemacs)
    (global-set-key '(shift button2)     'action-key-depress)
    (global-set-key '(shift button2up)   'action-mouse-key)
    (if (fboundp 'infodock-set-mouse-bindings)
	(infodock-set-mouse-bindings)
      (if (boundp 'mode-line-map)
	  (progn (define-key mode-line-map '(shift button3)
		   'assist-key-depress)
		 (define-key mode-line-map '(shift button3up)
		   'assist-mouse-key)
		 ))
      (global-set-key '(shift button3)     'assist-key-depress)
      (global-set-key '(shift button3up)   'assist-mouse-key)))
   ;;
   ;; X
   ((equal hyperb:window-system "xterm")
    (setq hmouse-set-point-command 'x-mouse-set-point)
    (define-key mouse-map x-button-s-middle 'action-key-depress)
    (define-key mouse-map x-button-s-middle-up 'action-mouse-key)
    (define-key mouse-map x-button-s-right 'assist-key-depress)
    (define-key mouse-map x-button-s-right-up 'assist-mouse-key)
    ;; Use these instead of the above for a true META-BUTTON binding.
    ;; (define-key mouse-map x-button-m-middle 'assist-key-depress)
    ;; (define-key mouse-map x-button-m-middle-up 'assist-mouse-key)
    )
   ;;
   ;; Epoch
   ((equal hyperb:window-system "epoch")
    (setq hmouse-set-point-command 'mouse::set-point)
    (global-set-mouse mouse-middle mouse-shift  'action-key-depress)
    (global-set-mouse mouse-middle mouse-shift-up    'action-mouse-key)
    (global-set-mouse mouse-right  mouse-shift  'assist-key-depress)
    (global-set-mouse mouse-right  mouse-shift-up  'assist-mouse-key)
    ;; Modeline mouse map
    (global-set-mouse mouse-mode-middle mouse-shift  'action-key-depress)
    (global-set-mouse mouse-mode-middle mouse-shift-up 'action-mouse-key)
    (global-set-mouse mouse-mode-right  mouse-shift  'assist-key-depress)
    (global-set-mouse mouse-mode-right  mouse-shift-up
		      'assist-mouse-key)
    )
   ;;
   ;; NeXT
   ((equal hyperb:window-system "next")
    (setq hmouse-set-point-command 'hmouse-move-point-eterm)
    ;; Use left button to set point.
    ;; Use shift-left button instead of non-existent middle as Action Key.
    (mapcar
     (function
      (lambda (region)
	(global-set-mouse (cons region '(shift left))    'action-key-depress)
	(global-set-mouse (cons region '(shift up left)) 'action-mouse-key)
	(global-set-mouse (cons region '(shift right))   'assist-key-depress)
	(global-set-mouse (cons region '(shift up right))
			  'assist-mouse-key)
	;; Use these instead of the above for a true META-BUTTON binding.
	;; (global-set-mouse (cons region '(meta    right)) 'assist-key-depress)
	;; (global-set-mouse (cons region '(meta up right)) 'assist-mouse-key)
	))
     '(text scrollbar modeline minibuffer))
    )
   ;;
   ;; SunView
   ((equal hyperb:window-system "sun")
    (setq hmouse-set-point-command 'hmouse-move-point-eterm)
    (mapcar
     (function
      (lambda (region)
	(global-set-mouse (cons region '(shift middle))  'action-key-depress)
	(global-set-mouse (cons region '(shift up middle))
			  'action-mouse-key)
	(global-set-mouse (cons region '(shift right))  'assist-key-depress)
	(global-set-mouse (cons region '(shift up right))
			  'assist-mouse-key)
	;; Use these instead of the above for a true META-BUTTON binding.
	;; (global-set-mouse (cons region '(meta    middle)) 'assist-key-depress)
	;; (global-set-mouse (cons region '(meta up middle)) 'assist-mouse-key)
	))
     '(text scrollbar modeline minibuffer))
    )
   ;;
   ;; Apollo DM
   ((equal hyperb:window-system "apollo")
    (setq hmouse-set-point-command 'apollo-mouse-move-point)
    (bind-apollo-mouse-button "M2S" 'action-key-depress)
    (bind-apollo-mouse-button "M2U" 'action-mouse-key)
    (bind-apollo-mouse-button "M3S" 'assist-key-depress)
    (bind-apollo-mouse-button "M3U" 'assist-mouse-key)
    ))
  (setq hmouse-bindings (hmouse-get-bindings)
	hmouse-bindings-flag t))
