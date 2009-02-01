;;!emacs
;;
;; FILE:         br-init.el
;; SUMMARY:      OO-Browser per Emacs session initialization.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    18-May-90
;; LAST-MOD:      1-Nov-95 at 20:22:46 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Personal Variable Defaults
;;; ************************************************************************

;; >>> If you wish to edit classes displayed by the browser in an editor
;;     other than Emacs, set the 'br-editor-cmd' variable to the command you
;;     want to edit with.  Arguments to the command should be placed in
;;     'br-ed[1-9], with one string argument per variable'.  Keep in
;;     mind that the command must generate a new window under your
;;     window system.  For example, to run vi under X, one needs to use the
;;     command line "xterm -e vi", the settings would then be:
;;
;;         (setq br-editor-cmd "xterm" br-ed1 "-e" 
;;               br-ed2 "vi")
;;
;;     This editor will only be used when the browser is run under a window
;;     system external to Emacs, like X.  (In such a case, the variable
;;     'hyperb:window-system' will be non-nil).
;;
;;
(defvar br-editor-cmd nil
  "When non-nil, the OO-Browser uses a non-standard command for editing files.
This may be either a string to invoke an external program or an Emacs
Lisp function which takes a single file argument.")

(setq br-ed1 nil br-ed2 nil br-ed3 nil br-ed4 nil br-ed5 nil
	br-ed6 nil br-ed7 nil br-ed8 nil br-ed9 nil)

;;
;; >>> If you want to view classes in a read-only fashion outside of Emacs,
;;     set the following 'br-viewer-cmd' and 'br-vw[1-9]' variables in a
;;     similar manner as you did for the editor variables above.
;;
;;     For example, to use "xmore", an X-compatible version of more, as your
;;     viewer, use the following settings:
;;
;;         (setq br-viewer-cmd "xmore")
;;
(defvar br-viewer-cmd nil
  "When non-nil, the OO-Browser uses a non-standard command for viewing files.
This may be either a string to invoke an external program or an Emacs
Lisp function which takes a single file argument.")

(setq br-vw1 nil br-vw2 nil br-vw3 nil br-vw4 nil br-vw5 nil
	br-vw6 nil br-vw7 nil br-vw8 nil br-vw9 nil)

;;
;;
(defvar br-skip-dir-regexps
  (if (eq system-type 'next-mach)
      ;; .E is an Eiffel system directory
      '("^RCS$" "^SCCS$" "\\.lproj$" "^obj$" "\\.E$")
    '("^RCS$" "^SCCS$" "\\.E$"))
  "*List of regexps matching subdirectories OO-Browser will not descend when scanning files.")


;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(or (fboundp 'frame-width)  (fset 'frame-width 'screen-width))
(or (fboundp 'frame-height) (fset 'frame-width 'screen-height))
(or (fboundp 'selected-frame)
    (cond ((fboundp 'selected-screen)
	   (fset 'selected-frame 'selected-screen))
	  ((fboundp 'epoch::current-screen)
	   (fset 'selected-frame 'epoch::current-screen))
	  (t
	    (defun selected-frame ()
	      "Always return t since current frame is always selected."
	      t))))

(defun br-after-term-init ()
  (let ((hypb (expand-file-name "hypb/" br-directory)))
    (or (featurep 'hyperbole)
	(br-member hypb load-path)
	;; br-site.el should have already loaded "hversion.el".
	(load "hyperbole" t t)
	;; Use Hyperbole mouse and keyboard handlers included with the
	;; OO-Browser since Hyperbole is not available on this system.
	(progn (or (br-member hypb load-path)
		   (setq load-path (cons hypb load-path)))
	       ;;
	       ;; Necessary to prevent action-key and assist-key from
	       ;; trying to load this Hyperbole library.
	       (provide 'hsite)
	       ;;
	       (defun hkey-either (arg)
		 "Executes `action-key' or with non-nil ARG executes `assist-key'."
		 (interactive "P")
		 (if arg (assist-key) (action-key)))
	       ;;
	       ;; A value of t for 'hkey-init' below will cause the
	       ;; Smart Keys to be bound to keyboard keys in addition to any
	       ;; mouse key bindings.  Comment it out or set it to nil if you
	       ;; don't want these bindings.  Or change the bindings in the
	       ;; succeeding lines.
	       (or (boundp 'hkey-init) (setq hkey-init t))
	       (and hkey-init
		    (not (global-key-binding "\M-\C-m"))
		    (global-set-key "\M-\C-m" 'hkey-either))
	       ;;
	       ;; Bind a key, {C-h A}, for Action Key help and {C-u C-h A}
	       ;; for Assist key help.
	       (and hkey-init
		    (not (where-is-internal 'hkey-help))
		    (define-key help-map "A" 'hkey-help))
	       
	       ;; Setup Action and Assist keys to perform only
	       ;; browser-related actions.
	       (require 'hui-mouse)
	       (setq hkey-alist
		 '(
		   ((and (not (eobp))
			 (or (eolp) (and selective-display
					 (= (following-char) ?\^M)))) .
					 ((smart-scroll-up) .
					  (smart-scroll-down)))
		   ;;
		   ((br-in-browser) .
		    ((smart-br-dispatch) . (smart-br-assist-dispatch)))
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
		   (buffer-read-only .
                    ((scroll-up) . (scroll-down)))
		   )))))
  ;;
  (if (stringp br-editor-cmd)
      (let ((br-editor-cmd (downcase br-editor-cmd)))
	(and (string-match "emacs" br-editor-cmd)
	     (setq br-editor-cmd nil))))
  (if (stringp br-viewer-cmd)
      (let ((br-viewer-cmd (downcase br-viewer-cmd)))
	(and (string-match "emacs" br-viewer-cmd)
	     (setq br-viewer-cmd nil))))
  ;;
  (require 'br)
  (require 'hmouse-br)
  (require 'hmouse-drv)
  ;;
  ;; Loads menus under non-tty InfoDock, XEmacs or Emacs19; does nothing
  ;; otherwise.
  (and (not (featurep 'br-menu)) hyperb:window-system
       (or hyperb:lemacs-p hyperb:emacs19-p) (require 'br-menu))
  ;;
  ;; Adds or replaces class entry in an Environment
  (global-set-key "\C-c^" 'br-add-class-file)
  ;;
  ;; Goes to and from class viewer window
  (global-set-key "\C-c\C-v" 'br-to-from-viewer)
  ;;
  (br-init-autoloads)
  ;;
  (if (boundp 'hmouse-bindings)
      ;; Mouse support has been initialized, possibly by Hyperbole.
      nil
    (require 'hmouse-key)
    ;; See the documentation for this function for instructions on how to
    ;; setup shifted or unshifted Action and Assist mouse buttons.
    (hmouse-shift-buttons)))

(defun br-init-autoloads ()
  "Setup OO-Browser autoloaded functions."
  (autoload 'br-add-class-file "br" "Add file to OO-Browser Environment" t) 
  (autoload 'br-env-browse "br" "Browse an existing OO-Browser Environment" t)
  (autoload 'br-env-load  "br" "Load a new OO-Browser Environment" t)
  (autoload 'br-to-from-viewer  "br" "Move between list and viewer windows" t)
  ;;
  (autoload 'hmail:compose      "hmail"
    "Compose mail with ADDRESS and evaluation of EXPR." t)
  (autoload 'hypb:configuration "hypb"
    "Insert Emacs configuration information into OUT-BUF or current buffer.")
  ;;
  (autoload 'c++-browse  "c++-browse" "C++ OO-Browser" t)
  (autoload 'clos-browse "clos-brows" "Common Lisp OO-Browser" t)
  (autoload 'eif-browse  "eif-browse" "Eiffel OO-Browser" t)
  (autoload 'info-browse "info-brows" "Info OO-Browser" t)
  (autoload 'java-browse "java-brows" "Java OO-Browser" t)
  (autoload 'objc-browse "objc-brows" "Objective-C OO-Browser" t)
  (autoload 'smt-browse  "smt-browse" "Smalltalk OO-Browser" t)
  )

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(provide 'br-init)
