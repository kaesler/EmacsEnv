;;!emacs
;;
;; FILE:         br-site.el
;; SUMMARY:      Site OO-Browser per Emacs session initialization.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     local, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    18-May-90
;; LAST-MOD:     28-Jul-95 at 15:11:17 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hpath:display-alist
  '(
    ;; Run the OO-Browser on OOBR or OOBR-FTR Environment files.
    ("OOBR\\(-FTR\\)?$" . br-env-browse)
   )
  "*Alist of (FILENAME-REGEXP . EDIT-FUNCTION) elements for calling special
functions to display particular file types within Emacs.  See also
'hpath:find-alist' for external display program settings.")

(defvar hpath:find-alist nil
  "*Alist of (FILENAME-REGEXP . EDIT-PROGRAM) elements for using window system
dependent external programs to edit/display particular file types.  See also
'hpath:display-alist' for internal, window-system independent display
settings.")

(defvar smart-scroll-proportional nil
  "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-setup-external ()
  "Site customizable function to configure the OO-Browser for non-Emacs editing and viewing.
This must be run after \"br-init\" has been loaded."
  (setq br-editor-cmd "xterm"
	br-ed1 "-e" br-ed2 "vi"
	br-viewer-cmd "xterm"
	br-vw1 "-e" br-vw2 "more"))

(defun br-site-after-term-init ()
  (interactive)
  (if noninteractive
      (br-init-autoloads)
    (br-after-term-init))
  ;;
  ;;     DON'T PUT IN br-init.el
  ;;
  (require 'br)
  (if noninteractive
      nil
    (setq c++-cpp-include-dirs '("/usr/include/")
	  c++-include-dirs 
	  (delq nil (mapcar 
		     (function (lambda (dir) (if (file-exists-p dir) dir)))
		     '("/usr/include/X11/" "/usr/openwin/include/X11/"))))))

;; Execute
(br-site-after-term-init)

(if hyperb:window-system (require 'br-tree))

(provide 'br-site)
