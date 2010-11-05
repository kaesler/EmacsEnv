;;!emacs
;;
;; FILE:         br-menu.el
;; SUMMARY:      Pulldown and popup menus for the OO-Browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    27-Oct-93 at 21:13:36
;; LAST-MOD:     22-Oct-95 at 00:41:44 by Bob Weiner
;;
;; Copyright (C) 1994, 1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; This definition is used by InfoDock only.
(defconst id-menubar-br
  '(
    ("OO-Browser"
     ["Copyright"           br-copyright                   t]
     ["Help-Commands"       br-help                        t]
     ["Help-Mode"           describe-mode                  t]
     ["Help-Mouse"          br-help-ms                     t]
     ["Manual"              (id-info "(oo-browser.info)Top") t]
     ["Version"             br-version                     t]
     "----"
     ["Compose-Mail-to-List"
      (progn (br-quit)
	     (mail nil "oo-browser@hub.ucsb.edu"
		   "Replace this line with a descriptive sentence.")
	     (goto-char (point-min))
	     (search-forward "Subject: " nil t)) t]
     ["Mail-List-Request"
      (progn (br-quit)
	     (mail nil "oo-browser-request@hub.ucsb.edu")
	     (insert "Use a subject line like one of the following:\n"
		     "  Subject: Subscribe <joe@any.com> (Joe Williams).\n"
		     "  Subject: Unsubscribe <joe@any.com>.\n")
	     (goto-char (point-min))
	     (search-forward "Subject: " nil t)) t]
     "----"
     ["Reinitialize"        br-refresh                     t]
     ["Exit-Temporarily"    (id-tool-quit '(br-quit))    t]
     ["Quit"                (id-tool-quit '(br-quit t))  t]
     )
    ("Class"
     ["Edit-Current"        br-edit-entry                  t]
     ["Edit-Named"          br-find                        t]
     ["View-Current"        br-view-entry                  t]
     "----"
     ["Match-from-Listing"  br-match                       t]
     ["Where-is-Any"        (br-where t)                   t]
     ["Where-is-Current"    br-where                       t]
     "----"
     ["Graphical-Descendants"  br-tree                     t]
     "----"
     ["Ancestors"           (br-ancestors -1)              t]
     ["Children"            br-children                    t]
     ["Descendants"         br-descendants                 t]
     ["Features"            br-features                    t]
     ["Info"                br-class-info                  t]
     ["Level"               br-at                          t]
     ["Parents"             br-parents                     t]
     ["Class-Statistics"    br-class-stats                 t]
     )
    ("Environment"
     ["Create"              (id-tool-invoke id-tool-oo-browser) t]
     ["Load"                (id-tool-invoke id-tool-oo-browser) t]
     ["Rebuild"             br-env-rebuild                 t]
     ["Rebuild-Lib-Part"    br-lib-rebuild                 t]
     ["Rebuild-Sys-Part"    br-sys-rebuild                 t]
     ["Save"                br-env-save                    t]
     ["Statistics"          br-env-stats                   t]
     "----"
     ["Add-File"            br-add-class-file              t]
     ["Delete-Class"        br-delete                      t]
     )
    ("Feature"
     ["Edit-Current"        br-edit-entry                  t]
     ["Edit-Named"          br-find                        t]
     ["View-Current"        br-view-entry                  t]
     ["View-Friend-Def"     br-view-friend                 t]
     "----"
     ["Implementors"        br-implementors                t]
     ["Signature"           br-ftr-signature               t]
     )
    ("List-Window"
     ["Graphical-View"      br-tree-graph                  t]
     ["Graphical-Descendants"  (br-tree t)                 t]
     ["Kill-Graphical-Views" br-tree-kill                  t]
     "----"
     ["Write (Save as)"     br-write-buffer                t]
     "----"
     ["Count-Entries"       br-count                       t]
     ["Order-Entries"       (progn (br-order 1)
				   (br-unique))            t]
     "----"
     ["Ancestors"           (br-ancestors -2)              t]
     ["Children"            (br-children t)                t]
     ["Descendants"         (br-descendants t)             t]
     ["Features"            (br-features 1)                t]
     ["Files"               br-buffer-menu                 t]
     ["Level"               (br-at t)                      t]
     ["Parents"             (br-parents t)                 t]
     "----"
     ["Show-All-Classes"      (br-top-classes t)           t]
     ["Show-All-Lib-Classes"  (br-lib-top-classes t)       t]
     ["Show-All-Sys-Classes"  (br-sys-top-classes t)       t]
     ["Show-Top-Classes"      br-top-classes               t]
     ["Show-Top-Lib-Classes"  br-lib-top-classes           t]
     ["Show-Top-Sys-Classes"  br-sys-top-classes           t]
     "----"
     ["Exit-This-Level"       br-exit-level                t]
     ["Narrow-by-10"          br-resize-narrow             t]
     ["Widen-by-10"           br-resize-widen              t]
     )
    ("Options"
     ["Keep-Viewed-Classes" br-toggle-keep-viewed
      :style toggle :selected br-keep-viewed-classes]
     ["Graphical-Show-Features" br-tree-features-toggle
      :style toggle :selected br-show-features]
     )
    ("View-Window"
     ["Full-Frame"          br-view-full-frame             t]
     ["Kill-Buffer"         br-kill                        t]
     ["Move-To-or-From"     br-to-from-viewer              t]
     )
    ))

;;; This definition is used by InfoDock and XEmacs.
(defconst id-popup-br-menu
  '("OO-Browser"
    ["Copyright"           br-copyright                   t]
    ["Help-Commands"       br-help                        t]
    ["Help-Mode"           describe-mode                  t]
    ["Help-Mouse"          br-help-ms                     t]
    ["Manual"              (id-info "(oo-browser.info)Top") t]
    ["Version"             br-version                     t]
    "----"
    ["Compose-Mail-to-List"
     (progn (br-quit)
	    (mail nil "oo-browser@hub.ucsb.edu"
		  "Replace this line with a descriptive sentence.")
	    (goto-char (point-min))
	    (search-forward "Subject: " nil t)) t]
    ["Mail-List-Request"
     (progn (br-quit)
	    (mail nil "oo-browser-request@hub.ucsb.edu")
	    (insert "Use a subject line like one of the following:\n"
		    "  Subject: Subscribe <joe@any.com> (Joe Williams).\n"
		    "  Subject: Unsubscribe <joe@any.com>.\n")
	    (goto-char (point-min))
	    (search-forward "Subject: " nil t)) t]
     "----"
    ("Class"
     ["Edit-Current"        br-edit-entry                  t]
     ["Edit-Named"          br-find                        t]
     ["View-Current"        br-view-entry                  t]
     "----"
     ["Match-from-Listing"  br-match                       t]
     ["Where-is-Any"        (br-where t)                   t]
     ["Where-is-Current"    br-where                       t]
     "----"
     ["Graphical-View"      br-tree-graph                  t]
     ["Graphical-Descendants"  (br-tree t)                 t]
     ["Kill-Graphical-Views" br-tree-kill                  t]
     "----"
     ["Ancestors"           (br-ancestors -1)              t]
     ["Children"            br-children                    t]
     ["Descendants"         br-descendants                 t]
     ["Features"            br-features                    t]
     ["Info"                br-class-info                  t]
     ["Level"               br-at                          t]
     ["Parents"             br-parents                     t]
     ["Class-Statistics"    br-class-stats                 t]
     )
    ("Environment"
     ["Create"              (id-tool-invoke id-tool-oo-browser) t]
     ["Load"                (id-tool-invoke id-tool-oo-browser) t]
     ["Rebuild"             br-env-rebuild                 t]
     ["Rebuild-Lib-Part"    br-lib-rebuild                 t]
     ["Rebuild-Sys-Part"    br-sys-rebuild                 t]
     ["Save"                br-env-save                    t]
     ["Statistics"          br-env-stats                   t]
     "----"
     ["Add-File"            br-add-class-file              t]
     ["Delete-Class"        br-delete                      t]
     )
    ("Feature"
     ["Edit-Current"        br-edit-entry                  t]
     ["Edit-Named"          br-find                        t]
     ["View-Current"        br-view-entry                  t]
     ["View-Friend-Def"     br-view-friend                 t]
     "----"
     ["Implementors"        br-implementors                t]
     ["Signature"           br-feature-signature           t]
     )
    ("List-Window"
     ["Write (Save as)"     br-write-buffer                t]
     "----"
     ["Count-Entries"       br-count                       t]
     ["Order-Entries"       (progn (br-order 1)
				   (br-unique))            t]
     "----"
     ["Ancestors"           (br-ancestors -2)              t]
     ["Children"            (br-children t)                t]
     ["Descendants"         (br-descendants t)             t]
     ["Features"            (br-features 1)                t]
     ["Files"               br-buffer-menu                 t]
     ["Level"               (br-at t)                      t]
     ["Parents"             (br-parents t)                 t]
     "----"
     ["Show-All-Classes"      (br-top-classes t)           t]
     ["Show-All-Lib-Classes"  (br-lib-top-classes t)       t]
     ["Show-All-Sys-Classes"  (br-sys-top-classes t)       t]
     ["Show-Top-Classes"      br-top-classes               t]
     ["Show-Top-Lib-Classes"  br-lib-top-classes           t]
     ["Show-Top-Sys-Classes"  br-sys-top-classes           t]
     "----"
     ["Exit-This-Level"       br-exit-level                t]
     ["Narrow-by-10"          br-resize-narrow             t]
     ["Widen-by-10"           br-resize-widen              t]
     )
    ("Options"
     ["Keep-Viewed-Classes" br-toggle-keep-viewed
      :style toggle :selected br-keep-viewed-classes]
     ["Graphical-Show-Features" br-tree-features-toggle
      :style toggle :selected br-show-features]
     )
    ("View-Window"
     ["Full-Frame"          br-view-full-frame             t]
     ["Kill-Buffer"         br-kill                        t]
     ["Move-To-or-From"     br-to-from-viewer              t]
     )
    "----"
    ["Reinitialize"        br-refresh                     t]
    ["Exit-Temporarily"    (id-tool-quit '(br-quit))    t]
    ["Quit"                (id-tool-quit '(br-quit t))  t]
    ))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; This definition is used only by XEmacs and Emacs19.
(defun br-menubar-menu ()
  "Add an OO-Browser menu to the menubar for each listing buffer."
  (cond ((fboundp 'popup-mode-menu)
	 (setq mode-popup-menu id-popup-br-menu))
	(hyperb:lemacs-p
	 (define-key br-mode-map 'button3 'br-popup-menu))
	(t ;; hyperb:emacs19-p
	 (define-key br-mode-map [down-mouse-3] 'br-popup-menu)
	 (define-key br-mode-map [mouse-3] nil)))
  (if (and (boundp 'current-menubar)
	   (or hyperb:emacs19-p current-menubar)
	   (not (car (find-menu-item current-menubar '("OO-Browser")))))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(if (fboundp 'add-submenu)
	    (add-submenu nil id-popup-br-menu)
	  (add-menu nil (car id-popup-br-menu) (cdr id-popup-br-menu))))))

;;; This definition is used only by XEmacs and Emacs19.
(defun br-popup-menu (event)
  "Popup the OO-Browser listing buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (if (fboundp 'popup-mode-menu)
      (popup-mode-menu)
    (popup-menu id-popup-br-menu)))

(cond ((null hyperb:window-system))
      ((fboundp 'id-menubar-set)
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'br-mode 'id-menubar-br))
      (hyperb:lemacs-p
       ;; XEmacs under a window system
       (add-hook 'br-mode-hook 'br-menubar-menu))
      (hyperb:emacs19-p
       ;; Emacs 19 under a window system
       (require 'lmenu)
       (add-hook 'br-mode-hook 'br-menubar-menu)))

(provide 'br-menu)
