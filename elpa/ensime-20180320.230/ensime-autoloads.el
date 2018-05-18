;;; ensime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ensime" "../../../../.emacs.d/elpa/ensime-20180320.230/ensime.el"
;;;;;;  "0d1f56c042cf90a7f6d9db5e82bad312")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ensime-20180320.230/ensime.el

(autoload 'ensime "ensime" "\
Read config file for settings then start an ensime-server and connect.

\(fn)" t nil)

(autoload 'ensime-remote "ensime" "\
Read config file for settings. Then connect to an existing ENSIME server.

\(fn HOST PORT)" t nil)

;;;***

;;;### (autoloads nil "ensime-auto-complete" "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-auto-complete.el"
;;;;;;  "64ba44d8cc3244aa60714363bf97b7a3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ensime-20180320.230/ensime-auto-complete.el

(autoload 'ensime-ac-enable "ensime-auto-complete" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ensime-company" "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-company.el"
;;;;;;  "52cfaa51d4720467ffa51714a60165d6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ensime-20180320.230/ensime-company.el

(autoload 'ensime-company-enable "ensime-company" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ensime-ivy" "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-ivy.el"
;;;;;;  "7aa5d1d857e7e60651b87973d97fecbf")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ensime-20180320.230/ensime-ivy.el

(autoload 'ensime-search-ivy "ensime-ivy" "\
Search ensime with ivy.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ensime-mode" "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-mode.el"
;;;;;;  "cfa6d2ca075712bbaa4b06dbf2547650")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ensime-20180320.230/ensime-mode.el

(autoload 'ensime-mode "ensime-mode" "\
ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode).
\\{ensime-mode-map}

\(fn &optional ARG)" t nil)

(add-hook 'scala-mode-hook (lambda nil (when (fboundp 'ensime) (ensime-mode))))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/ensime-20180320.230/ensime-auto-complete.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-client.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-comint-utils.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-company.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-compat.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-completion-util.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-config.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-debug.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-doc.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-editor.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-eldoc.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-expand-region.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-helm.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-http.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-inf.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-ivy.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-macros.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-model.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-notes.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-overlay.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-popup.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-refactor.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-sbt.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-search.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-semantic-highlight.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-stacktrace.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-startup.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-ui.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-undo.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-util.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime-vars.el"
;;;;;;  "../../../../.emacs.d/elpa/ensime-20180320.230/ensime.el")
;;;;;;  (23295 14989 813105 416000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ensime-autoloads.el ends here
