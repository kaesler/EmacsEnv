;;; idris-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "idris-commands" "idris-commands.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from idris-commands.el

(register-definition-prefixes "idris-commands" '("def-region-" "idris-" "proof-region-"))

;;;***

;;;### (autoloads nil "idris-common-utils" "idris-common-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from idris-common-utils.el

(register-definition-prefixes "idris-common-utils" '(">=-protocol-version" "destructure-case" "idris-"))

;;;***

;;;### (autoloads nil "idris-core" "idris-core.el" (0 0 0 0))
;;; Generated autoloads from idris-core.el

(register-definition-prefixes "idris-core" '("idris-is-ident-char-p"))

;;;***

;;;### (autoloads nil "idris-events" "idris-events.el" (0 0 0 0))
;;; Generated autoloads from idris-events.el

(register-definition-prefixes "idris-events" '("idris-"))

;;;***

;;;### (autoloads nil "idris-highlight-input" "idris-highlight-input.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from idris-highlight-input.el

(register-definition-prefixes "idris-highlight-input" '("idris-highlight-"))

;;;***

;;;### (autoloads nil "idris-hole-list" "idris-hole-list.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from idris-hole-list.el

(register-definition-prefixes "idris-hole-list" '("idris-"))

;;;***

;;;### (autoloads nil "idris-info" "idris-info.el" (0 0 0 0))
;;; Generated autoloads from idris-info.el

(register-definition-prefixes "idris-info" '("idris-" "with-idris-info-buffer"))

;;;***

;;;### (autoloads nil "idris-ipkg-mode" "idris-ipkg-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from idris-ipkg-mode.el

(autoload 'idris-ipkg-mode "idris-ipkg-mode" "\
Major mode for Idris package files
     \\{idris-ipkg-mode-map}
Invokes `idris-ipkg-mode-hook'.

\(fn)" t nil)

(register-definition-prefixes "idris-ipkg-mode" '("idris-"))

;;;***

;;;### (autoloads nil "idris-keys" "idris-keys.el" (0 0 0 0))
;;; Generated autoloads from idris-keys.el

(register-definition-prefixes "idris-keys" '("idris-define-"))

;;;***

;;;### (autoloads nil "idris-log" "idris-log.el" (0 0 0 0))
;;; Generated autoloads from idris-log.el

(register-definition-prefixes "idris-log" '("idris-"))

;;;***

;;;### (autoloads nil "idris-mode" "idris-mode.el" (0 0 0 0))
;;; Generated autoloads from idris-mode.el

(autoload 'idris-mode "idris-mode" "\
Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'.

\(fn)" t nil)

(push '("\\.idr$" . idris-mode) auto-mode-alist)

(push '("\\.lidr$" . idris-mode) auto-mode-alist)

(register-definition-prefixes "idris-mode" '("idris-mode-"))

;;;***

;;;### (autoloads nil "idris-navigate" "idris-navigate.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from idris-navigate.el

(register-definition-prefixes "idris-navigate" '("empty-line-p-chars" "idris-" "toplevel-nostart-chars"))

;;;***

;;;### (autoloads nil "idris-prover" "idris-prover.el" (0 0 0 0))
;;; Generated autoloads from idris-prover.el

(register-definition-prefixes "idris-prover" '("idris-"))

;;;***

;;;### (autoloads nil "idris-repl" "idris-repl.el" (0 0 0 0))
;;; Generated autoloads from idris-repl.el

(autoload 'idris-repl "idris-repl" nil t nil)

(register-definition-prefixes "idris-repl" '("find-common-prefix" "idris-"))

;;;***

;;;### (autoloads nil "idris-settings" "idris-settings.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from idris-settings.el

(register-definition-prefixes "idris-settings" '("idris-"))

;;;***

;;;### (autoloads nil "idris-simple-indent" "idris-simple-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from idris-simple-indent.el

(autoload 'idris-simple-indent-mode "idris-simple-indent" "\
Simple Idris indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `idris-simple-indent-backtab'
\(bound to <backtab> by default) will move the cursor the
previous indent point. An indent point is a non-whitespace
character following whitespace.

This is a minor mode.  If called interactively, toggle the
`Idris-Simple-Indent mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `idris-simple-indent-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Runs `idris-simple-indent-hook' on activation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-idris-simple-indent "idris-simple-indent" "\
Turn on function `idris-simple-indent-mode'." t nil)

(register-definition-prefixes "idris-simple-indent" '("idris-simple-indent" "turn-off-idris-simple-indent"))

;;;***

;;;### (autoloads nil "idris-syntax" "idris-syntax.el" (0 0 0 0))
;;; Generated autoloads from idris-syntax.el

(register-definition-prefixes "idris-syntax" '("idris-"))

;;;***

;;;### (autoloads nil "idris-test-utils" "idris-test-utils.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from idris-test-utils.el

(register-definition-prefixes "idris-test-utils" '("check-rest" "idris-"))

;;;***

;;;### (autoloads nil "idris-tree-info" "idris-tree-info.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from idris-tree-info.el

(register-definition-prefixes "idris-tree-info" '("idris-tree-info-"))

;;;***

;;;### (autoloads nil "idris-warnings" "idris-warnings.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from idris-warnings.el

(register-definition-prefixes "idris-warnings" '("idris-"))

;;;***

;;;### (autoloads nil "idris-warnings-tree" "idris-warnings-tree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from idris-warnings-tree.el

(register-definition-prefixes "idris-warnings-tree" '("idris-"))

;;;***

;;;### (autoloads nil "inferior-idris" "inferior-idris.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from inferior-idris.el

(register-definition-prefixes "inferior-idris" '("idris-"))

;;;***

;;;### (autoloads nil nil ("idris-compat.el" "idris-mode-pkg.el"
;;;;;;  "idris-tests2.el" "idris-tests3.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; idris-mode-autoloads.el ends here
