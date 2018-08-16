;;; bash-completion-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bash-completion" "bash-completion.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from bash-completion.el

(autoload 'bash-completion-setup "bash-completion" "\
Register bash completion for the shell buffer and shell command line.

This function adds `bash-completion-dynamic-complete' to the completion
function list of shell mode, `shell-dynamic-complete-functions'.

This function is convenient, but it might not be the best way of enabling
bash completion in your .emacs file because it forces you to load the module
before it is needed. For an autoload version, add:

  (autoload 'bash-completion-dynamic-complete \"bash-completion\"
    \"BASH completion hook\")
  (add-hook 'shell-dynamic-complete-functions
  	  'bash-completion-dynamic-complete)

\(fn)" nil nil)

(autoload 'bash-completion-dynamic-complete "bash-completion" "\
Return the completion table for bash command at point.

This function is meant to be added into
`shell-dynamic-complete-functions'.  It uses `comint' to figure
out what the current command is and returns a completion table or
nil if no completions available.

When doing completion outside of a comint buffer, call
`bash-completion-dynamic-complete-nocomint' instead.

\(fn)" nil nil)

(autoload 'bash-completion-dynamic-complete-nocomint "bash-completion" "\
Return completion information for bash command at an arbitrary position.

The bash command to be completed begins at COMP-START in the
current buffer. COMP-POS is the point where completion should
happen.

This function is meant to be usable even in non comint buffers.
It is meant to be called directly from any completion engine.

Returns (list stub-start stub-end completions) with
 - stub-start, the position at which the completed region starts
 - stub-end, the position at which the completed region ends
 - completions, a possibly empty list of completion candidates
   or a function, if DYNAMIC-TABLE is non-nil, a lambda such as the one
   returned by `completion-table-dynamic'

\(fn COMP-START COMP-POS &optional DYNAMIC-TABLE)" nil nil)

(autoload 'bash-completion-reset "bash-completion" "\
Force the next completion command to start with a fresh BASH process.

This function kills any existing BASH completion process. This
way, the next time BASH completion is requested, a new process
will be created with the latest configuration. The BASH
completion process that will be killed depends on the
default-directory of the buffer where the command is executed.

Call this method if you have updated your .bashrc or any bash init scripts
and would like bash completion in Emacs to take these changes into account.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bash-completion" '("bash-completion-" "completion")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bash-completion-autoloads.el ends here
