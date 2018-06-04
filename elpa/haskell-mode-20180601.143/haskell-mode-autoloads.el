;;; haskell-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ghc-core" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghc-core.el"
;;;;;;  "96453ab274d3f4542e7ccf9af74bbfc2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghc-core.el

(let ((loads (get 'ghc-core 'custom-loads))) (if (member '"ghc-core" loads) nil (put 'ghc-core 'custom-loads (cons '"ghc-core" loads))))

(autoload 'ghc-core-create-core "ghc-core" "\
Compile and load the current buffer as tidy core.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))

(add-to-list 'auto-mode-alist '("\\.dump-simpl\\'" . ghc-core-mode))

(autoload 'ghc-core-mode "ghc-core" "\
Major mode for GHC Core files.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ghc-core" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghc-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghc-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghc-core" '("ghc-core-")))

;;;***

;;;***

;;;### (autoloads nil "ghci-script-mode" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghci-script-mode.el"
;;;;;;  "88ca63573fc26aea310d7d28a34395d0")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghci-script-mode.el

(autoload 'ghci-script-mode "ghci-script-mode" "\
Major mode for working with .ghci files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))

;;;### (autoloads "actual autoloads are elsewhere" "ghci-script-mode"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghci-script-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghci-script-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghci-script-mode" '("ghci-script-mode-")))

;;;***

;;;***

;;;### (autoloads nil "haskell" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell.el"
;;;;;;  "49600b7e66a7a83285e9352df0e4f58b")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell.el

(autoload 'interactive-haskell-mode "haskell" "\
Minor mode for enabling haskell-process interaction.

\(fn &optional ARG)" t nil)

(autoload 'haskell-interactive-mode-return "haskell" "\
Handle the return key.

\(fn)" t nil)

(autoload 'haskell-session-kill "haskell" "\
Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer unless LEAVE-INTERACTIVE-BUFFER is not given.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list.

\(fn &optional LEAVE-INTERACTIVE-BUFFER)" t nil)

(autoload 'haskell-interactive-kill "haskell" "\
Kill the buffer and (maybe) the session.

\(fn)" t nil)

(autoload 'haskell-session "haskell" "\
Get the Haskell session, prompt if there isn't one or fail.

\(fn)" nil nil)

(autoload 'haskell-interactive-switch "haskell" "\
Switch to the interactive mode for this session.

\(fn)" t nil)

(autoload 'haskell-session-change "haskell" "\
Change the session for the current buffer.

\(fn)" t nil)

(autoload 'haskell-kill-session-process "haskell" "\
Kill the process.

\(fn &optional SESSION)" t nil)

(autoload 'haskell-interactive-mode-visit-error "haskell" "\
Visit the buffer of the current (or last) error message.

\(fn)" t nil)

(autoload 'haskell-mode-jump-to-tag "haskell" "\
Jump to the tag of the given identifier.

Give optional NEXT-P parameter to override value of
`xref-prompt-for-identifier' during definition search.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-after-save-handler "haskell" "\
Function that will be called after buffer's saving.

\(fn)" nil nil)

(autoload 'haskell-mode-tag-find "haskell" "\
The tag find function, specific for the particular session.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-interactive-bring "haskell" "\
Bring up the interactive mode for this session.

\(fn)" t nil)

(autoload 'haskell-process-load-file "haskell" "\
Load the current buffer file.

\(fn)" t nil)

(autoload 'haskell-process-reload "haskell" "\
Re-load the current buffer file.

\(fn)" t nil)

(autoload 'haskell-process-reload-file "haskell" "\


\(fn)" nil nil)

(autoload 'haskell-process-load-or-reload "haskell" "\
Load or reload. Universal argument toggles which.

\(fn &optional TOGGLE)" t nil)

(autoload 'haskell-process-cabal-build "haskell" "\
Build the Cabal project.

\(fn)" t nil)

(autoload 'haskell-process-cabal "haskell" "\
Prompts for a Cabal command to run.

\(fn P)" t nil)

(autoload 'haskell-process-minimal-imports "haskell" "\
Dump minimal imports.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell" '("haskell-" "xref-prompt-for-identifier" "interactive-haskell-mode-map")))

;;;***

;;;***

;;;### (autoloads nil "haskell-align-imports" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-align-imports.el"
;;;;;;  "41ea3dd92f1894369eca73112986e85b")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-align-imports.el

(autoload 'haskell-align-imports "haskell-align-imports" "\
Align all the imports in the buffer.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-align-imports"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-align-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-align-imports.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-align-imports" '("haskell-align-imports-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-c2hs" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-c2hs.el"
;;;;;;  "7b5a3da21c2a5970b9cef4668338dc2f")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-c2hs.el

(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))

(autoload 'haskell-c2hs-mode "haskell-c2hs" "\
Mode for editing *.chs files of the c2hs haskell tool.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-c2hs"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-c2hs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-c2hs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-c2hs" '("haskell-c2hs-font-lock-keywords")))

;;;***

;;;***

;;;### (autoloads nil "haskell-cabal" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-cabal.el"
;;;;;;  "10ebb54c9b822183ace2e81982995238")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

(autoload 'haskell-cabal-get-field "haskell-cabal" "\
Read the value of field with NAME from project's cabal file.
If there is no valid .cabal file to get the setting from (or
there is no corresponding setting with that name in the .cabal
file), then this function returns nil.

\(fn NAME)" t nil)

(autoload 'haskell-cabal-get-dir "haskell-cabal" "\
Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all.

\(fn &optional USE-DEFAULTS)" nil nil)

(autoload 'haskell-cabal-visit-file "haskell-cabal" "\
Locate and visit package description file for file visited by current buffer.
This uses `haskell-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'.

\(fn OTHER-WINDOW)" t nil)

(let ((loads (get 'haskell-cabal 'custom-loads))) (if (member '"haskell-cabal" loads) nil (put 'haskell-cabal 'custom-loads (cons '"haskell-cabal" loads))))

;;;### (autoloads "actual autoloads are elsewhere" "haskell-cabal"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-cabal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-cabal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-cabal" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-collapse" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-collapse.el"
;;;;;;  "dd16002465531f2f1031d4666a8df9d0")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-collapse.el

(autoload 'haskell-collapse-mode "haskell-collapse" "\
Minor mode to collapse and expand haskell expressions

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-collapse"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-collapse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-collapse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-collapse" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-commands" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-commands.el"
;;;;;;  "5e932a5af210240e13182b479e5f4d1b")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-commands.el

(autoload 'haskell-process-restart "haskell-commands" "\
Restart the inferior Haskell process.

\(fn)" t nil)

(autoload 'haskell-process-clear "haskell-commands" "\
Clear the current process.

\(fn)" t nil)

(autoload 'haskell-process-interrupt "haskell-commands" "\
Interrupt the process (SIGINT).

\(fn)" t nil)

(autoload 'haskell-describe "haskell-commands" "\
Describe the given identifier IDENT.

\(fn IDENT)" t nil)

(autoload 'haskell-rgrep "haskell-commands" "\
Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT.

\(fn &optional PROMPT)" t nil)

(autoload 'haskell-process-do-info "haskell-commands" "\
Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer.

\(fn &optional PROMPT-VALUE)" t nil)

(autoload 'haskell-process-do-type "haskell-commands" "\
Print the type of the given expression.

Given INSERT-VALUE prefix indicates that result type signature
should be inserted.

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-mode-jump-to-def-or-tag "haskell-commands" "\
Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-goto-loc "haskell-commands" "\
Go to the location of the thing at point.
Requires the :loc-at command from GHCi.

\(fn)" t nil)

(autoload 'haskell-mode-jump-to-def "haskell-commands" "\
Jump to definition of identifier IDENT at point.

\(fn IDENT)" t nil)

(autoload 'haskell-process-cd "haskell-commands" "\
Change directory.

\(fn &optional NOT-INTERACTIVE)" t nil)

(autoload 'haskell-process-cabal-macros "haskell-commands" "\
Send the cabal macros string.

\(fn)" t nil)

(autoload 'haskell-mode-show-type-at "haskell-commands" "\
Show type of the thing at point or within active region asynchronously.
This function requires GHCi 8+ or GHCi-ng.

\\<haskell-interactive-mode-map>
To make this function works sometimes you need to load the file in REPL
first using command `haskell-process-load-file' bound to
\\[haskell-process-load-file].

Optional argument INSERT-VALUE indicates that
recieved type signature should be inserted (but only if nothing
happened since function invocation).

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-process-unignore "haskell-commands" "\
Unignore any ignored files.
Do not ignore files that were specified as being ignored by the
inferior GHCi process.

\(fn)" t nil)

(autoload 'haskell-session-change-target "haskell-commands" "\
Set the build TARGET for cabal REPL.

\(fn TARGET)" t nil)

(autoload 'haskell-mode-stylish-buffer "haskell-commands" "\
Apply stylish-haskell to the current buffer.

Use `haskell-mode-stylish-haskell-path' to know where to find
stylish-haskell executable. This function tries to preserve
cursor position and markers by using
`haskell-mode-buffer-apply-command'.

\(fn)" t nil)

(autoload 'haskell-mode-find-uses "haskell-commands" "\
Find use cases of the identifier at point and highlight them all.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-commands"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-commands.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-commands" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-compile" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-compile.el"
;;;;;;  "fffc3fa91f12bed7fadfe7f560380594")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-compile.el

(let ((loads (get 'haskell-compile 'custom-loads))) (if (member '"haskell-compile" loads) nil (put 'haskell-compile 'custom-loads (cons '"haskell-compile" loads))))

(autoload 'haskell-compile "haskell-compile" "\
Compile the Haskell program including the current buffer.
Tries to locate the next cabal description in current or parent
folders via `haskell-cabal-find-dir' and if found, invoke
`haskell-compile-cabal-build-command' from the cabal package root
folder. If no cabal package could be detected,
`haskell-compile-command' is used instead.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), `haskell-compile' prompts for custom compile
command.

If EDIT-COMMAND contains the negative prefix argument `-',
`haskell-compile' calls the alternative command defined in
`haskell-compile-cabal-build-alt-command' if a cabal package was
detected.

`haskell-compile' uses `haskell-compilation-mode' which is
derived from `compilation-mode'. See Info
node `(haskell-mode)compilation' for more details.

\(fn &optional EDIT-COMMAND)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-compile"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-compile" '("haskell-compil")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-complete-module"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-complete-module.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-complete-module.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-complete-module" '("haskell-complete-module")))

;;;***

;;;### (autoloads nil "haskell-completions" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-completions.el"
;;;;;;  "b2bd16516d919c57816aeca6fd741b3d")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-completions.el

(let ((loads (get 'haskell-completions 'custom-loads))) (if (member '"haskell-completions" loads) nil (put 'haskell-completions 'custom-loads (cons '"haskell-completions" loads))))

(autoload 'haskell-completions-completion-at-point "haskell-completions" "\
Provide completion list for thing at point.
This function is used in non-interactive `haskell-mode'.  It
provides completions for haskell keywords, language pragmas,
GHC's options, and language extensions, but not identifiers.

\(fn)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-completions"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-completions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-completions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-completions" '("haskell-completions-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-customize" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-customize.el"
;;;;;;  "4605942be881cf51580fb67653f82bf8")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-customize.el

(let ((loads (get 'haskell 'custom-loads))) (if (member '"haskell-customize" loads) nil (put 'haskell 'custom-loads (cons '"haskell-customize" loads))))

(let ((loads (get 'haskell-interactive 'custom-loads))) (if (member '"haskell-customize" loads) nil (put 'haskell-interactive 'custom-loads (cons '"haskell-customize" loads))))

;;;### (autoloads "actual autoloads are elsewhere" "haskell-customize"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-customize.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-customize.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-customize" '("haskell-" "inferior-haskell-root-dir")))

;;;***

;;;***

;;;### (autoloads nil "haskell-debug" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-debug.el"
;;;;;;  "5e7e2a82e90df86e3bf5a9bf79e197b3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-debug.el

(let ((loads (get 'haskell-debug 'custom-loads))) (if (member '"haskell-debug" loads) nil (put 'haskell-debug 'custom-loads (cons '"haskell-debug" loads))))

(defface haskell-debug-warning-face '((t :inherit 'compilation-warning)) "\
Face for warnings." :group (quote haskell-debug))

(defface haskell-debug-trace-number-face '((t :weight bold :background "#f5f5f5")) "\
Face for numbers in backtrace." :group (quote haskell-debug))

(defface haskell-debug-newline-face '((t :weight bold :background "#f0f0f0")) "\
Face for newlines in trace steps." :group (quote haskell-debug))

(defface haskell-debug-keybinding-face '((t :inherit 'font-lock-type-face :weight bold)) "\
Face for keybindings." :group (quote haskell-debug))

(defface haskell-debug-heading-face '((t :inherit 'font-lock-keyword-face)) "\
Face for headings." :group (quote haskell-debug))

(defface haskell-debug-muted-face '((t :foreground "#999")) "\
Face for muteds." :group (quote haskell-debug))

;;;### (autoloads "actual autoloads are elsewhere" "haskell-debug"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-debug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-debug" '("haskell-debug")))

;;;***

;;;***

;;;### (autoloads nil "haskell-decl-scan" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-decl-scan.el"
;;;;;;  "27d2cd4b03954c149b0b886e4eed797b")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-decl-scan.el

(let ((loads (get 'haskell-decl-scan 'custom-loads))) (if (member '"haskell-decl-scan" loads) nil (put 'haskell-decl-scan 'custom-loads (cons '"haskell-decl-scan" loads))))

(autoload 'haskell-ds-create-imenu-index "haskell-decl-scan" "\
Function for finding `imenu' declarations in Haskell mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Haskell file for the `imenu' package.

\(fn)" nil nil)

(autoload 'turn-on-haskell-decl-scan "haskell-decl-scan" "\
Unconditionally activate `haskell-decl-scan-mode'.

\(fn)" t nil)

(autoload 'haskell-decl-scan-mode "haskell-decl-scan" "\
Toggle Haskell declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(haskell-mode)haskell-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`haskell-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `haskell-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (set automatically by `literate-haskell-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook' on activation.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-decl-scan"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-decl-scan.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-decl-scan.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-decl-scan" '("haskell-d" "literate-haskell-ds-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-doc" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-doc.el"
;;;;;;  "e6e2c60ff2954bbbf0ddbf0afe859786")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-doc.el

(let ((loads (get 'haskell-doc 'custom-loads))) (if (member '"haskell-doc" loads) nil (put 'haskell-doc 'custom-loads (cons '"haskell-doc" loads))))

(autoload 'haskell-doc-mode "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(defalias 'turn-on-haskell-doc 'haskell-doc-mode)

(autoload 'haskell-doc-current-info "haskell-doc" "\
Return the info about symbol at point.
Meant for `eldoc-documentation-function'.

\(fn)" nil nil)

(autoload 'haskell-doc-show-type "haskell-doc" "\
Show the type of the function near point or given symbol SYM.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-doc"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-doc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-doc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-doc" '("inferior-haskell-" "haskell-" "turn-off-haskell-doc")))

;;;***

;;;***

;;;### (autoloads nil "haskell-font-lock" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-font-lock.el"
;;;;;;  "6219052dd203b5c3d0672c2979b81104")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-font-lock.el

(let ((loads (get 'haskell-appearance 'custom-loads))) (if (member '"haskell-font-lock" loads) nil (put 'haskell-appearance 'custom-loads (cons '"haskell-font-lock" loads))))

(defface haskell-keyword-face '((t :inherit font-lock-keyword-face)) "\
Face used to highlight Haskell keywords." :group (quote haskell-appearance))

(defface haskell-type-face '((t :inherit font-lock-type-face)) "\
Face used to highlight Haskell types" :group (quote haskell-appearance))

(defface haskell-constructor-face '((t :inherit font-lock-type-face)) "\
Face used to highlight Haskell constructors." :group (quote haskell-appearance))

(defface haskell-operator-face '((t :inherit font-lock-variable-name-face)) "\
Face used to highlight Haskell operators." :group (quote haskell-appearance))

(defface haskell-pragma-face '((t :inherit font-lock-preprocessor-face)) "\
Face used to highlight Haskell pragmas ({-# ... #-})." :group (quote haskell-appearance))

(defface haskell-liquid-haskell-annotation-face '((t :inherit haskell-pragma-face)) "\
Face used to highlight LiquidHaskell annotations ({-@ ... @-})." :group (quote haskell-appearance))

(defface haskell-literate-comment-face '((t :inherit font-lock-doc-face)) "\
Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them." :group (quote haskell-appearance))

;;;### (autoloads "actual autoloads are elsewhere" "haskell-font-lock"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-font-lock" '("haskell-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-ghc-support"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-ghc-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-ghc-support.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-ghc-support" '("haskell-")))

;;;***

;;;### (autoloads nil "haskell-hoogle" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-hoogle.el"
;;;;;;  "842b2f03fa41bcc9c70689b3aa97a7e3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-hoogle.el

(autoload 'haskell-hoogle "haskell-hoogle" "\
Do a Hoogle search for QUERY.
When `haskell-hoogle-command' is non-nil, this command runs
that.  Otherwise, it opens a hoogle search result in the browser.

If prefix argument INFO is given, then `haskell-hoogle-command'
is asked to show extra info for the items matching QUERY..

\(fn QUERY &optional INFO)" t nil)

(defalias 'hoogle 'haskell-hoogle)

(autoload 'haskell-hoogle-lookup-from-local "haskell-hoogle" "\
Lookup by local hoogle.

\(fn)" t nil)

(autoload 'haskell-hayoo "haskell-hoogle" "\
Do a Hayoo search for QUERY.

\(fn QUERY)" t nil)

(defalias 'hayoo 'haskell-hayoo)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-hoogle"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-hoogle.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-hoogle.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-hoogle" '("haskell-h")))

;;;***

;;;***

;;;### (autoloads nil "haskell-indent" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indent.el"
;;;;;;  "00fe30bc09bb187a29fc47ba2b3f14ea")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indent.el

(let ((loads (get 'haskell-indent 'custom-loads))) (if (member '"haskell-indent" loads) nil (put 'haskell-indent 'custom-loads (cons '"haskell-indent" loads))))

(autoload 'turn-on-haskell-indent "haskell-indent" "\
Turn on ``intelligent'' Haskell indentation mode.

\(fn)" nil nil)

(autoload 'haskell-indent-mode "haskell-indent" "\
``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[haskell-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

If `ARG' is falsey, toggle `haskell-indent-mode'.  Else sets
`haskell-indent-mode' to whether `ARG' is greater than 0.

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-indent"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-indent" '("turn-off-haskell-indent" "haskell-indent-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-indentation" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indentation.el"
;;;;;;  "4ee7e075c86fcaee7acac0e655488519")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indentation.el

(let ((loads (get 'haskell-indentation 'custom-loads))) (if (member '"haskell-indentation" loads) nil (put 'haskell-indentation 'custom-loads (cons '"haskell-indentation" loads))))

(autoload 'haskell-indentation-mode "haskell-indentation" "\
Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-indentation "haskell-indentation" "\
Turn on the haskell-indentation minor mode.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-indentation"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indentation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-indentation" '("haskell-indentation-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-interactive-mode" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-interactive-mode.el"
;;;;;;  "89b680e837e5af7a8689e245ff8dec3e")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-interactive-mode.el

(defface haskell-interactive-face-prompt '((t :inherit font-lock-function-name-face)) "\
Face for the prompt." :group (quote haskell-interactive))

(defface haskell-interactive-face-prompt2 '((t :inherit font-lock-keyword-face)) "\
Face for the prompt2 in multi-line mode." :group (quote haskell-interactive))

(defface haskell-interactive-face-compile-error '((t :inherit compilation-error)) "\
Face for compile errors." :group (quote haskell-interactive))

(defface haskell-interactive-face-compile-warning '((t :inherit compilation-warning)) "\
Face for compiler warnings." :group (quote haskell-interactive))

(defface haskell-interactive-face-result '((t :inherit font-lock-string-face)) "\
Face for the result." :group (quote haskell-interactive))

(defface haskell-interactive-face-garbage '((t :inherit font-lock-string-face)) "\
Face for trailing garbage after a command has completed." :group (quote haskell-interactive))

(autoload 'haskell-interactive-mode-reset-error "haskell-interactive-mode" "\
Reset the error cursor position.

\(fn SESSION)" t nil)

(autoload 'haskell-interactive-mode-echo "haskell-interactive-mode" "\
Echo a read only piece of text before the prompt.

\(fn SESSION MESSAGE &optional MODE)" nil nil)

(autoload 'haskell-process-show-repl-response "haskell-interactive-mode" "\
Send LINE to the GHCi process and echo the result in some fashion.
Result will be printed in the minibuffer or presented using
function `haskell-presentation-present', depending on variable
`haskell-process-use-presentation-mode'.

\(fn LINE)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-interactive-mode"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-interactive-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-interactive-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-interactive-mode" '("haskell-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-lexeme"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-lexeme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-lexeme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-lexeme" '("haskell-lexeme-")))

;;;***

;;;### (autoloads nil "haskell-load" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-load.el"
;;;;;;  "94d3761bb87624db220d6ad83ef10d98")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-load.el

(defface haskell-error-face '((((supports :underline (:style wave))) :underline (:style wave :color "#dc322f")) (t :inherit error)) "\
Face used for marking error lines." :group (quote haskell-mode))

(defface haskell-warning-face '((((supports :underline (:style wave))) :underline (:style wave :color "#b58900")) (t :inherit warning)) "\
Face used for marking warning lines." :group (quote haskell-mode))

(defface haskell-hole-face '((((supports :underline (:style wave))) :underline (:style wave :color "#6c71c4")) (t :inherit warning)) "\
Face used for marking hole lines." :group (quote haskell-mode))

(autoload 'haskell-process-reload-devel-main "haskell-load" "\
Reload the module `DevelMain' and then run `DevelMain.update'.

This is for doing live update of the code of servers or GUI
applications.  Put your development version of the program in
`DevelMain', and define `update' to auto-start the program on a
new thread, and use the `foreign-store' package to access the
running context across :load/:reloads in GHCi.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-load"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-load.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-load.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-load" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-menu" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-menu.el"
;;;;;;  "67651925ac4864cd2fe92cc3bee301ea")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-menu.el

(autoload 'haskell-menu "haskell-menu" "\
Launch the Haskell sessions menu.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-menu"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-menu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-menu" '("haskell-menu-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-mode" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode.el"
;;;;;;  "c71c7e3604a247957804e7b5af273af8")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode.el

(autoload 'haskell-version "haskell-mode" "\
Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point.

\(fn &optional HERE)" t nil)

(autoload 'haskell-mode-view-news "haskell-mode" "\
Display information on recent changes to haskell-mode.

\(fn)" t nil)

(autoload 'haskell-mode "haskell-mode" "\
Major mode for editing Haskell programs.

\\<haskell-mode-map>

Literate Haskell scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook'.

Indentation modes:

    `haskell-indentation-mode', Kristof Bastiaensen, Gergely Risko
      Intelligent semi-automatic indentation Mk2

    `haskell-indent-mode', Guy Lapalme
      Intelligent semi-automatic indentation.

Interaction modes:

    `interactive-haskell-mode'
      Interact with per-project GHCi processes through a REPL and
      directory-aware sessions.

Other modes:

    `haskell-decl-scan-mode', Graeme E Moss
      Scans top-level declarations, and places them in a menu.

    `haskell-doc-mode', Hans-Wolfgang Loidl
      Echoes types of functions or syntax of keywords when the cursor is idle.

To activate a minor-mode, simply run the interactive command. For
example, `M-x haskell-doc-mode'. Run it again to disable it.

To enable a mode for every haskell-mode buffer, add a hook in
your Emacs configuration. To do that you can customize
`haskell-mode-hook' or add lines to your .emacs file. For
example, to enable `interactive-haskell-mode', use the following:

    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

Minor modes that work well with `haskell-mode':

- `smerge-mode': show and work with diff3 conflict markers used
  by git, svn and other version control systems.

\(fn)" t nil)

(autoload 'haskell-forward-sexp "haskell-mode" "\
Haskell specific version of `forward-sexp'.

Move forward across one balanced expression (sexp).  With ARG, do
it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment.

If unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (a parenthesis or
list marker of some kind), and end of the obstacle.

\(fn &optional ARG)" t nil)

(autoload 'literate-haskell-mode "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[gh]s\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hsig\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(add-to-list 'completion-ignored-extensions ".hi")

(autoload 'haskell-mode-generate-tags "haskell-mode" "\
Generate tags using Hasktags.  This is synchronous function.

If optional AND-THEN-FIND-THIS-TAG argument is present it is used
with function `xref-find-definitions' after new table was
generated.

\(fn &optional AND-THEN-FIND-THIS-TAG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-mode"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-mode" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-modules" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-modules.el"
;;;;;;  "63ddc21c2c3189347004c6b96b3dd370")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-modules.el

(autoload 'haskell-session-installed-modules "haskell-modules" "\
Get the modules installed in the current package set.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-all-modules "haskell-modules" "\
Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-project-modules "haskell-modules" "\
Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-modules"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-modules.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-modules.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-modules" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-move-nested" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-move-nested.el"
;;;;;;  "9ec4ee2be8031978b7ddc195047aef02")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-move-nested.el

(autoload 'haskell-move-nested "haskell-move-nested" "\
Shift the nested off-side-rule block adjacent to point by COLS columns to the right.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" nil nil)

(autoload 'haskell-move-nested-right "haskell-move-nested" "\
Increase indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(autoload 'haskell-move-nested-left "haskell-move-nested" "\
Decrease indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-move-nested"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-move-nested.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-move-nested.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-move-nested" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-navigate-imports" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-navigate-imports.el"
;;;;;;  "a0eee690f9c980af33f7072c145b63b0")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-navigate-imports.el

(autoload 'haskell-navigate-imports "haskell-navigate-imports" "\
Cycle the Haskell import lines or return to point (with prefix arg).

\(fn &optional RETURN)" t nil)

(autoload 'haskell-navigate-imports-go "haskell-navigate-imports" "\
Go to the first line of a list of consecutive import lines. Cycles.

\(fn)" t nil)

(autoload 'haskell-navigate-imports-return "haskell-navigate-imports" "\
Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-navigate-imports"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-navigate-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-navigate-imports.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-navigate-imports" '("haskell-navigate-imports-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-presentation-mode"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-presentation-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-presentation-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-presentation-mode" '("haskell-presentation-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-process"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-process.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-process" '("haskell-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-repl"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-repl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-repl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-repl" '("haskell-interactive-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-sandbox"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sandbox.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sandbox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-sandbox" '("haskell-sandbox-")))

;;;***

;;;### (autoloads nil "haskell-session" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-session.el"
;;;;;;  "bbc262bc6d91a5c6a7298802b4ff9097")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-session.el

(autoload 'haskell-session-maybe "haskell-session" "\
Maybe get the Haskell session, return nil if there isn't one.

\(fn)" nil nil)

(autoload 'haskell-session-process "haskell-session" "\
Get the session process.

\(fn S)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-session"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-session.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-session.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-session" '("haskell-session")))

;;;***

;;;***

;;;### (autoloads nil "haskell-sort-imports" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sort-imports.el"
;;;;;;  "20b46a912d2c2ed84b9add1e4bc3a7b7")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sort-imports.el

(autoload 'haskell-sort-imports "haskell-sort-imports" "\
Sort the import list at point. It sorts the current group
i.e. an import list separated by blank lines on either side.

If the region is active, it will restrict the imports to sort
within that region.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-sort-imports"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sort-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sort-imports.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-sort-imports" '("haskell-sort-imports-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-string"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-string.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-string.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-string" '("haskell-")))

;;;***

;;;### (autoloads nil "haskell-unicode-input-method" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-unicode-input-method.el"
;;;;;;  "5a1c3cff53191554949331bb16c5be75")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-unicode-input-method.el

(autoload 'turn-on-haskell-unicode-input-method "haskell-unicode-input-method" "\
Set input method `haskell-unicode'.

\(fn)" t nil)

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-utils"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-utils" '("haskell-")))

;;;***

;;;### (autoloads nil "highlight-uses-mode" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/highlight-uses-mode.el"
;;;;;;  "55334f1b8202e898a89d0be733b16421")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/highlight-uses-mode.el

(autoload 'highlight-uses-mode "highlight-uses-mode" "\
Minor mode for highlighting and jumping between uses.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "highlight-uses-mode"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/highlight-uses-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/highlight-uses-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-uses-mode" '("highlight-uses-mode-")))

;;;***

;;;***

;;;### (autoloads nil "inf-haskell" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/inf-haskell.el"
;;;;;;  "2473c5b532bf3f6c3444974a6bb5ee3f")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/inf-haskell.el

(let ((loads (get 'inferior-haskell 'custom-loads))) (if (member '"inf-haskell" loads) nil (put 'inferior-haskell 'custom-loads (cons '"inf-haskell" loads))))

(defalias 'run-haskell 'switch-to-haskell)

(autoload 'switch-to-haskell "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "inf-haskell"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/inf-haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/inf-haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inf-haskell" '("haskell-" "inf")))

;;;***

;;;***

;;;### (autoloads nil "w3m-haddock" "../../../../.emacs.d/elpa/haskell-mode-20180601.143/w3m-haddock.el"
;;;;;;  "bbc2bc102e680b5991d7d47073b71965")
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/w3m-haddock.el

(defface w3m-haddock-heading-face '((((class color)) :inherit highlight)) "\
Face for quarantines." :group (quote haskell))

;;;### (autoloads "actual autoloads are elsewhere" "w3m-haddock"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/w3m-haddock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/haskell-mode-20180601.143/w3m-haddock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-haddock" '("w3m-haddock-" "haskell-w3m-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghc-core.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/ghci-script-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-align-imports.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-c2hs.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-cabal.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-collapse.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-commands.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-compat.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-compile.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-complete-module.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-completions.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-customize.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-debug.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-decl-scan.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-doc.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-font-lock.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-ghc-support.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-hoogle.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indent.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-indentation.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-interactive-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-lexeme.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-load.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-menu.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-modules.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-move-nested.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-navigate-imports.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-presentation-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-process.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-repl.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sandbox.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-session.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-sort-imports.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-string.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-unicode-input-method.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell-utils.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/haskell.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/highlight-uses-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/inf-haskell.el"
;;;;;;  "../../../../.emacs.d/elpa/haskell-mode-20180601.143/w3m-haddock.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haskell-mode-autoloads.el ends here
