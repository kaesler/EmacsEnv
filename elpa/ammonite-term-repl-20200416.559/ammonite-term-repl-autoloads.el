;;; ammonite-term-repl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ammonite-term-repl" "ammonite-term-repl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ammonite-term-repl.el

(autoload 'ammonite-term-repl-send-defun "ammonite-term-repl" "\
Send the definition to the ammonite buffer.

\(fn)" t nil)

(autoload 'ammonite-term-repl-send-region "ammonite-term-repl" "\
Send the region to the ammonite buffer.
Argument START the start region.
Argument END the end region.

\(fn START END)" t nil)

(autoload 'ammonite-term-repl-send-buffer "ammonite-term-repl" "\
Send the buffer to the ammonite buffer.

\(fn)" t nil)

(autoload 'ammonite-term-repl-load-file "ammonite-term-repl" "\
Load a file to the ammonite buffer.
Argument FILE-NAME the file name.

\(fn FILE-NAME)" t nil)

(autoload 'ammonite-term-repl "ammonite-term-repl" "\
Run an Ammonite REPL.

\(fn)" t nil)

(defalias 'run-ammonite 'ammonite-term-repl)

(autoload 'ammonite-term-repl-import-ivy-dependencies-from-sbt "ammonite-term-repl" "\
Try to import ivy dependencies from sbt file.
Currently only form like
libraryDependencies += \"com.typesafe.akka\" %% \"akka-actor\" % \"2.5.21\"
is available.

\(fn)" t nil)

(autoload 'ammonite-term-repl-minor-mode "ammonite-term-repl" "\
Minor mode for interacting with an Ammonite REPL.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ammonite-term-repl" '("ammonite-term-repl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ammonite-term-repl-autoloads.el ends here
