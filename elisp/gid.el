;;;; gid.el -- run gid using compilation mode.

(require 'compile)
;;(require 'elisp-utils)
(provide 'gid)

(defvar gid-command "gid" "The command run by the gid function.")

(defun gid (args)
  "Run gid, with user-specified ARGS, and collect output in a buffer.
While gid runs asynchronously, you can use the \\[next-error] command to
find the text that gid hits refer to. The command actually run is
defined by the gid-command variable."
  (interactive (list (read-input
     (concat "Run " gid-command " (with args): ") (word-around-point))))
    ;; Preserve the present compile-command
  (let (compile-command
	(compilation-buffer-name-function
	 (lambda (mode) (concat "*gid " args "*"))))
    ;; For portability between v18 & v19, use compile rather than compile-internal
    (compile (concat gid-command " " args))))

(defun word-around-point ()
  "Return the word around the point as a string."
  (save-excursion
    (if (not (eobp))
	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (forward-sexp -1)
    (let ((beg (point)))
      (forward-sexp 1)
      (buffer-substring beg (point)))))

