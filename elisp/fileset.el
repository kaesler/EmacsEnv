;;; fileset.el --- Mode for manipulating sets of files

;;{{{ Introduction

;;}}}

;;{{{ Version info

(defconst fset-version-stamp "ClearCase-version: </main/1>")
(defconst fset-version "0.1")
(defconst fset-maintainer-address "esler@rational.com")
(defun fset-submit-bug-report ()
  "Submit via mail a bug report on Fileset Mode"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on Fileset Mode ? ")
       (reporter-submit-bug-report
        fset-maintainer-address
        (concat "fset.el " fset-version))))

;;}}}

;;{{{ Require calls

;;}}}

;;{{{ Debugging facilities

;; Setting this to true will enable some debug code.
;;
(defvar fset-debug t)
(defmacro fset-when-debugging (&rest forms)
  (list 'if 'fset-debug (cons 'progn forms)))

(defun fset-trace (string)
  (fset-when-debugging
   (let ((trace-buf (get-buffer "*fset-trace*")))
     (if trace-buf
         (save-excursion
           (set-buffer trace-buf)
           (goto-char (point-max))
           (insert string "\n"))))))

;;}}}

;;{{{ Global constants and variables

(defconst fset-suffix ".fls"
  "Suffix for fileset-mode files")

(defvar fset-predicate-list nil
  "List of available predicates of type FILENAME--> BOOLEAN.
Each is of the form [title function]")

(defvar fset-thunk-list nil
  "List of available thunks of type FILENAME-->().
Each is of the form [title function]")

(defvar fset-transform-list nil
  "List of available pathname transforms of type FILENAME-->LIST-OF-FILENAME.
Each is of the form [title function]")

(defvar fset-set-thunk-list nil
  "List of available functions of type LIST-OF-FILENAME-->().
Each is of the form [title function]")

(defvar fset-set-transform-list nil
  "List of available functions of type LIST-OF-FILENAME-->LIST-OF-FILENAME.
Each is of the form [title function]")

(defvar fset-set-binary-list nil
  "List of available functions of type LIST-OF-FILENAME x LIST-OF-FILENAME x -->LIST-OF-FILENAME.
Each is of the form [title function]")


;;}}}

;;{{{ Customizable variables

;;}}}

;;{{{ Major Mode

;;}}}

;;{{{ Commands

;;}}}

;;{{{ Functions

;;}}}

;;{{{ Menus

;;}}}

;;{{{ Integration with Emacs

;;}}}

(provide 'fset)

;;; fset.el ends here

;; Local variables:
;; folded-file: t
;; ccase-version-stamp-active: t
;; End: