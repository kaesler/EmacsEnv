;; Fix this
;;
(setq gud-dbx-directories '("."))
(defun gud-dbx-file-name (f)
  "Transform a relative file name to an absolute file name, for dbx."
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((ff (file-name-nondirectory f)))
        
        (let ((directories gud-dbx-directories))
          (while directories
            (let ((path (concat (car directories) "/" ff)))
              (if (file-exists-p path)
                  (setq result (expand-file-name path)
                        directories nil)))
            (setq directories (cdr directories))))))
    result))
