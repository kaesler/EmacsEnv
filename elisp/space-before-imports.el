(require 'f)

;; Code for ensuring there is a blank line between initial "package" line and first "import" line.

(defun kae-space-before-imports-in-tree (dir)
  (interactive "DDirectory: ")
  (mapcar
   'kae-space-before-imports
   (kae-scala-files-in-tree dir)))

(defun kae-space-before-imports (file)
  "Put a blank line between the initial \"import\" and the first \"package\" line
in FILE"
  (interactive "fFile: ")
  (save-excursion
    (find-file file)
    (kae-space-before-imports-current-buffer)
    (save-buffer)
    // TODO: Keep the buffer if we didn't just create it. See find-buffer-visiting
    (kill-buffer)))

(defun kae-space-before-imports-current-buffer ()
  "Put a blank line between the initial \"import\" and the first \"package\" line
in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (if (looking-at "import ")
        (insert "\n"))))

(defun kae-scala-files-in-tree (dir)
  (f-files
   dir
   (lambda (file) (f-ext? file "scala"))
   t))









