(defun kae-space-before-imports ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (if (looking-at "import ")
          (insert "\n"))))
