(defmacro with-output-to-string (&rest body)
  (` (save-excursion
       (set-buffer (get-buffer-create " *string-output*"))
       (setq buffer-read-only nil)
       (buffer-disable-undo (current-buffer))
       (erase-buffer)
       (let ((standard-output (current-buffer)))
	 (,@ body))
       (buffer-string))))
       
(defun exec-to-string (command)
  (with-output-to-string
    (call-process shell-file-name nil t nil "-c" command)))

;; Eg:  54999cfe.799c11cd.b38c.00:01:80:31:7a:a7
;;
(defconst oid-regexp
  (let ((hex "[0-9a-f]"))
    (concat hex hex hex hex hex hex hex hex 
            "\\."
            hex hex hex hex hex hex hex hex 
            "\\."
            hex hex hex hex
            "\\."
            hex hex
            ":"
            hex hex
            ":"
            hex hex
            ":"
            hex hex
            ":"
            hex hex
            ":"
            hex hex)))

(defun current-oid ()
  (interactive)
  (save-excursion
    (let ((oid nil))
      (while (and (null oid)
                  (not (eolp)))
        (if (looking-at oid-regexp)
            (setq oid (buffer-substring (match-beginning 0) (match-end 0)))
          (backward-char 1)))
      oid)))
    
(defun oid-at-cursor-to-date ()
  (interactive)
  (let ((oid (current-oid)))
    (if oid
        (message "%s"
                 (substring
                  (exec-to-string (concat "/usr/local/bin/oid_to_date"
                                          " "
                                          oid))
                  0 24)))))

    