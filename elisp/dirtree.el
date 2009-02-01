;; Amazing, I couldn't find a unix program to give me a diagram of a directory
;; tree, with indentation and number/size of files. So, I've put this up in a
;; couple of hours. If I get any interest/feedback/ requests, I could shape it
;; up in a better form.
;; 
;;; dirtree.el - creates a description of a directory tree
;; vladimir@cs.ualberta.ca, 15.02.1996
;; the format is as follows:
;;   dirname files/kbytes (totalfiles/totalKbytes)
;;     file1(kbytes) file2(kbytes) ...
;;     subdir ...

(defvar dirtree-block 1024
  "*Unit for printing the file sizes.")
(defvar dirtree-block-name "Kb"
  "*Printable name of `dirtree-block'.")
(defvar dirtree-files t 
  "*Include files in the listing?")
(defvar dirtree-full-names dirtree-files
  "*Print full dir names? Default is T if `dirtree-files' are included.")
(defvar dirtree-column (if dirtree-full-names 56 40)
  "*Column where the directory sizes are printed.")
(defvar dirtree-threshold 35
  "*File sizes are printed only if they are more than this many blocks.")

(require 'cl)

(defstruct fileinfo
  name bytes)

(defstruct (dirtree (:include fileinfo (bytes 0)))
  (files 0) (totbytes 0) (totfiles 0)
  fileinfos dirtrees)                   ; lists of child files/dirs

(defun dirtree (dir &optional buf)
  "Creates a description of a directory tree DIR in buffer BUF."
  (interactive "DDirectory: \nP")
  (save-excursion
    (if buf (set-buffer buf))
    (open-newline)
    (insert "dirname")
    (indent-to-column dirtree-column 1)
    (insert (format "files/%s (totalFiles/total%s)"
            dirtree-block-name dirtree-block-name))
    (open-newline)
    (if dirtree-files (insert (format "  filename(%s>%d) ..."
                                      dirtree-block-name dirtree-threshold))
      (insert "  subdir")
      (indent-to-column dirtree-column 1)
      (insert (format "files/%s (totalFiles/total%s)"
                      dirtree-block-name dirtree-block-name)))
    (open-newline)
    (insert-char ?- 70)
    (newline)
    (message "Scanning directories...")
    (dirtree-insert (dirtree-get dir)
                    (file-name-nondirectory (directory-file-name dir)))
    (message "Scanning directories...Done")))

(defun dirtree-get (dir)
  "Return struct dirtree about DIR and recursively about its subtree."
  (let ((prev-dir default-directory) self contents file)
    (cd dir)
    (setq self (make-dirtree :name dir)
          contents (directory-files "."))
    (while contents
      (setq file (car contents)
            contents (cdr contents))
      (when (not (or (equal file ".") (equal file "..")))
        (if (file-directory-p file)
            (let ((child (dirtree-get file)))
              (incf (dirtree-totbytes self) (dirtree-totbytes child))
              (incf (dirtree-totfiles self) (dirtree-totfiles child))
              (setf (dirtree-dirtrees self)
                    (cons child (dirtree-dirtrees self))))
          (let ((size (nth 7 (file-attributes file))))
              (incf (dirtree-bytes self) size)
              (incf (dirtree-files self))
              (incf (dirtree-totbytes self) size)
              (incf (dirtree-totfiles self))
              (setf (dirtree-fileinfos self) 
                    (cons (make-fileinfo :name file :bytes size)
                          (dirtree-fileinfos self)))))))
    (setf (dirtree-dirtrees self) (nreverse (dirtree-dirtrees self)))
    (setf (dirtree-fileinfos self) (nreverse (dirtree-fileinfos self)))
    (cd prev-dir)
    self))

(defun dirtree-insert (self full)
  "Print the struct dirtree SELF in the current buffer. FULL is the full name."
  (insert-fill-prefix)
  (insert (if dirtree-full-names full (dirtree-name self)) "/")
  (indent-to-column dirtree-column 1)
  (insert (format "%d/%d" (dirtree-files self)
                  (dirtree-blocks (dirtree-bytes self))))
  (if (dirtree-dirtrees self)           ; there are subdirs
      (insert (format " (%d/%d)" (dirtree-totfiles self)
                      (dirtree-blocks (dirtree-totbytes self)))))
  (newline)
  (let ((prev-fill-prefix fill-prefix))
    (setq fill-prefix (concat "  " fill-prefix))
    (let ((files (and dirtree-files (dirtree-fileinfos self)))
          (dirs (dirtree-dirtrees self)))
      (if files (insert-fill-prefix))
      (while files
        (insert (fileinfo-name (car files)))
        (let ((size (dirtree-blocks (fileinfo-bytes (car files)))))
          (if (> size dirtree-threshold) (insert (format "(%d)" size))))
        (setq files (cdr files))
        (if files (insert " ") (newline)))
      (while dirs
        (dirtree-insert (car dirs)
                        (concat full "/" (dirtree-name (car dirs))))
        (setq dirs (cdr dirs))))
    (setq fill-prefix prev-fill-prefix)))

(defun dirtree-blocks (n)
  (/ (+ n (/ dirtree-block 2)) dirtree-block))

(defun open-newline (&optional arg)
  "Insert a newline and the fill-prefix.  With arg, insert that many newlines.
`newline' does not insert the fill prefix, while `open-line' does not move."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (newline)
    (insert-fill-prefix)
    (setq arg (1- arg))))

(defun insert-fill-prefix ()
  (if fill-prefix (insert fill-prefix)))

(provide 'dirtree)
