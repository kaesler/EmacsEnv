;; Simple Emacs server. It polls a request queue on disk reading requests
;; consisting of files of ELisp. It executes each request
;;

(defvar kserver-queue-dir "~/apps/emacs/kserver/queue")

(defvar kserver-queue-lock (concat kserver-queue-dir "/lock"))

(defvar kserver-timer nil)

(defun kserver-start ()
  (interactive)

  ;; Stop any existing timer.
  ;;
  (if kserver-timer
      (kserver-stop))

  ;; Start new timer
  ;;
  (setq kserver-timer
        (run-with-idle-timer 2 2 (function kserver-poll))))

(defun kserver-stop ()
  (interactive)
  (if kserver-timer
      (cancel-timer kserver-timer))
  (setq kserver-timer nil))

(defun kserver-poll ()
  (message "Polling...")
  ;; Read all requests from the queue and execute them
  ;;
  (if (kserver-acquire-queue-lock)
      (unwind-protect
          (let ((files (directory-files kserver-queue-dir)))
            (while files
              (let ((file (concat kserver-queue-dir "/" (car files))))
                ;; NYI: only fetch the .el files
                ;; NYI: sort by ctime
                (if (and (not (file-directory-p file))
                         (not (string= "lock" (file-name-nondirectory file))))
                    (kserver-execute-request-file (car files)))
                (setq files (cdr files)))))
        ;; Unwind
        (kserver-release-queue-lock))))

(defun kserver-acquire-queue-lock ()
  (let ((unique-file nil))
    ;; Only temporarily rebind the temp file dir
    ;;
    (let ((temporary-file-directory (file-name-as-directory kserver-queue-dir)))
      (setq unique-file (make-temp-file "lock")))
    (let ((was-acquired nil))
      (condition-case error-description
          (progn
            ;; This is atomic.
            ;;
            (rename-file unique-file kserver-queue-lock)
            (setq was-acquired t))
        (file-already-exists
         ;; NYI: steal stale locks
         (setq was-acquired nil)
         (delete-file unique-file)))
      ;; NYI: write info into lock file
      was-acquired)))

(defun kserver-release-queue-lock ()
  ;; nyi: check current process owns the file
  (condition-case error-desc
      (delete-file kserver-queue-lock)
    (error nil)))

(defun kserver-execute-request-file (file)
  (save-excursion
    (condition-case error-desc
        (progn
          (find-file file)
          (eval-buffer)
          (delete-file file)
          (kill-buffer))
      (error nil))))

