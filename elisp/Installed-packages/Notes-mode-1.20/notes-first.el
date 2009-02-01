
;;;
;;; notes-first.el
;;; $Id: notes-first.el,v 1.1 2000/05/05 21:11:17 johnh Exp $
;;;
;;; Copyright (C) 2000 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License, version 2.
;;;

;;;
(defun notes-first-use-init ()
  "Set up notes mode for the first time for a new user."
  ;; note that we CAN'T assume the contents of notes-variables is loaded.
  (notes-first-start-shell)
  (switch-to-buffer "notes-first-shell" t)
)

(provide 'notes-first)

(defun notes-first-shell-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))))

(defun notes-first-shell-running ()
  (and (get-process "notes-first-shell")
       (eq (process-status (get-process "notes-first-shell")) 'run)))

(defun notes-first-kill-job ()
  "Kill the currently running Notes-First job."
  (interactive)
  (quit-process (get-process "notes-first-shell") t))

;;; notes-first-start-shell based on tex-start-shell
;;;###autoload
(defun notes-first-start-shell ()
  ;; clean up old shell?
  (if (notes-first-shell-running)
      (notes-first-kill-job))
  ;; start it again
  (save-excursion
    (set-buffer
     (make-comint
      "notes-first-shell"
      (concat notes-utility-dir "/notesinit")
      nil))
    (let ((proc (get-process "notes-first-shell")))
      (set-process-sentinel proc 'notes-first-shell-sentinel)
      (process-kill-without-query proc)
      (setq comint-prompt-regexp shell-prompt-pattern)
;      (setq notes-first-shell-map (nconc (make-sparse-keymap) shell-mode-map))
;      (tex-define-common-keys tex-shell-map)
;      (use-local-map tex-shell-map)
      (compilation-shell-minor-mode t)
      (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t)
      (make-local-variable 'list-buffers-directory)
      (make-local-variable 'shell-dirstack)
      (make-local-variable 'shell-last-dir)
      (make-local-variable 'shell-dirtrackp)
;      (run-hooks 'tex-shell-hook)
      (while (zerop (buffer-size))
	(sleep-for 1)))))
