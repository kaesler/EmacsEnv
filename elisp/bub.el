(require 'comint)

(defcustom bub-executable "bub" "Where is bub?")

(defun bub-convert-option-label-to-string (label)
  (cond
   ((keywordp label) (substring (symbol-name label) 1))
   ((symbolp label) (symbol-name label))
   (t label)))

(defun bub-convert-options-to-arguments (options)
  (when (car options)
    (cons (concat "--" (bub-convert-option-label-to-string (car options)) "=" (cadr options))
          (bub-convert-options-to-arguments (cddr options)))))

(defvar bub-history nil)
(defun bub (command &rest options)
  (interactive (list (read-string "bub " nil 'bub-history)))
  (let* ((args (append (split-string command) (bub-convert-options-to-arguments options)))
         (name (concat bub-executable " " (mapconcat 'identity args " ")))
         (buffer-name (concat "*" name "*"))
         (buffer (get-buffer-create buffer-name)))
    (apply 'make-comint-in-buffer name buffer bub-executable nil args)
    (switch-to-buffer buffer)))

(defvar bub-api-ping-env nil)
(defvar bub-api-ping-stage nil)
(defun bub-api-ping (env stage)
  (interactive (list (read-string "env: " nil 'bub-api-ping-env)
                     (read-string "stage: " nil 'bub-api-ping-stage)))
  (bub "api ping" :env env :stage stage))

(provide 'bub)
