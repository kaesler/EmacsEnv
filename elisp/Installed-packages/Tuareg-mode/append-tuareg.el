;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; append-tuareg.el - Tuareg quick installation: Append this file to .emacs.

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(if (and (boundp 'window-system) window-system)
    (if (string-match "XEmacs" emacs-version)
	(require 'sym-lock)
      (require 'font-lock)))
