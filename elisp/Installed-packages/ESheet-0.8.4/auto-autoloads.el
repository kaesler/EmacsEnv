(autoload 'esheet-mode "esheet.el" "makes emacs act like a spreadsheet" t)
(autoload 'csv-mode "esheet.el" "makes emacs act like a spreadsheet" t)
(setq auto-mode-alist (cons (cons "\\.esh\\'" 'esheet-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.csv\\'" 'csv-mode) auto-mode-alist))

