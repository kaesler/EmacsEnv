;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-tuareg.el - Tuareg customization example: Append this file to .emacs.

;; Here is an example of Tuareg variables customization:

(add-hook 'tuareg-mode-hook
	  '(lambda ()
	     (setq tuareg-lazy-= t)
					; indent `=' like a standard keyword
	     (setq tuareg-lazy-paren t)
					; indent [({ like standard keywords
	     (setq tuareg-in-indent 0)
					; no indentation after `in' keywords
             (setq tuareg-font-lock-function-name '("brown" "cyan" nil t t t))
					; bold function names in color mode
					; bold-italic in font mode
	     (auto-fill-mode 1)
					; turn on auto-fill minor mode
	     (if (featurep 'sym-lock)   ; Sym-Lock customization only
		 (setq sym-lock-mouse-face-enabled nil))
					; turn off special face under mouse
	     ))

;; If you use Sym-Lock, you could also add some customization code after the
;; `(require 'sym-lock)' in your `.emacs'

(if (featurep 'sym-lock)
    (setq tuareg-sym-lock-keywords
	  '(("<-" 0 1 172) ("->" 0 1 174)
	    ;; (":=" 0 1 220)
	    ("<=" 0 1 163) (">=" 0 1 179)
	    ("<>" 0 1 185) ("==" 0 1 186)
	    ("||" 0 1 218) ("&&" 0 1 217)
	    ("[^*]\\(\\*\\)\\." 1 8 180)
	    ("\\(/\\)\\." 1 3 184)
	    ;; (":>" 0 1 202)
	    ;; (";;" 0 1 191)
	    ("\\<_\\>" 0 3 188) ("\\<sqrt\\>" 0 3 214)
	    ("\\<unit\\>" 0 3 198) ("\\<fun\\>" 0 3 108)
	    ("\\<or\\>" 0 3 218) ("\\<not\\>" 0 3 216))))
