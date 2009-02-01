;/============================================================\;
;|               Esheet Module Version 0.8.3                  |;
;|                                                            |;
;| This module acts as a spreadsheet within Xemacs            |;
;| Author and Maintainer:  Daniel Speyer, dspeyer@choate.edu  |;
;|                                                            |;
;| This module is distributed under the terms of the General  |;
;| Public License.  To obtain  a copy of  the license, go to  |;
;| http://www.gnu.org/copyleft/gpl.html                       |;
;\============================================================/;


;;the esheet cursor is displayed in this face
(make-face 'curson)
(set-face-background 'curson "blue")
(set-face-foreground 'curson "white")

(if (not (fboundp 'dolist)) (require 'cl))

(if (equal (substring (version) 0 3) "GNU") 
    (progn
      (defmacro number-char-or-marker-p (&rest args) `(numberp ,@args))
      (copy-face 'region 'primary-selection)))


(load "esh-misc-prop")
(load "esh-cell-range")
(load "esh-refresh-cell")
(load "esh-edit-line")
(load "esh-cursor")
(load "esh-region")
(load "esh-runtime-troubleshooting")
(load "esh-operators")
(load "esh-num-to-string")
(load "esh-statistics")
(load "esh-file")
(load "esh-tree")
(load "esh-infix")
(load "esh-csv")

(if (equal (substring (version) 0 3) "XEm") (progn
					      (if (featurep 'mouse) (load "esh-mouse"))
					      (if (featurep 'xpm) (load "esh-real-toolbar"))))

(autoload 'sort-region "esh-sort")
(autoload 'graph-region "esh-graph")

;;dummy functions for modes

(defun esheet-edit-line-mode ()   
"This mode is used for the edit line in Esheet; never invoke it mannually.

Usually, keybindings are:
enter,tab   finnish editing cell (enter goes down, tab goes right)
C-r         get a cell or range Relative to the current cell
C-a         get a cell or range Absolutle referenced

These will change at times.  For real info, see esheet-mode"
  (interactive)
  (setq major-mode 'esheet-edit-line-mode)
  (setq mode-name "")
  (use-local-map nil)
  (setq truncate-lines nil)
  (local-set-key [return] 'set-cell-value)
  (local-set-key [tab] 'set-cell-value-and-go-right)
  (local-set-key [(meta ?w)] 'message-w)
  (local-set-key [(control ?a)] 'get-absolute-cell-or-region)
  (local-set-key [(control ?r)] 'get-relative-cell-or-region))

(setq esheet-keys '(
                   ([up] . go-up)
                   ([down] . go-down)
                   ([right] . go-right)
                   ([left] . go-left)
                   ([delete] . intel-del)
                   ([backspace] . intel-del) ;observe C-h is unbound
                   ([return] . edit-cell-value)
                   ([?\ ] . edit-cell-value)
                   ([tab] . edit-cell-value)
                   ([(control ?e)] . restore-edit-line)
                   ([(shift up)] . shift-up)
                   ([(shift down)] . shift-down)
                   ([(shift left)] . shift-left)
                   ([(shift right)] . shift-right)
                   ([(control ?w)] . esheet-cut)  ;Thanks to Paul Foley
                   ([(meta ?w)] . esheet-copy)    ;mycroft@actrix.gen.nz
                   ([(control ?y)] . esheet-paste);for these bindings
                   ([(meta ?e)] . esheet-copy-values)
                   ([(control meta ?w)] . esheet-copy-values)
                   ([cut] . esheet-cut)
                   ([copy] . esheet-copy)   ;;for anyone who has these keys
                   ([(shift copy)] . esheet-copy-values)
                   ([paste] . esheet-paste)
                   ([(control c) (control s)] . sort-region)
                   ([(control c) (control g)] . graph-region)
                   ([(control ?x) (control ?s)] . esheet-save)
                   ([(control ?x) (control ?w)] . esheet-save-as)))

(defun message-q () (interactive) (messge "Q"))
(defun message-w () (interactive) (messge "W"))


;;load command
(defun esheet-mode () 
"Load esheet.  Esheet is a spreadsheet modules.  Esheet puts a thin
edit-line window across the top of the frame in which to edit cells,
while in the cell itsef, only so much as fits or the value of the
formula, if appropriate.

Key bindings:
[0-9a-zA-Z]      Jump to the edit line to set the cell's value
enter,space,tab  Edit the cell in the edit line non-destructively
arrows           Move the _Esheet_ cursor
Shift+arrows     Move the Esheet cursor expanding the region
C-e              Restore the default window configuration
C-x C-f          Load esheet format
C-x C-s          Save sheet format
C-w              Cut (Esheet)
M-w              Copy (Esheet)
M-e              Copy values-only (Esheet)
C-M-w            Copy values-only (Esheet)
C-y              Paste Esheet
C-c C-g          Graph
C-c C-s          Sort

Esheet is written and maintained by Daniel Speyer: comments, complaints,
ideas, bug reports, and code snippets should be send to dspeyer@users.sourceforge.net"
  (interactive)
  (if (fboundp 'establish-esheet-toolbar) (establish-esheet-toolbar))
  ;establish mode
  (setq major-mode 'esheet-mode)
  (setq mode-name "Esheet")
  (use-local-map nil)
  ;set local variables
  (setq cursX 0)
  (setq cursY 0)
  (setq truncate-lines t)
  (setq esheet-region nil)
  (setq esheet-clipboard [[]])
  ;keybindings
  (let ((c ?!)) (while (<= c ?~) (local-set-key (concat (cons c nil)) 'replace-cell-value) 
                      (setq c (+ c 1))))
  (mapcar #'(lambda (k) (local-set-key (car k) (cdr k))) esheet-keys)
  (if (and (featurep 'mouse) (equal (substring (version) 0 3) "XEm")) 
      (progn
        (setq mouse-track-down-hook (cons 'esheet-mouse-down mouse-track-down-hook))
        (setq mouse-track-drag-hook (cons 'esheet-drag mouse-track-drag-hook))
        (setq mouse-track-drag-up-hook (cons 'esheet-drag-up mouse-track-drag-hook))
        (push 'esheet-block-click mouse-track-click-hook)))
	;other
  (esheet-load)
  (if (not (and (get-buffer "*edit line*") 
	      (get-buffer-window (get-buffer "*edit line*"))))
      (progn
        (let ((new-win (split-window (selected-window) 4)))
          (switch-to-buffer (get-buffer-create "*edit line*"))
          (esheet-edit-line-mode)
          (select-window new-win))))
  (message "esheet is under the GPL, for more info, M-x describe-copying"))
