;; c-format.el  --  written by Kevin Esler Wed Dec 27 14:18:48 1989

;; Notes:
;;
;;    Where to send output ?
;;    foo.c --> foo.fmt
;;    foo.c --> foo.c
;;    foo.c --> foo.c<2>
;;    foo.c --> foo.cf
;;
;; Make all of these available, selectable by:
;;  - separate functions
;;  - global variables
;;
;; Would be nice if we could look at the user's c-mode parameters
;; and invoke indent accordingly.  Perhaps only if ~/.indent.pro
;; and ./.indent.pro don't exist ?
;;

;; Interfaces

(defun c-format (input-buffer output-buffer current-dir)

  "Invokes indent using INPUT-BUFFER as input, sending output to
OUTPUT-BUFFER.  Indent will use the .indent.pro in CURRENT-DIR, if
it exists."
)

(defun c-format-current-buffer ()
)

(defun c-format-region ()
)
