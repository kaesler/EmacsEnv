;; Change the column of text nested below a line.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; This module is intended for Haskell mode users, but is
;; independent of Haskell mode.

;; Example usage:

;; (require 'haskell-move-nested)
;; (define-key haskell-mode-map (kbd "C-<left>")
;;   (lambda ()
;;     (interactive)
;;     (haskell-move-nested -1)))

;; (define-key haskell-mode-map (kbd "C-<right>")
;;   (lambda ()
;;     (interactive)
;;     (haskell-move-nested 1)))


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(defun haskell-move-nested (columns)
  (interactive) 
  (save-excursion
    (let ((region (haskell-nested-region)))
      (when region
        (indent-rigidly (car region) (cdr region) columns)))))

(defun haskell-nested-region ()
  (save-excursion
    (let ((starting-level (current-column)))
      (forward-line)
      (let ((current-level (haskell-indentation-level)))
        (let ((start-point (line-beginning-position))
              (start-end-point (line-end-position))
              (end-point nil))
          (forward-line)
          (while (or (> (haskell-indentation-level) starting-level)
                     (and (> current-level starting-level)
                          (>= (haskell-indentation-level) current-level)))
            (setq end-point (line-end-position))
            (forward-line))
          (cons start-point (or end-point
                                start-end-point)))))))

(defun haskell-indentation-level ()
  (1- (length
       (buffer-substring-no-properties
        (line-beginning-position)
        (or (save-excursion (goto-char (line-beginning-position))
                            (search-forward-regexp "[^ ]" (line-end-position) t 1))
            (line-beginning-position))))))

(provide 'haskell-move-nested)
