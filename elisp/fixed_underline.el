;;; fixed_underline.el -- Show underlined text using an underline face
;;; Copyright (C) 1995 Greenwich Capital Markets, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Tired of seeing all those _^H 's in your emacs buffers?
;;; This should help.  I use it with emacs-19.28 under X.
;;; Note that the _^H's will still be in your files, they'll
;;; just look better.

;;; Send bug reports and improvements to dcw@gcm.com

;;; $Revision: 1.3 $
;;; $Log: fixed-underline.el,v $
; Revision 1.3  1995/05/17  17:12:57  dcw
; Significant speed-up of automatic underline fixing when finding files
;
; Revision 1.2  1995/05/17  17:01:27  dcw
; Automatically fix underlining of files when finding them
;

(and window-system

     (defun how-faces-differ (face1 face2 &optional frame)
       "How the two faces differ, or nil if they're the same."
       (setq face1 (internal-get-face face1 frame)
             face2 (internal-get-face face2 frame))
       (let ((difference nil))
         (or (equal (face-foreground face1 frame)
                    (face-foreground face2 frame))
             (setq difference (cons 'foreground difference)))
         (or (equal (face-background face1 frame)
                    (face-background face2 frame))
             (setq difference (cons 'background difference)))
         (or (equal (face-font face1 frame)
                    (face-font face2 frame))
             (setq difference (cons 'font difference)))
         (or (eq (face-underline-p face1 frame)
                 (face-underline-p face2 frame))
             (setq difference (cons 'underline difference)))
         difference))
     
     (defun find-underlined-version-of-face (base)
       "Returns a (possibly new) face, similar to base but underlined."
       (if (null base)
           'underline
         (if (face-underline-p base)
             base
           (let ((all-faces (face-list))
                 (retval    nil))
             (while all-faces
               (if (equal (how-faces-differ base (car all-faces)) '(underline))
                   (progn
                     (setq retval (car all-faces))
                     (setq all-faces nil))
                 (setq all-faces (cdr all-faces))))
             (or retval
                 (progn
                   (setq retval (concat base "-underlined"))
                   (make-face retval)
                   (copy-face base retval)
                   (set-face-underline-p retval t)))
             retval))))
	 
     (defun find-not-underlined-version-of-face (base)
       "Returns a (possibly new) face, similar to base but not underlined."
       (cond
        ((or (null base) (not (face-underline-p base)))
         base)
        ((equal base 'underline)
         nil)
        (t
         (let ((all-faces (face-list))
               (retval    nil))
           (while all-faces
             (if (equal (how-faces-differ base (car all-faces)) '(underline))
                 (progn
                   (setq base (car all-faces))
                   (setq all-faces nil))
               (setq all-faces (cdr all-faces))))
           (or retval
               (progn
                 (setq retval (concat base "-ununderlined"))
                 (make-face retval)
                 (copy-face base retval)
                 (set-face-underline-p retval nil)))
           retval))))
	 
     (defun make-invisible (start end)
       "Make the region invisible"
       (interactive "r")
       (put-text-property start end 'invisible t))
	 
     (defun make-visible (start end)
       "Make the region visible"
       (interactive "r")
       (put-text-property start end 'invisible nil))
	 
     (defun face-underline-char (N)
       "Make the character appear underlined"
       (interactive "d")
       (let ((face (get-text-property N 'face)))
         (put-text-property N (1+ N) 'face
                            (find-underlined-version-of-face face))))
	 
     (defun face-ununderline-char (N)
       "Make the character not appear underlined"
       (interactive "d")
       (let ((face (get-text-property N 'face)))
         (put-text-property N (1+ N) 'face
                            (find-not-underlined-version-of-face face))))
	 
     (defun make-underlined (start end)
       "Make the region appear underlined"
       (interactive "r")
       (if (< start end)
           (let ((mid  (1+ start))
                 (face (get-text-property start 'face)))
                                        ; Find the largest same-faced region
             (while (and (<= mid end)
                         (equal (get-text-property mid 'face) face))
               (setq mid (1+ mid)))
             (put-text-property start mid 'face
                                (find-underlined-version-of-face face))
             (make-underlined (1+ mid) end))))
	 
     (defun make-ununderlined (start end)
       "Make the region not underlined"
       (interactive "r")
       (if (< start end)
           (let ((mid  (1+ start))
                 (face (get-text-property start 'face)))
                                        ; Find the largest same-faced region
             (while (and (<= mid end)
                         (equal (get-text-property mid 'face) face))
               (setq mid (1+ mid)))
             (put-text-property start mid 'face
                                (find-not-underlined-version-of-face face))
             (make-ununderlined (1+ mid) end))))
	 
     (defun fix-underlining-of-region (start end)
       "Make underlining of the type _ <backspace> <char> look correct."
       (interactive "r")
       (save-excursion
         (let ((end1 (make-marker))
               (modified (buffer-modified-p)))
           (move-marker end1 (max start end))
           (goto-char (min start end))
           (while (< (point) end1)
             (if (looking-at "_\b\\|\b_")
                 (progn
                   (make-invisible (point) (+ (point) 2))
                   (face-underline-char (+ (point) 2))
                   (forward-char 3))
               (progn
                 (face-ununderline-char (point))
                 (forward-char 1))))
           (set-buffer-modified-p modified))))
	 
     (defun fix-underlining-of-buffer ()
       "Make underlining of the type _ <backspace> <char> look correct."
       (interactive)
       (fix-underlining-of-region (point-min) (point-max)))
	 
     (defun quick-underline-buffer ()
       "Make underlining of the type _ <backspace> <char> look underlined,
but don't necessarily preserve any other font information."
       (interactive)
       (save-excursion
         (let ((modified (buffer-modified-p)))
           (goto-char (point-min))
           (while (re-search-forward "_\b\\|\b_" (point-max) t)
             (make-invisible (- (point) 2) (point))
             (put-text-property (point) (1+ (point)) 'face 'underline))
           (set-buffer-modified-p modified))))

     (defun underline-region (start end)
       "Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on."
       (interactive "r")
       (save-excursion
         (let ((end1  (make-marker)))
           (move-marker end1 (max start end))
           (goto-char (min start end))
           (while (< (point) end1)
             (if (not (looking-at "[_\^@- ]"))
                 (progn
                   (insert "_\b")
                   (put-text-property (- (point) 2) (point) 'invisible t)
                   (face-underline-char (point))))
             (forward-char 1)))))
	 
     (defun ununderline-region (start end)
       "Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
       (interactive "r")
       (save-excursion
         (let ((end1 (make-marker)))
           (move-marker end1 (max start end))
           (goto-char (min start end))
           (while (re-search-forward "_\b\\|\b_" end1 t)
             (delete-char -2)
             (face-ununderline-char (point))))))

     (add-hook 'find-file-hooks 'quick-underline-buffer))

(provide 'fixed-underline)
