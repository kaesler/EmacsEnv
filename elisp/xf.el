;;From marc@mpi.nl Fri Apr 28 10:13:23 1995
;;From: marc@mpi.nl (Marc Fleischeuers)
;;Subject: Re: Pop-up menu for fonts: how to get all of them?
;;Newsgroups: comp.emacs
;;Date: Thu, 27 Apr 1995 20:44:02 GMT
;;Organization: IWTS, Nijmegen University, Nijmegen, NL
;;Path: news.ultranet.com!news.mathworks.com!gatech!swrinde!howland.reston.ans.net!news.nic.surfnet.nl!sun4nl!sci.kun.nl!news-srv.sci.kun.nl!marc
;;In-Reply-To: samuels@ford.uchicago.edu's message of Wed, 26 Apr 1995 21:01:28 GMT
;;Message-ID: <MARC.95Apr27224402@iwtspc09.mpi.nl>
;;Sender: news@sci.kun.nl (News owner)
;;Nntp-Posting-Host: iwtspc09.mpi.nl
;;References: <D7nuEH.12M@midway.uchicago.edu>
;;Lines: 308
;;
;;>>>>> "Jordan" == Jordan Samuels <samuels@ford.uchicago.edu> writes:
;;
;;Jordan> I have many fonts which are accessible (/usr/lib/X11/fonts/*) but
;;Jordan> which don't pop up in the pop-up menu (C-mouse 3 on my platform).
;;Jordan> I was unable to find info in the FAQ's and online info.
;;Jordan> Can anyone tell me a font-path to change, or another fix?
;;
;;Jordan> Platform: Emacs 19.27.1 (no toolkit), AIX 3.2.5, X11-R5
;;
;;Jordan> Thanks in advance,
;;
;;Jordan> Jordan
;;
;;I love this tool, got it off here some while ago but it's about time
;;it was reposted. Anyway, what it does is extract *all* fonts from your
;;server that emacs can use, and presents them in a form ready to be put
;;in fixed-font-alist. You run it only once, it takes a long time but
;;then you're all set. Here goes:
;;
;;--------------------------------------------------
;;Newsgroups: comp.emacs
;;Path: sci.kun.nl!sun4nl!EU.net!uunet!newsfeed.ACO.net!fuw.edu.pl!cocos.fuw.edu.pl!pfs
;;From: pfs@mimuw.edu.pl (Piotr Filip Sawicki)
;;Subject: xf.el - create mouse font menu (C-m-3) on-line (UPDATED)
;;Message-ID: <1994Oct31.193723.11461@fuw.edu.pl>
;;Sender: news@fuw.edu.pl
;;Nntp-Posting-Host: parnas.mimuw.edu.pl
;;Reply-To: pfs@mimuw.edu.pl (Piotr Filip Sawicki)
;;Organization: Institute of Informatics, Warsaw University
;;X-Newsreader: mxrn 6.18-9
;;Date: Mon, 31 Oct 1994 19:37:23 GMT
;;Lines: 267
;;
;;
;;(Updated version: smaller code and more instructions how to use it)
;;
;;Hey!
;;
;;   Writing ISO Latin 2 support for Emacs (now gone to testing, will be
;;out in two weeks or so) I found myself in the dire need for checking
;;the existence of Latin 2 fonts on the server... and then for adding
;;these fonts to x-fixed-font-alist (mouse font menu, that is).
;;
;;   Also, being fed up with enhancing manually x-fixed-font-alist of
;;every single emacs version released, I decided to write a general
;;function to do this for me. Here is the result, after 3 days of work.
;;
;;   Upon loading this code just queries the server; to update list, you
;;have to call xf-load-fonts function yourself. See comments for more
;;information.
;;
;;WARNING:
;;
;;   Querying server takes enormous time... To save time when you run
;;emacs on one type of x-server only, e.g. your workstation, either ask
;;your administrator for help, or help yourself in the way described in
;;the file header.
;;
;;   What else? Regular expressions used for font parsing are more
;;general than those used in faces.el. Whether I can cope with strange
;;fonts, is quite a different matter, though.
;;
;;fi.
;;
;;P.S.: If emacs authors already created that kind of code and do not
;;      release it for some reason, I hope Gods will forgive them.
;;--
;;'vi'ssic Park: Come and see those who did not find time to learn emacs!
;;
;;Cut here:
;;------------------------------------------------------------------------------
`;;; xf.el --- dynamically create x-fixed-font-alist of ISO Latin fonts

;;; Copyright (C) 1994 Piotr Filip Sawicki

;; Author: Piotr Filip Sawicki (pfs@mimuw.edu.pl)
;; Organization: Institute of Informatics at University of Warsaw, Poland
;; Maintainter: PFS
;; Keywords: none

;;; This file is NOT a part of GNU Emacs.

;;; This package is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.

;;; The package is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;; Commentary:

;; This code creates dynamically x-fixed-font-alist of all ISO Latin fonts.

;; This package may require X11R4 or higher.

;; Upon loading the package queries the server (this operation takes
;; enormously long time) two times for monospaced "m" and "c"
;; (cell-based?) fonts, and records the list in the variable
;; xf-latin-fonts. Then the function xf-load-fonts creates sorted
;; lists of all fonts, divided by encoding type and family. Surplus
;; fonts that differ only by resolution and average char width (I had
;; no choice, there are too many fonts to accept all) are removed.

;; Since some fonts of strange size will never be used anyway, you can
;; remove too big or too small fonts. By default, all fonts smaller
;; than 10 or bigger than 24 points are removed (see variables below).

;; You should call xf-load-fonts by yourself; it can be used only
;; once, as the code at the end sets xf-latin-fonts to nil.

;; Previously this code created a three-level font list, on the top
;; level split according to encoding, but that kind of structure could
;; not be handled by x-popup-menu. The drawback of current approach is
;; that you can find yourself with the long list of entries starting
;; with useless in this case "Latin1" prefix. If you dislike that,
;; edit the file.

;; To maintain backward compatibility, the very first font on the very
;; first sublist is the default font (surprisingly).

;; To make use of this code, put in your .emacs the following two lines:
;;	(load-file "<the full path to this file on your system>")
;;	(xf-load-fonts)

;; IMPORTANT:

;; To avoid endlessly waiting on every emacs load, do the following:
;; 1. Save this file somewhere.
;; 2. Start emacs in the following way:
;;	> emacs -l path_to_this_file
;; 3. After a long time, this will put you into *scratch* buffer. Now type:
;;	(xf-load-fonts)
;;    and then LFD or ^J and the end of line. This will run the function.
;; 4. You will find yourself at the end of the very long single line. Type
;;    ^P or up-arrow to go up, then _insert_ at the beginning of the line:
;;	(setq x-fixed-font-alist
;;    Remember to put a space between "x-fixed-font-alist" and "(".
;; 5. Jump to the end of line and insert ")". Minibuffer will read:
;; 	Matches (setq ...
;; 6. And now append this _one_ line to your .emacs ... and your done!

;;; Code:

(defvar xf-latin-fonts
  (and (eq window-system-version 11)
       (append (x-list-fonts "*-m-*-iso8859-*")
	       (x-list-fonts "*-c-*-iso8859-*")))
  "List of all appropriate Latin fonts.")

(defvar xf-x-font-family "^-[^-]*-\\([^-]*\\)-")
(defvar xf-x-font-weight "^-[^-]*-[^-]*-\\([^-]*\\)-")
(defvar xf-x-font-encoding "-iso8859-\\(.*\\)$")
(defvar xf-x-font-other
  "^-[^-]*-[^-]*-[^-]*-[^-]*-\\([^-]*\\)-\\([^-]*\\)-\\([0-9]+\\)-")

(defvar xf-font-too-big "25"
  "Remove all fonts bigger than or equal to this size.")
(defvar xf-font-too-small "10"
  "Remove all fonts smaller than this size.
If the size is one digit long, prepend a space.")

(defun xf-substr (str reg &optional num)
  "Cut the match out of string."
  (if (not num) (setq num 1))
  (if (not (string-match reg str)) nil
    (substring str (match-beginning num) (match-end num))))

(defun xf-strip-list (font-list)
  "Removes from the list all fonts that have the same or too big/small size.
Also adds dividing lines between fonts of the different size."
  (let ((font-new '())
	(last-font "")
	(last-size "")
	this-font this-size)
    (while font-list
      (setq this-font (car (car font-list)))
      (and (not (string= last-font this-font))
	   (string< xf-font-too-small this-font)
	   (string< this-font xf-font-too-big)
	   (setq this-size (xf-substr (car (cdr (car font-list)))
				     xf-x-font-other 3)
		 font-new (cons (car font-list)
				(cond
				 ((string= this-size last-size) font-new)
				 ((string= last-size "") font-new)
				 (t (cons '("") font-new))))
		 last-font this-font
		 last-size this-size))
      (setq font-list (cdr font-list)))
    font-new))

(defun xf-comp-font-list (l r)
  "Used for font sorting: compare by encoding and family (reversed order)."
  (let* ((l-f (car (cdr l))) (r-f (car (cdr r)))
	 (l-latin (xf-substr l-f xf-x-font-encoding))
	 (r-latin (xf-substr r-f xf-x-font-encoding)))
    (cond
     ((string< l-latin r-latin) nil)
     ((string< r-latin l-latin) t)
     ((string< (xf-substr r-f xf-x-font-family)
	       (xf-substr l-f xf-x-font-family))))))

; call this function yourself

(defun xf-load-fonts ()
  (interactive)
  (if xf-latin-fonts
      (let ((font-list xf-latin-fonts)
	    (font-sub '())
	    font-found font-enc font-fam font-new font-size)
	; first construct linear list, then sort, divide, and sub-sort again
	; also, downcase all font names, to avoid troubles later
	(while font-list
	  (setq font-found (downcase (car font-list))
		font-sub 
		(cons
		 (list (progn
			 (string-match xf-x-font-other font-found)
			 (setq font-size (substring font-found
						     (match-beginning 3)
						     (match-end 3)))
			 (if (< (string-to-number font-size) 9)
			     (setq font-size (concat " " font-size)))
			 (concat
			  font-size
			  (if (= (match-beginning 2) (match-end 2))
			      ""
			    (concat " "
				    (substring font-found
					       (match-beginning 2)
					       (match-end 2))))
			  (if (string= (substring font-found
						  (match-beginning 1)
						  (match-end 1))
				       "normal")
			      ""
			    (concat " "
				    (substring font-found
					       (match-beginning 1)
					       (match-end 1))))
			  (if (string-match "-medium-" font-found)
			      ""
			    (string-match xf-x-font-weight font-found)
			    (concat
			     " "
			     (substring font-found
					(match-beginning 1)
					(match-end 1))))
			  (if (string-match "-[oi]-" font-found) " slant" "")))
		       font-found)
		 font-sub)
		font-list (cdr font-list)))
	(setq font-list (sort font-sub 'xf-comp-font-list)
	      font-found (car (cdr (car font-list)))
	      font-enc (xf-substr font-found xf-x-font-encoding)
	      font-fam (xf-substr font-found xf-x-font-family)
	      font-sub '()
	      font-new '())
	(while font-list ; not always advances ...
	  (cond
	   ((eq font-list 0) (setq font-list nil font-found nil))
	   (t (setq font-found (car (cdr (car font-list))))))
	  (if (and font-found
		   (string= font-enc (xf-substr font-found xf-x-font-encoding))
		   (string= font-fam (xf-substr font-found xf-x-font-family)))
	      (setq font-sub (cons (car font-list) font-sub)
		    font-list (cond ((cdr font-list)) (t 0)))
	    (setq font-new (cons
			    (cons
			     (concat
			      "Latin" font-enc
			      " "
			      (capitalize font-fam))
			     (xf-strip-list
			      (sort font-sub (lambda (l r) ; reversed order
					       (string< (car r) (car l))))))
			    font-new)
		  font-sub '()
		  font-enc (if (not font-found) font-enc
			     (xf-substr font-found xf-x-font-encoding))
		  font-fam (if (not font-found) font-fam
			     (xf-substr font-found xf-x-font-family)))))
	(setq xf-latin-fonts nil
	      x-fixed-font-alist
	      (cons
	       (car x-fixed-font-alist)
	       (cons
		(cons
		 (car (car (cdr x-fixed-font-alist)))
		 (list (car (cdr (car (cdr x-fixed-font-alist))))))
		font-new))))))

;;; The end
