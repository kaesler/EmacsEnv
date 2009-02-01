;; From nobody Tue Nov 30 11:39:17 1999
;; Path: atria.com!cambridge1-snf1.gtei.net!news.gtei.net!news.shore.net!newsfeed.mathworks.com!btnet-peer!btnet!nntp.news.xara.net!xara.net!gxn.net!server6.netnews.ja.net!newsfeed.ed.ac.uk!kane.dcs.ed.ac.uk!tardis.tardis.ed.ac.uk!skx
;; From: Steve Kemp <skx@tardis.ed.ac.uk>
;; Newsgroups: gnu.emacs.sources
;; Subject: Slashdot.el v.07
;; Date: Tue, 30 Nov 1999 09:20:28 +0000
;; Organization: Division of Informatics, The University of Edinburgh
;; Lines: 402
;; Message-ID: <Pine.GSO.4.10.9911300919080.1879-100000@tardis.tardis.ed.ac.uk>
;; NNTP-Posting-Host: muck.dcs.ed.ac.uk
;; Mime-Version: 1.0
;; Content-Type: TEXT/PLAIN; charset=US-ASCII
;; X-Trace: kane.dcs.ed.ac.uk 943953631 30094 129.215.216.15 (30 Nov 1999 09:20:31 GMT)
;; X-Complaints-To: abuse@dcs.ed.ac.uk
;; NNTP-Posting-Date: 30 Nov 1999 09:20:31 GMT
;; Cache-Post-Path: muck.dcs.ed.ac.uk!unknown@tardis.tardis.ed.ac.uk
;; X-Cache: nntpcache 2.3.3 (see http://www.nntpcache.org/)
;; 
;; 
;;   The latest version of Slashdot.el - this allows you to browse
;;  the headlines with a choice of browsers; either an external
;;  browser, or using w3.
;; 
;; Steve
;; ---
;; 
;;; slashdot.el -- View headlines from Slashdot.org.

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Keywords: tools, convenience, new, slashdot
;; Revision: 0.0.7

;; This file is not yet ;) part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;   This is a simple mode that will download the latest headlines from
;; the Slashdot.org website, and allow you to visit them from within Emacs.
;;

;;; Compatability
;;
;; Tested with the following version of Emacs:
;;
;;  GNU Emacs 20.4.1
;;  GNU Emacs 20.3.x
;;  GNU XEmacs 20.1 (patch 7) - Biscayne
;;
;;  As this package relies upon having access to the custom widgets it
;; is unlikely to work with Emacs v19.x - unless they have a recent
;; version of custom installed.
;;

;;; Usage:
;;
;; 1.  Put slashdot.el in your emacs' site-lisp directory, or your
;;    home directory, and optionally byte-compile it.
;;
;; 2.  Place the folling line in your .emacs file :
;;    (require 'slashdot)
;;
;; 3.  Fill in values for slashdot-username, and slashdot-password,
;;    etc, if necessary.
;;
;; 4.  M-x slashdot will give you the latest headlines from Slashdot.
;;
;;


;;
;;; Version History:
;;
;;    0.0.1.  Initial implementation.
;;
;;    0.0.2.  Switched to using the Widgets rather than (goto-address)
;;
;;    0.0.3.  Cleaned up for posting to gnu.emacs.sources.
;;
;;    0.0.4.  Fixed typo, and updated the metafile parsing, As suggested
;;           by Dave Pearson <davep@hagbard.demon.co.uk>.
;;
;;            Updated the handling of buffers, and added the read only fix,
;;           as suggested by Alan Shutko <ats@acm.org>.
;;
;;            Added update button.
;;
;;    0.0.5   Got my news feed back, added the Proxy support by
;;           Johan Vromans <JVromans@Squirrel.nl>, and reposted.
;;
;;    0.0.6   Added support for logging into Slashdot, and retrieving
;;           personalized headlines - this is preliminary
;;            Fixed _my_ mistake in the proxy code from Johan.
;;
;;    0.0.7   Added notes on the version(s) of Emacs that I had tested
;;           this with.
;;            Added support for W3.
;;            Allow user to choose the browser then want to use graphically.
;;
;;   You should now be able to obtain the most up-to-date version of this file
;; from http://www.tardis.ed.ac.uk/~skx/win/NTEmacs.html, or from my new site
;; http://www.gnusoftware.com/
;;

;;; Preamble:

(require 'widget)
(require 'wid-edit)
(require 'browse-url)


(defvar slashdot-use-w3 t
  "Set to non-nil to enable use of `w3'.")

(eval-when-compile
  (if slashdot-use-w3
    (require 'w3)))

(defvar slashdot-username nil
  "Username to use when retrieving the slashdot headlines.")

(defvar slashdot-password nil
  "Password to use when retrieving the slashdot headlines.")
    
(defvar slashdot-mode-map nil
  "The keymap we use in slashdot mode.")

(defvar slashdot-browser "external"
  "The type of browser to use for viewing the slashdot headlines.
Valid choices are; \"external\", and \"w3\".")

(if slashdot-use-w3
    (require 'w3))

(defvar slashdot--cookie nil
  "`slashdot' Internal.
Cookie to use when retrieving the slashdot headlines.")

(defvar slashdot-temp-buffer "*Slashdot-Temp*"
  "The name of the temporary buffer we use.")

(defvar slashdot-cookie-buffer "*Slashdot-Cookie*"
  "The name of the temporary buffer for users who wish to login to slashdot")

(defvar slashdot-buffer "*Slashdot*"
  "The name of the buffer the found headlines should be placed into.")

(defvar slashdot-file "/slashdot.xml"
  "The name of the file to download from the slashdot web site.")

(defvar slashdot-host "www.slashdot.org"
  "The hostname to use when connecting to Slashdot")

(defvar slashdot-proxy nil
  "The name of the firewall proxy, or nil if no proxy is required.")

(defvar slashdot-proxyport 8001
  "Port number for firewall proxy.")


(if slashdot-mode-map
    ()
  (if (fboundp 'widget-minor-mode-map)
      (setq slashdot-mode-map (copy-keymap widget-minor-mode-map))
    (setq slashdot-mode-map (copy-keymap widget-global-map))))

(define-key slashdot-mode-map "q" 'bury-buffer)
(define-key slashdot-mode-map " " 'scroll-up)


;;; Code:

;;;###autoload
(defun slashdot ()
  "Attempt to view the current Slashdot headlines.
This function will open a connection and download the latest headlines from the
slashdot.org, \"News for Nerds\" website, and display them.  This will also
place clickable links to allow you to read the articles with either `w3', or
and external browser."
  (interactive)
  (setq slashdot--cookie nil) ; Remove this to enable cookie..
  (message "Downloading latest headlines ...")
  (kill-buffer (get-buffer-create slashdot-buffer))
  (slashdot-get-cookie)
)


(defun slashdot-get-cookie ( )
  "Get the cookie for the username `slashdot-username' on Slashdot.
This is will not bother to fetch a cookie if the username is nil, and instead
downloads the headlines immediately."
  (if slashdot-username
      (progn
	;; If there is a username .. get the cookie.
	(let ((tcp-connection)
	      (file (concat "users.pl?unickname=" slashdot-username
			    "&returnto=index.pl&upasswd=" slashdot-password 
			    "&op=userlogin"))
	      (buffer (get-buffer-create slashdot-cookie-buffer))
	      (host slashdot-host)
	      (port 80))

	  ;; Allow people to use a proxy-server.
	  (if (null slashdot-proxy)
	      (setq file (concat "/" file))
	    (progn
	      (setq file (concat "http://" slashdot-host "/" file))
	      (setq host slashdot-proxy)
	      (setq port slashdot-proxyport)
	      ))

	  (set-buffer buffer)
	  (or
	   (setq tcp-connection
		 (open-network-stream
		  "GET process-name"
		  buffer
		  host
		  port
		  ))
	   (error "Could not open connection to %s:%d" host port))
	  (set-marker (process-mark tcp-connection) (point-min))
	  (set-process-sentinel tcp-connection 'slashdot-cookie-sentinel)
	  (process-send-string tcp-connection (concat "GET " file " HTTP/1.0\n\n"))))
    )
  ;; Else get headline.
  (slashdot-get-url (get-buffer-create slashdot-temp-buffer) slashdot-host
		    80 slashdot-file))


(defun slashdot-cookie-sentinel (process string)
  "Process the results from the slashdot network connection.
Here we are looking for a successfull cookie being sent to us,
if we find one we will send this back, which will allow us to
retrieve a user's personalized headlines.
process - The process object that is being notified.
string - The string that describes the notification."
  (interactive)
  (set-buffer (get-buffer-create slashdot-cookie-buffer))
  (goto-char 0)
  (if (re-search-forward "Set-Cookie: \\(.*\\)" nil t)
      (setq slashdot--cookie (match-string 1))
	    (setq slashdot--cookie "\n"))
  (kill-buffer (get-buffer-create slashdot-cookie-buffer))
  ;; Now get headline.
  (slashdot-get-url (get-buffer-create slashdot-temp-buffer) slashdot-host
		    80 slashdot-file))


(defun slashdot-get-url(buf host port file)
  "Attempt to download a URL into a buffer.
buf The buffer to use for outputting the data from.
host The hostname to contact.
port The port number to use.
file The file to ask for, relative to the server root."
  (interactive)
  (let ((tcp-connection))
    (set-buffer buf)
    (if (null slashdot-proxy)
	(setq file (concat "/" file))
      (progn
	(setq file (concat "http://" slashdot-host "/" file))
	(setq host slashdot-proxy)
	(setq port slashdot-proxyport)
      ))
    (or
     (setq tcp-connection
	   (open-network-stream
	    "GET process-name"
	    buf
	    host
	    port
	    ))
     (error "Could not open connection to %s:%d" host port))
    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-sentinel tcp-connection 'slashdot-sentinel)
    (let ((request (concat "GET " file " HTTP/1.0\n")))
      (if slashdot--cookie
	  (progn
	    (setq request (concat request slashdot--cookie "\n\n"))
	    (message "Sending Cookie for user %s ..." slashdot-username))
	(setq request (concat request "\n")))
      (process-send-string tcp-connection request))))


(defun slashdot-sentinel (process string)
  "Process the results from the slashdot network connection.
process - The process object that is being notified.
string - The string that describes the notification."
  (let ((buffer (get-buffer-create slashdot-buffer)))
    (set-buffer buffer)
    (erase-buffer)
    (goto-char 0)
    (insert "\n\tSlashdot Headlines")
    (if slashdot-username
	(insert (concat " for user " slashdot-username)))
    (insert "\t")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (slashdot))
     		 "Refresh")
    (insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (bury-buffer))
     		 "Bury")

    (if slashdot-use-w3
	(progn
	  (insert " ")
	  (widget-create 'menu-choice
			 :tag "Browser"
			 :value "external"
			 :help-echo "Choose me, please!"
			 :notify (lambda (widget &rest ignore)
				   (setq slashdot-browser (widget-value widget)))
			 '(item :tag "External" :value "external")
			 '(item :tag "W3" :value "w3"))))
    (insert "\n\n")
    (slashdot-parse buffer)
    (pop-to-buffer buffer)
    (kill-all-local-variables)
    (widget-minor-mode 1)
    (setq major-mode 'w32-registry-mode
	  mode-name "Slashdot")
    (use-local-map slashdot-mode-map)
    (widget-setup)
    (goto-char 0)
;    (setq buffer-read-only t)
    (kill-buffer (get-buffer-create slashdot-temp-buffer))))




(defun slashdot-parse (buffer)
  "Parse the result of the Slashdot temporary buffer.
BUFFER is the buffer where the beautified headlines should appear."
  (interactive)
  (let ((buf (get-buffer-create slashdot-temp-buffer)))
    (set-buffer buf)
    (goto-char 0)
    (while (slashdot-parse-article buffer))))


(defun slashdot-grab-headline-item (item)
  "Parse headline ITEM out of the current line of the current buffer."
  (re-search-forward (format "<%s>\\(.*\\)</%s>" item item))
  (match-string 1))

(defun slashdot-parse-article (buffer)
  "Parse a single article in the slashdot buffer.
BUFFER is the buffer where the beautified headlines should appear."
   (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
       (progn
	 (let* ((title      (match-string 1))
		(url        (slashdot-grab-headline-item "url"))
		(author     (slashdot-grab-headline-item "author"))
		(department (slashdot-grab-headline-item "department"))
		(topic      (slashdot-grab-headline-item "topic"))
		(comments   (slashdot-grab-headline-item "comments"))
		(section    (slashdot-grab-headline-item "section")))
	   (message "Found %s" title)
	   (slashdot-insert buffer title url author department topic comments section)
	   t))
     nil))

(defun slashdot-insert (buffer title url author department topic comments section)
  "Insert and article, and links into the buffer `buffer'.
BUFFER is the buffer where the beautified headlines should appear.
TITLE is the title of the article.
URL is the URL that the story lives at.
AUTHOR the author of the story.
DEPARTMENT The department that the article belongs to.
TOPIC The topic the article belongs to.
COMMENTS The number of comments the article has.
SECTION The section the article belongs to."
  (let ((current (current-buffer))
	(inhibit-read-only t))
    (set-buffer buffer)
    (insert "\n\t")
    (widget-create 'push-button
		   :url url
		   :title title
		   :notify (lambda (widget &rest ignore)
			     (let ((url (widget-get widget :url)))
			       (message "Viewing %s" url)
			       (if (or (equal slashdot-browser "external")
				       (not slashdot-use-w3))
				   (browse-url url))
			       (if slashdot-use-w3
				   (if (equal slashdot-browser "w3")
				       (w3-fetch url)))
			       ))
		   title)
    (insert (concat "\n\t\tby " author ", in " section " section (" comments " comment"
                    (if (= (car (read-from-string comments)) 1) "" "s") ")."))
    (insert (concat "\n"))
    (set-buffer current)))


(provide 'slashdot)
;;; slashdot.el ends here

