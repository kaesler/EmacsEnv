;; vm-complain.el --- automate reply and complaint message to Spam and UCE
;;
;; Copyright (C) 1997-1998 Francois Felix Ingrand
;; 
;; Author          : Francois Felix Ingrand (felix@laas.fr)
;; Created On      : Wed Jun  4 15:44:47 1997
;; Last Modified By: Francois Felix Ingrand
;; Last Modified On: Fri Feb 20 16:28:02 1998
;; Update Count    : 117
;; Status          : OK
;; Version         : 1.10
;; $Id$

;; What is this Bruno?
;; Functions to make (under VM) responses/complaints to Spam and UCE
;; with a few keystrokes.

;; Keywords: mail unsolicited commercial junk vm uce spam

;; Invoking 'vm-forward-message-and-complain will create a forward buffer
;; (using vm-forward-message-all-headers{-other-frame}).
;;
;; In this buffer, it will insert a X-UCE-Spam-Reported-by: header which you
;; can then look for with procmail to discard complaints bouncing.
;;
;; It will also insert a nice and polite string explaining why you are fed up
;; with all these junk mails.
;;
;; Last it will try to retrieve from the original mail, hostname from which the
;; Spam/UCE mail may have been posted. From this it will build a recipients
;; list based on the variable vm-complain-recipients, and ask the user to
;; confirm if the adress seem valid or not. (no need to send a mail to
;; 234234.com, it does not exist...).  The original mail is presented with all
;; the headers shown so you can check those hostname.
;;
;; Host are extracted from various field. The most interesting being the
;; Received field... From your host -> outside. Basically, the received fields
;; are the only fields spamer cannot really change or fool (as they are written
;; by the consecutive sendmail daemons). They can create fake one, but if you
;; work your way from your site thru the received fields, you are bound to find
;; a site which either has been abused or is supporting spam and UCE
;; mail. vm-complain will propose you most host starting from the nearest (I am
;; not sure what this mean under Internet) toward the farest...
;;
;; Then the forward buffer is presented (in a new frame or in a new buffer).
;; You can then manually check what are the extracted hosts and recipients
;; before sending the complaint.
;; 
;; I also strongly invite you to use procmail to filter out bounced
;; complaints using the following rule (put your user-mail-address).
;;
;; :0
;; * ^FROM_DAEMON
;; {
;;   :0 B
;;   * ^X-UCE-Spam-Reported-by: user-mail-address
;;   junk
;; }

;; As this package mostly use standard VM functions... this code should work on
;; any version of Emacs, but I have only tested it under Xemacs 19.15, Sparc,
;; Solaris 2.5.1., but other people have reported successful use under GNU FSF
;; Emacs.

;; It is strongly inspired from gnus-junk.el by Robert Bihlmeyer
;; <robbe@orcus.priv.at> but different enough to make a different
;; package.

;; Where do I get the most recent version of this little gem...
;; Most likely here: ftp://ftp.laas.fr/pub/ria/felix/vm-complain.el
;; You can also check: http://www.laas.fr/~felix/despam.html

;; This file is not yet part of anything (and is probably not worth
;; being part of anything ;-)

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with your Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Installation:
;; Something like the following should do the job.
;;
;; (autoload 'vm-forward-message-and-complain "vm-complain")
;; (define-key vm-mode-map "Z" 'vm-forward-message-and-complain)
;; 
;; I was told that the "Z" key is also used by other packages (such as TM), so
;; you may have to pick another key.
;;
;; You may also use to load it:
;; (require 'vm-complain)
;;
;; Also, DO NOT FORGET TO PUT YOUR DOMAIN IN THE vm-complain-domain-name
;; variable so it will not present you with your host as possible complaint
;; destination, and also it will know when to stop collecting Received: headers
;; (your domain would not issue spam... would you?)
;;

;; Future Plans:
;;
;; - Nuke those Spamer ;-)
;;
;; - use DNS/nameserver to check the hostname.

;; Caveats:
;;
;; Using vm-forward-message-and-complain will set the forwarded flag on the
;; original message... I consider it a feature so I know I have already
;; complained about this message.
;;
;; If you improve this package, let me know.

;; History: 
;; 20-Feb-1998		Francois Felix Ingrand	
;;    Last Modified: Fri Feb 20 16:28:02 1998 #117 (Francois Felix Ingrand)
;;    * Mandatory complain addresses are Cc'ed;
;;    * Added uce@ftc.gov for proper reporting.
;;
;; 29-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Mon Sep 29 17:22:25 1997 #107 (Francois Felix Ingrand)
;;    * One can add some adresses to always complain with a t field in
;;      vm-complain-recipients. Suggested by Ralphy and Greg Trafton.
;;
;; 10-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Wed Sep 10 17:40:14 1997 #105 (Francois Felix Ingrand)
;;    * release 1.9
;;
;; 10-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Wed Sep 10 09:12:26 1997 #104 (Francois Felix Ingrand)
;;    * removed the "for" from the regexp looking for host in received fields.
;;
;; 5-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Fri Sep  5 11:11:29 1997 #101 (Francois Felix Ingrand)
;;    * vm-complain-interactive to nil was not working properly (reported by
;;      Ralphy). 
;;
;; 4-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Thu Sep  4 15:35:43 1997 #98 (Francois Felix Ingrand)
;;    * define vm-complain-domain-name to ease the user configuration.
;;
;; 4-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Thu Sep  4 11:38:40 1997 #93 (Francois Felix Ingrand)
;;    * Changed the complain message (suggested by Michael Hucka).
;;    * release 1.6
;;
;; 1-Sep-1997		Francois Felix Ingrand	
;;    Last Modified: Mon Sep  1 15:21:59 1997 #88 (Francois Felix Ingrand)
;;    * Added a (provide 'vm-complain) for users using require.
;;    * Laurent Wacrenier <laurent@anet.fr>:
;;        - added a (require 'vm) to compile it.
;;        - changed the host seeking regexp in Received lines. The host in the
;;          parenthesis is  presumably more reliabe as it is given by a dns.
;;
;; 23-Jul-1997		Francois Felix Ingrand	
;;    Last Modified: Wed Jul 23 16:33:07 1997 #85 (Francois Felix Ingrand)
;;    * release a version using string-member instead of:
;;    	   (not (member* host hosts :test 'string=)))
;;
;; 7-Jul-1997		Francois Felix Ingrand	
;;    Last Modified: Mon Jul  7 15:39:06 1997 #76 (Francois Felix Ingrand)
;;    * Added vm-complain-interactive to ask for each host/adress if it is
;;      a valid complain host/adress or not (check the header by yourself).
;;    * Added vm-complain-collect-all-received and
;;      vm-complain-stop-received-collect to collect all the host found in the
;;      Received fields, but stop when encountering one host matching
;;      vm-complain-stop-received-collect.
;;    * Added vm-complain-nocomplain-domain which is a regexp of host/domain to
;;      which you do not want to complain.
;;    * Tested, all this seem to work.
;;    
;; 24-Jun-1997		Francois Felix Ingrand	
;;    Last Modified: Tue Jun 24 11:19:46 1997 #30 (Francois Felix Ingrand)
;;	* Added support for abuse.net usage (check http://www.abuse.net).
;;      * Fixed some typos.
;;      * Fixed some Xemacs'ism.
;;      * Started to use an history.
;;

(require 'cl)				;For union :test
(require 'vm)
(require 'vm-reply)

;;; Some variables you may want to change

(defvar vm-complain-message "(a copy of this message is also sent to the US Federal Trade Commission)

Madam, Sir,

I have received the following message which is either a Spam or an UCE.  After
analyzing the various mail headers, I have concluded that the message appears:
- to come from a user at your site, or
- to have been posted from your site, or 
- to have been relayed by your site.

Network connections and resources are not free.  Just like junk faxes, junk
email incurs a cost to the recipient or the recipient's institution.  Because
it is unsolicited, it thus constitutes theft of service.

I would appreciate if you could take any action to make it stop.

If I receive other Spams/UCEs from your site or if you support such use of the
Internet, I will take further action, including but not limited to: (1)
complaining to your network service provider or the sites directly connected to
yours\; (2) asking our Postmaster to filter all messages coming from your
domain and/or IP addresses\; and (3) reporting it to the proper law enforcement
organizations as a form of harrassment.

Thanks in advance,

Spam or UCE message follows:
"
  "Self explanatory... Be nice and polite... You are writing to
civilized people who understand THE Sendmail language: Postmaster."
  )

(defvar vm-complain-id-header 
  (concat "X-UCE-Spam-Reported-by: " user-mail-address "\n")
  "Mainly to enable some procmail parsing on rejected complain mails
(you will get some for sure).
I can thus include in my .procmailrc a rule like:
:0
* ^FROM_DAEMON
{
  :0 B
  * ^X-UCE-Spam-Reported-by: your-name-here@your-domain-here
  junk
}
")

(defvar vm-forwarding-complain-subject-format
  "Forwarded SPAM or UCE from %F (Subject: %s)"
  "The format string used to create the Subject of the complaint message.")

(defvar vm-complain-interactive t
  "It t will ask for each adress/host collected if it looks OK or not.")

(defvar vm-complain-collect-all-received t
  "Will try to collect all the host name in the Received: fields.")

(defvar vm-complain-domain-name "laas\\.fr"
  "Put your domain name here... do not ask why...")

(defvar vm-complain-stop-received-collect vm-complain-domain-name
  "When collecting host name from the Received fields, will stop the collect
process when encountering an host matching this domain.")

(defvar vm-complain-nocomplain-domain
  (concat "\\(" 
	  vm-complain-domain-name "\\|"	; your domain
	  "cyberpromo\\.com" "\\|"	; No comment
	  "^\\([^.]+\\.\\)*[0-9]+\\.[^.]+$"	; 234234.com often used...
	  "\\)")
  "A Regexp of domains to which you do not want to send complain (either
  because they do not care, or it is obviously fake, or it is your domain).")

(defvar vm-complain-recipients
  '(("Received:" "postmaster" "abuse") ;abuse_net
    ("From:" t "postmaster" "abuse")
    ("Reply-To:" t "postmaster" "abuse")
    ("Sender:" t "postmaster" "abuse")
    ("Message-ID:" "postmaster" "abuse")
    (t "uce@ftc.gov")				;Added in the Cc...
    (t "abuse@oleane.net")				;Added in the Cc...
    )
    "Defines the mailboxes that will receive the complaint.
It is a list of list such that:
First element is the Header in which we should look for
username@hostname (or hostname alone). Remaining element are the people to
which (at this hostname) the complain should be sent. If t (useful for the
From), the complete Header value is used. If abuse_net, then the host is
extracted and the complain recipient is host@abuse.net which will then forward
the complain to the appropriate people (to use abuse.net, you need to register
yourself first). 
If the first element in the list is t, the remaining adress are always
included.")

;;; You are leaving the user configuration area...
;;; 
;;; <><><><><><><><><><><><><><>------------<><><><><><><><><><><><><><>
;;;
;;; You are now entering the programmer area, proceed at you own risk...

(defun vm-forward-message-and-complain ()
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-expose-hidden-headers)		;So we can check the header in the other window.
  (let ((vm-forwarding-subject-format
	 vm-forwarding-complain-subject-format)
	(rec (vm-get-possible-recipients)))
    (if vm-mutable-frames
 	(vm-forward-message-all-headers-other-frame)
      (vm-forward-message-all-headers))
    (vm-insert-complain-recipient (car rec))
    (if (cdr rec)
	(progn 
	  (insert"\nCc: ")
	  (vm-insert-complain-recipient (cdr rec)))
      )
    (vm-insert-complain-message)
    (mail-position-on-field "FCC")	;Do not need to archive this...
    (delete-region (save-excursion (beginning-of-line 1) (point))
 		   (1+ (save-excursion (end-of-line 1) (point))))
    (mail-position-on-field "To")))

(defun vm-insert-complain-recipient (list)
  (let ((first t)
	(rlist list))
    (while rlist
      (if (not first) (insert ",\n    ")
	(setq first nil))
      (insert (car rlist))
      (setq rlist (cdr rlist)))))


(defun vm-get-possible-recipients ()
  (let* ((message (car vm-message-pointer))
	 to cc addr recp comp
	 (complain vm-complain-recipients))
    (while complain
      (setq comp (car complain))
      (cond ((string= "Received:" (car comp)) ;received fields are special
	     (and (setq recp (cdr comp))  
		  (setq addr (vm-get-header-contents-reverse message
							     (car comp) " "))
		  (setq to (union to (vm-complain-make-recipients-from-received
				      addr recp)
				  :test (function string=)))))
	    ((equal (car comp) t)
	     (setq cc (union cc (cdr comp)
			     :test (function string=))))
	    (t (and (setq recp (cdr comp))
		    (setq addr (vm-get-header-contents message
						       (car comp)))
		    (setq to (union to (vm-complain-make-recipients-from-address
					addr recp (car comp))
				    :test (function string=))))))
      (setq complain (cdr complain)))
    (cons to cc)))

(defun vm-insert-complain-message ()
  "Insert a complain id marker in the Header and the complain message in the
body"
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator)
			     "\n"))
  (goto-char (match-end 0))
  (forward-line -1)
  (insert vm-complain-id-header)
  (forward-line 1)
  (insert vm-complain-message))

(defun vm-complain-check-hostname (hostname)
  "Check if hostname looks valid and is complainable..."
  (if (and (string-match "^[0-9a-zA-Z-]+\\(\\.[0-9a-zA-Z-]+\\)+$" hostname)
	   (not (string-match vm-complain-nocomplain-domain hostname)))
	   hostname))

(defun vm-complain-host-from-mail-address (mail-address)
  "Parse the hostname from a mail-address and return it."
  (if (string-match "^[^@]+@\\([^ ,>]+\\)" mail-address)
      (vm-complain-check-hostname (match-string 1 mail-address))))

(defun vm-complain-make-recipient-from-host (host adress recipients field)
  (if (or (not vm-complain-interactive)
	   (y-or-n-p (concat "From " field
			     ", does the host: " host
			     (if (memq t recipients)
				 (concat " and: " adress))
			     " seems valid? ")))
      (if host (mapcar (lambda (user)
			 (cond ((eq user t) adress)
			       ((eq user 'abuse_net) (concat host "@abuse.net"))
			       (t (concat user "@" host))))
		       recipients))))

(defun vm-complain-make-recipients-from-address (address recipients field)
  "Returns a list of recipients on address's host."
  (let ((host (vm-complain-host-from-mail-address address)))
    (if host
	(progn (setq host (vm-complain-check-hostname host))
	       (if host (vm-complain-make-recipient-from-host host address recipients field))))))

(defun vm-complain-make-recipients-from-received (header recipients)
  "Returns a list of recipients on the host mentioned in the received header."
  (let ((continue t)
	(pos 0)
	host 
	hosts
	res)
    (while (and continue
		(string-match
		 "\\b\\(by\\|from\\)\\s +\\([.0-9a-zA-Z-]+@\\)?\\([.0-9a-zA-Z-]+\\)\\s *\\((\\([a-zA-Z]+[.0-9a-zA-Z-]+\\)\\s \\)?" header pos))
      (setq pos (match-end 0))
      (setq host (or (match-string 5 header) (match-string 3 header)))
; The change above was suggested by Laurent Wacrenier
; Was
;		 "\\b\\(by\\|from\\|for\\)\\s +\\([.0-9a-zA-Z-]+@\\)?\\([.0-9a-zA-Z-]+\\)\\Sw"
;		 header pos))
;      (setq pos (match-end 0))
;      (setq host (match-string 3 header))
      (if (string-match vm-complain-stop-received-collect host)
	  (setq continue nil)
	(progn
	  (setq host (vm-complain-check-hostname host))
	  (if (and host 
		   (not (member* host hosts :test (function string=))))
	      (setq hosts (cons host hosts))))))
    (while hosts
      (setq res (append (vm-complain-make-recipient-from-host
			 (car hosts) nil recipients "Received:")
			res))
      (if (and res (not vm-complain-collect-all-received))
	  (setq hosts nil)
	(setq hosts (cdr hosts))))

    res))

;;; Stolen from vm and modified

(defun vm-get-header-contents-reverse (message header-name-regexp &optional clump-sep)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)")
	  message (vm-real-message-of message))
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-text-of message))
	(let ((case-fold-search t))
	  (while (and (or (null contents) clump-sep)
		      (re-search-backward regexp (vm-headers-of message) t)
		      (save-excursion (goto-char (match-beginning 0))
				      (vm-match-header)))
	    (if contents
		(setq contents
		      (concat contents clump-sep (vm-matched-header-contents)))
	      (setq contents (vm-matched-header-contents))))))
      contents )))

(provide 'vm-complain)

;;; vm-complain.el
