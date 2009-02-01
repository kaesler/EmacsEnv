;; Code to destructively convert AZ mail folders to VM format.
;;

;; Reuse handy code for date manipulation.
;;
(require 'calendar)

;; Reuse handy code for email address analysis
;;
(require 'mail-extr)

;; The regexp to match the AZ header is divided into these fields:
;;   1   yyyy  (year)
;;   2   mm    (month)
;;   3   dd    (day)
;;   4   hh    (hour)
;;   5   mm    (minute)
;;   6   ss    (second)
;;   7   size  (size)
;;
(defvar az-header-re "^\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\),\\([0-9]+\\);")

;; This is often found at the end of an AZ folder.
;;
(defvar az-end-of-folder-regexp "-------")

(defconst az-to-vm-work-buffer-name "*az-to-vm work buffer*")

;; The command to do a whole directory
;;
(defun az-to-vm-directory (directory)
  (interactive "DAZ folder directory: ")

  ;; Get a list of files, ignoring backups, autosaves and ".", "..".
  ;;
  (let ((file-list (directory-files directory
                                    'full
                                    "[^.#].*[^~]$"
                                    'nosort)))
    (mapcar 'az-to-vm file-list)))
     
;; The command to do a single folder.
;;
(defun az-to-vm (az-folder)
  (interactive "f")
  
  ;; Check that the AZ folder exists and is readable.
  ;;
  (if (not (file-exists-p az-folder))
      (error "File %s does not exist." az-folder))
  (if (not (file-readable-p az-folder))
      (error "File %s is not readable." az-folder))

  ;; Read it in to work buffer
  ;;
  (save-excursion
    (set-buffer (get-buffer-create az-to-vm-work-buffer-name))
    (erase-buffer)
    (insert-file-contents az-folder)
    (goto-char (point-min))
    
    (while (and (not (looking-at az-end-of-folder-regexp))
                (not (eobp)))
      
      ;; We should be looking at an AZ-style message header
      ;; like "1991/04/01.09:17:30,1461;01"
      ;;
      (if (not (looking-at az-header-regexp))
          (error "Not in AZ format"))
      (let* (
             ;; (month day year) for calendar.el functions to manipulate
             ;;
             (message-date (list
                            (string-to-int (buffer-substring (match-beginning 2)
                                                             (match-end 2)))
                            (string-to-int (buffer-substring (match-beginning 3)
                                                             (match-end 3)))
                            (string-to-int (buffer-substring (match-beginning 1)
                                              (match-end 1)))))
             (message-time (concat
                            (buffer-substring (match-beginning 4)
                                              (match-end 4))
                            ":"
                            (buffer-substring (match-beginning 5)
                                              (match-end 5))
                            ":"
                            (buffer-substring (match-beginning 6)
                                              (match-end 6))))
             (message-size (string-to-int (buffer-substring (match-beginning 7)
                                                            (match-end 7))))
             (message-beginning (set-marker (make-marker) (save-excursion
                                                            (forward-line 1)
                                                            (point))))
             (message-end (set-marker (make-marker) (save-excursion
                                                      (forward-line 1)
                                                      (forward-char message-size)
                                                      (point))))
             (message-from (save-excursion
                             (save-restriction
                               (forward-line 1)
                               (narrow-to-region message-beginning message-end)
                               (if (re-search-forward "^From:[ 	]*\\(.*\\)$" nil t)
                                   (let* ((raw-address-string (buffer-substring (match-beginning 1) (match-end 1)))
                                          (tuple (mail-extract-address-components raw-address-string)))
                                     (if tuple
                                         (nth 1 tuple)
                                       "unknown"))
                                 "unknown")))))
        
        ;; Delete the AZ header line
        ;;
        (kill-line 1)

        ;; Insert a VM header, like "From esler@apollo.hp.com Wed Jul 13 12:00:37 1994"
        ;;
        (if (not (bobp))
            (insert "\n"))
        (insert (format "From %s %s %s %s %s %s\n"
                        message-from
                        (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
                              (calendar-day-of-week message-date))
                        (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
                              (nth 0 message-date))
                        (let ((day-of-month (nth 1 message-date)))
                          (if (< day-of-month 1)
                              (format " %d" day-of-month)
                            (format "%d" day-of-month)))
                        message-time
                        (int-to-string (nth 2 message-date))))

        (forward-char message-size)))

    ;; Now rename the folder by adding a ".az" suffix.
    ;;
    ;;(rename-file az-folder (concat az-folder ".az") 1)

    ;; Now write the new folder.
    ;;
    (write-file az-folder)))




