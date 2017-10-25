;;; magithub-user.el --- Inspect users  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Code for dealing with the current users and other users

;;; Code:

(require 'ghub+)
(require 'cl-lib)

(require 'magithub-core)

(defvar magit-magithub-user-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] #'magithub-user-visit)
    m))

(defvar magit-magithub-assignee-section-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m magit-magithub-user-section-map)
    (define-key m "a" #'magithub-assignee-add)
    (define-key m [remap magit-delete-thing] #'magithub-assignee-remove)
    m))

(defun magithub-user-me ()
  "Return the currently-authenticated user."
  (magithub-cache :user-demographics
    '(ghubp-get-user)
    "user object for the currently-authenticated user"
    :context nil))

(defun magithub-user (user)
  "Return the full object for USER."
  (ghubp-get-users-username user))

(defun magithub-assignee-add (issue user)
  (interactive (let ((issue (magit-section-parent-value (magit-current-section))))
                 (list issue
                       (magithub-user-choose-assignee
                        "Choose an assignee: "
                        (magithub-issue-repo issue)))))
  (let-alist `((repo . ,(magithub-issue-repo issue))
               (issue . ,issue)
               (user . ,user))
    (if (yes-or-no-p (format "Assign '%s' to %s#%d? "
                             .user.login
                             (magithub-repo-name .repo)
                             .issue.number))
        (prog1 (ghubp-post-repos-owner-repo-issues-number-assignees
                .repo .issue (list .user))
          (let ((sec (magit-current-section)))
            (magithub-cache-without-cache :issues
              (magit-refresh-buffer))
            (magit-section-show sec)))
      (user-error "Aborted"))))

(defun magithub-assignee-remove (issue user)
  (interactive (list (magit-section-parent-value (magit-current-section))
                     (magit-section-value (magit-current-section))))
  (let-alist `((repo . ,(magithub-issue-repo issue))
               (issue . ,issue)
               (user . ,user))
    (if (yes-or-no-p (format "Remove '%s' from %s#%d? "
                             .user.login
                             (magithub-repo-name .repo)
                             .issue.number))
        (prog1
            (ghubp-delete-repos-owner-repo-issues-number-assignees .repo .issue (list .user))
          (magithub-cache-without-cache :issues
            (magit-refresh-buffer)))
      (user-error "Aborted"))))

(defun magithub-user-choose (prompt &optional default-user)
  (let (ret-user new-username)
    (while (not ret-user)
      (setq new-username
            (magit-read-string-ns
             (concat prompt
                     (if new-username (format " ['%s' not found]" new-username)))
             (alist-get 'login default-user)))
      (when-let ((try (condition-case err
                          (ghubp-get-users-username `((login . ,new-username)))
                        (ghub-404 nil))))
        (setq ret-user try)))
    ret-user))

(defun magithub-user-choose-assignee (prompt &optional repo default-user)
  (magithub--completing-read
   prompt
   (ghubp-get-repos-owner-repo-assignees repo)
   (lambda (user) (let-alist user .login))
   nil t default-user))

(defun magithub-user-visit (user)
  "Visit USER."
  (interactive (list (magit-section-value (magit-current-section))))
  (if user
      (browse-url (alist-get 'html_url user))
    (user-error "No user here")))

(provide 'magithub-user)
;;; magithub-user.el ends here
