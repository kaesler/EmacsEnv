(require 's)
(require 'magit)
(require 'magithub-issue)
(require 'magithub-pr)
(require 'magithub-label)

(defun magithub-issue-refresh (even-if-offline)
  "Refresh issues for this repository.
If EVEN-IF-OFFLINE is non-nil, we'll still refresh (that is,
we'll hit the API) if Magithub is offline."
  (interactive "P")
  (let ((magithub-cache (if even-if-offline nil magithub-cache)))
    (magithub-cache-without-cache :issues
      (ignore (magithub--issue-list))))
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defvar magit-magithub-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    (define-key map "L" #'magithub-issue-add-labels)
    (define-key map "N" #'magithub-issue-personal-note)
    map)
  "Keymap for `magithub-issue' sections.")

(defvar magit-magithub-issue-list-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-issue-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-issue-list' sections.")

(defvar magit-magithub-pull-request-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-pull-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    (define-key map "L" #'magithub-issue-add-labels)
    map)
  "Keymap for `magithub-pull-request' sections.")

(defvar magit-magithub-pull-request-list-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magithub-pull-browse)
    (define-key map [remap magit-refresh] #'magithub-issue-refresh)
    map)
  "Keymap for `magithub-pull-request-list' sections.")

;;; By maintaining these as lists of functions, we're setting
;;; ourselves up to be able to dynamically apply new filters from the
;;; status buffer (e.g., 'bugs' or 'questions' assigned to me)
(defcustom magithub-issue-issue-filter-functions nil
  "List of functions that filter issues.
Each function will be supplied a single issue object.  If any
function returns nil, the issue will not be listed in the status
buffer."
  :type '(repeat function)
  :group 'magithub)

(defcustom magithub-issue-pull-request-filter-functions nil
  "List of functions that filter pull-requests.
Each function will be supplied a single issue object.  If any
function returns nil, the issue will not be listed in the status
buffer."
  :type '(repeat function)
  :group 'magithub)

(defun magithub-issue-add-labels (issue labels)
  "Update ISSUE's labels to LABELS."
  (interactive
   (when (magithub-verify-manage-labels t)
     (let* ((fmt (lambda (l) (alist-get 'name l)))
            (issue (or (magithub-thing-at-point 'issue)
                       (magithub-thing-at-point 'pull-request)))
            (current-labels (alist-get 'labels issue))
            (to-remove (magithub--completing-read-multiple
                        "Remove labels: " current-labels fmt)))
       (setq current-labels (cl-set-difference current-labels to-remove))
       (list issue (magithub--completing-read-multiple
                    "Add labels: " (magithub-label-list) fmt
                    nil nil current-labels)))))
  (when (ghubp-patch-repos-owner-repo-issues-number
         (magithub-repo) issue `((labels . ,labels)))
    (setcdr (assq 'labels issue) labels))
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defun magithub-issue--label-string (issue)
  (let-alist issue
    (mapconcat #'magithub-label-propertize .labels " ")))

(defun magithub-issue--format (issue justify type)
  (magithub--object-propertize type issue
    (let-alist issue
      (let* ((fc fill-column)
             (issue-format
              (format " %%%ds %%%ds %%s "
                      (alist-get 'number justify)
                      (+ 2 (alist-get 'comments justify))))
             (issue-prefix
              (format issue-format
                      (number-to-string .number)
                      (if (= .comments 0) ""
                        (format "(%d)" .comments))
                      (if (magithub-issue-has-personal-note-p issue)
                          "N" " ")))

             (issue-title-width (- fc (length issue-prefix)))
             (indent (make-string (length issue-prefix) ?\ )))
        (with-temp-buffer
          (save-excursion
            (insert issue-prefix (s-word-wrap issue-title-width .title)))

          (save-excursion
            (forward-line)
            (while (not (eobp))
              (insert indent)
              (forward-line)))

          (save-excursion
            (move-to-column fc t)
            (insert (magithub-issue--label-string issue)))
          (concat (s-trim-right (buffer-string)) "\n"))))))

(defun magithub-issue--format-justify ()
  (let* ((issue-list (magithub--issue-list))
         (fn1 (lambda (p i) (length (format "%d" (alist-get p i)))))
         (fn2 (lambda (p) (apply #'max (mapcar (apply-partially fn1 p) issue-list)))))
    `((number . ,(funcall fn2 'number))
      (comments . ,(funcall fn2 'comments)))))

(defun magithub-issue--insert (issue is-pr)
  "Insert ISSUE as a Magit section into the buffer."
  (when issue
    (let* ((justify (magithub-issue--format-justify))
           (issue-string (magithub-issue--format issue justify (if is-pr 'pull-request 'issue))))
      (if is-pr (magit-insert-section (magithub-pull-request issue)
                  (insert issue-string))
        (magit-insert-section (magithub-issue issue)
          (insert issue-string))))))

(defun magithub-issue--insert-issue-section ()
  "Insert GitHub issues if appropriate."
  (when (magithub-usable-p)
    (magithub-issue--insert-generic-section
     (magithub-issues-list)
     "Issues"
     (magithub-issues)
     magithub-issue-issue-filter-functions
     (lambda (i) (magithub-issue--insert i nil)))))

(defun magithub-issue--insert-pr-section ()
  "Insert GitHub pull requests if appropriate."
  (when (magithub-usable-p)
    (magithub-feature-maybe-idle-notify
     'pull-request-merge
     'pull-request-checkout)
    (magithub-issue--insert-generic-section
     (magithub-pull-requests-list)
     "Pull Requests"
     (magithub-pull-requests)
     magithub-issue-pull-request-filter-functions
     (lambda (i) (magithub-issue--insert i t)))))

(defmacro magithub-issue--insert-generic-section
    (spec title list filters handler)
  (let ((sym-filtered (cl-gensym)))
    `(when-let ((,sym-filtered (magithub-filter-all ,filters ,list)))
       (magit-insert-section ,spec
         (insert (format "%s%s:"
                         (propertize ,title 'face 'magit-header-line)
                         (if ,filters
                             (propertize " (filtered)" 'face 'magit-dimmed)
                           "")))
         (magit-insert-heading)
         (mapc ,handler ,sym-filtered)
         (insert ?\n)))))

(defun magithub-issue-browse (issue)
  "Visits ISSUE in the browser.
Interactively, this finds the issue at point."
  (interactive (list (or (magithub-thing-at-point 'issue)
                         (magithub-issue-completing-read-issues))))
  (magithub-issue--browse issue))

(defun magithub-pull-browse (pr)
  "Visits PR in the browser.
Interactively, this finds the pull request at point."
  (interactive (list (or (magithub-thing-at-point 'pull-request)
                         (magithub-issue-completing-read-pull-requests))))
  (magithub-issue--browse pr))

(defun magithub-issue--browse (issue-or-pr)
  "Visits ISSUE-OR-PR in the browser.
Interactively, this finds the issue at point."
  (when-let ((url (alist-get 'html_url issue-or-pr)))
    (browse-url url)))

(defun magithub-repolist-column-issue (_id)
  "Insert the number of open issues in this repository."
  (when (magithub-usable-p)
    (number-to-string (length (magithub-issues)))))

(defun magithub-repolist-column-pull-request (_id)
  "Insert the number of open pull requests in this repository."
  (when (magithub-usable-p)
    (number-to-string (length (magithub-pull-requests)))))

(magithub--deftoggle magithub-toggle-issues
  magit-status-sections-hook #'magithub-issue--insert-issue-section "issues")
(magithub--deftoggle magithub-toggle-pull-requests
  magit-status-sections-hook #'magithub-issue--insert-pr-section "pull requests")

(magithub-toggle-pull-requests)
(magithub-toggle-issues)

(provide 'magithub-issue-status)
