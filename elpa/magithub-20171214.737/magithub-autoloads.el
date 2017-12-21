;;; magithub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magithub" "../../../../.emacs.d/elpa/magithub-20171214.737/magithub.el"
;;;;;;  "f11e8950dc8c63f8147cbdb119fc928f")
;;; Generated autoloads from ../../../../.emacs.d/elpa/magithub-20171214.737/magithub.el
 (autoload 'magithub-dispatch-popup "magithub" nil t)

;;;***

;;;### (autoloads nil "magithub-comment" "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-comment.el"
;;;;;;  "6e97482694b82b1e98bec6c7c1047526")
;;; Generated autoloads from ../../../../.emacs.d/elpa/magithub-20171214.737/magithub-comment.el

(autoload 'magithub-comment-new "magithub-comment" "\
Comment on ISSUE in a new buffer.
If prefix argument DISCARD-DRAFT is specified, the draft will not
be considered.

If INITIAL-CONTENT is specified, it will be inserted as the
initial contents of the reply if there is no draft.

\(fn ISSUE &optional DISCARD-DRAFT INITIAL-CONTENT)" t nil)

;;;***

;;;### (autoloads nil "magithub-dash" "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-dash.el"
;;;;;;  "c56a112904ea71de44a5724ba89df614")
;;; Generated autoloads from ../../../../.emacs.d/elpa/magithub-20171214.737/magithub-dash.el

(autoload 'magithub-dashboard "magithub-dash" "\
View your Github dashboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magithub-edit-mode" "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-edit-mode.el"
;;;;;;  "8e44c2497dd4989361cf6def577b98d7")
;;; Generated autoloads from ../../../../.emacs.d/elpa/magithub-20171214.737/magithub-edit-mode.el

(autoload 'magithub-edit-mode "magithub-edit-mode" "\
Major mode for editing Github issues and pull requests.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magithub-issue-tricks" "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-tricks.el"
;;;;;;  "567723848f4c8ee13952e1e2fb8bd11a")
;;; Generated autoloads from ../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-tricks.el

(autoload 'magithub-pull-request-merge "magithub-issue-tricks" "\
Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default.

\(fn PULL-REQUEST &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "magithub-issue-view" "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-view.el"
;;;;;;  "9a6d2528da3df20dd981d93dc2113056")
;;; Generated autoloads from ../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-view.el

(autoload 'magithub-issue-view "magithub-issue-view" "\
View ISSUE in a new buffer.

\(fn ISSUE)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/magithub-20171214.737/magithub-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-ci.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-comment.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-core.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-dash.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-edit-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-faces.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-post.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-tricks.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue-view.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-issue.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-label.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-notification.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-orgs.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-proxy.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-repo.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub-user.el"
;;;;;;  "../../../../.emacs.d/elpa/magithub-20171214.737/magithub.el")
;;;;;;  (23098 44533 973864 491000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magithub-autoloads.el ends here
