;;;
;;; NOT CONVERTED TO V3 YET - may not be supported
;;;

;; (defun githubv3/user-search (user)
;;   "Run a GitHub user search for USER.
;; Return an array of all matching users.

;; WARNING: WARNING: This function currently doesn't work fully,
;; since GitHub's user search API only returns an apparently random
;; subset of users."
;;   (if (string= user "") []
;;     (let ((url-request-method "GET"))
;;       (plist-get
;;        (githubv3/retrieve-synchronously
;;         (list "user" "search" user))
;;        :users))))

(provide 'github-api-v2-obsolete)
