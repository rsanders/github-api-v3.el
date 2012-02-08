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


;;;
;;; NOT CONVERTED TO V3 YET
;;;

;;;###autoload
(defun githubv3/send-pull-request (text recipients)
  "Send a pull request with text TEXT to RECIPIENTS.
RECIPIENTS should be a list of usernames."
  (let ((url-request-method "POST")
        (githubv3/request-data (cons (cons "message[body]" text)
                                     (mapcar (lambda (recipient)
                                               (cons "message[to][]" recipient))
                                             recipients)))
        (githubv3/api-base githubv3/github-url)
        (url-max-redirections 0) ;; GitHub will try to redirect, but we don't care
        githubv3/parse-response)
    (githubv3/retrieve (list (githubv3/repo-owner) (githubv3/repo-name)
                             "pull_request" (githubv3/name-rev-for-remote "HEAD" "origin"))
                       (lambda (_)
                         (kill-buffer)
                         (message "Your pull request was sent.")))))

(provide 'github-api-v2-obsolete)
