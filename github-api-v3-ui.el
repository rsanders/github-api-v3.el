;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-pull-request-recipients ()
  "Read a list of recipients for a GitHub pull request."
  (let ((collabs (githubv3/repo-parent-collaborators))
        (network (githubv3/repo-network)))
    (githubv3/-remove-if
     (lambda (s) (string= s ""))
     (completing-read-multiple
      "Send pull request to: "
      (mapcar (lambda (repo) (plist-get repo :owner)) (githubv3/repo-network))
      nil nil (concat (mapconcat 'identity collabs crm-separator)
                      (if (= (length collabs) (length network)) "" crm-separator))))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-untracked-fork ()
  "Read the name of a fork of this repo that we aren't yet tracking.
This will accept either a username or a username/repo pair,
and return (USERNAME . REPONAME)."
  (let ((fork
         (completing-read
          "Track fork (user or user/repo): "
          (githubv3/-lazy-completion-callback
           (lambda ()
             (mapcar (lambda (repo) (concat (plist-get repo :owner) "/"
                                       (plist-get repo :name)))
                     (githubv3/untracked-forks)))
           'noarg)
          nil nil nil 'githubv3/repos-history)))
    (cond
     ((string= fork "") (error "No fork given"))
     ((string-match "/" fork) (githubv3/parse-repo fork))
     (t (cons fork (githubv3/repo-name))))))

(provide 'github-api-v3-ui)
