;;; githubv3.el --- implementation of GitHub API v3

;; Copyright (c) 2012 Robert Sanders
;; Copyright (c) 2010 Nathan Weizenbaum
;; Licensed under the same terms as Emacs.

;; Author: Robert Sanders
;; URL: http://github.com/rsanders/github-api-v3.el
;; Version: 0.1
;; Created: 2012-02-05
;; By: Nathan Weizenbaum
;; Keywords: git, github, magit, api
;; Package-Requires: ((magit "0.8") (json "1.2"))

;; Borrowing *heavily* from the partial GitHub v2 API included in Magithub:

;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/magithub
;; Version: 0.1
;; Created: 2010-06-06
;; By: Nathan Weizenbaum

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
