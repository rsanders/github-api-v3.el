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

;;; Reading Input

(defun githubv3/-lazy-completion-callback (fn &optional noarg)
  "Converts a simple string-listing FN into a lazy-loading completion callback.
FN should take a string (the contents of the minibuffer) and
return a list of strings (the candidates for completion).  This
method takes care of any caching and makes sure FN isn't called
until completion needs to happen.

If NOARG is non-nil, don't pass a string to FN."
  (lexical-let ((fn (githubv3/-cache-function fn)) (noarg noarg))
    (lambda (string predicate allp)
      (let ((strs (if noarg (funcall fn) (funcall fn string))))
        (if allp (all-completions string strs predicate)
          (try-completion string strs predicate))))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-user (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub username from the minibuffer with completion.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub user: \".  HIST defaults to
'githubv3/users-history.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users."
  (setq hist (or hist 'githubv3/users-history))
  (completing-read (or prompt "GitHub user: ")
                   (githubv3/-lazy-completion-callback
                    (lambda (s)
                      (mapcar (lambda (user) (plist-get user :name))
                              (githubv3/user-search s))))
                   predicate require-match initial-input hist def inherit-input-method))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-repo-for-user (user &optional prompt predicate require-match
                                         initial-input hist def inherit-input-method)
  "Read a GitHub repository from the minibuffer with completion.
USER is the owner of the repository.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo: <user>/\"."
  (lexical-let ((user user))
    (completing-read (or prompt (concat "GitHub repo: " user "/"))
                     (githubv3/-lazy-completion-callback
                      (lambda ()
                        (mapcar (lambda (repo) (plist-get repo :name))
                                (githubv3/repos-for-user user)))
                      'noarg)
                     predicate require-match initial-input hist def
                     inherit-input-method)))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-repo (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub user-repository pair with completion.
Return (USERNAME . REPO), or nil if the user enters no input.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo (user/repo): \".  HIST defaults to
'githubv3/repos-history.  If REQUIRE-MATCH is non-nil and the
user enters no input, raises an error.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users, and also has no way to search for users whose names
begin with certain characters."
  (setq hist (or hist 'githubv3/repos-history))
  (let ((result (completing-read
                 (or prompt "GitHub repo (user/repo): ")
                 (githubv3/-lazy-completion-callback 'githubv3/-repo-completions)
                 predicate require-match initial-input hist def inherit-input-method)))
    (if (string= result "")
        (when require-match (error "No repository given"))
      (githubv3/parse-repo result))))

(defun githubv3/-repo-completions (string)
  "Try completing the given GitHub user/repository pair.
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (destructuring-bind (username . rest) (split-string string "/")
    (if (not rest) ;; Need to complete username before we start completing repo
        (mapcar (lambda (user) (concat (plist-get user :name) "/"))
                (githubv3/user-search username))
      (if (not (string= (car rest) ""))
          (githubv3/-use-cache (concat username "/"))
        (mapcar (lambda (repo) (concat username "/" (plist-get repo :name)))
                (githubv3/repos-for-user username))))))

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
