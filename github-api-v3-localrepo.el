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


(defun githubv3/repo-obj (&optional username repo)
  "Return an object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist)."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (remhash (cons username repo) githubv3/-repo-obj-cache)
  (githubv3/cached-repo-obj username repo))

(defun githubv3/cached-repo-obj (&optional username repo)
  "Return a (possibly cached) object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist).

This differs from `githubv3/repo-obj' in that it returns a cached
copy of the repo object if one exists.  This is useful for
properties such as :parent and :fork that are highly unlikely to
change."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (let ((cached (gethash (cons username repo) githubv3/-repo-obj-cache)))
    (or cached
        (let* ((url-request-method "GET")
               (obj (plist-get
                     (githubv3/retrieve-synchronously
                      (list "repos" "show" username repo))
                     :repository)))
          (puthash (cons username repo) obj githubv3/-repo-obj-cache)
          obj))))


;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-collaborators (&optional username repo)
  "Return an array of names of collaborators on USERNAME/REPO.
Defaults to the current repo."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (githubv3/retrieve-synchronously
      (list "repos" "show" username repo "collaborators"))
     :collaborators)))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-network (&optional username repo)
  "Return an array of forks and/or parents of USERNAME/REPO.
Defaults to the current repo.

Each fork is a decoded JSON object (plist)."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (githubv3/retrieve-synchronously
      (list "repos" "show" username repo "network"))
     :network)))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-parent-collaborators (&optional username repo)
  "Return an array of names of collaborators on the parent of USERNAME/REPO.
These are the default recipients of a pull request for this repo.
Defaults to the current repo.

If this repo has no parents, return the collaborators for it instead."
  (let ((parent (plist-get (githubv3/cached-repo-obj username repo) :parent)))
    (if (not parent) (githubv3/repo-collaborators username repo)
      (destructuring-bind (parent-owner . parent-repo) (githubv3/parse-repo parent)
        (githubv3/repo-collaborators parent-owner parent-repo)))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/untracked-forks ()
  "Return a list of forks of this repo that aren't being tracked as remotes.
Returned repos are decoded JSON objects (plists)."
  (lexical-let ((remotes (magit-git-lines "remote")))
    (delq "origin" remotes)
    (push (githubv3/repo-owner) remotes)
    (githubv3/-remove-if
     (lambda (repo) (member-ignore-case (plist-get repo :owner) remotes))
     (githubv3/repo-network))))


;;; Local Repo Information

(defun githubv3/repo-info ()
  "Return information about this GitHub repo.
This is of the form given by `githubv3/remote-info'.

Error out if this isn't a GitHub repo."
  (or (githubv3/remote-info "origin")
      (error "Not in a GitHub repo")))

(defun githubv3/repo-owner ()
  "Return the name of the owner of this GitHub repo.

Error out if this isn't a GitHub repo."
  (car (githubv3/repo-info)))

(defun githubv3/repo-name ()
  "Return the name of this GitHub repo.

Error out if this isn't a GitHub repo."
  (cadr (githubv3/repo-info)))

(defun githubv3/repo-ssh-p ()
  "Return non-nil if this GitHub repo is checked out via SSH.

Error out if this isn't a GitHub repo."
  (caddr (githubv3/repo-info)))

;;; Creating Repos

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/gist-repo (&optional private)
  "Upload the current repo as a Gist.
If PRIVATE is non-nil or with a prefix arg, the Gist is private.

Copies the URL of the Gist into the kill ring.  If
`githubv3/view-gist' is non-nil (the default), opens the gist in
the browser with `browse-url'."
  (interactive "P")
  (let ((url-max-redirections 0)
        (url-request-method "POST")
        (githubv3/api-base githubv3/gist-url)
        (githubv3/request-data
         `(,@(if private '(("private" . "1")))
           ("file_ext[gistfile1]" . ".dummy")
           ("file_name[gistfile1]" . "dummy")
           ("file_contents[gistfile1]" .
            "Dummy Gist created by GithubV3. To be replaced with a real repo.")))
        githubv3/parse-response)
    (let (url)
      (with-current-buffer (githubv3/retrieve-synchronously "gists")
        (goto-char (point-min))
        (re-search-forward "^Location: \\(.*\\)$")
        (setq url (match-string 1))
        (kill-buffer))
      (kill-new url)
      (let ((ssh-url (replace-regexp-in-string
                      "^http://gist\\.github\\.com/"
                      "git@gist.github.com:" url)))
        (magit-run-git "remote" "add" "origin" ssh-url)
        (magit-set "origin" "branch" "master" "remote")
        (magit-set "refs/heads/master" "branch" "master" "merge")
        (magit-run-git-async "push" "-v" "-f" "origin" "master")
        (when githubv3/view-gist (browse-url url))
        (message "Gist created: %s" url)))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/create-from-local (name &optional description homepage private)
  "Create a new GitHub repository for the current Git repository.
NAME is the name of the GitHub repository, DESCRIPTION describes
the repository, URL is the location of the homepage.  If PRIVATE
is non-nil, a private repo is created.

When called interactively, prompts for NAME, DESCRIPTION, and
HOMEPAGE.  NAME defaults to the name of the current Git
directory.  By default, creates a public repo; with a prefix arg,
creates a private repo."
  (interactive
   (list (read-string "Repository name: "
                      (file-name-nondirectory
                       (directory-file-name
                        (expand-file-name
                         (magit-get-top-dir default-directory)))))
         (read-string "Description: ")
         (read-string "Homepage: ")
         current-prefix-arg))
  
  (let ((url-request-method "POST")
        (githubv3/request-data `(("name" . ,name)
                                 ("description" . ,description)
                                 ("homepage" . ,homepage)
                                 ("private" . ,(if private "0" "1")))))
    (githubv3/retrieve "repos/create"
                       (lambda (data name)
                         (magit-git-string
                          "remote" "add" "origin"
                          (githubv3/repo-url (githubv3/config "user") name 'ssh))
                         (magit-set "origin" "branch" "master" "remote")
                         (magit-set "refs/heads/master" "branch" "master" "merge")
                         (magit-run-git-async "push" "-v" "origin" "master")
                         (message "GitHub repository created: %s"
                                  (plist-get (plist-get data :repository) :url)))
                       (list name))))

;;; Forking Repos

;;;
;;; NOT CONVERTED TO V3 YET
;;;


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

(defun githubv3/toggle-ssh (&optional arg)
  "Toggle whether the current repo is checked out via SSH.
With ARG, use SSH if and only if ARG is positive."
  (interactive "P")
  (if (null arg) (setq arg (if (githubv3/repo-ssh-p) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (magit-set (githubv3/repo-url (githubv3/repo-owner) (githubv3/repo-name) (> arg 0))
             "remote" "origin" "url")
  (magit-refresh-status))

(provide 'github-api-v3-localrepo)
